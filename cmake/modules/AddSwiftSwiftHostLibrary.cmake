# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

# Add a new host library written in Swift with the given name.
function(add_swift_swift_host_library name)
  cmake_parse_arguments(ASHL
                        "" # flags
                        "COMPONENT" # single var args
                        "" # multi var args
                        ${ARGN})
  set(ASHL_SOURCES ${ASHL_UNPARSED_ARGUMENTS})

  # Create the library target.
  add_library(${name} SHARED ${ASHL_SOURCES})


  # Determine where Swift modules will be built and installed.
  set(module_triple ${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_ARCH_${SWIFT_HOST_VARIANT_ARCH}_MODULE})
  set(module_dir "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/swift/host/")
  set(module_base "${module_dir}/$<TARGET_LINKER_FILE_BASE_NAME:${name}>.swiftmodule")
  set(module_file "${module_base}/${module_triple}.swiftmodule")
  set(module_interface_file "${module_base}/${module_triple}.swiftinterface")
  set(module_sourceinfo_file "${module_base}/${module_triple}.swiftsourceinfo")

  # Set the appropriate target triple.
  if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_DARWIN_PLATFORMS)
    set(DEPLOYMENT_VERSION "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
  elseif(SWIFT_HOST_VARIANT_SDK STREQUAL ANDROID)
    set(DEPLOYMENT_VERSION ${SWIFT_ANDROID_API_LEVEL})
  endif()
  get_target_triple(target target_variant "${SWIFT_HOST_VARIANT_SDK}" "${SWIFT_HOST_VARIANT_ARCH}"
    MACCATALYST_BUILD_FLAVOR ""
    DEPLOYMENT_VERSION "${DEPLOYMENT_VERSION}")

  target_compile_options(${name} PRIVATE $<$<COMPILE_LANGUAGE:Swift>:-target;${target}>)

  # Add a custom target to create the module directory.
  add_custom_command(
      TARGET ${name}
      PRE_BUILD
      COMMAND "${CMAKE_COMMAND}" -E make_directory ${module_base}
      COMMENT "Generating module directory for ${name}")

  # Touch the library and objects to workaround their mtime not being updated
  # when there are no real changes (eg. a file was updated with a comment).
  # Ideally this should be done in the driver, which could only update the
  # files that have changed.
  add_custom_command(
      TARGET ${name}
      POST_BUILD
      COMMAND "${CMAKE_COMMAND}" -E touch_nocreate $<TARGET_FILE:${name}> $<TARGET_OBJECTS:${name}>
      COMMAND_EXPAND_LISTS
      COMMENT "Update mtime of library outputs workaround")

  # Install the Swift module into the appropriate location.
  set_target_properties(${name}
    PROPERTIES Swift_MODULE_DIRECTORY ${module_dir}
  )

  # Configure the emission of the Swift module files.
  target_compile_options("${name}" PRIVATE
    $<$<COMPILE_LANGUAGE:Swift>:
      -module-name;$<TARGET_LINKER_FILE_BASE_NAME:${name}>;
      -enable-library-evolution;
      -emit-module-path;${module_file};
      -emit-module-source-info-path;${module_sourceinfo_file};
      -emit-module-interface-path;${module_interface_file}
      >)

  # NOTE: workaround for CMake not setting up include flags yet
  set_target_properties(${name} PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES ${module_dir}
  )

  set_target_properties(${name} PROPERTIES
    BUILD_WITH_INSTALL_RPATH YES
  )

  if(NOT ${ASHL_SWIFT_COMPONENT} STREQUAL "no_component")
    # Install this target
    swift_install_in_component(TARGETS ${name}
        ARCHIVE DESTINATION lib/${SWIFT_HOST_LIBRARIES_SUBDIRECTORY}
        LIBRARY DESTINATION lib/${SWIFT_HOST_LIBRARIES_SUBDIRECTORY}
        RUNTIME DESTINATION bin
        COMPONENT ${ASHL_SWIFT_COMPONENT}
    )

    # Install the module files.
    swift_install_in_component(DIRECTORY ${module_base}
        DESTINATION lib/${SWIFT_HOST_LIBRARIES_SUBDIRECTORY}
        FILES_MATCHING PATTERN "*.swiftinterface"
        COMPONENT ${ASHL_SWIFT_COMPONENT}
    )
  endif()
endfunction()

# Workaround a cmake bug, see the corresponding function in swift-syntax
function(force_target_link_libraries TARGET)
  cmake_parse_arguments(ARGS "" "" "PUBLIC" ${ARGN})

  foreach(DEPENDENCY ${ARGS_PUBLIC})
    target_link_libraries(${TARGET} PRIVATE
        ${DEPENDENCY}
    )
    add_dependencies(${TARGET} ${DEPENDENCY})

    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
        COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
        DEPENDS ${DEPENDENCY}
    )
    target_sources(${TARGET} PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}/forced-${DEPENDENCY}-dep.swift
    )
  endforeach()
endfunction()

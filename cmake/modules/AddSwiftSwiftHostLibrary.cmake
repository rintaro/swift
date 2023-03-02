# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors


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

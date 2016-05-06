# CMake script that writes Git revision information to a header.
# NOTE: Not for include.
#
# Input variables:
#   REPOSITORY_ROOT - root directory of the repository work tree
#   NAME            - The macro prefix for the repository's info
#   HEADER_FILE     - The header file to write
#   SOURCE_PATH     - Semi-colon separated list of paths from where query revision
#
# The output header will contain macros ${NAME}_REPOSITORY and ${NAME}_REVISION,

if(NOT DEFINED SOURCE_PATH)
    set(SOURCE_PATH "${REPOSITORY_ROOT}")
endif()

find_program(git_executable NAMES git git.exe git.cmd)
if(git_executable)
    execute_process(
        COMMAND
        ${git_executable} log -1 --pretty=format:%H -- ${SOURCE_PATH}
        WORKING_DIRECTORY ${REPOSITORY_ROOT}
        TIMEOUT 5
        RESULT_VARIABLE git_result
        OUTPUT_VARIABLE git_output)
    if (git_result EQUAL 0)
        string(STRIP "${git_output}" revision)
    endif()
    execute_process(
        COMMAND
        ${git_executable} remote -v
        WORKING_DIRECTORY ${REPOSITORY_ROOT}
        TIMEOUT 5
        RESULT_VARIABLE git_result
        OUTPUT_VARIABLE git_output)
    if (git_result EQUAL 0)
        string(REGEX REPLACE "^(.*\n)?[^ \t]+[ \t]+([^ \t\n]+)[ \t]+\\(fetch\\).*"
            "\\2" git_url "${git_output}")
        string(STRIP "${git_url}" repository)
    endif()
endif()

if(DEFINED revision AND DEFINED repository)
    file(APPEND "${HEADER_FILE}.tmp"
        "#define ${NAME}_REVISION \"${revision}\"\n")
    file(APPEND "${HEADER_FILE}.tmp"
        "#define ${NAME}_REPOSITORY \"${repository}\"\n")
    execute_process(COMMAND
        ${CMAKE_COMMAND} -E copy_if_different "${HEADER_FILE}.tmp" "${HEADER_FILE}")
endif()

# Remove tmp file unconditionally
file(REMOVE "${HEADER_FILE}.tmp")

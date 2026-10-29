cmake_minimum_required (VERSION 3.14)

project (descur C Fortran)

add_executable (xdescur)
add_dependencies (xdescur pull_descur)

target_link_libraries (xdescur PUBLIC stell)

add_subdirectory (Sources)

install (TARGETS xdescur)

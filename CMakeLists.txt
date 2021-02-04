cmake_minimum_required (VERSION 3.14)

project (descur C Fortran)

add_executable (xdescur)
add_dependencies (xdescur stell)

target_link_libraries (xdescur stell)
target_include_directories (xdescur PUBLIC $<TARGET_PROPERTY:stell,BINARY_DIR>)

add_subdirectory (Sources)

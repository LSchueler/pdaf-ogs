###################
### OGS-PDAF ######
###################

cmake_minimum_required(VERSION 3.10)
project(OGS-PDAF Fortran)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(BUILD_SHARED_LIBS ON)

add_subdirectory(pdaf_base/src)
add_subdirectory(pdaf_user)

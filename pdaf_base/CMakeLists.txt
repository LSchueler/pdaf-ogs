########################
### OGS-PDAF-Base ######
########################

cmake_minimum_required(VERSION 3.10)

set(PDAF_FLAGS "-fdefault-real-8 -fPIC -O3")
project(OGS-PDAF-Base Fortran)

add_definitions(${PDAF_FLAGS})

add_subdirectory(src)

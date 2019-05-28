###################
### OGS-PDAF ######
###################

cmake_minimum_required(VERSION 3.10)

set(PDAF_FLAGS "-fPIC")
project(OGS-PDAF Fortran)

add_definitions(${PDAF_FLAGS})

add_subdirectory(pdaf_user)

cmake_minimum_required(VERSION 3.10)

file(GLOB PDAF_USER_SOURCES *.F90)

include_directories(dummympi)

add_library(PDAF_user SHARED ${PDAF_USER_SOURCES})
set_target_properties(PDAF_user PROPERTIES PREFIX "")
set_target_properties(PDAF_user PROPERTIES OUTPUT_NAME "pdaf")

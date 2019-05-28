file(GLOB PDAF_BASE_SOURCES *.F90)

include_directories(dummympi)

add_library(PDAF_base ${PDAF_BASE_SOURCES})

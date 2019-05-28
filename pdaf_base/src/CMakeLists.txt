file(GLOB PDAF_BASE_SOURCES *.F90)

add_library(PDAF_base STATIC ${PDAF_BASE_SOURCES})
target_include_directories(PDAF_base PRIVATE dummympi)
target_compile_options(PDAF_base PRIVATE "-fdefault-real-8")

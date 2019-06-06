file(GLOB PDAF_BASE_SOURCES *.F90)

find_package(LAPACK REQUIRED)
find_package(BLAS REQUIRED)

add_library(PDAF_base STATIC ${PDAF_BASE_SOURCES})
target_include_directories(PDAF_base PRIVATE dummympi)
target_link_libraries(PDAF_base PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
target_compile_options(PDAF_base PRIVATE "-fdefault-real-8")

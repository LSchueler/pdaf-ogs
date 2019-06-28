file(GLOB PDAF_USER_SOURCES *.F90)

find_package(MPI REQUIRED)
find_package(LAPACK REQUIRED)
find_package(BLAS REQUIRED)

add_library(pdaf ${PDAF_USER_SOURCES})
target_include_directories(pdaf PRIVATE dummympi)
target_link_libraries(pdaf PRIVATE ${MPI_LIBRARIES} ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
target_link_libraries(pdaf PRIVATE PDAF_base)
target_compile_options(pdaf PRIVATE "-fdefault-real-8")

if(APPLE AND BUILD_SHARED_LIBS)
    target_link_libraries(pdaf PUBLIC "-framework Accelerate")
endif()

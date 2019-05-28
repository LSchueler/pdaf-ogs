file(GLOB PDAF_USER_SOURCES *.F90)

add_library(pdaf ${PDAF_USER_SOURCES})
target_include_directories(pdaf PRIVATE dummympi)
target_link_libraries(pdaf PRIVATE PDAF_base)

if(APPLE AND BUILD_SHARED_LIBS)
    target_link_libraries(pdaf PUBLIC "-framework Accelerate")
endif()

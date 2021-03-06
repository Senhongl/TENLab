cmake_minimum_required(VERSION 3.0)

set(CMAKE_C_COMPILER "gcc")
set(CMAKE_CXX_COMPILER "g++")
set(CMAKE_ASM_COMPILER "gcc")
set(SOURCE_FILE "" CACHE FILEPATH "path to source file")

project(tenlab_func)

set(CMAKE_PREFIX_PATH "./libtorch")
find_package(Torch REQUIRED)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${TORCH_CXX_FLAGS}")

enable_language(ASM)

file(GLOB SOURCES "lib/*.cc")
add_library(func OBJECT ${SOURCES})
target_link_libraries(func "${TORCH_LIBRARIES}")
set_property(TARGET func PROPERTY CXX_STANDARD 14)

set(EXECUTABLE_OUTPUT_PATH "..")

add_executable(${SOURCE_FILE}.exe $<TARGET_OBJECTS:func> ${SOURCE_FILE}.s)
target_link_libraries(${SOURCE_FILE}.exe "${TORCH_LIBRARIES}")
set_property(TARGET ${SOURCE_FILE}.exe PROPERTY CXX_STANDARD 14)
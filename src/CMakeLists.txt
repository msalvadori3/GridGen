cmake_minimum_required(VERSION 2.6)

project(GridGen)

enable_language(Fortran)

include(CMakeDependentOption)
include(${PROJECT_SOURCE_DIR}/cmake/functions.cmake)

set(GRIDGEN_MAJOR_VERSION 1)
set(GRIDGEN_MINOR_VERSION 0)
set(GRIDGEN_PATCH_VERSION 0)
set(GRIDGEN_VERSION
				${GRIDGEN_MAJOR_VERSION}.${GRIDGEN_MINOR_VERSION}.${GRIDGEN_PATCH_VERSION})

set(BUILD_TYPE_AVAIL Release Debug)
set(BUILD_TYPE_DESC "Choose the type of build, options are: ")
foreach(Item ${BUILD_TYPE_AVAIL})
				set(BUILD_TYPE_DESC "${BUILD_TYPE_DESC} ${item}")
endforeach()
if(NOT CMAKE_BUILD_TYPE)
				set(CMAKE_BUILD_TYPE Release)
endif()
validate_option(CMAKE_BUILD_TYPE
				BUILD_TYPE_AVAIL
				Release
				STRING
				${BUILD_TYPE_DESC})

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
				set(CMAKE_Fortran_FLAGS_DEBUG
								"-Wall -O0 -fcheck=recursion -fcheck=bounds -fcheck=do -fcheck=mem -fcheck=pointer -g -fbacktrace -Wno-unused-variable -Wno-unused-dummy-argumet -Wno-unused-fuction -Wall -ffpe-trap=zero,overflow,underflow,denormal -Wno-unused-label" CACHE STRING "" FORCE)
				mark_as_advanced(CMAKE_Fortran_FLAGS_DEBUG)
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Release")
				if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
								set(CMAKE_Fortran_FLAGS_RELEASE
												"-O3 -fbacktrace" CACHE STRING "" FORCE)
								mark_as_advanced(CMAKE_Fortran_FLAGS_RELEASE)
								exec_program(${CMAKE_Fortran_COMPILER}
												ARGS --version
												OUTPUT_VARIABLE _compiler_output)
								string(REGEX REPLACE ".* ([0-9]+\\.[0-9]+\\.[0-9]+) .*" "\\1"
												gnu_compiler_version ${_compiler_output})
								string(SUBSTRING ${gnu_compiler_version} 0 3 gnu_compiler_major_version)
								message(STATUS "GNU Fortran compiler version: ${gnu_compiler_version}")
								if(${gnu_compiler_major_version} LESS 4.9)
												add_definitions(-DPURE)
								endif()
				endif()
endif()


if (CMAKE_BUILD_TYPE STREQUAL "Debug")
				add_definitions(-DPURE)
endif()

#find xml-fortran path

find_package(xml-fortran
				1.5.0
				HINTS $ENV{XML_FORTRAN_ROOT})
include_directories(${XML-FORTRAN_INCLUDE_DIRS})
link_directories(${XML-FORTRAN_LIBRARY_DIRS})

get_directory_property(GRIDGEN_PROPS COMPILE_DEFINITIONS)
#
# add sub-directories where main files are located
#
add_subdirectory(modules)
add_subdirectory(io)
add_subdirectory(main)
add_subdirectory(math)
add_subdirectory(utils)

#
# set executable file name
#
set(GridGen_EXE_NAME grid.x CACHE STRING "Grid executable name")

#
# set source files
#
set(GridGen_SRC_FILES 
				${MODULES_SRC_FILES}
				${IO_SRC_FILES}
				${MAIN_SRC_FILES}
				${MATH_SRC_FILES}
				${UTILS_SRC_FILES}
				${PROJECT_BINARY_DIR}/input_file.f90
				)
#
# define executable
#
add_executable(${GridGen_EXE_NAME} ${GridGen_SRC_FILES})
target_link_libraries(${GridGen_EXE_NAME} xmlparse)
#target_link_libraries(${GridGen_EXE_NAME} ${XML-FORTRAN_LIBRARY})
set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR}/../
				CACHE STRING "Install path prefix, prepend onto install directories." FORCE)
install(TARGETS ${GridGen_EXE_NAME}
				RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
set_target_properties(${GridGen_EXE_NAME} PROPERTIES OUTPUT_NAME ${GridGen_EXE_NAME})


# ---------------------------------------------------------
# General options
# ---------------------------------------------------------

PROJECT(EDL-comp Fortran)

#Check version of cmake
cmake_minimum_required(VERSION 2.6)

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib)
SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/includes)

# Type of build
#message( STATUS "Build type : " ${CMAKE_BUILD_TYPE})

# ---------------------------------------------------------
# Compiler options
# ---------------------------------------------------------

# ---------------------------------------------------------
# Libraries
# ---------------------------------------------------------

# class meshbuilder
SET(PREC prec)
ADD_LIBRARY(${PREC} STATIC src/base/precision.f90)

# class meshbuilder
SET(MESH mesh)
ADD_LIBRARY(${MESH} STATIC src/mesh/meshbuilder.f90)
target_link_libraries(${MESH} ${PREC})




# ---------------------------------------------------------
# Test executables
# ---------------------------------------------------------

# Test mesh
SET(TESTMESH testmesh)
ADD_EXECUTABLE(${TESTMESH} src/base/test_mesh.f90)
target_link_libraries(${TESTMESH} ${MESH})


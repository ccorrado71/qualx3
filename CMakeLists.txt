cmake_minimum_required(VERSION 3.14)

get_filename_component(ProjectId ${CMAKE_CURRENT_LIST_DIR} NAME)
string(REPLACE " " "_" ProjectId ${ProjectId})
project(${ProjectId} VERSION 1.0.0 LANGUAGES CXX Fortran)
set(EXECUTABLE_NAME "QualX3")

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_VERBOSE_MAKEFILE OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 REQUIRED COMPONENTS Widgets Sql PrintSupport Network)
if(Qt6_VERSION VERSION_GREATER_EQUAL 6.3)
   qt_standard_project_setup()
else()
   set(CMAKE_AUTOUIC ON)
   set(CMAKE_AUTOMOC ON)
   set(CMAKE_AUTORCC ON)
endif()


add_subdirectory(src)

#target_link_libraries(${CMAKE_PROJECT_NAME} PRIVATE Qt6::Widgets)
#target_link_libraries(${CMAKE_PROJECT_NAME} PRIVATE Qt6::Sql)

#For Fortran support
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake)

# Stampa il nome del compilatore Fortran
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message("Fortran compiler is: ${Fortran_COMPILER_NAME}")


cmake_minimum_required(VERSION 3.14)

get_filename_component(ProjectId ${CMAKE_CURRENT_LIST_DIR} NAME)
string(REPLACE " " "_" ProjectId ${ProjectId})
project(${ProjectId} VERSION 1.0.0 LANGUAGES CXX Fortran)
#set(CMAKE_Fortran_COMPILER "ifort") non funziona!
#per differenziare la compilazione in base al compilatore fortran puoi usare i file config.sh
#1) Modifica opportunamente il file config.sh e lancialo
#2) Allinea qtcreator selezionando in project la build directory giusta, così puoi continuare a lavorare in qtcreator
#oppure:
#1) crea due cartelle build_debug_gfortran e build_debug_ifort, io preferisco sotto QualX3
#2) aggiungi -DCMAKE_Fortran_COMPILER=nome_compiler in current configuration > additional CMake options > run cmake.
#Per gfortran non è necessario visto che è il default
#3) rebuild

set(CMAKE_INCLUDE_CURRENT_DIR ON)



set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

#find_package(QT NAMES Qt6 Qt5 COMPONENTS Widgets Sql REQUIRED)
#find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Widgets Sql REQUIRED)
find_package(Qt6 REQUIRED COMPONENTS Widgets Sql)
#qt_standard_project_setup()

add_subdirectory(src)

#target_link_libraries(${CMAKE_PROJECT_NAME}  Qt${QT_VERSION_MAJOR}::Widgets)
#target_link_libraries(${CMAKE_PROJECT_NAME}  Qt${QT_VERSION_MAJOR}::Sql)

target_link_libraries(${CMAKE_PROJECT_NAME} PRIVATE Qt6::Widgets)
target_link_libraries(${CMAKE_PROJECT_NAME} PRIVATE Qt6::Sql)

#For Fortran support
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake)

# Stampa il nome del compilatore Fortran
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message("Fortran compiler is: ${Fortran_COMPILER_NAME}")


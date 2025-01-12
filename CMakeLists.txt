cmake_minimum_required(VERSION 3.20)

get_filename_component(ProjectId ${CMAKE_CURRENT_LIST_DIR} NAME)
string(REPLACE " " "_" ProjectId ${ProjectId})
project(${ProjectId} VERSION 1.0.0 LANGUAGES CXX Fortran)
set(EXECUTABLE_NAME "qualx")

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

# Add our local modules to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

add_subdirectory(src)

INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake)

# Print compilers name
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message("Fortran compiler is: ${Fortran_COMPILER_NAME}")

#file(GLOB INDEXING_FILES "${CMAKE_CURRENT_SOURCE_DIR}/indexing/*")
#if(WIN32)
#    file(COPY ${INDEXING_FILES} DESTINATION "${CMAKE_BINARY_DIR}")
#    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/share DESTINATION ${CMAKE_BINARY_DIR})
#elseif(UNIX AND NOT APPLE)
#    file(COPY ${INDEXING_FILES} DESTINATION "${CMAKE_BINARY_DIR}/src")
#    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/share DESTINATION ${CMAKE_BINARY_DIR})
#elseif(APPLE)
#    file(COPY ${INDEXING_FILES} DESTINATION "${CMAKE_BINARY_DIR}/src/${EXECUTABLE_NAME}.app/Contents/MacOS/")
#    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/share DESTINATION "${CMAKE_BINARY_DIR}/src/${EXECUTABLE_NAME}.app/Contents/")
#endif()

# Install the executable into "${CMAKE_INSTALL_PREFIX}/bin
set(COMPONENT_NAME_MAIN "QualX")
install(TARGETS ${EXECUTABLE_NAME}
    BUNDLE DESTINATION .
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT ${COMPONENT_NAME_MAIN})
#install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/share/qualx DESTINATION share COMPONENT ${COMPONENT_NAME_MAIN})
#install(FILES ${INDEXING_FILES} DESTINATION bin COMPONENT ${COMPONENT_NAME_MAIN})
if (WIN32)
    if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
       install(
         FILES
         "${IFORT_DLL_DIR}/libifcoremd.dll"
         "${IFORT_DLL_DIR}/libmmd.dll"
         "${IFORT_DLL_DIR}/svml_dispmd.dll"
         "${IFORT_DLL_DIR}/libifportmd.dll"
         DESTINATION "${CMAKE_INSTALL_BINDIR}"
         COMPONENT ${COMPONENT_NAME_MAIN}
       )
    endif()
endif()

if(Qt6_VERSION VERSION_GREATER_EQUAL 6.5)
    # Generate the deployment script for the target MyApp.
    qt_generate_deploy_app_script(
        TARGET ${EXECUTABLE_NAME}
        OUTPUT_SCRIPT deploy_script
        NO_UNSUPPORTED_PLATFORM_ERROR
    )

    # Call the deployment script on "cmake --install".
    install(SCRIPT ${deploy_script})
endif()
message("Qt6_VERSION: ${Qt6_VERSION}")

# CPack configuration

#set(CPACK_RESOURCE_FILE_LICENSE  ${CMAKE_CURRENT_SOURCE_DIR}/deploy/Windows/licence.txt)
#set(CPACK_PACKAGE_VENDOR "Institute of Crystallography - CNR")
#set(CPACK_PACKAGE_CONTACT "corrado.cuocci@ic.cnr.it")
#set(CPACK_PACKAGE_DESCRIPTION "Automatic solution and refinement of crystal structures")

#if(WIN32)
#    set(CPACK_GENERATOR "IFW")
#    set(CPACK_IFW_ROOT "C:/Qt/QtIFW-4.7.0")
#    set(CPACK_IFW_PACKAGE_NAME "Expo")
#    set(CPACK_IFW_PACKAGE_TITLE "Expo")
#    set(CPACK_IFW_PACKAGE_PUBLISHER "Corrado Cuocci")
#    set(CPACK_IFW_PACKAGE_WIZARD_STYLE "Aero")
#    set(CPACK_IFW_TARGET_DIRECTORY "@ApplicationsDir@/Expo2")
#    set(CPACK_IFW_PACKAGE_CONTROL_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/deploy/Windows/controlscript.qs")    
#    message("CONTROL: ${CPACK_IFW_PACKAGE_CONTROL_SCRIPT}")
#
#    include(CPackIFW)
#
#    CPACK_ADD_COMPONENT(${COMPONENT_NAME_MAIN})
#
#    # The component HAS to be installed
#    cpack_ifw_configure_component(${COMPONENT_NAME_MAIN} ESSENTIAL FORCED_INSTALLATION)
#
#    # Adds license info to package.xml file
#    # Also copies licence.txt to 'meta' directory to display the license text in the installer.
#    cpack_ifw_configure_component(${COMPONENT_NAME_MAIN} LICENSES "Expo License" ${CPACK_RESOURCE_FILE_LICENSE})
#
#    # Customizing the UI with a script
#    cpack_ifw_configure_component(${COMPONENT_NAME_MAIN} SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/deploy/Windows/installscript.qs)
#
#    # Adds basic repository entries to config.xml file
#    # Runs repogen command on the packages directory
#    cpack_ifw_add_repository("Expo2Repository" URL "https://www.ba.ic.cnr.it/content/old/expo2/repository")
#
#    include(CPack)
#
#elseif(UNIX AND NOT APPLE)
#    set(CPACK_GENERATOR "DEB")
#    execute_process(
#        COMMAND dpkg --print-architecture
#        OUTPUT_VARIABLE CPACK_DEBIAN_PACKAGE_ARCHITECTURE
#        OUTPUT_STRIP_TRAILING_WHITESPACE
#    )
#    execute_process(
#        COMMAND lsb_release -c -s
#        OUTPUT_VARIABLE DISTRO_CODENAME
#        OUTPUT_STRIP_TRAILING_WHITESPACE
#    )
#    set(CPACK_DEBIAN_FILE_NAME "${EXECUTABLE_NAME}-${PROJECT_VERSION}-${DISTRO_CODENAME}_${CPACK_DEBIAN_PACKAGE_ARCHITECTURE}.deb")
#    #message(CPACK_DEBIAN_FILE_NAME "Debian file name: ${CPACK_DEBIAN_FILE_NAME}")
#    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Corrado Cuocci <corrado.cuocci@ic.cnr.it>")
#    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
#    set(CPACK_SOURCE_GENERATOR "TGZ")
#    include(CPack)
#elseif(APPLE)
#    #DA TESTARE
#    set(CPACK_GENERATOR "DragNDrop;Bundle")
#    set(CPACK_BUNDLE_NAME ${EXECUTABLE_NAME})
#    include(CPack)
#endif()

#include(CPack)

#CPACK_ADD_COMPONENT(${COMPONENT_NAME_MAIN})



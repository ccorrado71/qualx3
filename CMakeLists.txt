cmake_minimum_required(VERSION 3.20)

get_filename_component(ProjectId ${CMAKE_CURRENT_LIST_DIR} NAME)
string(REPLACE " " "_" ProjectId ${ProjectId})
project(${ProjectId} VERSION 1.0.0 LANGUAGES CXX Fortran)
set(EXECUTABLE_NAME "qualx")

# Option to build Debian package (installs to /usr)
option(BUILD_DEB_PACKAGE "Build for Debian package installation in /usr" OFF)

# Set default installation prefix
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    if(BUILD_DEB_PACKAGE)
        # For Debian package: install to /usr (standard location)
        set(CMAKE_INSTALL_PREFIX "/usr" CACHE PATH "Installation prefix for Debian package" FORCE)
        message(STATUS "Building Debian package, using prefix: ${CMAKE_INSTALL_PREFIX}")
    else()
        # For manual installation: install to /opt/qualx3-version
        set(CMAKE_INSTALL_PREFIX "/opt/qualx3-${PROJECT_VERSION}" CACHE PATH "Installation prefix" FORCE)
        message(STATUS "CMAKE_INSTALL_PREFIX not specified, defaulting to: ${CMAKE_INSTALL_PREFIX}")
    endif()
endif()

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_VERBOSE_MAKEFILE OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Set RPATH for Qt libraries on Linux
if(UNIX AND NOT APPLE)
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
    set(CMAKE_BUILD_RPATH_USE_ORIGIN TRUE)
    # RPATH points to ../lib relative to the executable location
    set(CMAKE_INSTALL_RPATH "$ORIGIN/../lib:$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
endif()

find_package(Qt6 REQUIRED COMPONENTS Widgets Sql PrintSupport Network)

# Add Qt library path to RPATH for Linux
if(UNIX AND NOT APPLE)
    if(Qt6_FOUND)
        get_target_property(_qt6_core_location Qt6::Core LOCATION)
        get_filename_component(_qt6_lib_dir "${_qt6_core_location}" DIRECTORY)
        list(APPEND CMAKE_INSTALL_RPATH "${_qt6_lib_dir}")
        list(APPEND CMAKE_BUILD_RPATH "${_qt6_lib_dir}")
    endif()
endif()
if(Qt6_VERSION VERSION_GREATER_EQUAL 6.3)
   qt_standard_project_setup()
else()
   set(CMAKE_AUTOUIC ON)
   set(CMAKE_AUTOMOC ON)
   set(CMAKE_AUTORCC ON)
endif()

# Add our local modules to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

add_subdirectory(configured)
add_subdirectory(src)

INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake)

# Print compilers name
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message("Fortran compiler is: ${Fortran_COMPILER_NAME}")

# Copy files in building folder
if(WIN32)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/share DESTINATION ${CMAKE_BINARY_DIR})
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/DB DESTINATION ${CMAKE_BINARY_DIR})
elseif(UNIX AND NOT APPLE)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/share DESTINATION ${CMAKE_BINARY_DIR})
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/DB DESTINATION ${CMAKE_BINARY_DIR})
elseif(APPLE)
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/share DESTINATION "${CMAKE_BINARY_DIR}/src/${EXECUTABLE_NAME}.app/Contents/")
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/DB DESTINATION "${CMAKE_BINARY_DIR}/src/${EXECUTABLE_NAME}.app/Contents/")
endif()

# Install the executable into "${CMAKE_INSTALL_PREFIX}/bin
set(COMPONENT_NAME_MAIN "QualX")
install(TARGETS ${EXECUTABLE_NAME}
    BUNDLE DESTINATION .
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT ${COMPONENT_NAME_MAIN})
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/share/qualx DESTINATION share COMPONENT ${COMPONENT_NAME_MAIN})
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/DB DESTINATION . COMPONENT ${COMPONENT_NAME_MAIN})

# Linux: Install Qt libraries and plugins manually (skip for Debian package)
if(UNIX AND NOT APPLE AND NOT BUILD_DEB_PACKAGE)
   if(Qt6_FOUND)
   get_target_property(_qt6_qmake_location Qt6::qmake IMPORTED_LOCATION)
   execute_process(
     COMMAND ${_qt6_qmake_location} -query QT_INSTALL_LIBS
     OUTPUT_VARIABLE QT6_LIB_DIR
     OUTPUT_STRIP_TRAILING_WHITESPACE
   )

   # Install Qt libraries to lib directory
   file(GLOB QT6_LIBS
     "${QT6_LIB_DIR}/libQt6*.so*"
     "${QT6_LIB_DIR}/libicu*.so*"
   )
   install(FILES ${QT6_LIBS}
     DESTINATION ${CMAKE_INSTALL_LIBDIR}
     COMPONENT ${COMPONENT_NAME_MAIN}
   )
    endif()
endif()

if (WIN32)
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
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

# Qt deployment script (must be before install commands to allow overriding)
if(Qt6_VERSION VERSION_GREATER_EQUAL 6.5)
    # Generate the deployment script for the target
    qt_generate_deploy_app_script(
        TARGET ${EXECUTABLE_NAME}
        OUTPUT_SCRIPT deploy_script
        NO_UNSUPPORTED_PLATFORM_ERROR
    )
endif()

# Execute Qt deployment script (called after custom installations)
# Skip for Debian package as dependencies are managed by apt
if(Qt6_VERSION VERSION_GREATER_EQUAL 6.5 AND NOT BUILD_DEB_PACKAGE)
    install(SCRIPT ${deploy_script})
elseif(NOT BUILD_DEB_PACKAGE)
    # Manual plugin installation for Qt < 6.5 (Linux only)
    if(UNIX AND NOT APPLE)
        # Get Qt6 plugin directory using qmake query (most reliable method)
        if(Qt6_FOUND)
        get_target_property(_qt6_qmake_location Qt6::qmake IMPORTED_LOCATION)
        execute_process(
            COMMAND ${_qt6_qmake_location} -query QT_INSTALL_PLUGINS
            OUTPUT_VARIABLE QT6_PLUGINS_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        
        message(STATUS "Qt ${Qt6_VERSION} < 6.5: Installing plugins manually from ${QT6_PLUGINS_DIR}")
        endif()
        
        # Install essential platforms plugin (required to run the application)
        if(EXISTS "${QT6_PLUGINS_DIR}/platforms")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/platforms"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        # Install platformthemes for GNOME/GTK integration
        if(EXISTS "${QT6_PLUGINS_DIR}/platformthemes")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/platformthemes"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        # Install xcbglintegrations for X11/OpenGL support
        if(EXISTS "${QT6_PLUGINS_DIR}/xcbglintegrations")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/xcbglintegrations"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        # Install imageformats for image support
        if(EXISTS "${QT6_PLUGINS_DIR}/imageformats")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/imageformats"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        # Install platforminputcontexts for input method support
        if(EXISTS "${QT6_PLUGINS_DIR}/platforminputcontexts")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/platforminputcontexts"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        # Install wayland plugins if available (for Wayland support)
        if(EXISTS "${QT6_PLUGINS_DIR}/wayland-shell-integration")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/wayland-shell-integration"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        if(EXISTS "${QT6_PLUGINS_DIR}/wayland-decoration-client")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/wayland-decoration-client"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        if(EXISTS "${QT6_PLUGINS_DIR}/wayland-graphics-integration-client")
            install(DIRECTORY "${QT6_PLUGINS_DIR}/wayland-graphics-integration-client"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                    COMPONENT ${COMPONENT_NAME_MAIN}
            )
        endif()
        
        # Install Qt translations
        execute_process(
            COMMAND ${_qt6_qmake_location} -query QT_INSTALL_TRANSLATIONS
            OUTPUT_VARIABLE QT6_TRANSLATIONS_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        if(EXISTS "${QT6_TRANSLATIONS_DIR}")
            install(DIRECTORY "${QT6_TRANSLATIONS_DIR}/"
                    DESTINATION ${CMAKE_INSTALL_PREFIX}/translations
                    COMPONENT ${COMPONENT_NAME_MAIN}
                    FILES_MATCHING PATTERN "*.qm"
            )
        endif()
    endif()
endif()

# Install Qt plugins for proper integration (Linux only) - Additional plugins for Qt >= 6.5
# Skip for Debian package
if(UNIX AND NOT APPLE AND Qt6_VERSION VERSION_GREATER_EQUAL 6.5 AND NOT BUILD_DEB_PACKAGE)
    get_target_property(_qt6_qmake_location Qt6::qmake IMPORTED_LOCATION)
    get_filename_component(_qt6_install_prefix "${_qt6_qmake_location}" DIRECTORY)
    get_filename_component(_qt6_install_prefix "${_qt6_install_prefix}" DIRECTORY)
    set(QT6_PLUGINS_DIR "${_qt6_install_prefix}/plugins")
    
    # Install additional platformthemes for GNOME/GTK integration if not already done by deploy script
    if(EXISTS "${QT6_PLUGINS_DIR}/platformthemes")
        install(DIRECTORY "${QT6_PLUGINS_DIR}/platformthemes"
                DESTINATION ${CMAKE_INSTALL_PREFIX}/plugins
                COMPONENT ${COMPONENT_NAME_MAIN}
        )
    endif()
    
    # Create a custom qt.conf that uses relative paths for portability
    # This allows the software to work regardless of installation prefix
    file(WRITE ${CMAKE_BINARY_DIR}/install_qtconf.cmake
"# Determine relative paths based on installation structure
file(WRITE \"\${CMAKE_INSTALL_PREFIX}/bin/qt.conf\"
\"[Paths]
Prefix = ..
Libraries = lib
Plugins = plugins
Translations = translations
Data = share
\")
message(STATUS \"Installed qt.conf to \${CMAKE_INSTALL_PREFIX}/bin/qt.conf\")
message(STATUS \"Software installed to: \${CMAKE_INSTALL_PREFIX}\")
message(STATUS \"  Executable: \${CMAKE_INSTALL_PREFIX}/bin/qualx\")
message(STATUS \"  Libraries: \${CMAKE_INSTALL_PREFIX}/lib\")
message(STATUS \"  Plugins: \${CMAKE_INSTALL_PREFIX}/plugins\")
")
    install(SCRIPT ${CMAKE_BINARY_DIR}/install_qtconf.cmake)
endif()

message("Qt6_VERSION: ${Qt6_VERSION}")

# CPack configuration

set(CPACK_RESOURCE_FILE_LICENSE  ${CMAKE_CURRENT_SOURCE_DIR}/deploy/Windows/licence.txt)
set(CPACK_PACKAGE_VENDOR "Institute of Crystallography - CNR")
set(CPACK_PACKAGE_CONTACT "corrado.cuocci@ic.cnr.it")
set(CPACK_PACKAGE_DESCRIPTION "Automatic solution and refinement of crystal structures")

if(WIN32)
    set(CPACK_GENERATOR "IFW")
    set(CPACK_IFW_ROOT "C:/Qt/QtIFW-4.7.0")
    set(CPACK_IFW_PACKAGE_NAME "QualX")
    set(CPACK_IFW_PACKAGE_TITLE "QualX")
    set(CPACK_IFW_PACKAGE_PUBLISHER "Corrado Cuocci")
    set(CPACK_IFW_PACKAGE_WIZARD_STYLE "Aero")
    set(CPACK_IFW_TARGET_DIRECTORY "@ApplicationsDir@/QualX")
    set(CPACK_IFW_PACKAGE_CONTROL_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/deploy/Windows/controlscript.qs")    
    message("CONTROL: ${CPACK_IFW_PACKAGE_CONTROL_SCRIPT}")

    include(CPackIFW)

    CPACK_ADD_COMPONENT(${COMPONENT_NAME_MAIN})

    # The component HAS to be installed
    cpack_ifw_configure_component(${COMPONENT_NAME_MAIN} ESSENTIAL FORCED_INSTALLATION)

    # Adds license info to package.xml file
    # Also copies licence.txt to 'meta' directory to display the license text in the installer.
    cpack_ifw_configure_component(${COMPONENT_NAME_MAIN} LICENSES "QualX License" ${CPACK_RESOURCE_FILE_LICENSE})

    # Customizing the UI with a script
    cpack_ifw_configure_component(${COMPONENT_NAME_MAIN} SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/deploy/Windows/installscript.qs)

    # Adds basic repository entries to config.xml file
    # Runs repogen command on the packages directory
    cpack_ifw_add_repository("Expo2Repository" URL "https://www.ba.ic.cnr.it/content/old/expo2/repository")

    include(CPack)

elseif(UNIX AND NOT APPLE)
    set(CPACK_GENERATOR "DEB")
    execute_process(
        COMMAND dpkg --print-architecture
        OUTPUT_VARIABLE CPACK_DEBIAN_PACKAGE_ARCHITECTURE
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    execute_process(
        COMMAND lsb_release -c -s
        OUTPUT_VARIABLE DISTRO_CODENAME
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Set default architecture if dpkg is not available
    if(NOT CPACK_DEBIAN_PACKAGE_ARCHITECTURE)
        set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
    endif()
    
    # Set default codename if lsb_release is not available
    if(NOT DISTRO_CODENAME)
        set(DISTRO_CODENAME "debian")
    endif()
    
    set(CPACK_DEBIAN_FILE_NAME "${EXECUTABLE_NAME}-${PROJECT_VERSION}-${DISTRO_CODENAME}_${CPACK_DEBIAN_PACKAGE_ARCHITECTURE}.deb")
    set(CPACK_DEBIAN_PACKAGE_NAME "qualx")
    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Corrado Cuocci <corrado.cuocci@ic.cnr.it>")
    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
    set(CPACK_SOURCE_GENERATOR "TGZ")
    include(CPack)
elseif(APPLE)
    #DA TESTARE
    set(CPACK_GENERATOR "DragNDrop;Bundle")
    set(CPACK_BUNDLE_NAME ${EXECUTABLE_NAME})
    include(CPack)
endif()

#include(CPack)

#CPACK_ADD_COMPONENT(${COMPONENT_NAME_MAIN})



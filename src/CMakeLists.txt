set(PROJECT_SOURCES
    backgroundcoefwidget.cpp
    backgroundcoefwidget.h
    backgroundcoefwidget.ui
    backgrounddialog.cpp
    backgrounddialog.h
    backgrounddialog.ui
    colorbutton.cpp
    colorbutton.h
    colorwidget.cpp         
    colorwidget.h
    colorwidget.ui
    commandline.cpp
    commandline.h
    customplotzoom.cpp
    customplotzoom.h
    dataset.h
    dbmanager.h
    dbmanager.cpp
    dbquerybuilder.h
    dbquerybuilder.cpp
    fileutils.cpp
    fileutils.h
    graphitem.cpp
    graphitem.h
    libcomune.cpp
    libcomune.h
    libfor.cpp
    libmath.cpp
    libmath.h
    libmatrix.h
    libvector.h
    locate.cpp
    main.cpp
    mainwindow.h
    mainwindow.cpp
    mainwindow.ui
    nr.h
    peaksearchdialog.cpp
    peaksearchdialog.h
    peaksearchdialog.ui
    plotsettings.h
    plotsettings.cpp
    progkeysettings.h
    qcustomplot.h
    qcustomplot.cpp
    qt_utils.h
    qt_utils.cpp
    qualxdbmanager.h
    qualxdbmanager.cpp
    wavedialog.h
    wavedialog.cpp
    wavedialog.ui
    wavelengthscombobox.cpp
    wavelengthscombobox.h
    wavelengthswidget.cpp
    wavelengthswidget.h
    wavelengthswidget.ui
    xpdutils.h
    xpdutils.cpp
    xpdviewwidget.h
    xpdviewwidget.cpp
)

set(FORTRAN_SOURCES 
    abscorr.f90
    arrayutil.f90
    atombasic.f90
    atomsutil.F90
    back_interop.f90
    background.f90
    bond2014.f90
    bondsutil.f90
    bump.f90
    bvalence.f90
    centroids.f90
    cif_form.f90
    cgeom.f90
    constants.f90
    constraints.f90
    contacts.F90
    counts.f90
    data.f90
    dataset.f90
    datautil.f90
    elements.f90
    error.F90
    filetype.f90
    fileutil.F90
    fract.f90
    fragments.F90
    genlsq.f90
    goptutil.F90	
    kspecmod.f90
    lib_array.f90
    lsqnew.f90
    mapinfo.f90
    marquardt.f90
    math_util.F90
    modeltype.f90
    moduli.f90
    molgraph.f90
    mpi_util.F90
    nist_table.f90
    nl2sol.f90
    peak_interop.f90
    peakutil.f90
    pfunction.f90
    plotstyle.f90
    prog_names.F90
    progtype.F90
    qualx.f90
    recipes.f90
    reflectionsutil.f90
    restraints.f90
    rigidb.f90
    scatt_params.F90
    screen_new.f90
    smooth.f90
    specif.f90
    spginfo.f90
    sphase.f90
    stat.f90
    strutil.f90
    symm_table.f90
    symmgrid.f90
    unit_cell.f90
    util.f
    utiln.f90
    variables.f90
    vedi2005.f90
)

set(LIBRARY_INCLUDES "./" "${CMAKE_BINARY_DIR}/configured_files/include")

qt_add_resources(PROJECT_SOURCES resource.qrc)

qt_add_executable(${EXECUTABLE_NAME}
	${PROJECT_SOURCES}
        ${FORTRAN_SOURCES}
)

target_link_libraries(${EXECUTABLE_NAME} PRIVATE
    Qt6::Widgets
    Qt6::Sql
    Qt6::PrintSupport
    Qt6::Network
)

target_include_directories(${EXECUTABLE_NAME} PUBLIC ${LIBRARY_INCLUDES})

target_compile_definitions(${EXECUTABLE_NAME} PUBLIC "USE_CONFIG_H")

if (WIN32)
    if (IFORT_LIB_DIR)
        target_link_libraries(${EXECUTABLE_NAME} PRIVATE
        "${IFORT_LIB_DIR}/ifmodintr.lib"
        "${IFORT_LIB_DIR}/ifwin.lib"
        "${IFORT_LIB_DIR}/ifconsol.lib"
        "${IFORT_LIB_DIR}/libifportmd.lib"
        "${IFORT_LIB_DIR}/libirc.lib"
	"${IFORT_LIB_DIR}/svml_dispmd.lib"
        "${IFORT_LIB_DIR}/libircmt.lib"
        "${IFORT_LIB_DIR}/libmmd.lib"
        )

        STRING(TOUPPER "${CMAKE_BUILD_TYPE}" BUILDTYPE)
        IF(BUILDTYPE STREQUAL "RELEASE")
           target_link_libraries(${EXECUTABLE_NAME} PRIVATE
#"${IFORT_LIB_DIR}/libmmd.lib"
           "${IFORT_LIB_DIR}/libifcoremd.lib"
           )
        else()
           target_link_libraries(${EXECUTABLE_NAME} PRIVATE
		   #           "${IFORT_LIB_DIR}/libmmdd.lib"
           "${IFORT_LIB_DIR}/libifcoremdd.lib"
           )
        endif()
    endif()
    target_link_libraries(${EXECUTABLE_NAME} PRIVATE opengl32)
endif()

set_target_properties(${EXECUTABLE_NAME} PROPERTIES
    WIN32_EXECUTABLE TRUE
    MACOSX_BUNDLE TRUE
)

#Setting the Application Icon
if (WIN32)
    set(app_icon_resource_windows "${CMAKE_CURRENT_SOURCE_DIR}/pixmaps/qualx.rc")
    target_sources(${EXECUTABLE_NAME} PRIVATE ${app_icon_resource_windows})
elseif (APPLE)
    set(MACOSX_BUNDLE_ICON_FILE Expo.icns)
    set(app_icon_macos "${CMAKE_CURRENT_SOURCE_DIR}/pixmaps/qualx.icns")
    set_source_files_properties(${app_icon_macos} PROPERTIES
           MACOSX_PACKAGE_LOCATION "Resources")
    target_sources(${EXECUTABLE_NAME} PRIVATE ${app_icon_macos})
else()

endif()


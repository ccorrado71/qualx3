set(PROJECT_SOURCES
    colorbutton.cpp
    colorbutton.h
    colorwidget.cpp         
    colorwidget.h
    colorwidget.ui
    customplotzoom.cpp
    customplotzoom.h
    dbmanager.h
    dbmanager.cpp
    dbquerybuilder.h
    dbquerybuilder.cpp
    graphitem.cpp
    graphitem.h
    libcomune.cpp
    libcomune.h
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
    plotsettings.cpp
    plotsettings.h
    progkeysettings.h
    qcustomplot.cpp
    qcustomplot.h
    qualxdbmanager.cpp
    qualxdbmanager.h
    xpdutils.cpp
    xpdutils.h
    xpdviewwidget.h
    xpdviewwidget.cpp
)

set(FORTRAN_SOURCES 
    constants.f90
)

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

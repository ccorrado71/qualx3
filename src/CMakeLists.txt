set(PROJECT_SOURCES
    dbmanager.h
    dbmanager.cpp
    dbquerybuilder.h
    dbquerybuilder.cpp
    main.cpp
    mainwindow.h
    mainwindow.cpp
    mainwindow.ui
    qualxdbmanager.cpp
    qualxdbmanager.h
)

set(FORTRAN_SOURCES 
    constants.f90
)

qt_add_resources(PROJECT_SOURCES resource.qrc)

add_executable(${CMAKE_PROJECT_NAME}
	${PROJECT_SOURCES}
        ${FORTRAN_SOURCES}
)

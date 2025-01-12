#include "mainwindow.h"

#include <QApplication>

extern "C" void qualxmain();

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    MainWindow w;

    qualxmain();

    w.show();
    return a.exec();
}

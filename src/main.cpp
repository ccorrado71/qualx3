#include "mainwindow.h"
#include "commandline.h"
#include "qt_utils.h"
#if USE_CONFIG_H
#include "config.h"
#endif

#include <QApplication>

extern "C" void qualxmain(ProgOptions *p, const char *filein, int lenIn, const char *fileout, int lenOut, int *ier);

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

#ifdef Q_OS_MACOS
    QApplication::setWindowIcon(QIcon(":/images/images/qualx.icns"));
#else
    QApplication::setWindowIcon(QIcon(":/images/images/qualx.png"));
#endif
    qApp->setApplicationVersion(APP_VERSION);
    qApp->setApplicationName(APP_NAME);
    qApp->setOrganizationName("IC");
    qApp->setOrganizationDomain("www.ba.ic.cnr.it/softwareic");

    QLocale locale = QLocale::system();
    if (locale.decimalPoint() == ',') {
        QLocale::setDefault(QLocale::c());
        setlocale(LC_ALL,"C");
    }

    QCommandLineParser parser;
    parser.setApplicationDescription(QCoreApplication::translate("main","Program for structure solution process from powder diffraction data:"));
    QString errorMessage;
    ProgOptions opt;
    QString filein, fileout, testFolder;
    bool testApp;
    switch (parseCommandLine(parser, filein, fileout, opt, errorMessage, testApp, testFolder)) {
    case CommandLineOk:
        break;
    case CommandLineError:
        fputs(qPrintable(errorMessage), stderr);
        fputs("\n\n", stderr);
        fputs(qPrintable(parser.helpText()), stderr);
        return 1;
    case CommandLineVersionRequested:
    {
        AppVersionInfo appVersion = getVersionInfo();
        printf("%s Version %s\n%s\n%s\n%s\n",
               qPrintable(QCoreApplication::applicationName()),
               qPrintable(QCoreApplication::applicationVersion()),
               qPrintable(QString("Created on %1").arg(appVersion.data)),
               qPrintable(appVersion.compilersString),
               qPrintable(appVersion.qtVersionInfo));
    }
        return 0;
    case CommandLineHelpRequested:
        parser.showHelp();
        Q_UNREACHABLE();
    }

    int ier;
    if (opt.nogui) {

    } else {
        MainWindow w;
        w.show();
        qualxmain(&opt, filein.toUtf8().constData(), filein.length(), fileout.toUtf8().constData(), fileout.length(), &ier);

        return a.exec();
    }
}

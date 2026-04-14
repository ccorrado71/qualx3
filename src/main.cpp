#include "mainwindow.h"
#include "commandline.h"
#include "qt_utils.h"
#include "fileutils.h"
#include "databasebuilder.h"
#include "libcomune.h"
#include "appstate.h"
#if USE_CONFIG_H
#include "config.h"
#endif

#include <QApplication>
#include <QCoreApplication>
#include <QScopeGuard>

extern "C" void qualxmain(ProgOptions *p, const char *filein, int lenIn, const char *fileout, int lenOut, const char *exepath, int lenPath, int *ier);

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
    parser.setApplicationDescription(QCoreApplication::translate("main","Software for qualitative phase analysis from powder diffraction data:"));
    QString errorMessage;
    ProgOptions opt;
    DbBuildOptions dbopt;
    QString filein, fileout, testFolder;
    bool testApp;
    switch (parseCommandLine(parser, filein, fileout, opt, dbopt, errorMessage, testApp, testFolder)) {
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

    // Load databases only after early-exit cases (--version, --help, bad args).
    // Declared after 'a' so the scope guard is destroyed before QApplication,
    // ensuring closeDatabeses() is called while QCoreApplication still exists.
    AppState::load();
    auto dbCleanup = qScopeGuard([] { AppState::db().closeDatabeses(); });

    //Find folder with program files
    QString errPath;
    QStringList dataFiles = {"syminfo.lib","AtomProperties.xen"};
    QString pathDataFiles = fileutils::getDirDataFiles(dataFiles,errPath);
    MainWindow::setPathDataFiles(pathDataFiles);

    int ier;
    if (dbopt.enabled) {
        if (pathDataFiles.isEmpty()) {
            fputs(qPrintable(errPath), stderr);
            fputs("\n", stderr);
            return 1;
        }
        if (!initQualxTables(pathDataFiles)) {
            fputs("Error: could not initialise chemical tables.\n", stderr);
            return 1;
        }

        if (dbopt.source == DbBuildOptions::Source::Pdf2) {
            printf("Reading PDF-2 file: %s\n", qPrintable(dbopt.pdf2File));
            if (!DatabaseBuilder::buildPdfDatabase(dbopt.outputDb, dbopt.pdf2File,
                    [](int cards, qint64, qint64) {
                        printf("\r  %d cards processed...", cards);
                        fflush(stdout);
                    })) {
                fputs("Error: could not build PDF-2 database.\n", stderr);
                return 1;
            }
            printf("\nDone.\n");
            return 0;
        }

        {
            // CIF source
            if (dbopt.inorganic)
                printf("Scanning CIF folder (inorganic only): %s\n", qPrintable(dbopt.cifDir));
            else
                printf("Scanning CIF folder: %s\n", qPrintable(dbopt.cifDir));

            DatabaseBuilder::CifBuildStats stats;
            if (!DatabaseBuilder::buildCifDatabase(
                    dbopt.outputDb, dbopt.cifDir, dbopt.recursive,
                    dbopt.inorganic,
                    [](int ok, int skip, int err, const QString &path) {
                        printf("\r  [%d ok / %d skip / %d err] %s",
                               ok, skip, err, qPrintable(path));
                        fflush(stdout);
                    },
                    &stats)) {
                fputs("Error: could not build CIF database.\n", stderr);
                return 1;
            }
            if (dbopt.inorganic)
                printf("\nDone: %d ok, %d skipped (not inorganic), %d errors.\n",
                       stats.ok, stats.skipped, stats.errors);
            else
                printf("\nDone: %d ok, %d errors.\n", stats.ok, stats.errors);
        }
        return 0;
    } else if (opt.nogui) {

    } else {
        MainWindow w;
        w.show();
        if (pathDataFiles.isEmpty()) {
            QMessageBox::critical(&w,"Problem Occurred",errPath,QMessageBox::Ok);
            return EXIT_FAILURE;
        } else {
            qualxmain(&opt,filein.toLocal8Bit().constData(),filein.toLocal8Bit().size(),fileout.toLocal8Bit().constData(),
                      fileout.toLocal8Bit().size(),pathDataFiles.toLocal8Bit().constData(),pathDataFiles.toLocal8Bit().size(), &ier);
            //test_crystal_info_from_cif();
        }
        return a.exec();
    }
}

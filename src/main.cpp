#include "mainwindow.h"
#include "commandline.h"
#include "qt_utils.h"
#include "fileutils.h"
#include "databasebuilder.h"
#include "qualxdbcreator.h"
#include "cifdbpopulator.h"
#include "cifreader.h"
#include "libcomune.h"
#if USE_CONFIG_H
#include "config.h"
#endif

#include <QApplication>
#include <QCoreApplication>

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
            // CIF source: read each CIF file via Fortran get_crystal_info_from_cif
            QualxDbCreator db;
            if (!db.create(dbopt.outputDb, QualxDbCreator::DbType::CifFiles)) {
                fputs("Error: could not create database.\n", stderr);
                return 1;
            }
            int nProcessed = 0;
            int nSkipped   = 0;
            int nErrors    = 0;
            //constexpr int kMaxCif = 1000;  // TODO: remove limit
            CifReader reader;
            CifDbPopulator populator(&db);
            QObject::connect(&reader, &CifReader::cifFound,
                             [&](const QString &cifPath) {
                                 //if (nProcessed + nErrors >= kMaxCif) return;
                                 CifCrystalInfo info;
                                 const int result = [&]() -> int {
                                     // readCrystalInfoFromCif returns false both on
                                     // real errors (ier<0) and on inorganic-filter skip (ier=1).
                                     // We need to distinguish them: call directly via the wrapper
                                     // which maps ier==0 → true, anything else → false.
                                     // To tell skip from error we check subfile after the call.
                                     if (readCrystalInfoFromCif(cifPath, info, dbopt.inorganic))
                                         return 0;   // success
                                     // ier != 0: if inorganic filter is on and subfile is empty
                                     // (info was not filled) it's a skip, otherwise an error.
                                     if (dbopt.inorganic && info.nat == 0)
                                         return 1;   // skipped (not inorganic)
                                     return -1;      // real read error
                                 }();
                                 if (result == 0) {
                                     populator.onCifReady(cifPath, info);
                                     ++nProcessed;
                                     printf("\r  [%d ok / %d skip / %d err] %s",
                                            nProcessed, nSkipped, nErrors,
                                            qPrintable(cifPath));
                                     fflush(stdout);
                                 } else if (result == 1) {
                                     ++nSkipped;
                                 } else {
                                     ++nErrors;
                                     fprintf(stderr, "\n  Warning: failed to read %s\n",
                                             qPrintable(cifPath));
                                 }
                             });
            QObject::connect(&reader, &CifReader::finished,
                             [&](int n) {
                                 populator.onFinished(nProcessed);
                                 if (dbopt.inorganic)
                                     printf("\nScanned %d CIF files: %d ok, %d skipped (not inorganic), %d errors.\n",
                                            n, nProcessed, nSkipped, nErrors);
                                 else
                                     printf("\nScanned %d CIF files: %d ok, %d errors.\n",
                                            n, nProcessed, nErrors);
                             });
            if (dbopt.inorganic)
                printf("Scanning CIF folder (inorganic only): %s\n", qPrintable(dbopt.cifDir));
            else
                printf("Scanning CIF folder: %s\n", qPrintable(dbopt.cifDir));
            reader.scan(dbopt.cifDir, dbopt.recursive);
            db.close();
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

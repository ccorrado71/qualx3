#include "mainwindow.h"
#include "commandline.h"
#include "qt_utils.h"
#include "fileutils.h"
#include "qualxdbcreator.h"
#include "qualxdbpopulator.h"
#include "cifdbpopulator.h"
#include "pdf2reader.h"
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
    parser.setApplicationDescription(QCoreApplication::translate("main","Program for structure solution process from powder diffraction data:"));
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

        QualxDbCreator::DbType dbType = (dbopt.source == DbBuildOptions::Source::Pdf2)
                                        ? QualxDbCreator::DbType::Pdf2
                                        : QualxDbCreator::DbType::CifFiles;
        QualxDbCreator db;
        if (!db.create(dbopt.outputDb, dbType)) {
            fputs("Error: could not create database.\n", stderr);
            return 1;
        }

        if (dbopt.source == DbBuildOptions::Source::Pdf2) {
            Pdf2Reader reader;
            QualxDbPopulator populator(&db);
            QObject::connect(&reader, &Pdf2Reader::cardReady,
                             &populator, &QualxDbPopulator::onCardReady);
            QObject::connect(&reader, &Pdf2Reader::finished,
                             &populator, &QualxDbPopulator::onFinished);
            QObject::connect(&reader, &Pdf2Reader::progress,
                             [](int cards, qint64) {
                                 printf("\r  %d cards processed...", cards);
                                 fflush(stdout);
                             });
            printf("Reading PDF-2 file: %s\n", qPrintable(dbopt.pdf2File));
            if (!reader.parse(dbopt.pdf2File)) {
                fputs("Error: could not open PDF-2 file.\n", stderr);
                return 1;
            }
            printf("\nDone.\n");
        } else {
            // CIF source: read each CIF file via Fortran get_crystal_info_from_cif
            int nProcessed = 0;
            int nErrors    = 0;
            //constexpr int kMaxCif = 1000;  // TODO: remove limit
            CifReader reader;
            CifDbPopulator populator(&db);
            QObject::connect(&reader, &CifReader::cifFound,
                             [&](const QString &cifPath) {
                                 //if (nProcessed + nErrors >= kMaxCif) return;
                                 CifCrystalInfo info;
                                 if (readCrystalInfoFromCif(cifPath, info)) {
                                     populator.onCifReady(cifPath, info);
                                     ++nProcessed;
                                     printf("\r  [%d ok / %d err] %s",
                                            nProcessed, nErrors,
                                            qPrintable(cifPath));
                                     fflush(stdout);
                                 } else {
                                     ++nErrors;
                                     fprintf(stderr, "\n  Warning: failed to read %s\n",
                                             qPrintable(cifPath));
                                 }
                             });
            QObject::connect(&reader, &CifReader::finished,
                             [&](int n) {
                                 populator.onFinished(nProcessed);
                                 printf("\nScanned %d CIF files: %d ok, %d errors.\n",
                                        n, nProcessed, nErrors);
                             });
            printf("Scanning CIF folder: %s\n", qPrintable(dbopt.cifDir));
            reader.scan(dbopt.cifDir, dbopt.recursive);
        }

        db.close();
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

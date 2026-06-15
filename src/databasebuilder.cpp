#include "databasebuilder.h"
#include "qualxdbcreator.h"
#include "qualxdbpopulator.h"
#include "cifdbpopulator.h"
#include "cifreader.h"
#include "pdf2reader.h"
#include "libcomune.h"

#include <QObject>
#include <QApplication>
#include <QEventLoop>
#include <QFileInfo>
#include <QProgressDialog>
#include <QSqlDatabase>
#include <QSqlQuery>
#include <QTimer>
#include <QUuid>

bool DatabaseBuilder::buildPdfDatabase(const QString &basePath,
                                       const QString &pdf2FilePath,
                                       ProgressFn progress)
{
    QualxDbCreator db;
    if (!db.create(basePath, QualxDbCreator::DbType::Pdf2))
        return false;

    Pdf2Reader reader;
    QualxDbPopulator populator(&db);

    QObject::connect(&reader, &Pdf2Reader::cardReady,
                     &populator, &QualxDbPopulator::onCardReady);
    QObject::connect(&reader, &Pdf2Reader::finished,
                     &populator, &QualxDbPopulator::onFinished);

    if (progress) {
        QObject::connect(&reader, &Pdf2Reader::progress,
                         [progress](int cards, qint64 bytesRead, qint64 totalBytes) {
                             progress(cards, bytesRead, totalBytes);
                         });
    }

    return reader.parse(pdf2FilePath);
}

bool DatabaseBuilder::buildPdfDatabase(const QString &basePath,
                                       const QString &pdf2FilePath,
                                       QWidget *parent,
                                       bool *outCancelled)
{
    QualxDbCreator db;
    if (!db.create(basePath, QualxDbCreator::DbType::Pdf2))
        return false;

    Pdf2Reader reader;
    QualxDbPopulator populator(&db);

    QObject::connect(&reader, &Pdf2Reader::cardReady,
                     &populator, &QualxDbPopulator::onCardReady);
    QObject::connect(&reader, &Pdf2Reader::finished,
                     &populator, &QualxDbPopulator::onFinished);

    // Use total record count as progress bar maximum (each record is 80 bytes).
    const qint64 fileSize   = QFileInfo(pdf2FilePath).size();
    const int    totalSteps = (fileSize > 0) ? int(fileSize / 80) : 0;

    QProgressDialog dlg(QObject::tr("Building database, please wait…"),
                        QObject::tr("Cancel"), 0, totalSteps, parent);
    dlg.setWindowTitle(QObject::tr("Creating Database"));
    dlg.setWindowModality(Qt::WindowModal);
    dlg.setMinimumDuration(0);
    dlg.setMinimumWidth(350);
    dlg.setValue(0);
    dlg.show();
    {
        // Give the window manager time to map and paint the dialog before
        // starting the blocking parse below.
        QEventLoop loop;
        QTimer::singleShot(20, &loop, &QEventLoop::quit);
        loop.exec();
    }

    // Track cancellation via a local flag instead of dlg.wasCanceled() after close:
    // QProgressDialog::closeEvent() may call cancel() which sets the internal flag,
    // making wasCanceled() return true even when the user never clicked Cancel.
    bool wasCancelled = false;

    QObject::connect(&reader, &Pdf2Reader::progress,
                     [&reader, &dlg, &wasCancelled](int cards, qint64 bytesRead, qint64) {
                         dlg.setValue(int(bytesRead / 80));
                         dlg.setLabelText(QObject::tr("%1 cards processed…").arg(cards));
                         QApplication::processEvents();
                         if (dlg.wasCanceled() && !wasCancelled) {
                             wasCancelled = true;
                             reader.cancel();
                         }
                     });

    QApplication::setOverrideCursor(Qt::WaitCursor);
    const bool parsed = reader.parse(pdf2FilePath);
    QApplication::restoreOverrideCursor();

    // Use hide() instead of close() to avoid triggering closeEvent() -> cancel()
    // which would corrupt the wasCanceled() state.
    dlg.hide();

    if (outCancelled)
        *outCancelled = wasCancelled;

    return parsed && !wasCancelled;
}

bool DatabaseBuilder::buildCifDatabase(const QString &basePath,
                                       const QString &cifDir,
                                       bool recursive,
                                       bool inorganicOnly,
                                       CifProgressFn progress,
                                       CifBuildStats *stats)
{
    int ok = 0, skipped = 0, errors = 0;

    QualxDbCreator db;
    if (!db.create(basePath, QualxDbCreator::DbType::CifFiles))
        return false;

    CifReader reader;
    CifDbPopulator populator(&db);

    QObject::connect(&reader, &CifReader::cifFound,
                     [&](const QString &cifPath) {
                         CifCrystalInfo info;
                         const bool success = readCrystalInfoFromCif(cifPath, info, inorganicOnly);
                         if (success) {
                             populator.onCifReady(cifPath, info);
                             ++ok;
                         } else if (inorganicOnly && info.nat == 0) {
                             ++skipped;
                         } else {
                             ++errors;
                         }
                         if (progress)
                             progress(ok, skipped, errors, cifPath);
                     });
    QObject::connect(&reader, &CifReader::finished,
                     [&](int n) { populator.onFinished(n); });

    reader.scan(cifDir, recursive);
    db.close();

    if (stats) {
        stats->ok      = ok;
        stats->skipped = skipped;
        stats->errors  = errors;
    }
    return true;
}

bool DatabaseBuilder::buildCifDatabase(const QString &basePath,
                                       const QString &cifDir,
                                       bool recursive,
                                       QWidget *parent,
                                       bool *outCancelled)
{
    QualxDbCreator db;
    if (!db.create(basePath, QualxDbCreator::DbType::CifFiles))
        return false;

    CifReader reader;
    CifDbPopulator populator(&db);

    int ok = 0, errors = 0;
    bool wasCancelled = false;

    QProgressDialog dlg(QObject::tr("Building CIF database, please wait…"),
                        QObject::tr("Cancel"), 0, 0, parent);
    dlg.setWindowTitle(QObject::tr("Creating Database"));
    dlg.setWindowModality(Qt::WindowModal);
    dlg.setMinimumDuration(0);
    dlg.setMinimumWidth(350);
    dlg.show();
    {
        // Give the window manager time to map and paint the dialog before
        // starting the blocking scan below.
        QEventLoop loop;
        QTimer::singleShot(20, &loop, &QEventLoop::quit);
        loop.exec();
    }

    QObject::connect(&reader, &CifReader::cifFound,
                     [&](const QString &cifPath) {
                         CifCrystalInfo info;
                         if (readCrystalInfoFromCif(cifPath, info, /*inorganicOnly=*/false)) {
                             populator.onCifReady(cifPath, info);
                             ++ok;
                         } else {
                             ++errors;
                         }
                         dlg.setLabelText(
                             QObject::tr("%1 cards processed (%2 errors)…").arg(ok).arg(errors));
                         QApplication::processEvents();
                         if (dlg.wasCanceled() && !wasCancelled) {
                             wasCancelled = true;
                             reader.cancel();
                         }
                     });
    QObject::connect(&reader, &CifReader::finished,
                     [&](int n) { populator.onFinished(n); });

    QApplication::setOverrideCursor(Qt::WaitCursor);
    reader.scan(cifDir, recursive);
    QApplication::restoreOverrideCursor();

    dlg.hide();
    db.close();

    if (outCancelled)
        *outCancelled = wasCancelled;

    return !wasCancelled;
}

QString DatabaseBuilder::queryContentType(const QString &basePath)
{
    const QString connName = QStringLiteral("dbbuilder_type_%1")
                             .arg(QUuid::createUuid().toString(QUuid::Id128));
    QString type;
    {
        QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE", connName);
        db.setDatabaseName(basePath + ".sq");
        if (db.open()) {
            QSqlQuery q(db);
            if (q.exec("SELECT type FROM infodb") && q.first())
                type = q.value(0).toString().trimmed();
            db.close();
        }
    }
    QSqlDatabase::removeDatabase(connName);
    return type;
}

int DatabaseBuilder::queryEntries(const QString &basePath)
{
    // Use a unique connection name to avoid conflicts with open connections.
    const QString connName = QStringLiteral("dbbuilder_%1")
                             .arg(QUuid::createUuid().toString(QUuid::Id128));
    int ncard = -1;
    {
        QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE", connName);
        db.setDatabaseName(basePath + ".sq");
        if (db.open()) {
            QSqlQuery q(db);
            if (q.exec("SELECT ncard FROM infodb") && q.first())
                ncard = q.value(0).toInt();
            db.close();
        }
    }
    QSqlDatabase::removeDatabase(connName);
    return ncard;
}

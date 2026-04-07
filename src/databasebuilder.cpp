#include "databasebuilder.h"
#include "qualxdbcreator.h"
#include "qualxdbpopulator.h"
#include "pdf2reader.h"

#include <QObject>
#include <QApplication>
#include <QFileInfo>
#include <QProgressDialog>
#include <QSqlDatabase>
#include <QSqlQuery>
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
    dlg.setValue(0);

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

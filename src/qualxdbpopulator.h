#pragma once

#include <QObject>
#include <QSqlQuery>
#include <QMap>
#include <QPair>
#include <QStringList>
#include "pdf2reader.h"
#include "qualxdbcreator.h"

// -----------------------------------------------------------------------
// QualxDbPopulator
//
// Receives Pdf2Card objects from Pdf2Reader and inserts them into the
// databases managed by QualxDbCreator.
//
// Usage:
//   QualxDbCreator db;
//   db.create("/path/to/mydb");
//
//   Pdf2Reader reader;
//   QualxDbPopulator populator(&db);
//
//   QObject::connect(&reader, &Pdf2Reader::cardReady,
//                    &populator, &QualxDbPopulator::onCardReady);
//   QObject::connect(&reader, &Pdf2Reader::finished,
//                    &populator, &QualxDbPopulator::onFinished);
//   reader.parse("/path/to/pdf2.dat");
// -----------------------------------------------------------------------
class QualxDbPopulator : public QObject
{
    Q_OBJECT

public:
    explicit QualxDbPopulator(QualxDbCreator *db, QObject *parent = nullptr);

public slots:
    void onCardReady(const Pdf2Card &card);
    void onFinished(int totalCards);

private:
    // Extracts unique chemical element symbols from a PDF-2 formula string.
    // Mirrors the splitinfochemical + eliminaduplicati logic from the original C code:
    //   - removes digits, brackets, punctuation, and standalone 'n'/'x'/'z' variables
    //   - splits on whitespace and deduplicates
    static QStringList extractElements(const QString &formula);

    // Builds a comma-separated "%f," string of d-values sorted by d descending,
    // with duplicate (d, intensity) pairs removed.
    static QString buildDvalString(const QVector<Pdf2Peak> &peaks, int &ndOut);

    // Builds a comma-separated "%f," string of intensities in the same order as buildDvalString.
    static QString buildIvalString(const QVector<Pdf2Peak> &peaks);

    // Deduplicates peaks, sorts by intensity descending, takes the top n,
    // then sorts those by d descending.  Single shared sort used by both
    // buildBestDval and buildTopDval.
    static QVector<Pdf2Peak> selectTopPeaks(const QVector<Pdf2Peak> &peaks, int n);

    // Formats the pre-selected peaks as a comma-separated "%f," string (with trailing comma).
    static QString buildBestDval(const QVector<Pdf2Peak> &topPeaks);

    // Formats the first min(kNbestd, size) of the pre-selected peaks as a
    // comma-separated "%f" string (no trailing comma).
    // nOut is set to the actual number of formatted peaks.
    static QString buildTopDval(const QVector<Pdf2Peak> &topPeaks, int &nOut);

    void insertChemical(const Pdf2Card &card);
    void insertId(const Pdf2Card &card, const QVector<Pdf2Peak> &top6);
    void insertSubfiles(const Pdf2Card &card);
    void insertInfo(const Pdf2Card &card);
    void insertTop(const Pdf2Card &card, const QVector<Pdf2Peak> &top6);

    void populateSpgrStat();
    void accumulateInfoStat(const Pdf2Card &card);
    void populateInfoStat();
    static void insertNumTable(QSqlDatabase &db, const QString &table,
                               const QMap<double,  QStringList> &data);
    static void insertTxtTable(QSqlDatabase &db, const QString &table,
                               const QMap<QString, QStringList> &data);

    // Transaction helpers
    void beginTransaction();
    void commitTransaction();

    QualxDbCreator *m_db;

    QSqlQuery m_chemQuery;    // prepared INSERT for chemical table
    QSqlQuery m_idQuery;      // prepared INSERT for id table
    QSqlQuery m_subQuery;     // prepared INSERT for subfiles table
    QSqlQuery m_infoQuery;    // prepared INSERT for info table
    QSqlQuery m_topQuery;     // prepared INSERT for top table (.sq.search)

    static constexpr int kNbestd = 3;   // number of best d-values to store in top

    bool  m_inTransaction = false;
    int   m_cardCount     = 0;
    int   m_rowCounter    = 0;  // sequential 'n' primary key for id table
    static constexpr int kTransactionSize = 500;

    // Accumulated data for spgrstat: key=(spacegroup, crystalSystemName), value=list of card ids
    QMap<QPair<QString,QString>, QStringList> m_spgrStat;

    // Accumulated data for infostat tables
    QMap<double,  QStringList> m_statA, m_statB, m_statC;
    QMap<double,  QStringList> m_statAlpha, m_statBeta, m_statGamma;
    QMap<double,  QStringList> m_statVol, m_statDens, m_statCdens;
    QMap<QString, QStringList> m_statColor, m_statSpgr, m_statType;
};

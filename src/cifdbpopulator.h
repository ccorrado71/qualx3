#pragma once

#include <QObject>
#include <QSqlQuery>
#include <QMap>
#include <QPair>
#include <QStringList>
#include <QVector>
#include "qualxdbcreator.h"
#include "libcomune.h"   // CifCrystalInfo

// -----------------------------------------------------------------------
// CifDbPopulator
//
// Inserts CifCrystalInfo records (read via Fortran get_crystal_info_from_cif)
// into the four SQLite databases managed by QualxDbCreator (DbType::CifFiles).
//
// The card numeric ID is extracted from the CIF file name
// (e.g. "/path/to/1000099.cif" → 1000099).
//
// Fields not available in CifCrystalInfo are stored as empty string or 0:
//   name, mineralname, quality          (id table)
//   authors, journal, journal_year,
//   journal_volume, journal_issue,
//   page_start, page_end, color         (info table)
//   density (measured Dm)               (info table) → 0.0
//
// Usage:
//   QualxDbCreator db;
//   db.create("/path/mydb", QualxDbCreator::DbType::CifFiles);
//   CifDbPopulator pop(&db);
//   // inside CifReader::cifFound lambda:
//   pop.onCifReady(filePath, info);
//   // at end:
//   pop.onFinished(totalFiles);
// -----------------------------------------------------------------------
class CifDbPopulator : public QObject
{
    Q_OBJECT

public:
    explicit CifDbPopulator(QualxDbCreator *db, QObject *parent = nullptr);

public slots:
    void onCifReady(const QString &filePath, const CifCrystalInfo &info);
    void onFinished(int totalFiles);

private:
    // Extracts numeric COD id from file path  ("/…/1000099.cif" → 1000099)
    static int cifIdFromPath(const QString &filePath);

    // Returns indices into info.refl_d sorted by d descending
    static QVector<int> sortedByD(const CifCrystalInfo &info);

    // Builds comma-separated "%f," d-values sorted by d descending
    static QString buildDvalString(const CifCrystalInfo &info, int &ndOut);

    // Builds comma-separated "%f," intensities in the same d-sorted order
    static QString buildIvalString(const CifCrystalInfo &info);

    // Builds top kNbestd d-values (by intensity, already sorted) as "%f" comma-separated
    static QString buildTopDval(const CifCrystalInfo &info, int &nOut);

    // Encodes an int array as a raw int32 QByteArray
    static QByteArray intsToBlob(const int *arr, int n);

    void insertChemical(int id, const CifCrystalInfo &info);
    void insertId      (int id, const CifCrystalInfo &info);
    void insertSubfile (int id, const CifCrystalInfo &info);
    void insertInfo    (int id, const CifCrystalInfo &info);
    void insertTop     (int id, const CifCrystalInfo &info);

    void accumulateInfoStat(int id, const CifCrystalInfo &info);
    void populateSpgrStat();
    void populateInfoStat();

    static void insertNumTable(QSqlDatabase &db, const QString &table,
                               const QMap<double,  QStringList> &data);
    static void insertTxtTable(QSqlDatabase &db, const QString &table,
                               const QMap<QString, QStringList> &data);

    void beginTransaction();
    void commitTransaction();

    QualxDbCreator *m_db;

    QSqlQuery m_chemQuery;
    QSqlQuery m_idQuery;
    QSqlQuery m_subQuery;
    QSqlQuery m_infoQuery;
    QSqlQuery m_topQuery;

    static constexpr int kNbestd          = 3;
    static constexpr int kTransactionSize = 500;

    bool m_inTransaction = false;
    int  m_cardCount     = 0;
    int  m_rowCounter    = 0;

    // Accumulated data for spgrstat
    QMap<QPair<QString,QString>, QStringList> m_spgrStat;

    // Accumulated data for infostat tables
    QMap<double,  QStringList> m_statA, m_statB, m_statC;
    QMap<double,  QStringList> m_statAlpha, m_statBeta, m_statGamma;
    QMap<double,  QStringList> m_statVol, m_statDens;
    QMap<QString, QStringList> m_statSpgr, m_statType;
};

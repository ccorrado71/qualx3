#pragma once

#include <QObject>
#include <QString>
#include <QSqlDatabase>

// -----------------------------------------------------------------------
// QualxDbCreator
//
// Creates and manages the four SQLite files that make up a Qualx database.
// The schema varies slightly depending on the source:
//
//   DbType::Pdf2  – built from the ICDD PDF-2 / NBS*AIDS83 dataset
//   DbType::CifFiles   – built from the Crystallography Open Database (COD)
//
// The four files produced are:
//   <baseName>.sq          – main card data (id, chemical, subfiles, …)
//   <baseName>.sq.info     – bibliographic/crystallographic details
//   <baseName>.sq.infostat – statistical lookup tables (one per parameter)
//   <baseName>.sq.search   – pre-computed search index (top d-values)
//
// Schema differences between Pdf2 and CifFiles:
//
//  .sq / id table:
//    Pdf2 has  bestdval VARCHAR(300); CifFiles does not.
//    CifFiles  has  natoms INTEGER, nreflections INTEGER; Pdf2 does not.
//
//  .sq / extra tables:
//    CifFiles has warn and warningcif; Pdf2 does not.
//
//  .sq / infodb.id type:
//    Pdf2: INTEGER   CifFiles: VARCHAR(100)
//
//  info table location:
//    Pdf2: inside .sq (main database)
//    CifFiles:  inside .sq.info
//
//  .sq.info contents:
//    Pdf2: spgrstat only
//    CifFiles:  info (full) + spgrstat
//
// .sq.infostat and .sq.search are identical for both types.
//
// Usage:
//   QualxDbCreator db;
//   if (db.create("/path/to/mydb", QualxDbCreator::DbType::Pdf2)) {
//       // populate via the QSqlDatabase accessors below
//       db.close();
//   }
// -----------------------------------------------------------------------
class QualxDbCreator : public QObject
{
    Q_OBJECT

public:
    enum class DbType { Pdf2, CifFiles };

    explicit QualxDbCreator(QObject *parent = nullptr);
    ~QualxDbCreator() override;

    // Creates all four database files at <basePath>.sq / .sq.info / …
    // Existing files are overwritten. Returns false on any error.
    bool create(const QString &basePath, DbType type = DbType::Pdf2);

    // Closes all four database connections.
    void close();

    bool   isOpen()  const;
    DbType dbType()  const { return m_type; }

    // Access the individual databases for populating (after create())
    QSqlDatabase mainDb()     const;   // .sq
    QSqlDatabase infoDb()     const;   // .sq.info
    QSqlDatabase infoStatDb() const;   // .sq.infostat
    QSqlDatabase searchDb()   const;   // .sq.search

private:
    bool createMainSchema();
    bool createInfoSchema();
    bool createInfoStatSchema();
    bool createSearchSchema();

    // Executes a list of SQL statements on the given connection;
    // logs and returns false on the first error.
    static bool execStatements(const QSqlDatabase &db,
                               const QStringList &statements);

    // Returns a connection name unique to this instance and suffix
    QString connName(const QString &suffix) const;

    QString m_basePath;    // base path without any suffix
    QString m_instanceId;  // unique tag for QSqlDatabase connection names
    DbType  m_type = DbType::Pdf2;
};

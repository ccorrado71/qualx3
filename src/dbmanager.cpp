#include "dbmanager.h"

#include <QMessageBox>
#include <QFileInfo>
#include <QSqlDatabase>
#include <QSqlError>
#include <QSqlQuery>

DbManager::DbManager()
{
}

DbManager::DbManager(const QString &path)
{
    openDb(path);
}

void DbManager::closeDb()
{
    if (m_connName.isEmpty())
        return;
    {
        QSqlDatabase db = QSqlDatabase::database(m_connName, /*open=*/false);
        if (db.isOpen())
            db.close();
    }  // db copy destroyed here, before removeDatabase()
    QSqlDatabase::removeDatabase(m_connName);
    m_connName.clear();
}

DbManager::~DbManager()
{
    closeDb();
}

bool DbManager::openDb(const QString &path)
{
    closeDb();  // close any previously open connection
    m_connName = QFileInfo(path).fileName();
    {
        QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE", m_connName);
        db.setDatabaseName(path);
        if (!db.open()) {
            QMessageBox::critical(nullptr, "Database Error", db.lastError().text());
            QSqlDatabase::removeDatabase(m_connName);
            m_connName.clear();
            return false;
        }
    }
    return true;
}

bool DbManager::isOpen() const
{
    if (m_connName.isEmpty())
        return false;
    return QSqlDatabase::database(m_connName, /*open=*/false).isOpen();
}

QSqlDatabase DbManager::db() const
{
    if (m_connName.isEmpty())
        return QSqlDatabase();
    return QSqlDatabase::database(m_connName, /*open=*/false);
}

void DbManager::debugDatabaseInfo()
{
    qDebug() << "\n=== DATABASE DEBUG INFO ===";
    qDebug() << "Connection name:" << db().connectionName();
    qDebug() << "Database name:" << db().databaseName();
    qDebug() << "Is open?" << db().isOpen();
    qDebug() << "Last error:" << db().lastError().text();

    QSqlQuery query(db());
    if (query.exec("PRAGMA database_list")) {
        qDebug() << "\nAttached databases:";
        while (query.next()) {
            qDebug() << "Seq:" << query.value(0).toInt()
            << "Name:" << query.value(1).toString()
            << "File:" << query.value(2).toString();
        }
    }

    if (query.exec("SELECT * FROM sqlite_master WHERE name='id'")) {
        if (query.next()) {
            qDebug() << "\nTable 'id' exists, type:" << query.value("type").toString();
            qDebug() << "SQL:" << query.value("sql").toString();
        } else {
            qDebug() << "\nTable 'id' DOES NOT EXIST in this database connection";
        }
    }

    qDebug() << "\n=== END DEBUG INFO ===\n";
}

int DbManager::queryForCount(const QString &queryString)
{
    int count = -1;
    if (queryString.isEmpty()) return count;

    qInfo() << "Start query count";
    QSqlQuery querycount(db());
    querycount.prepare("SELECT COUNT(*) FROM ("+queryString+")");
    if (querycount.exec()) {
        if (querycount.first()) {
            count = querycount.value(0).toInt();
        }
    } else {
        // Log the error if exec() fails
        qCritical() << "Query execution failed:" << querycount.lastError().text();
    }
    qInfo() << "End query count";

    return count;
}

// int DbManager::queryForCount(const QString &queryString)
// {
//     debugDatabaseInfo();
//     qDebug() << "Database connection details:";
//     qDebug() << "Connection name:" << m_db.connectionName();
//     qDebug() << "Database name:" << m_db.databaseName();
//     qDebug() << "Is open?" << m_db.isOpen();

//     QSqlQuery schemaQuery(m_db);
//     if (schemaQuery.exec("SELECT sql FROM sqlite_master WHERE name='id'")) {
//         if (schemaQuery.next()) {
//             qDebug() << "Schema of 'id' table:" << schemaQuery.value(0).toString();
//         } else {
//             qDebug() << "Table 'id' not found in this database connection";
//         }
//     }

//     QSqlQuery tablesQuery(m_db);
//     if (tablesQuery.exec("SELECT name, type FROM sqlite_master")) {
//         qDebug() << "Available objects:";
//         while (tablesQuery.next()) {
//             qDebug() << tablesQuery.value(0).toString() << "("
//                      << tablesQuery.value(1).toString() << ")";
//         }
//     }

//     qInfo() << "Available drivers:" << QSqlDatabase::drivers();
//     qInfo() << "Current driver:" << m_db.driverName();
//     qInfo() << "SQLite version:" << m_db.driver()->handle();

//     QSqlQuery versionQuery(m_db);
//     if (versionQuery.exec("SELECT sqlite_version();")) {
//         if (versionQuery.next()) {
//             qInfo() << "SQLite Version:" << versionQuery.value(0).toString();
//         } else {
//             qCritical() << "Failed to fetch SQLite version!";
//         }
//     } else {
//         qCritical() << "Failed to execute version query:" << versionQuery.lastError().text();
//     }
//     qInfo() << "SQLite Driver Version:" << QSqlDatabase::database().driver()->handle();

//     int count = -1;
//     if (queryString.isEmpty()) {
//         qCritical() << "Query string is empty!";
//         return count;
//     }

//     if (!m_db.isOpen()) {
//         qCritical() << "Database is not open!";
//         return count;
//     }

//     QSqlQuery testQuery(m_db);
//     if (testQuery.exec("SELECT 1")) {
//         qInfo() << "Test query succeeded";
//     } else {
//         qCritical() << "Basic test query failed:" << testQuery.lastError();
//     }

//     qInfo() << "Start query count";
//     qInfo() << "Query String: " << queryString;

//     QSqlQuery querycount(m_db);
//     QString fullQuery = "SELECT COUNT(*) FROM (" + queryString + ")";
//     qInfo() << "Full Query: " << fullQuery;

//     // Rimuovi prepare e usa exec direttamente
//     if (querycount.exec(fullQuery)) {
//         if (querycount.first()) {
//             count = querycount.value(0).toInt();
//         } else {
//             qCritical() << "Failed to fetch the first row!";
//         }
//     } else {
//         qCritical() << "Query execution failed:" << querycount.lastError().text();
//         qDebug() << "Full query was:" << fullQuery;  // Aggiungi questo
//     }

//     // QSqlQuery querycount(m_db);
//     // QString fullQuery = "SELECT COUNT(*) FROM (" + queryString + ")";
//     // querycount.prepare(fullQuery);
//     // qInfo() << "Full Query: " << fullQuery;

//     // if (querycount.exec()) {
//     //     if (querycount.first()) {
//     //         count = querycount.value(0).toInt();
//     //     } else {
//     //         qCritical() << "Failed to fetch the first row!";
//     //     }
//     // } else {
//     //     qCritical() << "Query execution failed:" << querycount.lastError().text();
//     //     qDebug() << "Codice errore SQL:" << querycount.lastError().nativeErrorCode();
//     //     qDebug() << "Testo errore SQL:" << querycount.lastError().databaseText();
//     //     qDebug() << "Testo errore driver:" << querycount.lastError().driverText();
//     // }

//     qInfo() << "End query count";

//     return count;
// }


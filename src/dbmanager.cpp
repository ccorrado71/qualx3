#include "dbmanager.h"

#include <QMessageBox>
#include <QFileInfo>
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
    if (m_db.isOpen())
    {        
        //qInfo() << "ConnName: " << m_db.connectionName();
        QString connName = m_db.connectionName();
        m_db.close();
        m_db = QSqlDatabase(); //reset m_db by assigning a default constructor
        //m_db.removeDatabase(m_db.connectionName());
        //qInfo() << "ConnName: " << m_db.connectionName();
        //m_db.removeDatabase(connName);
        m_db.removeDatabase(connName);
    }

}

DbManager::~DbManager()
{
    closeDb();
}

bool DbManager::openDb(const QString &path)
{
    m_db = QSqlDatabase::addDatabase("QSQLITE", QFileInfo(path).fileName());
    m_db.setDatabaseName(path);

    if (!m_db.open()) {
        QMessageBox::critical(nullptr,"Database Error",m_db.lastError().text());
        return false;
    }
    return true;
}

bool DbManager::isOpen() const
{
    return m_db.isOpen();
}

QSqlDatabase DbManager::db() const
{
    return m_db;
}

void DbManager::debugDatabaseInfo()
{
    qDebug() << "\n=== DATABASE DEBUG INFO ===";
    qDebug() << "Connection name:" << m_db.connectionName();
    qDebug() << "Database name:" << m_db.databaseName();
    qDebug() << "Is open?" << m_db.isOpen();
    qDebug() << "Last error:" << m_db.lastError().text();

    QSqlQuery query(m_db);
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
    QSqlQuery querycount(m_db);
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


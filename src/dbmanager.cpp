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

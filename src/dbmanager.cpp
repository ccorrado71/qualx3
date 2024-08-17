#include "dbmanager.h"

#include <QMessageBox>

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

void DbManager::getInfo(int &ncard, QString &type)
{
    QSqlQuery queryInfo(m_db);
    queryInfo.prepare("SELECT ncard, type FROM infodb");

    if (queryInfo.exec()) {
        queryInfo.first();
        ncard = queryInfo.value(0).toInt();
        type = queryInfo.value(1).toString();        
    }
}

QSqlDatabase DbManager::db() const
{
    return m_db;
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
    }
    qInfo() << "End query count";

    return count;
}

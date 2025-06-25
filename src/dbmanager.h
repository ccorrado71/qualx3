#ifndef DBMANAGER_H
#define DBMANAGER_H

#include <QSqlDatabase>

class DbManager
{
public:
    DbManager();
    DbManager(const QString &path);
    ~DbManager();

    bool openDb(const QString &path);
    void closeDb();
    bool isOpen() const;
    int queryForCount(const QString &queryString);
    QSqlDatabase db() const;

private:
    QSqlDatabase m_db;
    QString queryElementString(const QString &elString);
    void debugDatabaseInfo();
};

#endif // DBMANAGER_H

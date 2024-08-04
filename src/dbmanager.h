#ifndef DBMANAGER_H
#define DBMANAGER_H

#include <QtSql>

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
    void getInfo(int &ncard, QString &type);
    QSqlDatabase db() const;

private:
    QSqlDatabase m_db;
    QString queryElementString(const QString &elString);
};

#endif // DBMANAGER_H

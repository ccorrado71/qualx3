#ifndef DBMANAGER_H
#define DBMANAGER_H

#include "dbquerybuilder.h"
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
    //void getCardInfo(const QString &idCard);
    //void getCardAdditionalInfo(const QString &idCard);
    //void makeQuery(const DbQueryBuilder &builder);

    QSqlDatabase db() const;

private:
    QSqlDatabase m_db;
    QString queryElementString(const QString &elString);
    //void makeQueryCrystalSystem(const QString  &qString);
    //void makeQueryInfoIds(const QString &idsString, int count);
};

#endif // DBMANAGER_H

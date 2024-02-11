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

    void getInfo(int &ncard, QString &type);
    void getCardInfo(const QString &idCard);
    void getCardAdditionalInfo(const QString &idCard);
    //void makeQuery(const QString &names, const QStringList &subfiles, const QString &elements);
    void makeQuery(const DbQueryBuilder &builder);
    //void queryRestraintName(const QString &name, const QString &subFile);

private:
    QSqlDatabase m_db;
    int queryForCount(const QString &queryString);
    QString querySubfilesString(const QStringList &subfiles);
    QString queryNameString(const QString &names);
    QString queryElementString(const QString &elString);
    void queryInfoIds(const QString &idsString, int count);
};

#endif // DBMANAGER_H

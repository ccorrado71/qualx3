#ifndef QUALXDBMANAGER_H
#define QUALXDBMANAGER_H

#include "dbmanager.h"

class QualxDbManager
{
public:
    QualxDbManager();
    ~QualxDbManager();
    bool openDatabases(const QString &path);
    void closeDatabeses();
    void makeQuery(const DbQueryBuilder &builder);

private:
    DbManager dbMain;
    DbManager dbInfo;
    DbManager dbInfoStat;
    DbManager dbSearch;

    void getCardInfo(const QString &idCard);
    void getCardAdditionalInfo(const QString &idCard);
    void makeQueryCrystalSystem(const QString  &qString);
    void makeQueryInfoIds(const QString &idsString, int count);
};

#endif // QUALXDBMANAGER_H

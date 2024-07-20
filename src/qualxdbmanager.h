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
    int  makeQueryCellPar(const QString &qString, QString &result);
    int  makeQueryCellParameters(const QStringList &qParList, QString &result);
    int  makeQuerySymmetry(const QString &qString, QString &result);
    void makeQueryInfoIds(const QString &idsString, int count);
    int stringInnerJoin(const QStringList &list1, const QStringList &list2, QStringList &result);
};

#endif // QUALXDBMANAGER_H

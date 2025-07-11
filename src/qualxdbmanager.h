#ifndef QUALXDBMANAGER_H
#define QUALXDBMANAGER_H

#include "dbmanager.h"
#include "dbquerybuilder.h"

class QualxDbManager
{
public:
    QualxDbManager();
    ~QualxDbManager();
    bool openDatabases(const QString &path);
    void closeDatabeses();
    void makeQuery(const DbQueryBuilder &builder);
    void makeQueryStrongest(const DbQueryBuilder &builder);
    void getInfo(int &ncard, QString &type);
    void getCardInfo(const QString &idCard);
    void getCardAdditionalInfo(const QString &idCard);

private:
    DbManager dbMain;
    DbManager dbInfo;
    DbManager dbInfoStat;
    DbManager dbSearch;
    int nStrongest;

    int  makeQueryCellPar(const QString &qString, QString &result);
    int  makeQueryCellParameters(const QStringList &qParList, QString &result);
    int  makeQuerySymmetry(const QString &qString, QString &result);
    void makeQueryInfoIds(const QString &idsString, bool addDeleted, int count);
    void makeQuerySearch(bool addDeleted, QString &result);
    void makeQuerySearchStrongest(QString &result);
    int stringInnerJoin(const QStringList &list1, const QStringList &list2, QStringList &result);
    //QVector<double> extractNumbers(const QString& input, int n, int m);
};

#endif // QUALXDBMANAGER_H

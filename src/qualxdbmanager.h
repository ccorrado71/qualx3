#ifndef QUALXDBMANAGER_H
#define QUALXDBMANAGER_H

#include <QString>

struct CardInfo {
    // id table
    QString id, name, mineralName, chemicalFormula, spaceGroup, quality, rir;
    int     nrec = 0, nd = 0;
    // info table
    QString authors, journal, journalVolume, pageStart, pageEnd, color, type;
    int     journalYear = 0, z = 0;
    double  crystalDensity = 0.0, volume = 0.0, density = 0.0;
    double  a = 0.0, b = 0.0, c = 0.0;
    double  alpha = 0.0, beta = 0.0, gamma = 0.0;
    double  muCuKa = 0.0;
    bool    valid = false;
};

#include "dbmanager.h"
#include "dbquerybuilder.h"
#include "cardtype.h"

#include <QList>
#include <QPair>
#include <QString>
#include <functional>

class QualxDbManager
{
public:
    using ProgressCallback = std::function<void(int current, int total)>;

    QualxDbManager();
    ~QualxDbManager();
    bool openDatabases(const QString &path);
    void closeDatabeses();
    QVector<CardType> makeQuery(const DbQueryBuilder &builder, ProgressCallback progress = nullptr);
    void makeQueryStrongest(const DbQueryBuilder &builder, QVector<CardType> &acceptedCards,
                            ProgressCallback progress = nullptr);
    void makeQueryWithoutStrongest(const DbQueryBuilder &builder, QVector<CardType> &acceptedCards,
                                   ProgressCallback progress = nullptr);
    void getInfo(int &ncard, QString &type);
    void getCardInfo(const QString &idCard);
    void getCardAdditionalInfo(const QString &idCard);
    CardInfo queryCard(const QString &idCard) const;
    QList<QPair<QString,int>> querySpaceGroups() const;
    QList<QPair<QString,int>> queryColors() const;

private:
    DbManager dbMain;
    DbManager dbInfo;
    DbManager dbInfoStat;
    DbManager dbSearch;
    int nStrongest;

    static QVector<double> blobToDoubleVector(const QByteArray &blob);

    void applyRestraintsToIds(const DbQueryBuilder &builder, QStringList &ids, int &count);
    int  makeQueryCellPar(const QString &qString, QString &result);
    int  makeQueryCellParameters(const QStringList &qParList, QString &result);
    int  makeQuerySymmetry(const QString &qString, QString &result);
    void makeQueryInfoIdsWithFom(const QString &idsString, const DbQueryBuilder &builder, int count, QVector<CardType> &acceptedCards, ProgressCallback progress=nullptr);
    QVector<CardType> makeQueryInfoIds(const QString &idsString, const DbQueryBuilder &builder, int count, ProgressCallback progress=nullptr);
    void makeQuerySearch(bool addDeleted, QString &result);
    void makeQuerySearchStrongest(QString &result);
    int stringInnerJoin(const QStringList &list1, const QStringList &list2, QStringList &result);
    //QVector<double> extractNumbers(const QString& input, int n, int m);
};

#endif // QUALXDBMANAGER_H

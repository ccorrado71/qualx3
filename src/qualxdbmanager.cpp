#include "qualxdbmanager.h"
#include "searchutil.h"
#include "scopedtimer.h"

#include <QSqlQuery>
#include <QSqlError>

QualxDbManager::QualxDbManager()
    : nStrongest(3)
{

}

QualxDbManager::~QualxDbManager()
{
    closeDatabeses();
}

bool QualxDbManager::openDatabases(const QString &path)
{
    if (!dbMain.openDb(path+".sq")) return false;
    if (!dbInfo.openDb(path+".sq.info")) return false;
    if (!dbInfoStat.openDb(path+".sq.infostat")) return false;
    if (!dbSearch.openDb(path+".sq.search")) return false;
    return true;
}

void QualxDbManager::closeDatabeses()
{
    dbMain.closeDb();
    dbInfo.closeDb();
    dbInfoStat.closeDb();
    dbSearch.closeDb();
}

void QualxDbManager::getCardInfo(const QString &idCard)
{
    QSqlQuery queryId(dbMain.db());
    queryId.prepare("SELECT id, name, mineralname, chemical_formula, spacegroup, quality, rir, nrec, intensita, dvalue, nd FROM id WHERE id="+idCard);
    if (queryId.exec()) {
        if (queryId.first()) {
            qInfo() << "QUERY ID" << "id: " << queryId.value(0).toString() << Qt::endl <<
                //                       "name: " << queryId.value(1).toString() << Qt::endl <<
                //                       "mineralname: " << queryId.value(2).toString() << Qt::endl <<
                //                       "chemical_formula: " << queryId.value(3).toString() << Qt::endl <<
                //                       "spacegroup: " << queryId.value(4).toString() << Qt::endl <<
                //                       "quality: " << queryId.value(5).toString() << Qt::endl <<
                //                       "RIR: " << queryId.value(6).toFloat() << Qt::endl <<
                //                       "nrec: " << queryId.value(7).toInt() << Qt::endl <<
                //                       "intensita: " << queryId.value(8).toString() << Qt::endl <<
                //                       "dvalue: " << queryId.value(9).toString() << Qt::endl <<
                "nd: " << queryId.value(10).toInt();
        } else {
            qInfo() << "ERROR: id non trovato";
        }
    }
}

void QualxDbManager::getCardAdditionalInfo(const QString &idCard)
{
    QSqlQuery queryId(dbInfo.db());
    queryId.prepare("SELECT id, authors, journal, journal_year, journal_volume, page_start, "
                    "page_end, color, crystal_density, spacegroup, type, volume, density, z, "
                    "rir, a, b, c, alpha, beta, gamma, h, k, l, mul, `mu(CuKa)` FROM info WHERE id="+idCard);
    if (queryId.exec()) {
        if (queryId.first()) {
            qInfo() << "QUERY ADD. INFO: " << queryId.value(0).toString();
        } else {
            qInfo() << "ERROR: id non trovato";
        }
    }
}

int QualxDbManager::makeQueryCellPar(const QString &qString, QString &result)
{
    //qInfo() << "Start query cell parameter";
    int nData = 0;
    result.clear();

    QSqlQuery query = QSqlQuery(dbInfoStat.db());

    query.prepare(qString);
    if (query.exec()) {
        while (query.next()) {
            if (result.isEmpty()) {
                result = query.value(0).toString();
            } else {
                result = result + "," + query.value(0).toString();
            }
            nData = nData + query.value(1).toInt();
        }
    }
    // qInfo() << "N: " << nData;
    // qInfo() << "End query cell parameter";
    return nData;
}

int QualxDbManager::makeQueryCellParameters(const QStringList &qParList, QString &result)
{

    int nData = 0;
    result.clear();

    for (int i = 0; i < qParList.size(); i++) {
        qInfo() << "Start query cell parameter n. " << i;
        if (i == 0) {
            nData = makeQueryCellPar(qParList.at(i), result);
        } else {
            QString tmpResult;
            int tmpData = makeQueryCellPar(qParList.at(i), tmpResult);
            QStringList list1 = result.split(",");
            QStringList list2 = tmpResult.split(",");
            QStringList resultTmp;
            nData = stringInnerJoin(list1, list2, resultTmp);
            qInfo() << "Inner join: " << list1.size() << " - " << list2.size() << " -> " << nData;
            result = resultTmp.join(",");
        }
        //qInfo() << "ID: " << result;
        qInfo() << "N: " << nData;
        qInfo() << "End query cell parameter n. " << i;
    }

    return nData;
}

int QualxDbManager::makeQuerySymmetry(const QString &qString, QString &result)
{
    qInfo() << "Start query symmetry";
    int ndata = 0;
    result.clear();

    QSqlQuery query;
    if (qString.contains("from spgrstat"))
        query = QSqlQuery(dbInfo.db());
    else
        query = QSqlQuery(dbInfoStat.db());

    query.prepare(qString);
    if (query.exec()) {
        while (query.next()) {
            if (result.isEmpty()) {
                result = query.value(0).toString();
            } else {
                result = result + "," + query.value(0).toString();
            }
            ndata = ndata + query.value(1).toInt();
        }
    }
    //qInfo() << "Space groups: " << result;
    qInfo() << "N: " << ndata;
    qInfo() << "End query symmetry";
    return ndata;
}

void QualxDbManager::makeQueryInfoIds(const QString &idsString, bool addDeleted, int count)
{
    ScopedTimer timer("QualxDbManager::makeQueryInfoIds");

    qInfo() << "Start queryIds";
    QSqlQuery queryIds(dbMain.db());
    QString queryString;
    queryString = "Select id, name, mineralname, chemical_formula, spacegroup, "
                  "quality, rir, nrec, intensita, dvalue,n from id";
    if (addDeleted) {
        queryString = queryString + " where trim(quality)!='D' and id in ("+idsString+")";
    } else {
        queryString = queryString + " where id in ("+idsString+")";
    }
    queryIds.prepare(queryString);
    // if (addDeleted) {
    //     queryIds.prepare("Select id, name, mineralname, chemical_formula, spacegroup, "
    //                      "quality, rir, nrec, intensita, dvalue,n from id where trim(quality)!='D' where id in ("+idsString+")");
    // } else {
    //     queryIds.prepare("Select id, name, mineralname, chemical_formula, spacegroup, "
    //                      "quality, rir, nrec, intensita, dvalue,n from id where id in ("+idsString+")");
    // }
    int nId = 0;
    if (queryIds.exec()) {
        while (queryIds.next()) {
            nId++;
            float perc = 100.0f*nId/count;
            if (fmod(perc,10) == 0) qInfo() << perc << "%";
            qInfo() << "id =" << queryIds.value(0).toString() << queryIds.value(3).toString()
                    << queryIds.value(4).toString();
        }
    }
    qInfo() << "End queryIds";
}

void QualxDbManager::makeQuerySearch(bool addDeleted, QString &result)
{
    qInfo() << "Start query for search";
    QSqlQuery querySearch(dbMain.db());
    QString queryString;
    queryString = "Select id, name, mineralname, chemical_formula, spacegroup, "
                  "quality, rir, nrec, intensita, dvalue,n from id";
    if (addDeleted) {
        queryString = queryString + " where trim(quality)!='D'";
    }
    int count = dbMain.queryForCount(queryString);
    qInfo() << "Count: " << count;
    querySearch.prepare(queryString);
    if (querySearch.exec()) {
        // while (querySearch.next()) {
        //     result = result + querySearch.value(0).toString() + ",";
        // }
    }
    qInfo() << "End query for search";
}

void QualxDbManager::makeQuerySearchStrongest(QString &result)
{
    qInfo() << "Start query search with strongest";
    int count = dbSearch.queryForCount("top");
    qInfo() << "Count: " << count;
    QSqlQuery queryStrong(dbSearch.db());
    queryStrong.prepare("select id, dval from top");
    if (queryStrong.exec()) {
        // while (queryStrong.next()) {
        //     result = result + queryStrong.value(0).toString() + ",";
        // }
    }
    qInfo() << "End query strongest";
}

int QualxDbManager::stringInnerJoin(const QStringList &list1, const QStringList &list2, QStringList &result)
{
    foreach(QString str, list1) {
        if (list2.contains(str)) {
            result.append(str);
        }
    }
    return result.size();
}

// QVector<double> QualxDbManager::extractNumbers(const QString &input, int n, int m)
// {
//     // n: Number of numbers in the string
//     // m: How many numbers you want to extract

//     QVector<double> result;
//     // Split the string into a list using comma as separator
//     QStringList numberStrings = input.split(',', Qt::SkipEmptyParts);

//     // Check: if n does not match the actual number of elements, adjust n
//     //if (n > numberStrings.size())
//     //    n = numberStrings.size();

//     // Extract up to m numbers, but not more than those available
//     int count = qMin(m, n);

//     for (int i = 0; i < count; ++i) {
//         bool ok;
//         double number = numberStrings[i].toDouble(&ok);
//         if (ok)
//             result.append(number);
//     }
//     return result;
// }

void QualxDbManager::makeQuery(const DbQueryBuilder &builder)
{
    // QString resultSearch;
    // makeQuerySearchStrongest(resultSearch);
    // makeQuerySearch(builder.deletedEnabled(), resultSearch);
    // return;

    //Step 1: build and make query for cell parameters
    QString queryResult;
    int nCountQuery = 0;
    QStringList queryCellPar = builder.getQueryCellPar();
    if (!queryCellPar.isEmpty()) {
        nCountQuery = makeQueryCellParameters(queryCellPar, queryResult);
    }

    //Step 2: build and make query for crystal system and space groups
    QString qSimmetryResult;
    int nCountSimmetry = 0;
    QString querySymm = builder.getSymmetryQueryString();
    if (!querySymm.isEmpty()) {
        nCountSimmetry = makeQuerySymmetry(querySymm, qSimmetryResult);
        if (!queryResult.isEmpty() && nCountSimmetry > 0) {
            QStringList list1 = queryResult.split(",");
            QStringList list2 = qSimmetryResult.split(",");
            QStringList resultTmp;
            nCountQuery = stringInnerJoin(list1, list2, resultTmp);
            qInfo() << "Inner join: " << list1.size() << " - " << list2.size() << " -> " << nCountQuery;
            queryResult = resultTmp.join(",");
        } else {
            nCountQuery = nCountSimmetry;
            queryResult = qSimmetryResult;
        }
    }

    //Spep 3: get query for id entries
    QString qIdEntry = builder.getQueryIdEntry();

    //Step 4: build query for chemical elements
    QString queryChemical = builder.getChemicalQueryString();
    if (!qIdEntry.isEmpty()) {
        if (!queryChemical.isEmpty()) {
            queryChemical = qIdEntry + " intersect " + queryChemical;
        } else {
            queryChemical = qIdEntry;
        }
    }
    if (nCountQuery > 0 && !queryChemical.isEmpty()) {
        queryChemical = queryChemical + " intersect select id from chemical where id in (" + queryResult + ")";
    }

    if (!queryChemical.isEmpty()) {
        //qInfo() << "Query chemical: " << queryChemical;
        int nCountChemical = dbMain.queryForCount(queryChemical);
        qInfo() << "NCOUNT: " << nCountChemical;
        qInfo() << "Start query for idsString";
        QSqlQuery query(dbMain.db());
        query.prepare(queryChemical);

        QString idsString;
        if (query.exec()) {
            while (query.next()) {
                idsString.append("'"+query.value(0).toString()+"',");
            }
            idsString.chop(1); //rimove last ','
            qInfo() << "End query for idsString";

            //qInfo() << "idsString: " << idsString;
            makeQueryInfoIds(idsString, builder.deletedEnabled(), nCountChemical);
        }

    } else if (nCountQuery > 0) {
        makeQueryInfoIds(queryResult, builder.deletedEnabled(), nCountQuery);
    }
}

void QualxDbManager::makeQueryStrongest(const DbQueryBuilder &builder)
{
    ScopedTimer timer("QualxDbManager::makeQueryStrongest");

    QSqlQuery query(dbSearch.db());
    query.prepare("SELECT id, n, dval FROM top");

    QString idsString;
    if (query.exec()) {
        int nq = 0;
        while (query.next()) {
            QString id = query.value(0).toString();
            int n = query.value(1).toInt();
            QString dvalStr = query.value(2).toString();

            QVector<double> dStrong = SearchUtil::extractNumbers(dvalStr, n, 3);
            bool result = SearchUtil::checkStrongValuesWithTolerance(builder.getDValues(), builder.getDTol(), dStrong);
            if (result) {
                nq++;
                idsString.append("'"+id+"',");
            }
        }
        idsString.chop(1); // Remove last ','
        qInfo() << "idsString: " << idsString.length();
        qInfo() << "Number of strongest matches: " << nq;
        makeQueryInfoIds(idsString, builder.deletedEnabled(), nq);
    } else {
        qCritical() << "Failed to execute strongest query:" << query.lastError().text();
    }
}

void QualxDbManager::getInfo(int &ncard, QString &type)
{
    QSqlQuery queryInfo(dbMain.db());
    queryInfo.prepare("SELECT ncard, type FROM infodb");

    if (queryInfo.exec()) {
        queryInfo.first();
        ncard = queryInfo.value(0).toInt();
        type = queryInfo.value(1).toString();
    }
}



#include "qualxdbmanager.h"
#include "searchutil.h"
#include "scopedtimer.h"
#include "cardtype.h"

#include <QSqlQuery>
#include <QSqlError>

extern "C" void computeFOM(double tth[], double intensity[], int tsize, double *fomd);

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

QVector<double> QualxDbManager::blobToDoubleVector(const QByteArray &blob)
{
    QVector<double> result;
    QString str = QString::fromUtf8(blob);
    QStringList list = str.split(',', Qt::SkipEmptyParts);
    for (int i = 0; i < list.size(); ++i) {
        bool ok = false;
        double val = list.at(i).toDouble(&ok);
        if (ok) result.append(val);
    }
    return result;
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

void QualxDbManager::makeQueryInfoIdsWithFom(const QString &idsString, const DbQueryBuilder &builder, int count, QVector<CardType> &acceptedCards, bool calcFom, ProgressCallback progress)
{
    ScopedTimer timer("QualxDbManager::makeQueryInfoIdsWithFom");

    qInfo() << "Start queryIds";
    QSqlQuery queryIds(dbMain.db());
    QString queryString;
    queryString = "Select id, name, mineralname, chemical_formula, spacegroup, "
                  "quality, rir, nrec, dvalue, intensita, n from id";
    if (builder.deletedEnabled()) {
        queryString = queryString + " where trim(quality)!='D' and id in ("+idsString+")";
    } else {
        queryString = queryString + " where id in ("+idsString+")";
    }
    queryIds.prepare(queryString);

    int nId = 0;
    int lastProg = -1;
    double fomLim = builder.getMinFom();
    if (queryIds.exec()) {
        if (calcFom) {
            //QVector<CardType> acceptedCards;
            while (queryIds.next()) {
                nId++;
                float perc = 100.0f*nId/count;
                //if (fmod(perc,10) == 0) qInfo() << perc << "%";
                int prog = static_cast<int>(perc);
                if (progress && prog != lastProg) { lastProg = prog; progress(nId, count); }
                QByteArray dByte = queryIds.value(8).toByteArray();
                QVector<double> dvalues = blobToDoubleVector(dByte);
                QByteArray iByte = queryIds.value(9).toByteArray();
                QVector<double> ivalues = blobToDoubleVector(iByte);
                //qInfo() << "dvalues: " << dvalues;
                //1- istanzia cardtype
                CardType card;
                //2- fai una setd(dvalues, wave) che fa un set anche dei tth
                card.setD(dvalues, builder.getWave());
                card.setIntensity(ivalues);
                //card.printDandTth();
                //3- calcola FOM
                int size = card.getTth().size();
                //qInfo() << "SIZE: " << nId << size;
                if (size > 0) {
                    double fomd;
                    computeFOM(card.getTth().data(), card.getIntensity().data(), size, &fomd);
                    if (fomd > fomLim) {
                        card.setId(queryIds.value(0).toString());
                        card.setChemicalName(queryIds.value(1).toString());
                        card.setMineralName(queryIds.value(2).toString());
                        card.setChemicalFormula(queryIds.value(3).toString());
                        card.setSpaceGroup(queryIds.value(4).toString());
                        card.setQuality(queryIds.value(5).toString());
                        card.setRIR(queryIds.value(6).toString());
                        card.setFomd(fomd);
                        acceptedCards.append(card);
                        //card.printCard(1);
                    }
                }
            }
        } else {
            while (queryIds.next()) {
                nId++;
                float perc = 100.0f*nId/count;
                if (fmod(perc,10) == 0) qInfo() << perc << "%";
                int prog = static_cast<int>(perc);
                if (progress && prog != lastProg) { lastProg = prog; progress(nId, count); }
                CardType card;
                card.setId(queryIds.value(0).toString());
                card.setChemicalName(queryIds.value(1).toString());
                card.setMineralName(queryIds.value(2).toString());
                card.setChemicalFormula(queryIds.value(3).toString());
                card.setSpaceGroup(queryIds.value(4).toString());
                card.setQuality(queryIds.value(5).toString());
                card.setRIR(queryIds.value(6).toString());
                acceptedCards.append(card);
            }
        }
    }
    qInfo() << "End queryIds";
}

QVector<CardType> QualxDbManager::makeQueryInfoIds(const QString &idsString, const DbQueryBuilder &builder, int count, ProgressCallback progress)
{
    QVector<CardType> cards;
    makeQueryInfoIdsWithFom(idsString, builder, count, cards, false, progress);
    return cards;
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

QVector<CardType> QualxDbManager::makeQuery(const DbQueryBuilder &builder, ProgressCallback progress)
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

    //Step 1b: density queries (stat_cdens = calculated, stat_dens = measured)
    QStringList queryDens = builder.getQueryDensity();
    if (!queryDens.isEmpty()) {
        QString densResult;
        int nCountDens = makeQueryCellParameters(queryDens, densResult);
        if (!queryResult.isEmpty() && nCountDens > 0) {
            QStringList list1 = queryResult.split(",");
            QStringList list2 = densResult.split(",");
            QStringList resultTmp;
            nCountQuery = stringInnerJoin(list1, list2, resultTmp);
            qInfo() << "Inner join density: " << list1.size() << " - " << list2.size() << " -> " << nCountQuery;
            queryResult = resultTmp.join(",");
        } else if (nCountDens > 0) {
            nCountQuery = nCountDens;
            queryResult = densResult;
        }
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

    //Step 2b: color query (stat_color in dbInfoStat)
    QString queryColor = builder.getColorQueryString();
    if (!queryColor.isEmpty()) {
        QString colorResult;
        int nCountColor = makeQuerySymmetry(queryColor, colorResult);
        if (!queryResult.isEmpty() && nCountColor > 0) {
            QStringList list1 = queryResult.split(",");
            QStringList list2 = colorResult.split(",");
            QStringList resultTmp;
            nCountQuery = stringInnerJoin(list1, list2, resultTmp);
            qInfo() << "Inner join color: " << list1.size() << " - " << list2.size() << " -> " << nCountQuery;
            queryResult = resultTmp.join(",");
        } else if (nCountColor > 0) {
            nCountQuery = nCountColor;
            queryResult = colorResult;
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
            return makeQueryInfoIds(idsString, builder, nCountChemical, progress);
        }

    } else if (nCountQuery > 0) {
        return makeQueryInfoIds(queryResult, builder, nCountQuery, progress);
    }
    return {};
}

void QualxDbManager::applyRestraintsToIds(const DbQueryBuilder &builder,
                                          QStringList &ids, int &count)
{
    if (ids.isEmpty()) return;

    // --- Stat-table restraints (cell params, symmetry, color) ---
    QString statResult;
    int     statCount = 0;
    bool    hasStatRestraints = false;

    const QStringList queryCellPar = builder.getQueryCellPar();
    if (!queryCellPar.isEmpty()) {
        statCount = makeQueryCellParameters(queryCellPar, statResult);
        hasStatRestraints = true;
    }

    const QString querySymm = builder.getSymmetryQueryString();
    if (!querySymm.isEmpty()) {
        QString symResult;
        int symCount = makeQuerySymmetry(querySymm, symResult);
        if (hasStatRestraints && symCount > 0) {
            QStringList l1 = statResult.split(QLatin1Char(','), Qt::SkipEmptyParts);
            QStringList l2 = symResult.split(QLatin1Char(','), Qt::SkipEmptyParts);
            QStringList tmp;
            statCount = stringInnerJoin(l1, l2, tmp);
            statResult = tmp.join(QLatin1Char(','));
        } else if (symCount > 0) {
            statCount = symCount;
            statResult = symResult;
        }
        hasStatRestraints = true;
    }

    const QString queryColor = builder.getColorQueryString();
    if (!queryColor.isEmpty()) {
        QString colResult;
        int colCount = makeQuerySymmetry(queryColor, colResult);
        if (hasStatRestraints && colCount > 0) {
            QStringList l1 = statResult.split(QLatin1Char(','), Qt::SkipEmptyParts);
            QStringList l2 = colResult.split(QLatin1Char(','), Qt::SkipEmptyParts);
            QStringList tmp;
            statCount = stringInnerJoin(l1, l2, tmp);
            statResult = tmp.join(QLatin1Char(','));
        } else if (colCount > 0) {
            statCount = colCount;
            statResult = colResult;
        }
        hasStatRestraints = true;
    }

    if (hasStatRestraints) {
        if (statCount == 0) { ids.clear(); count = 0; return; }
        const QStringList statIds = statResult.split(QLatin1Char(','), Qt::SkipEmptyParts);
        QStringList filtered;
        stringInnerJoin(ids, statIds, filtered);
        ids   = filtered;
        count = ids.size();
        if (ids.isEmpty()) return;
    }

    // --- Chemical / name / subfile / element restraints ---
    const QString queryChemical = builder.getChemicalQueryString();
    if (!queryChemical.isEmpty()) {
        // Build quoted id list for INTERSECT clause
        QString quotedIds;
        for (const QString &id : std::as_const(ids))
            quotedIds.append(QLatin1Char('\'') + id + QLatin1String("',"));
        quotedIds.chop(1);

        const QString combined = queryChemical
            + QStringLiteral(" INTERSECT SELECT id FROM id WHERE id IN (") + quotedIds + QLatin1Char(')');

        QSqlQuery q(dbMain.db());
        q.prepare(combined);
        if (q.exec()) {
            ids.clear();
            while (q.next())
                ids.append(q.value(0).toString());
            count = ids.size();
        }
    }
}

void QualxDbManager::makeQueryStrongest(const DbQueryBuilder &builder, QVector<CardType> &acceptedCards,
                                        ProgressCallback progress)
{
    ScopedTimer timer("QualxDbManager::makeQueryStrongest->makeQueryInfoIdsWithFom");

    QSqlQuery query(dbSearch.db());
    query.prepare("SELECT id, n, dval FROM top");

    if (!query.exec()) {
        qCritical() << "Failed to execute strongest query:" << query.lastError().text();
        return;
    }

    // Collect candidate IDs as plain strings (no quotes)
    QStringList idList;
    while (query.next()) {
        const QString id      = query.value(0).toString();
        const int     n       = query.value(1).toInt();
        const QString dvalStr = query.value(2).toString();
        const QVector<double> dStrong = SearchUtil::extractNumbers(dvalStr, n, 3);
        if (SearchUtil::checkStrongValuesWithTolerance(builder.getDValues(), builder.getDTol(), dStrong))
            idList.append(id);
    }
    qInfo() << "Number of strongest matches:" << idList.size();

    // Apply restraints from builder (cell params, symmetry, chemical, names…)
    int count = idList.size();
    applyRestraintsToIds(builder, idList, count);
    qInfo() << "Number of candidates after restraints:" << count;

    if (idList.isEmpty()) return;

    // Build quoted idsString for makeQueryInfoIdsWithFom
    QString idsString;
    for (const QString &id : std::as_const(idList))
        idsString.append(QLatin1Char('\'') + id + QLatin1String("',"));
    idsString.chop(1);

    makeQueryInfoIdsWithFom(idsString, builder, count, acceptedCards, true, progress);
}

QList<QPair<QString,int>> QualxDbManager::querySpaceGroups() const
{
    QList<QPair<QString,int>> result;
    if (!dbInfoStat.isOpen())
        return result;
    QSqlQuery q(dbInfoStat.db());
    q.prepare(QStringLiteral("SELECT val, n FROM stat_spgr ORDER BY n DESC"));
    if (q.exec()) {
        while (q.next())
            result.append({ q.value(0).toString(), q.value(1).toInt() });
    }
    return result;
}

QList<QPair<QString,int>> QualxDbManager::queryColors() const
{
    QList<QPair<QString,int>> result;
    if (!dbInfoStat.isOpen())
        return result;
    QSqlQuery q(dbInfoStat.db());
    q.prepare(QStringLiteral("SELECT val, n FROM stat_color ORDER BY n DESC"));
    if (q.exec()) {
        while (q.next())
            result.append({ q.value(0).toString(), q.value(1).toInt() });
    }
    return result;
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



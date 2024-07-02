#include "qualxdbmanager.h"

#include <QtSql>

QualxDbManager::QualxDbManager() {

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

int QualxDbManager::makeQueryCrystalSystem(const QString &qString, QString &result)
{
    qInfo() << "Start query crystal system";
    int ndata = 0;
    result.clear();
    QSqlQuery query(dbInfoStat.db());
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
    qInfo() << "End query crystal system";
    return ndata;
}

int QualxDbManager::makeQuerySpaceGroup(const QString &qString, QString &result)
{
    qInfo() << "Start query space group";
    int ndata = 0;
    result.clear();
    QSqlQuery query(dbInfo.db());
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
    qInfo() << "End query space group";
    return ndata;
}

void QualxDbManager::makeQueryInfoIds(const QString &idsString, int count)
{
    qInfo() << "Start queryIds";
    QSqlQuery queryIds(dbMain.db());
    queryIds.prepare("Select id, name, mineralname, chemical_formula, spacegroup, "
                     "quality, rir, nrec, intensita, dvalue,n from id where id in ("+idsString+")");
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

void QualxDbManager::makeQuery(const DbQueryBuilder &builder)
{
    //Gestione space groups + crystal system
    //1: scrivi due metodi (getQuery and makeQuery) che prendono in input entrambe le liste e fa un'unica query
    //simile a quella del gruppo spaziale ma con i sistemi cristallini in or (es. or type='Monoclinic')
    //2: se una delle 2 liste è vuota procedi come già indicata e sistema opportunamente il codice rimuovendo la doppia varibile
    //qCrySysResult e qSpgResult, ne basta solo una. Potresti scrivere anche una solo funzione che gestisce le quesry sulla simmetria


    //Step 1: build and make query for crystal system and space groups
    QString queryCrySys = builder.getCrySysQueryString();
    int nCountCry = 0;
    QString qCrySysResult;
    if (!queryCrySys.isEmpty()) {
        nCountCry = makeQueryCrystalSystem(queryCrySys, qCrySysResult);
    }

    QString querySpaceGroup = builder.getQuerySpaceGroup();
    int nCountSpg = 0;
    QString qSpgResult;
    if (!querySpaceGroup.isEmpty()) {
        nCountSpg = makeQuerySpaceGroup(querySpaceGroup, qSpgResult);
    }

    QString qSimmetryResult;
    int nCountSimmetry = 0;
    if (nCountCry > 0 && nCountSpg > 0) {
        //FIX THIS LATER
    }
    else if (nCountCry > 0) {
        qSimmetryResult = qCrySysResult;
        nCountSimmetry = nCountCry;
    } else if (nCountSpg > 0) {
        qSimmetryResult = qSpgResult;
        nCountSimmetry = nCountSpg;
    }

    //Step 2: build query for chemical elements
    QString queryChemical = builder.getChemicalQueryString();
    if (nCountSimmetry > 0 && !queryChemical.isEmpty()) {
        queryChemical = queryChemical + " intersect select id from chemical where id in (" + qSimmetryResult + ")";
    }

    if (!queryChemical.isEmpty()) {
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
            makeQueryInfoIds(idsString, nCountChemical);
        }

    } else if (nCountSimmetry > 0) {
        makeQueryInfoIds(qSimmetryResult, nCountSimmetry);
    }
}



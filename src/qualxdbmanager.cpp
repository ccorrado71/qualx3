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
    QSqlQuery query(dbInfoStat.db());
    query.prepare(qString);
    if (query.exec()) {
        while (query.next()) {
            result = query.value(0).toString();
            //qInfo() << "Space groups: " << result;
            ndata = query.value(1).toInt();
            qInfo() << "N: " << ndata;
        }
    }
    qInfo() << "End query crystal system";
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
    //Step 1: build and make query for crystal system and space groups
    QString queryCrySys = builder.getCrySysQueryString();
    int nCountCry = 0;
    QString qCrySysResult;
    if (!queryCrySys.isEmpty()) {
        nCountCry = makeQueryCrystalSystem(queryCrySys, qCrySysResult);
    }

    //Step 2: build query for chemical elements
    QString queryChemical = builder.getChemicalQueryString();
    if (nCountCry > 0 && !queryChemical.isEmpty()) {
        queryChemical = queryChemical + " intersect select id from chemical where id in (" + qCrySysResult + ")";
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

    } else if (nCountCry > 0) {
        makeQueryInfoIds(qCrySysResult, nCountCry);
    }
}



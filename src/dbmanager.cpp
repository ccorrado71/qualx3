#include "dbmanager.h"

#include <QMessageBox>

DbManager::DbManager()
{

}

DbManager::DbManager(const QString &path)
{
    openDb(path);
}

void DbManager::closeDb()
{
    if (m_db.isOpen())
    {        
        //qInfo() << "ConnName: " << m_db.connectionName();
        QString connName = m_db.connectionName();
        m_db.close();
        m_db = QSqlDatabase(); //reset m_db by assigning a default constructor
        //m_db.removeDatabase(m_db.connectionName());
        //qInfo() << "ConnName: " << m_db.connectionName();
        //m_db.removeDatabase(connName);
        m_db.removeDatabase(connName);
    }

}

DbManager::~DbManager()
{
    closeDb();
}

bool DbManager::openDb(const QString &path)
{
    m_db = QSqlDatabase::addDatabase("QSQLITE", QFileInfo(path).fileName());
    m_db.setDatabaseName(path);

    if (!m_db.open()) {
        QMessageBox::critical(nullptr,"Database Error",m_db.lastError().text());
        return false;
    }
    return true;
}

bool DbManager::isOpen() const
{
    return m_db.isOpen();
}

void DbManager::getInfo(int &ncard, QString &type)
{
    QSqlQuery queryInfo(m_db);
    queryInfo.prepare("SELECT ncard, type FROM infodb");

    if (queryInfo.exec()) {
        queryInfo.first();
        ncard = queryInfo.value(0).toInt();
        type = queryInfo.value(1).toString();        
    }
}

void DbManager::getCardInfo(const QString &idCard)
{
    QSqlQuery queryId(m_db);
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

void DbManager::getCardAdditionalInfo(const QString &idCard)
{
    QSqlQuery queryId(m_db);
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

void DbManager::makeQuery(const DbQueryBuilder &builder)
{
    //Step 1: query for crystal system and space groups
    QString qCrySys = builder.getCrySysQueryString();
    if (!qCrySys.isEmpty()) {
        makeQueryCrystalSystem(qCrySys);
    }

    //Step 2: query for chemical elements
    QString queryString = builder.getChemicalQueryString();

    int ncount = queryForCount(queryString);
    qInfo() << "NCOUNT: " << ncount;

    if (ncount > 0) {
        qInfo() << "Start query for idsString";
        QSqlQuery query(m_db);
        query.prepare(queryString);

        QString idsString;
        if (query.exec()) {
            while (query.next()) {
                idsString.append("'"+query.value(0).toString()+"',");
            }
            idsString.chop(1); //rimove last ','
            qInfo() << "End query for idsString";

            makeQueryInfoIds(idsString, ncount);
        }
    }
}

QSqlDatabase DbManager::db() const
{
    return m_db;
}

int DbManager::queryForCount(const QString &queryString)
{
    int count = -1;
    qInfo() << "Start query count";
    QSqlQuery querycount(m_db);
    querycount.prepare("SELECT COUNT(*) FROM ("+queryString+")");
    if (querycount.exec()) {
        if (querycount.first()) {
            count = querycount.value(0).toInt();
        }
    }
    qInfo() << "End query count";

    return count;
}

void DbManager::makeQueryCrystalSystem(const QString &qString)
{
    qInfo() << "Start query crystal system";
    QSqlQuery query(m_db);
    query.prepare(qString);
    if (query.exec()) {
        while (query.next()) {
            qInfo() << "Space groups: " << query.value(0).toString();
            qInfo() << "N: " << query.value(1).toInt();
        }
    }
    qInfo() << "End query crystal system";
}

void DbManager::makeQueryInfoIds(const QString &idsString, int count)
{
    qInfo() << "Start queryIds";
    QSqlQuery queryIds(m_db);
    queryIds.prepare("Select id, name, mineralname, chemical_formula, spacegroup, "
                     "quality, rir, nrec, intensita, dvalue,n from id where id in ("+idsString+")");
    int nId = 0;
    if (queryIds.exec()) {
        while (queryIds.next()) {
            nId++;
            float perc = 100.0f*nId/count;            
            if (fmod(perc,10) == 0) qInfo() << perc << "%";
            qInfo() << "id =" << queryIds.value(0).toString() << queryIds.value(3).toString();
        }
    }
    qInfo() << "End queryIds";
}

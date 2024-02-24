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
    //QSqlDatabase db = QSqlDatabase::database("cod2205ino.sq");

    //QString idCard = "2300375";
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

// void DbManager::makeQuery(const QString &names, const QStringList &subfiles, const QString &elements)
// {
//     QString queryString = queryNameString(names);
//     qInfo() << "Query chemical name: " << queryString;

//     QString querySubfile = querySubfilesString(subfiles);
//     if (!querySubfile.isEmpty()) {
//         if (!queryString.isEmpty()) queryString += " intersect ";
//         queryString += querySubfile;
//         qInfo() << "Query subfiles: " << queryString;
//     }

//     QString queryElements = queryElementString(elements);
//     if (!queryElements.isEmpty()) {
//         if (!queryString.isEmpty()) queryString += " intersect ";
//         queryString += queryElements;
//         qInfo() << "Query elements: " << queryString;
//     }

//     int ncount = queryForCount(queryString);
//     qInfo() << "NCOUNT: " << ncount;

//     if (ncount > 0) {
//         qInfo() << "Start query for idsString";
//         QSqlQuery query(m_db);
//         query.prepare(queryString);

//         QString idsString;
//         if (query.exec()) {
//             while (query.next()) {
//                 idsString.append("'"+query.value(0).toString()+"',");
//             }
//             idsString.chop(1); //rimove last ','
//             qInfo() << "End query for idsString";

//             queryInfoIds(idsString, ncount);
//         }
//     }
// }

void DbManager::makeQuery(const DbQueryBuilder &builder)
{
    QString queryString = builder.getChemicalString();

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

            queryInfoIds(idsString, ncount);
        }
    }
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

QString DbManager::querySubfilesString(const QStringList &subfiles)
{
    QString queryString;
    if (subfiles.size() > 0) {
        queryString = QString("Select id from subfiles where subfile = '%1'").arg(subfiles.at(0));
        for (int i = 1; i < subfiles.size(); i++) {
            queryString += QString("and id in (Select id from subfiles where subfile = '%1')").arg(subfiles.at(i));
        }
    }

    return queryString;
}

QString DbManager::queryNameString(const QString &names)
{
    //Select id, name from id where (
    //(name like '%silicon%' and name like '%oxide%')
    //or (mineralname like '%silicon' and mineralname like '%oxide'))
    QString queryName;
    QStringList nameList = names.split(" ", Qt::SkipEmptyParts);
    if (nameList.size() > 0) {
        queryName = "Select id from id where ( (";

        for (int i = 0; i < nameList.size(); i++) {
            queryName += "name like '%" + nameList.at(i) + "%'";
            if (i != nameList.size()-1) queryName += " and ";
        }
        queryName += ") or (";
        for (int i = 0; i < nameList.size(); i++) {
            queryName += "mineralname like '%" + nameList.at(i) + "%'";
            if (i != nameList.size()-1) queryName += " and ";
        }
        queryName += ")";
        queryName += ")";
    }

    return queryName;
}

// QString DbManager::queryElementString(const QString &elString)
// {
//     QString queryElement;

//     if (!elString.isEmpty()) {
//         QStringList list = elString.split(" ");
//         if (list.size() % 2 == 0) {
//             qInfo() << "Something wrong happens";
//             return queryElement;
//         }

//         QStringList elements(std::ceil(static_cast<double>(list.size()) / 2));
//         QStringList logicOper(elements.size()-1);
//         int j = 0, k = 0;
//         for (int i = 0; i < list.size(); i++) {
//             if (i % 2 == 0) {
//                 elements[j++] = list.at(i);
//             } else {
//                 logicOper[k++] = list.at(i);
//             }
//         }
//         qInfo() << "ELEMENTS: " << elements;
//         qInfo() << "LOGIC: " << logicOper;
//         if (elements.size() == 1) {
//             queryElement = "Select distinct(c0.id) from chemical as c0 where c0.chemical_element='"+elements.at(0)+"'";
//         } else {
//             bool allAnd = true;
//             foreach(const QString& item, logicOper) {
//                 if (item != "and") {
//                     allAnd = false;
//                     break;
//                 }
//             }
//             if (allAnd) {
//                 queryElement = "Select distinct(c0.id) from ";
//                 for (int i = 0; i < elements.size(); i++) {
//                     queryElement += QString("chemical as c%1").arg(i);
//                     if (i != elements.size()-1) queryElement+=", ";
//                 }
//                 queryElement += " where ";
//                 for (int i = 0; i < elements.size(); i++) {
//                     queryElement += QString("c%1.chemical_element='%2'").arg(i).arg(elements.at(i));
//                     if (i != elements.size()-1) {
//                         queryElement+= QString(" and c%1.id=c%2.id and ").arg(i).arg(i+1);
//                     }
//                 }
//             } else {
//                 //Start query with first element
//                 queryElement = QString("Select distinct(id) from chemical where chemical_element='%1'").arg(elements.at(0));

//                 //Add elements with 'and' operator to query
//                 for (int i = 0; i < logicOper.size(); i++) {
//                     if (logicOper.at(i) == "and") {
//                         queryElement += QString(" and id in (select distinct(id) from chemical where chemical_element='%1')").arg(elements.at(i+1));
//                     }
//                 }

//                 //Add elements with 'or' operator to query
//                 for (int i = 0; i < logicOper.size(); i++) {
//                     if (logicOper.at(i) == "or") {
//                         queryElement += QString(" union select distinct(id) from chemical where chemical_element='%1'").arg(elements.at(i+1));
//                     }
//                 }
//             }

//             //TODO:
//             //1) Scrivi una classe QueryBuilder che servirà per generare la query dalle info della GUI
//             //2) Testa le query gestite finora
//         }

//     }

//     return queryElement;
// }

void DbManager::queryInfoIds(const QString &idsString, int count)
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

#include "dbquerybuilder.h"
#include <QDebug>

DbQueryBuilder::DbQueryBuilder()
    : printEnabled(false),
    bOperator(AND_OR_OP)
{

}

void DbQueryBuilder::initialize()
{
    subfiles.clear();
    names.clear();
    subfiles.clear();
    elString.clear();
    spgString.clear();

    queryChemical.clear();
    queryCrySys.clear();
    querySpaceGroup.clear();
}

void DbQueryBuilder::setSubfiles(const QStringList &newSubfiles)
{
    subfiles = newSubfiles;
}

QString DbQueryBuilder::getChemicalQueryString() const
{
    return queryChemical;
}

QString DbQueryBuilder::getCrySysQueryString() const
{
    return queryCrySys;
}

void DbQueryBuilder::setPrintEnabled(bool newPrintEnabled)
{
    printEnabled = newPrintEnabled;
}

void DbQueryBuilder::setBOperator(boolOperator newBOperator)
{
    bOperator = newBOperator;
}

void DbQueryBuilder::setSpgString(const QStringList &newSpgString)
{
    spgString = newSpgString;
}

QString DbQueryBuilder::getQuerySpaceGroup() const
{
    return querySpaceGroup;
}

void DbQueryBuilder::setCsysString(const QStringList &newCsysString)
{
    csysString = newCsysString;
}

void DbQueryBuilder::setElements(const QString &newElements)
{
    elString = newElements;
}

void DbQueryBuilder::setNames(const QString &newNames)
{
    names = newNames;
}

void DbQueryBuilder::buildQuery()
{
    queryCrySys = buildQueryCrystalSystem();
    if (!queryCrySys.isEmpty() && printEnabled) {
        qInfo() << "Query crystal system: " << queryCrySys;
    }

    querySpaceGroup = buildQuerySpaceGroup();
    if (!querySpaceGroup.isEmpty() && printEnabled) {
        qInfo() << "Query space group: " << querySpaceGroup;
    }

    queryChemical = buildQueryNameString();
    if (!queryChemical.isEmpty() && printEnabled) {
        qInfo() << "Query chemical name: " << queryChemical;
    }

    QString querySubfile = buildQuerySubfilesString();
    if (!querySubfile.isEmpty()) {
        if (!queryChemical.isEmpty()) queryChemical += " intersect ";
        queryChemical += querySubfile;
        if (printEnabled) qInfo() << "Query subfiles: " << queryChemical;
    }

    QString queryElements = buildQueryElementString();
    if (!queryElements.isEmpty()) {
        if (!queryChemical.isEmpty()) queryChemical += " intersect ";
        queryChemical += queryElements;
        if (printEnabled) qInfo() << "Query elements: " << queryChemical;
    }
}

QString DbQueryBuilder::buildQueryNameString()
{
    //Example: Select id, name from id where (
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

QString DbQueryBuilder::buildQuerySubfilesString()
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

QString DbQueryBuilder::buildQueryElementString()
{
    QString queryElement;

    if (!elString.isEmpty()) {
        QStringList list = elString.split(" ");
        if (list.size() % 2 == 0) {
            qInfo() << "Something wrong happens";
            return queryElement;
        }
        QStringList elements(std::ceil(static_cast<double>(list.size()) / 2));

        switch (bOperator) {
        case AND_OR_OP:
        {            
            QStringList logicOper(elements.size()-1);
            int j = 0, k = 0;
            for (int i = 0; i < list.size(); i++) {
                if (i % 2 == 0) {
                    elements[j++] = list.at(i);
                } else {
                    logicOper[k++] = list.at(i);
                }
            }

            if (elements.size() == 1) {
                queryElement = "Select distinct(c0.id) from chemical as c0 where c0.chemical_element='"+elements.at(0)+"'";
            } else {
                bool allAnd = true;
                foreach(const QString& item, logicOper) {
                    if (item != "and") {
                        allAnd = false;
                        break;
                    }
                }
                if (allAnd) {
                    queryElement = "Select distinct(c0.id) from ";
                    for (int i = 0; i < elements.size(); i++) {
                        queryElement += QString("chemical as c%1").arg(i);
                        if (i != elements.size()-1) queryElement+=", ";
                    }
                    queryElement += " where ";
                    for (int i = 0; i < elements.size(); i++) {
                        queryElement += QString("c%1.chemical_element='%2'").arg(i).arg(elements.at(i));
                        if (i != elements.size()-1) {
                            queryElement+= QString(" and c%1.id=c%2.id and ").arg(i).arg(i+1);
                        }
                    }
                } else {
                    //Start query with first element
                    queryElement = QString("Select distinct(id) from chemical where chemical_element='%1'").arg(elements.at(0));

                    //Add elements with 'and' operator to query
                    for (int i = 0; i < logicOper.size(); i++) {
                        if (logicOper.at(i) == "and") {
                            queryElement += QString(" and id in (select distinct(id) from chemical where chemical_element='%1')").arg(elements.at(i+1));
                        }
                    }

                    //Add elements with 'or' operator to query
                    for (int i = 0; i < logicOper.size(); i++) {
                        if (logicOper.at(i) == "or") {
                            queryElement += QString(" union select distinct(id) from chemical where chemical_element='%1'").arg(elements.at(i+1));
                        }
                    }
                }
            }
            break;
        }
        case ONLY_OP:
        {
            //Example: Al and O and P and Si
            //Select distinct(c0.id) from chemical as c0, chemical as c1, chemical as c2, chemical as c3
            //where c0.chemical_element='Al' and c0.id=c1.id and
            //      c1.chemical_element='O' and c1.id=c2.id and
            //      c2.chemical_element='P' and c2.id=c3.id and
            //      c3.chemical_element='Si' and
            //      not exists (select * from chemical as c4 where c4.id=c0.id and
            //     ( c4.chemical_element!='Al' and
            //       c4.chemical_element!='O' and
            //       c4.chemical_element!='P' and
            //       c4.chemical_element!='Si'))        
            int j = 0;
            for (int i = 0; i < list.size(); i++) {
                if (i % 2 == 0) {
                    elements[j++] = list.at(i);
                }
            }
            queryElement = "Select distinct(c0.id) from chemical as c0";
            for (int i = 1; i < elements.size(); i++) {
                queryElement += QString(", chemical as c%1").arg(i);
            }
            queryElement += " where ";
            int lastEl = elements.size() - 1;
            for (int i = 0; i < lastEl; i++) {
                queryElement += QString("c%1.chemical_element='%2' and c%1.id=c%3.id and ").arg(i).arg(elements.at(i)).arg(i+1);
            }
            queryElement += QString("c%1.chemical_element='%2'").arg(lastEl).arg(elements.at(lastEl));
            queryElement += QString(" and not exists (select * from chemical as c%1 where c%1.id=c0.id and (").arg(elements.size());
            for (int i = 0; i < elements.size(); i++) {
                queryElement += QString(" c%1.chemical_element!='%2'").arg(elements.size()).arg(elements.at(i));
                if (i < lastEl) queryElement += " and ";
            }            
            queryElement += "))";
            break;
        }
        case JUST_OP:
            // Example: Al and P and O
            // Select distinct(C.id) from chemical as C where
            //    (C.chemical_element="Al" or
            //     C.chemical_element="P" or
            //     C.chemical_element="O")
            //     and C.id not in ( select id from chemical where
            //     chemical_element!="Al" and
            //     chemical_element!="P" and
            //     chemical_element!="O")
            int j = 0;
            for (int i = 0; i < list.size(); i++) {
                if (i % 2 == 0) {
                    elements[j++] = list.at(i);
                }
            }
            queryElement = "Select distinct(C.id) from chemical as C where (";
            int lastEl = elements.size() - 1;
            for (int i = 0; i < lastEl; i++) {
                queryElement += QString("C.chemical_element='%1' or ").arg(elements.at(i));
            }
            queryElement += QString("C.chemical_element='%1')").arg(elements.at(lastEl));
            queryElement += "and C.id not in ( select id from chemical where ";
            for (int i = 0; i < lastEl; i++) {
                queryElement += QString("chemical_element!='%1' and ").arg(elements.at(i));
            }
            queryElement += QString("chemical_element!='%1')").arg(elements.at(lastEl));
            break;
        }

    }

    return queryElement;
}

QString DbQueryBuilder::buildQueryCrystalSystem()
{
    QString queryCSys;

    if (csysString.size() == 0) return QString();

    queryCSys = "select ids,n from stat_type where (";
    for (int i = 0; i < csysString.size(); i++) {
        queryCSys += QString("trim(val) like '%1'").arg(csysString.at(i));
        if (i != csysString.size()-1) queryCSys += " or ";
    }
    queryCSys += ")";

    return queryCSys;
}

QString DbQueryBuilder::buildQuerySpaceGroup()
{
    QString querySpg;

    if (spgString.size() == 0) return QString();
    querySpg = "select ids,n from spgrstat where (";
    for (int i = 0; i < spgString.size(); i++) {
        querySpg += QString("trim(spacegroup) = '%1'").arg(spgString.at(i));
        if (i != spgString.size()-1) querySpg += " or ";
    }
    querySpg += ")";

    return querySpg;
}


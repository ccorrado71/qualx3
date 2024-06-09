#ifndef DBQUERYBUILDER_H
#define DBQUERYBUILDER_H

#include <QStringList>

class DbQueryBuilder
{
public:
    enum boolOperator {AND_OR_OP, ONLY_OP, JUST_OP};

    DbQueryBuilder();
    void initialize();

    void setNames(const QString &newNames);
    void setSubfiles(const QStringList &newSubfiles);
    void setElements(const QString &newElements);
    void setCsysString(const QStringList &newCsysString);

    void buildQuery();
    QString getChemicalQueryString() const;
//    QString getCrySysQueryString() const;
    QString getSymmetryQueryString() const;
    void setPrintEnabled(bool newPrintEnabled);    
    void setBOperator(boolOperator newBOperator);

    void setSpgString(const QStringList &newSpgString);

//    QString getQuerySpaceGroup() const;

private:
    QString queryChemical;
    QString queryCrySys;
    QString querySymmetry;
    QString names;
    QStringList subfiles;
    QString elString;
    QStringList csysString;
    QStringList spgString;
    bool printEnabled;
    boolOperator bOperator;

    QString buildQueryNameString();
    QString buildQuerySubfilesString();
    QString buildQueryElementString();
//    QString buildQueryCrystalSystem();
    QString buildQuerySymmetry();
};

#endif // DBQUERYBUILDER_H

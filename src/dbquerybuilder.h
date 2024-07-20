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
    void setSpgString(const QStringList &newSpgString);
    void setCellParameter(int index, double min, double max);
    void setPrintEnabled(bool newPrintEnabled);
    void setBOperator(boolOperator newBOperator);

    void buildQuery();
    QString getChemicalQueryString() const;
    QString getSymmetryQueryString() const;
    QStringList getQueryCellPar() const;

private:
    QString queryChemical;
    QString queryCrySys;
    QString querySymmetry;
    QStringList queryCellPar;
    QString names;
    QStringList subfiles;
    QString elString;
    QStringList csysString;
    QStringList spgString;
    double cellParMin[6], cellParMax[6];
    bool printEnabled;
    boolOperator bOperator;

    QString buildQueryNameString();
    QString buildQuerySubfilesString();
    QString buildQueryElementString();
    QString buildQuerySymmetry();
    QStringList buildQueryCellParameters();
};

#endif // DBQUERYBUILDER_H

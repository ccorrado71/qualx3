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
    void setIdEntry(const QStringList &newIdEntry);
    void setPrintEnabled(bool newPrintEnabled);
    void setBOperator(boolOperator newBOperator);
    void enableDeleted(bool newAddDeleted);
    void setDValues(const QVector<double> &newDValues, const QVector<double> &newDTol);

    void buildQuery();
    QString getChemicalQueryString() const;
    QString getSymmetryQueryString() const;
    QStringList getQueryCellPar() const;
    QString getQueryIdEntry() const;
    bool deletedEnabled() const;
    QVector<double> getDValues() const;
    QVector<double> getDTol() const;
    void setWave(double newWave);

    double getWave() const;

private:
    QString queryChemical;
    QString queryCrySys;
    QString querySymmetry;
    QStringList queryCellPar;
    QString queryIdEntry;
    QString names;
    QStringList subfiles;
    QString elString;
    QStringList csysString;
    QStringList spgString;
    QStringList idEntry;
    double cellParMin[6], cellParMax[6];
    bool printEnabled;
    boolOperator bOperator;
    bool addDeleted;
    QVector<double> dValues, dTol;
    double wave;

    QString buildQueryNameString();
    QString buildQuerySubfilesString();
    QString buildQueryElementString();
    QString buildQuerySymmetry();
    QStringList buildQueryCellParameters();
    QString buildQueryIdEntry();
};

#endif // DBQUERYBUILDER_H

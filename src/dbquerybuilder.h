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
    void setColorString(const QStringList &colors);
    void setCellParameter(int index, double min, double max);
    void setDensityCalc(double min, double max);
    void setDensityMeas(double min, double max);
    void setIdEntry(const QStringList &newIdEntry);
    void setPrintEnabled(bool newPrintEnabled);
    void setBOperator(boolOperator newBOperator);
    void enableDeleted(bool newAddDeleted);
    void setDValues(const QVector<double> &newDValues, const QVector<double> &newDTol);
    void   setMinFom(double v);
    double getMinFom()  const;
    void   setCalcFom(bool v);
    bool   getCalcFom() const;
    void   setWeight2thetaD(double v);
    double getWeight2thetaD()   const;
    void   setWeightIntensity(double v);
    double getWeightIntensity() const;
    void   setWeightPhases(double v);
    double getWeightPhases()    const;
    void   setDelta2theta(double v);
    double getDelta2theta()     const;

    void buildQuery();
    QString getChemicalQueryString() const;
    QString getSymmetryQueryString() const;
    QString getColorQueryString()    const;
    QStringList getQueryCellPar() const;
    QStringList getQueryDensity() const;
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
    QStringList colorList;
    QString     queryColor;
    QStringList idEntry;
    double cellParMin[6], cellParMax[6];
    double densCalcMin, densCalcMax;
    double densMeasMin, densMeasMax;
    QStringList queryDensity;
    bool printEnabled;
    boolOperator bOperator;
    bool addDeleted;
    QVector<double> dValues, dTol;
    double wave;
    double minFom;
    bool   calcFom;
    double weight2thetaD;
    double weightIntensity;
    double weightPhases;
    double delta2theta;

    QString buildQueryNameString();
    QString buildQuerySubfilesString();
    QString buildQueryElementString();
    QString buildQuerySymmetry();
    QString buildQueryColor();
    QStringList buildQueryCellParameters();
    QStringList buildQueryDensity();
    QString buildQueryIdEntry();
};

#endif // DBQUERYBUILDER_H

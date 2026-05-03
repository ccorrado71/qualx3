#ifndef CARDTYPE_H
#define CARDTYPE_H

#include <QVector>

class CardType
{
public:
    CardType();

    QVector<double> getD() const;
    void setD(const QVector<double> &newD, double wave = -1.0);
    QVector<double> getTth() const;
    void printCard(int level) const;
    QString getId() const;
    void setId(const QString &newId);
    QString getChemicalName() const;
    void setChemicalName(const QString &newChemicalName);
    QString getChemicalFormula() const;
    void setChemicalFormula(const QString &newChemicalFormula);
    QString getMineralName() const;
    void setMineralName(const QString &newMineralName);
    QString getQuality() const;
    void setQuality(const QString &newQuality);
    QString getRIR() const;
    void setRIR(const QString &newRIR);
    QString getSpaceGroup() const;
    void setSpaceGroup(const QString &newSpaceGroup);
    double getFom() const;
    void setFom(double newFomd);
    double getFomPeakPos()  const;
    void   setFomPeakPos(double v);
    double getFomIntensity() const;
    void   setFomIntensity(double v);
    double getScale() const;
    void   setScale(double v);

    QVector<double> getIntensity() const;
    void setIntensity(const QVector<double> &newIntensity);

private:
    QString id;
    QString chemicalName;
    QString chemicalFormula;
    QString mineralName;
    QString quality;
    QString RIR;
    QString spaceGroup;
    double fom;
    double fomPeakPos;
    double fomIntensity;
    double scale;
    QVector<double> d;       // d-spacing
    QVector<double> tth;     // two-theta
    QVector<double> intensity; // intensity
};

#endif // CARDTYPE_H

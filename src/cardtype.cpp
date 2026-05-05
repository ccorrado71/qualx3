#include "cardtype.h"
#include "xpdutils.h"

CardType::CardType()
    : fom(0.0), fomCalculated(false), fomPeakPos(0.0), fomIntensity(0.0), scale(0.0)
{}

QVector<double> CardType::getD() const
{
    return d;
}

void CardType::setD(const QVector<double> &newD, double wave)
{
    d = newD;
    if (wave > 0.0) {
        tth = xpdutils::tthvalue_safe(d, wave);
    } else {
        tth.clear();
    }
}

QVector<double> CardType::getTth() const
{
    return tth;
}

void CardType::printCard(int level) const
{
    qDebug() << "Id: " << id << " Formula: " << chemicalFormula << "Fomd: " << fom;
    if (level < 2)
        return;
    int n = std::min(d.size(), tth.size());
    qDebug() << "d\t tth";
    for (int i = 0; i < n; ++i)
        qDebug() << d[i] << "\t" << tth[i];
}

QString CardType::getId() const
{
    return id;
}

void CardType::setId(const QString &newId)
{
    id = newId;
}

QString CardType::getChemicalName() const
{
    return chemicalName;
}

void CardType::setChemicalName(const QString &newChemicalName)
{
    chemicalName = newChemicalName;
}

QString CardType::getChemicalFormula() const
{
    return chemicalFormula;
}

void CardType::setChemicalFormula(const QString &newChemicalFormula)
{
    chemicalFormula = newChemicalFormula;
}

QString CardType::getMineralName() const
{
    return mineralName;
}

void CardType::setMineralName(const QString &newMineralName)
{
    mineralName = newMineralName;
}

QString CardType::getQuality() const
{
    return quality;
}

void CardType::setQuality(const QString &newQuality)
{
    quality = newQuality;
}

QString CardType::getRIR() const
{
    return RIR;
}

void CardType::setRIR(const QString &newRIR)
{
    RIR = newRIR;
}

QString CardType::getSpaceGroup() const
{
    return spaceGroup;
}

void CardType::setSpaceGroup(const QString &newSpaceGroup)
{
    spaceGroup = newSpaceGroup;
}

double CardType::getFom() const
{
    return fom;
}

void CardType::setFom(double newFomd)   { fom = newFomd; fomCalculated = true; }
bool CardType::isFomCalculated() const  { return fomCalculated; }
double CardType::getFomPeakPos()  const { return fomPeakPos; }
void   CardType::setFomPeakPos(double v){ fomPeakPos   = v; }
double CardType::getFomIntensity() const{ return fomIntensity; }
void   CardType::setFomIntensity(double v){ fomIntensity = v; }
double CardType::getScale()        const{ return scale; }
void   CardType::setScale(double v)     { scale        = v; }

QVector<double> CardType::getIntensity() const
{
    return intensity;
}

void CardType::setIntensity(const QVector<double> &newIntensity)
{
    intensity = newIntensity;
}

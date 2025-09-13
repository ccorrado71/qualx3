#include "cardtype.h"
#include "xpdutils.h"

CardType::CardType() {}

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
    qDebug() << "Id: " << id << " Formula: " << chemicalFormula << "Fomd: " << fomd;
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

double CardType::getFomd() const
{
    return fomd;
}

void CardType::setFomd(double newFomd)
{
    fomd = newFomd;
}

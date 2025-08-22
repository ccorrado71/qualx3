#include "cardtype.h"

CardType::CardType() {}

QVector<double> CardType::getD() const
{
    return d;
}

void CardType::setD(const QVector<double> &newD)
{
    d = newD;
}

QVector<double> CardType::getTth() const
{
    return tth;
}

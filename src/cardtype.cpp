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
        tth = xpdutils::tthvalue(d, wave);
    } else {
        tth.clear();
    }
}

QVector<double> CardType::getTth() const
{
    return tth;
}

void CardType::printDandTth() const
{
    int n = std::min(d.size(), tth.size());
    qDebug() << "d\t tth";
    for (int i = 0; i < n; ++i)
        qDebug() << d[i] << "\t" << tth[i];
}

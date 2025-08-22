#ifndef CARDTYPE_H
#define CARDTYPE_H

#include <QVector>

class CardType
{
public:
    CardType();

    QVector<double> getD() const;
    void setD(const QVector<double> &newD);

    QVector<double> getTth() const;

private:
    QVector<double> d;      // d-spacing
    QVector<double> tth;    // two-theta
};

#endif // CARDTYPE_H

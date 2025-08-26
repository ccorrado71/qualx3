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
    void printDandTth() const;

private:
    QVector<double> d;      // d-spacing
    QVector<double> tth;    // two-theta
};

#endif // CARDTYPE_H

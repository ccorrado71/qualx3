#ifndef GRAPHITEM_H
#define GRAPHITEM_H

#include "qcustomplot.h"

#include <QVector>
#include <QPen>

class graphItem
{    
public:
    enum ItemType {
        Observed, Calculated, Background, Background_Points, Difference,
        Cumulative, Reflections, Peaks, Smoothing, Unindexed_Peaks,
        Systematic_Absences, Selected_Reflections, Selected_Peaks, Intervals,
        Profile_Curves
    };
    static const QStringList ItemTypeString;
    double wave;
    int itemIndexStart, itemIndexEnd;

    ItemType gtype;
    double min, max;
    double yPos;
    double lengthRef;
    int graphIndex;
    int defaultIDColor;
    bool visible;

    graphItem();
    void setColorLine(int idColor = -1);
    QPen getDefaultPen(int idColor = -1) const;
    QCPScatterStyle getDefaultScatter(int idColor = -1) const;
    static QColor getPaletteColor(int id);

    void setX(const QVector<double> &value);
    void setX(const QVector<int> &value);
    QVector<double> getX() const;
    QVector<int> getIx() const;
    double getX(int pos);
    int xSize() const;
    int ixSize() const;
    void setName(const QString &value = "");

    QString getName() const;

    int findLocation(double x);

    QPen getPen() const;
    void setLineStyle(const Qt::PenStyle &style);
    void setLineColor(const QColor &color);
    void setLineWidth(int width);

    QCPScatterStyle getScatter() const;

    void setPen(const QPen &value);

    void setScatter(const QCPScatterStyle &value);
    QString getKeyString(QString item, int id) const;

    void setData(const QVector<double> &xvet, const QVector<double> &yvet);

    QCPGraph::LineStyle getLineConnectionType() const;
    void setLineConnectionType(const QCPGraph::LineStyle &value);

private:
    QString name;
    QPen pen;
    QVector<double> x;
    QVector<int> ix;
    QCPScatterStyle scatter;
    QCPGraph::LineStyle lineConnectionType;
    void setDefaultLine(int id);
    void setDefaultScatter(int id);
};

#endif // GRAPHITEM_H

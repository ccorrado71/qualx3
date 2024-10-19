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

    graphItem();
    void setGraphIndex(int value);
    void setColorLine(int idColor = -1);
    void setMin(double value);
    void setMax(double value);
    void setGtype(const ItemType &value);
    QPen getDefaultPen(int idColor = -1) const;
    QCPScatterStyle getDefaultScatter(int idColor = -1) const;
    static QColor getPaletteColor(int id);

    ItemType getGtype() const;
    double getMin() const;
    double getMax() const;

    void setX(const QVector<double> &value);
    void setX(const QVector<int> &value);
    void setY(const QVector<double> &value);
    QVector<double> getX() const;
    QVector<int> getIx() const;
    double getX(int pos);
    int xSize() const;
    int ixSize() const;
    void setName(const QString &value = "");

    QString getName() const;

    int findLocation(double x);

    double getYPos() const;
    void setYPos(double value);

    QPen getPen() const;
    void setLineStyle(const Qt::PenStyle &style);
    void setLineColor(const QColor &color);
    void setLineWidth(int width);

    QCPScatterStyle getScatter() const;

    void setPen(const QPen &value);

    void setScatter(const QCPScatterStyle &value);
    QString getKeyString(QString item, int id) const;

    int getGraphIndex() const;
    int itemIndexStart, itemIndexEnd;

    double getLengthRef() const;
    void setLengthRef(double value);

    void setDefaultIDColor(int value);
    int getDefaultIDColor() const;

    bool isVisible() const;
    void setVisible(bool value);
    void setData(const QVector<double> &xvet, const QVector<double> &yvet);

    QCPGraph::LineStyle getLineConnectionType() const;
    void setLineConnectionType(const QCPGraph::LineStyle &value);

private:
    ItemType gtype;
    QString name;
    QPen pen;
    int graphIndex;
    double min; //forse non serve
    double max; //forse non serve
    bool visible;
    QVector<double> x,y;
    QVector<int> ix;
    double yPos;
    double lengthRef;
    QCPScatterStyle scatter;
    int defaultIDColor;
    QCPGraph::LineStyle lineConnectionType;
    void setDefaultLine(int id);
    void setDefaultScatter(int id);
};

#endif // GRAPHITEM_H

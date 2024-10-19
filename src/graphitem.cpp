#include "graphitem.h"
#include "nr.h"

#include <QSettings>
#include <QList>
#include <float.h>
#include <QDebug>

//const QColor listColors[] = {Qt::blue, Qt::red, Qt::green, Qt::black, Qt::gray, Qt::cyan, Qt::magenta, Qt::darkRed, Qt::darkBlue, Qt::darkCyan};
//const size_t sizePalette = sizeof(listColors)/sizeof(listColors[0]);

const QList<QColor> listColors = {Qt::blue, Qt::red, Qt::green, Qt::black, Qt::gray, Qt::cyan, Qt::magenta, Qt::darkRed, Qt::darkBlue, Qt::darkCyan};
const QStringList graphItem::ItemTypeString = {"Observed", "Calculated", "Background", "Background_Points",
                                               "Difference", "Cumulative", "Reflections", "Peaks", "Smoothing",
                                               "Unindexed_Peaks", "Systematic_Absences", "Selected_Reflections",
                                               "Selected_Peaks", "Intervals", "Profile_Curves"};

graphItem::graphItem() :
    wave(-1),
    itemIndexStart(0),
    itemIndexEnd(0),
    gtype(ItemType::Observed),
    graphIndex(-1),
    min(DBL_MAX), max(-DBL_MAX),
    visible(false),
    defaultIDColor(0),
    lineConnectionType(QCPGraph::lsLine)
{

}

void graphItem::setGraphIndex(int value)
{
    graphIndex = value;
}

void graphItem::setColorLine(int idColor)
{
    QSettings settings;
    QString keyScatterStyle;
    QString keyPen;
    QString keyScatterPen;

    if (idColor < 0) idColor = defaultIDColor;

    switch (gtype) {
    case Observed:
        keyPen = ItemTypeString.at(gtype)+"/pen" + QString::number(idColor);
        keyScatterStyle = ItemTypeString.at(gtype)+"/scatterStyle" + QString::number(idColor);
        keyScatterPen = ItemTypeString.at(gtype)+"/scatterPen" + QString::number(idColor);
        break;
    case Reflections:
        keyPen = ItemTypeString.at(gtype)+"/pen" + QString::number(idColor);
        break;
    case Background:        
    case Calculated:
    case Difference:
    case Cumulative:
    case Peaks:
        keyPen = ItemTypeString.at(gtype)+"/pen";
        break;
    case Background_Points:
        keyScatterStyle = ItemTypeString.at(gtype)+"/scatterStyle";
        keyScatterPen = ItemTypeString.at(gtype)+"/scatterPen";
        break;            
    default:
        break;
    }

    if (!keyPen.isEmpty()) {
        if (settings.contains(keyPen)) {
            this->setPen(settings.value(keyPen).value<QPen>());
        } else {
            setDefaultLine(idColor);
            settings.setValue(keyPen,pen);
        }
    }

    if (!keyScatterStyle.isEmpty()) {
        if (settings.contains(keyScatterPen) && settings.contains(keyScatterStyle)) {
            scatter.setPen(settings.value(keyScatterPen).value<QPen>());
            scatter.setShape(static_cast<QCPScatterStyle::ScatterShape>(settings.value(keyScatterStyle).toInt()));
        } else {
            setDefaultScatter(idColor);
            settings.setValue(keyScatterPen, scatter.pen());
            settings.setValue(keyScatterStyle, scatter.shape());
        }
    }
}

QPen graphItem::getDefaultPen(int idColor) const
{
    if (idColor < 0) idColor = defaultIDColor;
    QPen defaultPen;
    QColor color = graphItem::getPaletteColor(idColor);
    defaultPen.setColor(color);
    defaultPen.setWidth(1);
    return defaultPen;
}

QCPScatterStyle graphItem::getDefaultScatter(int idColor) const
{
    QPen defaultPen = getDefaultPen(idColor);
    QCPScatterStyle defaultScatter;
    defaultScatter.setPen(defaultPen);
    if (gtype == Background_Points) {
        defaultScatter.setShape(QCPScatterStyle::ssCircle);
    } else {
        defaultScatter.setShape(QCPScatterStyle::ssNone);
    }
    return defaultScatter;
}

QString graphItem::getKeyString(QString item, int id) const
{
    if (id < 0) return ItemTypeString.at(gtype) + "/" + item;
    return ItemTypeString.at(gtype) + "/" + item + QString::number(id);
}

void graphItem::setDefaultLine(int id)
{
    pen = getDefaultPen(id);
}

void graphItem::setDefaultScatter(int id)
{
    scatter = getDefaultScatter(id);
}

void graphItem::setGtype(const ItemType &value)
{
    gtype = value;
}

graphItem::ItemType graphItem::getGtype() const
{
    return gtype;
}

double graphItem::getMin() const
{
    return min;
}

void graphItem::setMin(double value)
{
    min = value;
}

void graphItem::setMax(double value)
{
    max = value;
}

double graphItem::getMax() const
{
    return max;
}

QColor graphItem::getPaletteColor(int id)
{
    int index = id%(listColors.size());
    return QColor(listColors[index]);
}

int graphItem::getDefaultIDColor() const
{
    return defaultIDColor;
}

bool graphItem::isVisible() const
{
    return visible;
}

void graphItem::setVisible(bool value)
{
    visible = value;
}

void graphItem::setData(const QVector<double> &xvet, const QVector<double> &yvet)
{
    x = xvet;
    y = yvet;
}

QCPGraph::LineStyle graphItem::getLineConnectionType() const
{
    return lineConnectionType;
}

void graphItem::setLineConnectionType(const QCPGraph::LineStyle &value)
{
    lineConnectionType = value;
}

void graphItem::setDefaultIDColor(int value)
{
    defaultIDColor = value;
}

double graphItem::getLengthRef() const
{
    return lengthRef;
}

void graphItem::setLengthRef(double value)
{
    lengthRef = value;
}

void graphItem::setScatter(const QCPScatterStyle &value)
{
    scatter = value;
}

int graphItem::getGraphIndex() const
{
    return graphIndex;
}

QCPScatterStyle graphItem::getScatter() const
{
    return scatter;
}

void graphItem::setPen(const QPen &value)
{
    pen = value;
}

double graphItem::getYPos() const
{
    return yPos;
}

void graphItem::setYPos(double value)
{
    yPos = value;
}

QPen graphItem::getPen() const
{
    return pen;
}

void graphItem::setLineStyle(const Qt::PenStyle &style)
{
    pen.setStyle(style);
}

void graphItem::setLineColor(const QColor &color)
{
    pen.setColor(color);
}

void graphItem::setLineWidth(int width)
{
    pen.setWidthF(width);
}

QVector<double> graphItem::getX() const
{
    return x;
}

QVector<int> graphItem::getIx() const
{
    return ix;
}

double graphItem::getX(int pos)
{
    return x.at(pos);
}

int graphItem::xSize() const
{
    return x.size();
}

int graphItem::ixSize() const
{
    return ix.size();
}

void graphItem::setName(const QString &value)
{
    if (value.isEmpty()) {
        name = ItemTypeString.at(gtype);
    } else {
        name = value;
    }
}

QString graphItem::getName() const
{
    return name;
}

int graphItem::findLocation(double value)
{
    return NR::locateClosest(x, value);
}

void graphItem::setX(const QVector<double> &value)
{
    x = value;
}

void graphItem::setX(const QVector<int> &value)
{
    ix = value;
}

void graphItem::setY(const QVector<double> &value)
{
    y = value;
}

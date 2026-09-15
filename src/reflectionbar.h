#ifndef REFLECTIONBAR_H
#define REFLECTIONBAR_H

#include <QPen>
#include <QString>
#include <QVector>

typedef struct {
    int hkl[3];
    double x;
} refInfo;

// One phase's set of reflections, drawn as a row of tick marks below the
// diffraction pattern (data + drawing style for that row).
class ReflectionBar
{
public:
    QVector<refInfo> ref;
    QString id;
    QString name;
    double wave = 0;
    QPen pen;
    bool visible = false;
    int graphIndex = -1;
    int itemIndexStart = 0, itemIndexEnd = 0;
    double yPos = 0;
    double lengthRef = 0;

    void setName(const QString &value = QString());
    void setColorLine(int idColor);
    QPen getDefaultPen(int idColor) const;
    QString getKeyString(const QString &item, int id) const;
};

#endif // REFLECTIONBAR_H

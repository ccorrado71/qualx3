#include "reflectionbar.h"
#include "graphitem.h"

#include <QSettings>

void ReflectionBar::setName(const QString &value)
{
    name = value.isEmpty() ? "Reflections" : value;
}

QString ReflectionBar::getKeyString(const QString &item, int id) const
{
    if (id < 0) return "Reflections/" + item;
    return "Reflections/" + item + QString::number(id);
}

QPen ReflectionBar::getDefaultPen(int idColor) const
{
    QPen defaultPen;
    defaultPen.setColor(graphItem::getPaletteColor(idColor));
    defaultPen.setWidth(1);
    return defaultPen;
}

void ReflectionBar::setColorLine(int idColor)
{
    QSettings settings;
    QString keyPen = getKeyString("pen", idColor);
    if (settings.contains(keyPen)) {
        pen = settings.value(keyPen).value<QPen>();
    } else {
        pen = getDefaultPen(idColor);
        settings.setValue(keyPen, pen);
    }
}

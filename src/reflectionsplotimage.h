#ifndef REFLECTIONSPLOTIMAGE_H
#define REFLECTIONSPLOTIMAGE_H

#include <QColor>
#include <QImage>
#include <QPalette>
#include <QSize>
#include <QString>
#include <QVector>

// Draws the reflections of a card as a stick pattern: one vertical line per
// reflection at x (e.g. 2theta), with height proportional to its intensity.
// Used in the card browser, where the image is embedded in the HTML.
QImage reflectionsPlotImage(const QVector<double> &x, const QVector<double> &intensity,
                            const QColor &stickColor, const QPalette &palette,
                            const QSize &size, qreal devicePixelRatio,
                            const QString &xLabel);

#endif // REFLECTIONSPLOTIMAGE_H

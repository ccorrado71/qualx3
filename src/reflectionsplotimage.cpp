#include "reflectionsplotimage.h"

#include <QFontMetrics>
#include <QPainter>

#include <algorithm>
#include <cmath>

namespace {

// Tick step giving about nTicks ticks over range, rounded to 1, 2 or 5 x 10^n
double niceStep(double range, int nTicks)
{
    const double raw  = range / nTicks;
    const double mag  = std::pow(10.0, std::floor(std::log10(raw)));
    const double norm = raw / mag;
    const double nice = norm < 1.5 ? 1.0 : norm < 3.0 ? 2.0 : norm < 7.0 ? 5.0 : 10.0;
    return nice * mag;
}

} // namespace

QImage reflectionsPlotImage(const QVector<double> &x, const QVector<double> &intensity,
                            const QColor &stickColor, const QPalette &palette,
                            const QSize &size, qreal devicePixelRatio,
                            const QString &xLabel)
{
    QImage image(size * devicePixelRatio, QImage::Format_ARGB32_Premultiplied);
    image.setDevicePixelRatio(devicePixelRatio);
    image.fill(palette.color(QPalette::Base));
    if (x.isEmpty() || x.size() != intensity.size()) return image;

    const auto [xMinIt, xMaxIt] = std::minmax_element(x.begin(), x.end());
    const double pad  = std::max((*xMaxIt - *xMinIt) * 0.03, 0.5);
    const double xMin = std::max(0.0, *xMinIt - pad);
    const double xMax = *xMaxIt + pad;
    const double yMax = *std::max_element(intensity.begin(), intensity.end());
    if (yMax <= 0.0) return image;

    QPainter p(&image);
    p.setRenderHint(QPainter::Antialiasing);
    QFont font = p.font();
    font.setPointSizeF(8);
    p.setFont(font);
    const QFontMetrics fm(font);

    const QColor textColor = palette.color(QPalette::Text);
    QColor gridColor = textColor;
    gridColor.setAlpha(40);

    const double yStep = niceStep(yMax, 4);
    const int left   = fm.horizontalAdvance(QString::number(std::floor(yMax / yStep) * yStep)) + 8;
    const int right  = 10;
    const int top    = fm.height() / 2 + 2;
    const int bottom = 2 * fm.height() + 6;
    const QRectF plot(left, top, size.width() - left - right, size.height() - top - bottom);
    if (plot.width() < 20 || plot.height() < 20) return image;

    auto mapX = [&](double v) { return plot.left() + (v - xMin) / (xMax - xMin) * plot.width(); };
    auto mapY = [&](double v) { return plot.bottom() - v / yMax * plot.height(); };

    // x ticks and vertical grid
    const double xStep = niceStep(xMax - xMin, std::max(2, int(plot.width() / 70)));
    const int ixFirst = int(std::ceil(xMin / xStep));
    const int ixLast  = int(std::floor(xMax / xStep));
    for (int i = ixFirst; i <= ixLast; ++i) {
        const double v  = i * xStep;
        const double px = mapX(v);
        p.setPen(gridColor);
        p.drawLine(QPointF(px, plot.top()), QPointF(px, plot.bottom()));
        p.setPen(textColor);
        p.drawLine(QPointF(px, plot.bottom()), QPointF(px, plot.bottom() + 3));
        p.drawText(QRectF(px - 40, plot.bottom() + 4, 80, fm.height()),
                   Qt::AlignHCenter | Qt::AlignTop, QString::number(v, 'g', 6));
    }
    p.drawText(QRectF(plot.left(), plot.bottom() + 4 + fm.height(), plot.width(), fm.height()),
               Qt::AlignHCenter | Qt::AlignTop, xLabel);

    // y ticks and horizontal grid
    const int nyTicks = int(std::floor(yMax / yStep + 1e-9));
    for (int i = 0; i <= nyTicks; ++i) {
        const double v  = i * yStep;
        const double py = mapY(v);
        p.setPen(gridColor);
        p.drawLine(QPointF(plot.left(), py), QPointF(plot.right(), py));
        p.setPen(textColor);
        p.drawLine(QPointF(plot.left() - 3, py), QPointF(plot.left(), py));
        p.drawText(QRectF(0, py - fm.height() / 2.0, left - 5, fm.height()),
                   Qt::AlignRight | Qt::AlignVCenter, QString::number(v, 'g', 6));
    }

    // Frame
    p.setPen(textColor);
    p.setBrush(Qt::NoBrush);
    p.drawRect(plot);

    // Reflections
    QPen stickPen(stickColor, 1.5);
    stickPen.setCapStyle(Qt::FlatCap);
    p.setPen(stickPen);
    for (int i = 0; i < x.size(); ++i) {
        const double px = mapX(x[i]);
        p.drawLine(QPointF(px, plot.bottom()), QPointF(px, mapY(intensity[i])));
    }

    return image;
}

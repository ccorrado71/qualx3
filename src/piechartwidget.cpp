#include "piechartwidget.h"

#include <QPainter>
#include <QPainterPath>
#include <QFontMetrics>
#include <cmath>

PieChartWidget::PieChartWidget(QWidget *parent) : QWidget(parent)
{
    setMinimumSize(120, 120);
}

void PieChartWidget::setSlices(const QVector<PieSlice> &slices)
{
    m_slices = slices;
    update();
}

void PieChartWidget::paintEvent(QPaintEvent *)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing);

    if (m_slices.isEmpty()) {
        p.setPen(Qt::gray);
        p.drawText(rect(), Qt::AlignCenter, tr("No phases"));
        return;
    }

    const int margin = 10;

    // Reserve a legend column on the right (color swatch + label per slice).
    const int swatchSize = 10;
    const int legendSpacing = 6;
    const QFontMetrics fm(p.font());
    int legendWidth = 0;
    for (const PieSlice &s : m_slices)
        legendWidth = qMax(legendWidth, fm.horizontalAdvance(s.label));
    legendWidth += swatchSize + legendSpacing;
    legendWidth = qMin(legendWidth, width() / 2); // never take more than half the widget

    const int pieAreaWidth = width() - legendWidth - margin;
    const int side = qMin(pieAreaWidth, height()) - 2 * margin;
    if (side <= 0) return;

    const QRectF pieRect(
        margin + (pieAreaWidth - side) / 2.0,
        (height() - side) / 2.0,
        side, side);

    double startAngle = 90.0; // start at 12 o'clock
    for (const PieSlice &s : m_slices) {
        const double span = s.percentage / 100.0 * 360.0;
        p.setBrush(QBrush(s.color));
        p.setPen(QPen(Qt::white, 1));
        p.drawPie(pieRect, qRound(startAngle * 16), qRound(-span * 16));
        startAngle -= span;
    }

    // Legend: one row per slice (color swatch + label)
    const int rowHeight = qMax(swatchSize, fm.height()) + 4;
    const int totalLegendHeight = rowHeight * m_slices.size();
    const int legendX = width() - legendWidth;
    int y = qMax(margin, (height() - totalLegendHeight) / 2);
    p.setPen(palette().color(QPalette::WindowText));
    for (const PieSlice &s : m_slices) {
        p.setBrush(QBrush(s.color));
        p.setPen(QPen(Qt::black, 1));
        p.drawRect(legendX, y + (rowHeight - swatchSize) / 2, swatchSize, swatchSize);
        p.setPen(palette().color(QPalette::WindowText));
        p.drawText(legendX + swatchSize + legendSpacing,
                   y + rowHeight / 2 + fm.ascent() / 2 - 2, s.label);
        y += rowHeight;
    }
}

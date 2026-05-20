#include "piechartwidget.h"

#include <QPainter>
#include <QPainterPath>
#include <QFontMetrics>
#include <cmath>

PieChartWidget::PieChartWidget(QWidget *parent) : QWidget(parent)
{
    setMinimumSize(120, 120);
}

void PieChartWidget::setSlices(const QVector<QPair<QColor, double>> &slices)
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
    const int side = qMin(width(), height()) - 2 * margin;
    if (side <= 0) return;

    const QRectF pieRect(
        (width()  - side) / 2.0,
        (height() - side) / 2.0,
        side, side);

    double startAngle = 90.0; // start at 12 o'clock
    for (const auto &slice : m_slices) {
        const double span = slice.second / 100.0 * 360.0;
        p.setBrush(QBrush(slice.first));
        p.setPen(QPen(Qt::white, 1));
        p.drawPie(pieRect, qRound(startAngle * 16), qRound(-span * 16));
        startAngle -= span;
    }
}

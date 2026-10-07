#include "piechartwidget.h"

#include <QPainter>
#include <QPainterPath>
#include <QFontMetrics>
#include <QtMath>
#include <algorithm>
#include <cmath>

namespace {

// Slices at least this big get their label drawn right next to the wedge;
// smaller ones get their label stacked on the side, with a leader line
// pointing back to the wedge, to avoid overlapping text.
constexpr double kDirectLabelMinPercentage = 15.0;

struct SliceLabel {
    int     index;
    double  midAngleDeg; // standard math convention: 0 = east, counter-clockwise positive
    QPointF anchor;       // point on the pie edge, at the middle of the wedge's arc
    bool    direct;       // true: label drawn directly beside the wedge
};

} // namespace

PieChartWidget::PieChartWidget(QWidget *parent) : QWidget(parent)
{
    setMinimumSize(160, 160);
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

    const QFontMetrics fm(p.font());

    QVector<QString> labelText(m_slices.size());
    for (int i = 0; i < m_slices.size(); ++i)
        labelText[i] = QString("%1 (%2%)").arg(m_slices[i].label)
                           .arg(m_slices[i].percentage, 0, 'f', 1);

    int maxLabelWidth = 0;
    for (const QString &t : labelText)
        maxLabelWidth = qMax(maxLabelWidth, fm.horizontalAdvance(t));

    // Reserve space on both sides of the pie for labels and leader lines.
    const int margin = 10;
    const int sideMargin = qBound(60, maxLabelWidth + 24, (width() - 40) / 2);
    const int pieAreaWidth = width() - 2 * sideMargin;
    const int side = qMin(pieAreaWidth, height() - 2 * margin);
    if (side <= 0) return;

    const QRectF pieRect((width() - side) / 2.0, (height() - side) / 2.0, side, side);
    const QPointF center = pieRect.center();
    const double radius = side / 2.0;

    QVector<SliceLabel> labels;
    labels.reserve(m_slices.size());

    double startAngle = 90.0; // start at 12 o'clock
    for (int i = 0; i < m_slices.size(); ++i) {
        const PieSlice &s = m_slices[i];
        const double span = s.percentage / 100.0 * 360.0;
        p.setBrush(QBrush(s.color));
        p.setPen(QPen(Qt::white, 1));
        p.drawPie(pieRect, qRound(startAngle * 16), qRound(-span * 16));

        const double midAngle = startAngle - span / 2.0;
        const double rad = qDegreesToRadians(midAngle);
        const QPointF anchor(center.x() + radius * std::cos(rad),
                              center.y() - radius * std::sin(rad));
        labels.append({i, midAngle, anchor, s.percentage >= kDirectLabelMinPercentage});

        startAngle -= span;
    }

    // Direct labels: drawn right beside their wedge, on whichever side it falls.
    // The available text width is whatever space is left to the widget edge,
    // so the label never gets clipped off-screen regardless of sideMargin.
    for (const SliceLabel &sl : labels) {
        if (!sl.direct) continue;
        const bool rightSide = std::cos(qDegreesToRadians(sl.midAngleDeg)) >= 0.0;
        const int flags = (rightSide ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter;
        const double edgeX = rightSide ? sl.anchor.x() + 6 : sl.anchor.x() - 6;
        const QRectF textRect(rightSide ? edgeX : 0.0,
                               sl.anchor.y() - fm.height(),
                               qMax(0.0, rightSide ? (width() - edgeX) : edgeX),
                               fm.height() * 2);
        p.setPen(palette().color(QPalette::WindowText));
        p.drawText(textRect, flags, labelText[sl.index]);
    }

    // Remaining (small) wedges: labels stacked along the left/right edge,
    // each connected to its wedge by a leader line.
    QVector<SliceLabel> leftSmall, rightSmall;
    for (const SliceLabel &sl : labels) {
        if (sl.direct) continue;
        if (std::cos(qDegreesToRadians(sl.midAngleDeg)) >= 0.0)
            rightSmall.append(sl);
        else
            leftSmall.append(sl);
    }

    auto drawStack = [&](QVector<SliceLabel> group, bool rightSide) {
        if (group.isEmpty()) return;
        std::sort(group.begin(), group.end(), [](const SliceLabel &a, const SliceLabel &b) {
            return a.anchor.y() < b.anchor.y();
        });

        const int rowHeight = fm.height() + 6;
        const double totalHeight = rowHeight * group.size();
        double rowCenterY = qMax<double>(margin, (height() - totalHeight) / 2.0) + rowHeight / 2.0;
        // Bend point sits a small fixed gap outside the pie, leaving the rest of
        // the margin (down to the widget edge) free for the label text itself.
        const double lineX = rightSide ? pieRect.right() + 15.0 : pieRect.left() - 15.0;

        for (const SliceLabel &sl : group) {
            const QPointF bend(lineX, rowCenterY);
            p.setPen(QPen(Qt::darkGray, 1, Qt::DotLine));
            p.drawLine(sl.anchor, bend);
            const double textX = rightSide ? lineX + 4 : lineX - 4;
            p.drawLine(bend, QPointF(textX, bend.y()));

            const int flags = (rightSide ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter;
            const QRectF textRect(rightSide ? textX : 0.0,
                                   rowCenterY - fm.height() / 2.0,
                                   qMax(0.0, rightSide ? (width() - textX) : textX),
                                   fm.height());
            p.setPen(palette().color(QPalette::WindowText));
            p.drawText(textRect, flags, labelText[sl.index]);

            rowCenterY += rowHeight;
        }
    };
    drawStack(leftSmall, false);
    drawStack(rightSmall, true);
}

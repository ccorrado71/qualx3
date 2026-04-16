#include "elementbutton.h"
#include <QPainter>
#include <QPaintEvent>

ElementButton::ElementButton(int atomicNumber, const QString& symbol, const QString& name,
                             const QColor& baseColor, QWidget* parent)
    : QPushButton(parent)
    , m_atomicNumber(atomicNumber)
    , m_symbol(symbol)
    , m_baseColor(baseColor)
{
    setFixedSize(40, 40);
    setCursor(Qt::PointingHandCursor);
    setToolTip(QString("%1 — %2").arg(symbol, name));
    connect(this, &QPushButton::clicked, this, [this]() {
        emit elementClicked(m_symbol, m_atomicNumber);
    });
}

void ElementButton::setElementSelected(bool selected)
{
    if (m_selected == selected)
        return;
    m_selected = selected;
    update();
}

void ElementButton::paintEvent(QPaintEvent*)
{
    QPainter p(this);

    QColor bg = m_selected ? m_baseColor.darker(160) : m_baseColor;
    p.fillRect(rect(), bg);

    // Border
    if (m_selected)
        p.setPen(QPen(QColor(20, 20, 180), 2));
    else
        p.setPen(QPen(QColor(80, 80, 80), 1));
    p.drawRect(rect().adjusted(0, 0, -1, -1));

    p.setPen(Qt::black);

    // Atomic number — piccolo, in alto a sinistra
    QFont numFont = font();
    numFont.setPointSize(6);
    p.setFont(numFont);
    p.drawText(QRect(2, 1, width() - 4, 12),
               Qt::AlignLeft | Qt::AlignTop,
               QString::number(m_atomicNumber));

    // Simbolo — grande, centrato nella parte inferiore
    QFont symFont = font();
    symFont.setPointSize(11);
    symFont.setBold(true);
    p.setFont(symFont);
    p.drawText(QRect(0, 10, width(), height() - 10), Qt::AlignCenter, m_symbol);
}

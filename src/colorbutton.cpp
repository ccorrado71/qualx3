#include <QPainter>
#include "colorbutton.h"

ColorButton::ColorButton(QWidget *parent)
    : QPushButton(parent),
      WI(32), HE(24), m_square(false), m_alpha(false)
{
    connect(this, &QPushButton::clicked, [=] { colorbutton_clicked(); });
}

void ColorButton::SetSquare()
{
    WI = 24;
    HE = 24;
    setFixedSize(WI, HE);
    m_square = true;
}

ColorButton::ColorButton(QColor color_but, int k, QWidget *parent)
    : QPushButton(parent),
      WI(32), HE(24), m_square(false), m_alpha(false)
{
    setFixedSize(WI, HE);
    SetColor(color_but);
    setFocusPolicy(Qt::NoFocus);
    connect(this, &QPushButton::clicked, [=] { colorbutton_clicked(k); });
}

void ColorButton::SetColor(QColor color_but)
{
    m_color = color_but;
    int lato = std::min(WI, HE) - 4;
    if(m_square)
       lato = WI - 2;
    QPixmap pixmap(lato,lato);
    if(m_alpha)
    {
       QPixmap pixmap1(lato,lato);
       pixmap1.fill(color_but);
       pixmap.fill(Qt::transparent);
       QPainter p(&pixmap);
       p.setOpacity(m_color.alphaF());
       p.drawPixmap(0, 0, pixmap1);
       p.end();    
    }
    else
       pixmap.fill(color_but);

    QIcon newIcon(pixmap);
    setIcon(newIcon);
}

void ColorButton::colorbutton_clicked(int k)
{
    QColor color = m_color;
    if(m_alpha)
       color = QColorDialog::getColor(color, parentWidget(), "Set color and alpha", QColorDialog::ShowAlphaChannel);
    else
       color = QColorDialog::getColor(color, parentWidget());
    if (!color.isValid()) return;
    if(color != m_color)
    {
       SetColor(color);
       if(k >= 0)
           emit(colorChanged(color, k));
       else
           emit(colorChanged(color));
    }
}

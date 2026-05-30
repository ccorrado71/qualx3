#include "linestylescombobox.h"

#include <QPainter>
#include <QDebug>

LineStylesComboBox::LineStylesComboBox(QWidget *parent) : QComboBox(parent),
    WI(120), HE(14)
{
    populateComboBox();

    //use activated (instead currentIndexChanged) to avoid programmatical signal
    connect(this,QOverload<int>::of(&QComboBox::activated),[=](int index){
        Qt::PenStyle style = Qt::PenStyle(this->itemData(index).toInt());
        if (style != mStyle) {
            mStyle = style;
            emit lineStyleChanged(style);
        }
    });
}

void LineStylesComboBox::setStyle(const Qt::PenStyle &style)
{
    mStyle = style;    
    this->setCurrentIndex(style);
}

Qt::PenStyle LineStylesComboBox::style() const
{
    return mStyle;
}

void LineStylesComboBox::populateComboBox()
{
    this->setIconSize(QSize(WI,HE));
    this->setMinimumWidth(WI);
    this->addItem("No Line",QVariant(0));
    for (int i = Qt::SolidLine; i < Qt::CustomDashLine; i++)
    {
        QPixmap pix(WI,HE);
        pix.fill(Qt::white);

        QBrush brush(Qt::black);
        QPen pen(brush,2.5,static_cast<Qt::PenStyle>(i));

        QPainter painter(&pix);
        painter.setPen(pen);
        painter.drawLine(2,HE/2,WI-2,HE/2);

        this->addItem(QIcon(pix),QString(),QVariant(i));
    }
}

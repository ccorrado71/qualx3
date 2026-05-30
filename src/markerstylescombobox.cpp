#include "markerstylescombobox.h"

#include <QDebug>

MarkerStylesComboBox::MarkerStylesComboBox(QWidget *parent) : QComboBox(parent)
{
    populateComboBox();

    connect(this, QOverload<int>::of(&QComboBox::activated), [=](int index) {
        QCPScatterStyle::ScatterShape style = QCPScatterStyle::ScatterShape(this->itemData(index).toInt());
        if (style != mStyle) {
            mStyle = style;
            emit markerStyleChanged(style);
        }
    });
}

void MarkerStylesComboBox::setStyle(QCPScatterStyle::ScatterShape style)
{
    mStyle = style;
    this->setCurrentIndex(style);
}

QCPScatterStyle::ScatterShape MarkerStylesComboBox::style() const
{
    return mStyle;
}

void MarkerStylesComboBox::populateComboBox()
{
    QVector<QCPScatterStyle::ScatterShape> shapes;

    shapes << QCPScatterStyle::ssNone;
    shapes << QCPScatterStyle::ssDot;
    shapes << QCPScatterStyle::ssCross;
    shapes << QCPScatterStyle::ssPlus;
    shapes << QCPScatterStyle::ssCircle;
    shapes << QCPScatterStyle::ssDisc;
    shapes << QCPScatterStyle::ssSquare;
    shapes << QCPScatterStyle::ssDiamond;
    shapes << QCPScatterStyle::ssStar;
    shapes << QCPScatterStyle::ssTriangle;
    shapes << QCPScatterStyle::ssTriangleInverted;
    shapes << QCPScatterStyle::ssCrossSquare;
    shapes << QCPScatterStyle::ssPlusSquare;
    shapes << QCPScatterStyle::ssCrossCircle;
    shapes << QCPScatterStyle::ssPlusCircle;
    shapes << QCPScatterStyle::ssPeace;

    for(int i=0;i<shapes.count();i++)
    {
        QString shape(QCPScatterStyle::staticMetaObject.enumerator(QCPScatterStyle::staticMetaObject.indexOfEnumerator("ScatterShape")).valueToKey(shapes.at(i)));

        QCPScatterStyle ss = shapes.at(i);
        ss.setSize(10);
        QPixmap pm(15,15);
        QCPPainter qp(&pm);
        qp.fillRect(0,0,15,15,QBrush(Qt::white, Qt::SolidPattern));
        ss.applyTo(&qp, QPen(Qt::black));
        ss.drawShape(&qp,7,7);
        QIcon   icon = QIcon(pm);
        this->addItem(icon, shape.remove(0,2), QVariant(shapes.at(i)));
    }
}

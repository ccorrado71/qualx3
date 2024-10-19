#include "colorwidget.h"
#include "ui_colorwidget.h"

#include <QDebug>

ColorWidget::ColorWidget(QWidget *parent) :
    QWidget(parent),
    colorType(TYPE_BLACK),
    color(Qt::black),
    ui(new Ui::ColorWidget)
{
    ui->setupUi(this);

    connect(ui->color0Button, &ColorButton::colorChanged, this, [=](QColor c) {
       color = c;       
       emit colorWidgetChanged();       
    });
    connect(ui->color1Button, &ColorButton::colorChanged, this, [=](QColor c) {
       color1 = c;       
       emit colorWidgetChanged();
    });
    connect(ui->color2Button, &ColorButton::colorChanged, this, [=](QColor c) {
       color2 = c;       
       emit colorWidgetChanged();
    });
}

void ColorWidget::setColorWidget(const ColorType &type, const QColor &c, const QColor &c1, const QColor &c2)
{
    colorType = type;
    color = c;
    color1 = c1;
    color2 = c2;
    ui->comboBox->setCurrentIndex(type);
    ui->color0Button->SetColor(c);
    ui->color1Button->SetColor(c1);
    ui->color2Button->SetColor(c2);
}

void ColorWidget::getColorWidget(ColorWidget::ColorType &type, QColor &c, QColor &c1, QColor &c2) const
{
    type = colorType;
    c = color;
    c1 = color1;
    c2 = color2;
}

bool ColorWidget::isDifferent(const ColorType &type, const QColor &c, const QColor &c1, const QColor &c2) const
{
    if (colorType != type) return true;
    if (color != c) return true;
    if (color1 != c1) return true;
    if (color2 != c2) return true;
    return false;
}

ColorWidget::~ColorWidget()
{
    delete ui;
}

void ColorWidget::on_comboBox_currentIndexChanged(int index)
{
    ColorType cType = ColorType(index);

    switch (cType) {
    case (TYPE_BLACK):
    case (TYPE_WHITE):
        ui->color0Button->hide();
        ui->color1Button->hide();
        ui->color2Button->hide();
        if (cType == TYPE_BLACK)
            color = Qt::black;
        else
            color = Qt::white;
        break;
    case (TYPE_FLAT):
        ui->color0Button->show();
        ui->color1Button->hide();
        ui->color2Button->hide();
        break;
    default:
        ui->color0Button->hide();
        ui->color1Button->show();
        ui->color2Button->show();
        break;
    }

    if (cType != colorType) {
        colorType = cType;        
        //colorState = TOAPPLY;
        emit colorWidgetChanged();
    }
}

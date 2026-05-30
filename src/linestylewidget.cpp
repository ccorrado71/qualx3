#include "linestylewidget.h"
#include "ui_linestylewidget.h"
#include "colorbutton.h"

#include <QPen>

LineStyleWidget::LineStyleWidget(QWidget *parent) :
    QWidget(parent),    
    ui(new Ui::LineStyleWidget)
{
    ui->setupUi(this);

    //set width to align lineStyleWidget and markerStyleWidget
    ui->styleComboBox->setMinimumWidth(165);
    ui->styleLabel->setMinimumWidth(45);
    ui->widthLabel->setMinimumWidth(45);

    connect(ui->colorButton, &ColorButton::colorChanged, this, [=](QColor c) {        
        emit lineColorChanged(c);
    });
    connect(ui->widthSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this,[=](int i){        
        emit lineWidthChanged(i);
    });
    connect(ui->styleComboBox, &LineStylesComboBox::lineStyleChanged, this, [=](Qt::PenStyle style){        
        emit lineStyleChanged(style);
    });
}

void LineStyleWidget::setLineStyle(const Qt::PenStyle &s, const QColor &c, int w)
{
    ui->styleComboBox->setStyle(s);
    ui->colorButton->SetColor(c);
    ui->widthSpinBox->setValue(w);
}

void LineStyleWidget::setLineStyle(const QPen &pen)
{
    setLineStyle(pen.style(), pen.color(), pen.width());
}

void LineStyleWidget::setBoxName(const QString &name)
{
    ui->groupBox->setTitle(name);
}

bool LineStyleWidget::isDifferent(const QPen &pen) const
{
    if (pen.style() != ui->styleComboBox->style()) return true;
    if (pen.color() != ui->colorButton->GetColor()) return true;
    if (pen.width() != ui->widthSpinBox->value()) return true;

    return false;
}

Qt::PenStyle LineStyleWidget::getStyle() const
{
   return ui->styleComboBox->style();
}

QColor LineStyleWidget::getColor() const
{
    return ui->colorButton->GetColor();
}

int LineStyleWidget::getWidth() const
{
    return ui->widthSpinBox->value();
}

LineStyleWidget::~LineStyleWidget()
{
    delete ui;
}


#include "markerstylewidget.h"
#include "ui_markerstylewidget.h"

MarkerStyleWidget::MarkerStyleWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::MarkerStyleWidget)
{
    ui->setupUi(this);

    //set width to align lineStyleWidget and markerStyleWidget
    ui->shapeComboBox->setMinimumWidth(165);
    ui->shapeLabel->setMinimumWidth(45);
    ui->sizeLabel->setMinimumWidth(45);

    connect(ui->colorButton, &ColorButton::colorChanged, this, [=](QColor c){
        emit markerColorChanged(c);
    });
    connect(ui->sizeSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, [=](int size) {
        emit markerSizeChanged(size);
    });
    connect(ui->shapeComboBox, &MarkerStylesComboBox::markerStyleChanged, this, [=](QCPScatterStyle::ScatterShape shape) {
        emit markerShapeChanged(shape);
    });
}

void MarkerStyleWidget::setMarkerStyle(const QCPScatterStyle::ScatterShape &shape, const QColor &c, int w)
{
    ui->shapeComboBox->setStyle(shape);
    ui->colorButton->SetColor(c);
    ui->sizeSpinBox->setValue(w);
}

void MarkerStyleWidget::setMarkerStyle(const QCPScatterStyle &scatter)
{
    setMarkerStyle(scatter.shape(), scatter.pen().color(), scatter.size());
}

QCPScatterStyle::ScatterShape MarkerStyleWidget::getShape() const
{
    return ui->shapeComboBox->style();
}

int MarkerStyleWidget::getSize() const
{
    return ui->sizeSpinBox->value();
}

QColor MarkerStyleWidget::getColor() const
{
    return ui->colorButton->GetColor();
}

bool MarkerStyleWidget::isDifferent(const QCPScatterStyle &scatter)
{
    if (ui->shapeComboBox->style() != scatter.shape()) return true;
    if (ui->colorButton->GetColor() != scatter.pen().color()) return true;
    if (ui->sizeSpinBox->value() != scatter.size()) return true;
    return false;
}

MarkerStyleWidget::~MarkerStyleWidget()
{
    delete ui;
}

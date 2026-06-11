#include "rangedialog.h"
#include "ui_rangedialog.h"
#include "mainwindow.h"

extern "C" void set_range(int kaction, double *minValue, double *maxValue);

RangeDialog::RangeDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::RangeDialog),
    rangeChanged(false),
    moveCursor(true),
    signalActive(false)
{
    ui->setupUi(this);

    connect(ui->minSpinBox, QOverload<double>::of(&QDoubleSpinBox::valueChanged), this,  &RangeDialog::setMinimum);
    connect(ui->maxSpinBox, QOverload<double>::of(&QDoubleSpinBox::valueChanged), this,  &RangeDialog::setMaximum);
}

void RangeDialog::setRange(CustomPlotZoom *plot)
{
    mPlot = plot;
    connect(mPlot, &CustomPlotZoom::leftCursorPositionChanged, this, &RangeDialog::lcursorPositionChanged);
    connect(mPlot, &CustomPlotZoom::rightCursorPositionChanged, this, &RangeDialog::rcursorPositionChanged);

    set_range(0, &minValue, &maxValue);

    signalActive = false;
    ui->minSpinBox->setValue(minValue);
    ui->maxSpinBox->setValue(maxValue);
    ui->maxSpinBox->setRange(minValue, maxValue);
    ui->minSpinBox->setRange(minValue, maxValue);

    rangeChanged = false;
    mPlot->setDoubleCursorPosition(minValue,maxValue);
    signalActive = true;

    //To prevent that enter key close the dialog. Call this after the show. See also accept()
    ui->buttonBox->button(QDialogButtonBox::Ok)->setDefault(false);
    ui->buttonBox->button(QDialogButtonBox::Ok)->setAutoDefault(false);

    minValueSav = minValue;
    maxValueSav = maxValue;

    MainWindow *mw = qobject_cast<MainWindow *>(this->parent());
    mw->saveAction();
    mw->checkAction(MainWindow::NoZoom);
}

RangeDialog::~RangeDialog()
{
    delete ui;
}

void RangeDialog::setCloseDialog()
{
    mPlot->deleteDoubleCursor();
    disconnect(mPlot, &CustomPlotZoom::leftCursorPositionChanged, this, &RangeDialog::lcursorPositionChanged);
    disconnect(mPlot, &CustomPlotZoom::rightCursorPositionChanged, this, &RangeDialog::rcursorPositionChanged);

    MainWindow *mw = qobject_cast<MainWindow *>(this->parent());
    mw->restoreAction();
}

void RangeDialog::on_buttonBox_clicked(QAbstractButton *button)
{
    QDialogButtonBox::StandardButton stdButton = ui->buttonBox->standardButton(button);

    if(stdButton == QDialogButtonBox::Ok) {
        set_range(2, &minValue, &maxValue); //no if on rangeChanged to allow deallocation of thetac
        setCloseDialog();

    } else if (stdButton == QDialogButtonBox::Apply) {
        if (rangeChanged) {
            set_range(1, &minValue, &maxValue);
            rangeChanged = false;
        }
        mPlot->setDoubleCursorPosition(minValue, maxValue);

    } else if (stdButton == QDialogButtonBox::Cancel) {

    }
}

void RangeDialog::accept()
{
    //'ok' get focus if you hide the dialog. This avoid that enter key close the dialog
    if (!ui->buttonBox->button(QDialogButtonBox::Ok)->hasFocus()) return;

    QDialog::accept();
}

void RangeDialog::reject()
{
    set_range(3, &minValueSav, &maxValueSav); //no if on rangeChanged to allow deallocation of thetac
    setCloseDialog();

    QDialog::reject();
}

void RangeDialog::setMinimum(double value)
{
    if (signalActive == false) return;
    if (!qFuzzyCompare(value, minValue)){
        if (moveCursor) mPlot->setDoubleCursorPosition(value, maxValue);

        ui->maxSpinBox->setMinimum(value);
        minValue = value;
        rangeChanged = true;
    }
}

void RangeDialog::setMaximum(double value)
{
    if (signalActive == false) return;
    if (!qFuzzyCompare(value, maxValue)){
        if (moveCursor) mPlot->setDoubleCursorPosition(minValue, value);

        ui->minSpinBox->setMaximum(value);
        maxValue = value;
        rangeChanged = true;
    }
}

void RangeDialog::lcursorPositionChanged(double pos)
{
    moveCursor = false;
    ui->minSpinBox->setValue(pos);
    moveCursor = true;
}

void RangeDialog::rcursorPositionChanged(double pos)
{
    moveCursor = false;
    ui->maxSpinBox->setValue(pos);
    moveCursor = true;
}

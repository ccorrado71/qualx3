#ifndef RANGEDIALOG_H
#define RANGEDIALOG_H

#include <QDialog>
#include <QAbstractButton>

class CustomPlotZoom;

namespace Ui {
class RangeDialog;
}

class RangeDialog : public QDialog
{
    Q_OBJECT

public:
    explicit RangeDialog(QWidget *parent = nullptr);
    void setRange(CustomPlotZoom *plot);
    ~RangeDialog();

private slots:
    void on_buttonBox_clicked(QAbstractButton *button);
    void setMinimum(double value);
    void setMaximum(double value);
    void lcursorPositionChanged(double pos);
    void rcursorPositionChanged(double pos);
    void accept() override;
    void reject() override;

private:
    Ui::RangeDialog *ui;
    double minValue, maxValue;
    double minValueSav, maxValueSav;
    bool rangeChanged;
    bool moveCursor;
    bool signalActive;
    CustomPlotZoom *mPlot;
    void setCloseDialog();
};

#endif // RANGEDIALOG_H

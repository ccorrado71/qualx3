#ifndef PRINTDIALOG_H
#define PRINTDIALOG_H

#include <QDialog>

QT_BEGIN_NAMESPACE
namespace Ui { class PrintDialog; }
QT_END_NAMESPACE

class PrintDialog : public QDialog
{
    Q_OBJECT

public:
    enum PrintArea {
        Report = 0,
        ResultList,
        PeakList,
        Pattern,
        Card,
        Quantitative,
        Compare
    };

    explicit PrintDialog(QWidget *parent = nullptr);
    ~PrintDialog();

    PrintArea selectedArea() const;

private:
    Ui::PrintDialog *ui;
};

#endif // PRINTDIALOG_H

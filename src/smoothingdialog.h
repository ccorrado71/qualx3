#ifndef SMOOTHINGDIALOG_H
#define SMOOTHINGDIALOG_H

class MainWindow;

#include <QDialog>
#include <QPushButton>

typedef struct {
    int method;
    int npoints_sg;
    int npoints_ave;
    int pol_order;
} smoothOptions;

namespace Ui {
class SmoothingDialog;
}

class SmoothingDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SmoothingDialog(MainWindow *mw);
    ~SmoothingDialog();
    enum SmoothingMethod {SavGol, Averaging};
    void setSmoothing();

private slots:
    void on_methodComboBox_currentIndexChanged(int index);
    void on_pointsSpinBox_valueChanged(int arg1);
    void on_polynomialSpinBox_valueChanged(int arg1);
    void accept() override;
    void reject() override;

    void on_buttonBox_clicked(QAbstractButton *button);

private:
    Ui::SmoothingDialog *ui;
    MainWindow *mWindow;
    smoothOptions sOptions, sOptionsSaved;
    int getPoints() const;
    void setPoints(int npoints);
    void restoreOptions();
};

#endif // SMOOTHINGDIALOG_H

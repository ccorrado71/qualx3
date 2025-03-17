#ifndef PEAKSEARCHDIALOG_H
#define PEAKSEARCHDIALOG_H

#include <QDialog>
#include <QAbstractButton>

class MainWindow;

typedef struct {
    double minRange, maxRange;
    double minSearch, maxSearch;
    double threshold;
    int sensitivity;
    int numPeaks;
    int numPeaksTot;
    bool append;
} peakSearchSettings;

namespace Ui {
class PeakSearchDialog;
}

class PeakSearchDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PeakSearchDialog(MainWindow *mw);
    void setOptions();
    ~PeakSearchDialog();

private slots:
    void onNumPeaksSpinBoxValueChanged();
    void onButtonBoxClicked(QAbstractButton *button);
    void onIntensitySpinBoxValueChanged(double arg1);
    void onMinSpinBoxValueChanged(double arg1);
    void onMaxSpinBoxValueChanged(double arg1);
    void onSensitivitySpinBoxValueChanged(int arg1);
    void accept() override;
    void reject() override;

private:
    Ui::PeakSearchDialog *ui;
    MainWindow *mWindow;
    peakSearchSettings pkSettings, pkSettingsSaved;
    void setWidgets(const peakSearchSettings &psettings);
    void restoreSavedSettings();
    void updatePeakListTable();
};

#endif // PEAKSEARCHDIALOG_H

#ifndef BACKGROUNDDIALOG_H
#define BACKGROUNDDIALOG_H

#include "dataset.h"

#include <QDialog>
#include <QAbstractButton>

namespace Ui {
class BackgroundDialog;
}

class BackgroundDialog : public QDialog
{
    Q_OBJECT

public:
    explicit BackgroundDialog(QWidget *parent = nullptr);
    ~BackgroundDialog();

    enum BackgroundType {Chebyshev, Polynomial, Cosine_Fourier, Cubic_Spline, Bezier_Spline, Filter, None};
    Q_ENUM(BackgroundType)
    void setBackground();

private slots:
    void onTypeComboBoxCurrentIndexChanged(int index);
    void onButtonBoxClicked(QAbstractButton *button);
    void applyAutoChanged();
    void applyNcoefChanged();
    void onApplyFilterButtonClicked();
    void onResetFilterButtonClicked();
    void accept() override;
    void reject() override;

private:
    Ui::BackgroundDialog *ui;
    BackgroundSettings bkSettings[7];
    BackgroundSettings bkCurrentSettings, bkSavedSettings;
    int oldIndex;
    void setWidgets(const BackgroundSettings &bSettings);
    void setPage(const BackgroundSettings &bSettings);
    void enableBkWidget(bool enabled, int index);
    void restoreSavedSettings();
};

#endif // BACKGROUNDDIALOG_H

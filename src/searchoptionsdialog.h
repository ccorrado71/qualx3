#ifndef SEARCHOPTIONSDIALOG_H
#define SEARCHOPTIONSDIALOG_H

#include <QDialog>

namespace Ui { class SearchOptionsDialog; }
class QAbstractButton;

class SearchOptionsDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SearchOptionsDialog(QWidget *parent = nullptr);
    ~SearchOptionsDialog();

    // Static helpers — read saved settings without creating the UI
    static double savedMinFom();
    static int    savedMaxEntries();
    static bool   savedCheckStrongest();

    double minFom()              const;
    double weight2thetaD()       const;
    double weightIntensity()     const;
    double weightPhases()        const;
    double delta2theta()         const;
    bool   delta2thetaAuto()     const;
    bool   residualSearching()   const;
    bool   checkStrongestPeaks() const;
    bool   checkDeletedCards()   const;
    int    maxEntries()          const;

private slots:
    void onButtonClicked(QAbstractButton *button);
    void onAutoToggled(bool checked);

private:
    void loadSettings();
    void saveSettings();
    void resetToDefaults();

    Ui::SearchOptionsDialog *ui;
};

#endif // SEARCHOPTIONSDIALOG_H

#ifndef RESTRAINTSDIALOG_H
#define RESTRAINTSDIALOG_H

#include <QDialog>
#include <QStringList>

namespace Ui {
class RestraintsDialog;
}

class PeriodicTableWidget;
class QLineEdit;
class QRadioButton;
class QPushButton;
class QSpinBox;

class RestraintsDialog : public QDialog
{
    Q_OBJECT

public:
    explicit RestraintsDialog(QWidget *parent = nullptr);
    ~RestraintsDialog();

    // --- Composition tab accessors ---
    QStringList compositionSymbols()  const;
    QString     compositionFormula()  const;
    bool        isExactComposition()  const;
    bool        isContainsAny()       const;
    int         compositionMinSpecies() const;
    int         compositionMaxSpecies() const;

    // --- Chemical Name tab accessors ---
    QString     chemicalName() const;

signals:
    void loadCardsRequested();
    void loadAndMergeCardsRequested();
    void searchWithRestraintsRequested();
    void cancelAllRestraintsRequested();

private slots:
    void onHelpClicked();
    void onCancelAllRestraintsClicked();

    // Composition tab
    void onCompositionSelectionChanged(const QStringList &symbols);
    void onCompositionClearClicked();
    void onSpecialModeToggled(bool checked);

private:
    void setupCompositionTab();
    QString currentOperator() const;

    Ui::RestraintsDialog *ui;

    // Composition tab widgets
    PeriodicTableWidget *m_periodicTable      = nullptr;
    QLineEdit           *m_formulaEdit        = nullptr;
    QRadioButton        *m_radioAnd           = nullptr;
    QRadioButton        *m_radioOr            = nullptr;
    QRadioButton        *m_radioNot           = nullptr;
    QRadioButton        *m_radioExact         = nullptr;
    QRadioButton        *m_radioContainsAny   = nullptr;
    QPushButton         *m_clearButton        = nullptr;
    QSpinBox            *m_minSpecies         = nullptr;
    QSpinBox            *m_maxSpecies         = nullptr;

    QStringList m_lastSelectedSymbols;

};

#endif // RESTRAINTSDIALOG_H

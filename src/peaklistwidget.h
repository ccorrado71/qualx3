#ifndef PEAKLISTWIDGET_H
#define PEAKLISTWIDGET_H

#include <QWidget>
#include <QStandardItemModel>
#include <QSortFilterProxyModel>

typedef struct  {
                  float x;
                  float xd;
                  float y;
                  float fwhm;
                } peak_type;

namespace Ui {
class PeakListWidget;
}

class PeakListWidget : public QWidget
{
    Q_OBJECT

public:
    explicit PeakListWidget(QWidget *parent = nullptr);
    ~PeakListWidget();
    void updatePeakListTable();

    QStandardItemModel *peakListModel;

private slots:
    void createContextMenu(QPoint pos);
    void deletePeaks();
    void addPeak();
    void peakListTableChanged(const QModelIndex &topLeft, const QModelIndex &);
    void markSelectedPeaks();

private:
    Ui::PeakListWidget *ui;
    QSortFilterProxyModel *peakListProxyModel;
    QAction *deletePeakAction;
    QAction *addPeakAction;
    bool tableChangedEnabled;

    void createPeakListModel();
    void createContextMenuActions();
    void updatePeakListModel();
    void setPeakTableView();
    void drawPeaks();
    void addRow();
};

#endif // PEAKLISTWIDGET_H

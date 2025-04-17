#include "peaklistwidget.h"
#include "ui_peaklistwidget.h"
#include "floatdelegate.h"
#include "mainwindow.h"

extern "C" int peak_number();
extern "C" void get_peak_list(peak_type peakList[]);
extern "C" void peak_list_change(peak_type *peak, int irow, int icol, int *ier);
extern "C" int  get_plot_size(int ptype);
extern "C" void get_plot_xy(float x[], float y[], float *wave, int plot_type);

PeakListWidget::PeakListWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::PeakListWidget),
    tableChangedEnabled(true)
{
    ui->setupUi(this);

    createPeakListModel();
    setPeakTableView();
    createContextMenuActions();
}

PeakListWidget::~PeakListWidget()
{
    delete ui;
}

void PeakListWidget::updatePeakListTable()
{
    updatePeakListModel();

    ui->peakTableView->resizeColumnsToContents();
}

void PeakListWidget::createContextMenu(QPoint pos)
{
    QModelIndex indexMenu=ui->peakTableView->indexAt(pos);
    QMenu *menu=new QMenu(this);
    menu->addAction(addPeakAction);
    menu->addAction(deletePeakAction);

    deletePeakAction->setEnabled(indexMenu.isValid());

    menu->popup(ui->peakTableView->viewport()->mapToGlobal(pos));
}

void PeakListWidget::deletePeaks()
{
    tableChangedEnabled = false;
    MainWindow *mw = qobject_cast<MainWindow *>(QApplication::activeWindow());
    mw->xpdViewer()->deleteSelectedPeaks();
    QModelIndexList selection = ui->peakTableView->selectionModel()->selectedRows();
    peakListProxyModel->removeRows(selection.at(0).row(),selection.count());
    //mw->xpdViewer()->drawPeaks();
    tableChangedEnabled = true;
}

void PeakListWidget::addRow()
{
    QList<QStandardItem *> items;
    items.append(new QStandardItem());
    items.append(new QStandardItem());
    QStandardItem *item = new QStandardItem();
    item->setFlags(item->flags() &  ~Qt::ItemIsEditable);  //Not editable
    items.append(item);
    item = new QStandardItem();
    item->setFlags(item->flags() &  ~Qt::ItemIsEditable);  //Not editable
    items.append(item);
    peakListModel->insertRow(peakListModel->rowCount(),items);
}

void PeakListWidget::addPeak()
{
    addRow();
    int lastRow = peakListModel->rowCount() - 1;
    QModelIndex index = peakListProxyModel->mapFromSource(peakListModel->index(lastRow, 0, QModelIndex()));
    ui->peakTableView->selectRow(index.row());
    ui->peakTableView->edit(index);
}

void PeakListWidget::drawPeaks()
{
    MainWindow *mw = qobject_cast<MainWindow *>(QApplication::activeWindow());
    //mw->xpdViewer()->drawPeaks();

    //Get peaks
    auto pk = mw->xpdViewer()->peaks;
    int npk = get_plot_size(pk.getGtype());

    if (npk > 0) {
        float *xv = new float[npk];
        float *yv = new float[npk];
        float wave;
        get_plot_xy(xv, yv, &wave, pk.getGtype());

        QVector<double> xvet(npk), yvet(npk);
        for (int i = 0; i < npk; i++) {
            xvet[i] = xv[i];
            yvet[i] = yv[i];
        }
        delete [] xv;
        delete [] yv;

        //Draw peaks
        mw->xpdViewer()->drawGraphicItem(pk, xvet, yvet, wave);
    } else {
        mw->xpdViewer()->clearGraphSelection(pk);
    }
}

void PeakListWidget::peakListTableChanged(const QModelIndex &topLeft, const QModelIndex &)
{
    if (tableChangedEnabled) {        
        QModelIndexList indices;
        for (int i=0; i < peakListModel->columnCount(); i++) {
            indices.push_back(topLeft.siblingAtColumn(i));
        }
        peak_type peak;
        peak.x = indices[0].data().toFloat();
        peak.xd = indices[1].data().toFloat();
        peak.y = indices[2].data().toFloat();
        peak.fwhm = indices[3].data().toFloat();        
        int ier;
        peak_list_change(&peak, topLeft.row()+1, topLeft.column()+1, &ier);
        if (ier == 2) {
            peakListModel->removeRow(topLeft.row());
        } else {
            tableChangedEnabled = false;
            if (topLeft.column() == 0) {
                if (ier == 1) peakListModel->setData(indices[0], peak.x);
                peakListModel->setData(indices[1], peak.xd);
            } else {
                peakListModel->setData(indices[0], peak.x);
                if (ier == 1) peakListModel->setData(indices[1], peak.xd);
            }
            peakListModel->setData(indices[2], peak.y);
            peakListModel->setData(indices[3], peak.fwhm);
            peakListModel->sort(0,Qt::AscendingOrder);
            tableChangedEnabled = true;
            drawPeaks();
            markSelectedPeaks();
        }
    }
}

void PeakListWidget::createPeakListModel()
{
    peakListModel = new QStandardItemModel(this);
    peakListProxyModel = new QSortFilterProxyModel(this);
    peakListProxyModel->setSourceModel(peakListModel);
    peakListModel->setColumnCount(4);
    //const QStringList titles = { "2"+QString(0x03B8), "d", "Intensity", "FWHM"}; Not compatible with Qt6
    const QStringList titles = { "2"+QString(QChar(0x03B8)), "d", "Intensity", "FWHM"};
    peakListModel->setHorizontalHeaderLabels(titles);
    connect(peakListModel, &QStandardItemModel::dataChanged, this, &PeakListWidget::peakListTableChanged);
}

void PeakListWidget::createContextMenuActions()
{
    deletePeakAction = new QAction("Delete Peaks", this);
    connect(deletePeakAction, &QAction::triggered, this, &PeakListWidget::deletePeaks);
    addPeakAction = new QAction("Add Peak", this);
    connect(addPeakAction, &QAction::triggered, this, &PeakListWidget::addPeak);
}

void PeakListWidget::updatePeakListModel()
{
    tableChangedEnabled = false;
    int npeaks = peak_number();
    peakListModel->setRowCount(npeaks);
    if (npeaks > 0) {
        peak_type *peaks = new peak_type[npeaks];

        get_peak_list(peaks);

        for (int r = 0; r < npeaks; r++) {
            QStandardItem *item = new QStandardItem();
            item->setData(peaks[r].x, Qt::DisplayRole);
            peakListModel->setItem(r,0,item);

            item = new QStandardItem();
            item->setData(peaks[r].xd, Qt::DisplayRole);
            peakListModel->setItem(r,1,item);

            item = new QStandardItem();
            item->setData(peaks[r].y, Qt::DisplayRole);
            item->setFlags(item->flags() &  ~Qt::ItemIsEditable);
            peakListModel->setItem(r,2,item);

            item = new QStandardItem();
            item->setData(peaks[r].fwhm, Qt::DisplayRole);
            item->setFlags(item->flags() &  ~Qt::ItemIsEditable);
            peakListModel->setItem(r,3,item);
        }

        delete [] peaks;
    }
    tableChangedEnabled = true;
}

void PeakListWidget::markSelectedPeaks()
{
    QModelIndexList selection = ui->peakTableView->selectionModel()->selectedRows();
    QVector<int> selPeak(selection.count());
    for(int i=0; i < selection.count(); i++)
    {
        selPeak[i] = peakListProxyModel->mapToSource(selection.at(i)).row();
    }
    MainWindow *mw = qobject_cast<MainWindow *>(QApplication::activeWindow());
    mw->xpdViewer()->drawSelectedPeaks(selPeak);
}

void PeakListWidget::setPeakTableView()
{
    ui->peakTableView->setModel(peakListProxyModel);
    ui->peakTableView->verticalHeader()->setDefaultSectionSize(22);
    ui->peakTableView->setSortingEnabled(true);
    ui->peakTableView->sortByColumn(0,Qt::AscendingOrder);
    ui->peakTableView->verticalHeader()->hide();
    ui->peakTableView->setShowGrid(false);
#ifndef _WIN32
    ui->peakTableView->setAlternatingRowColors(true);
#endif
    ui->peakTableView->setSelectionMode(QAbstractItemView::ExtendedSelection);
    ui->peakTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui->peakTableView->horizontalHeader()->stretchLastSection();

    FloatDelegate *floatDelegate = new FloatDelegate(ui->peakTableView);
    floatDelegate->setDecimals(5);
    ui->peakTableView->setItemDelegateForColumn(0, floatDelegate);
    ui->peakTableView->setItemDelegateForColumn(1, floatDelegate);
    FloatDelegate *floatDelegate1 = new FloatDelegate(ui->peakTableView);
    ui->peakTableView->setItemDelegateForColumn(2, floatDelegate1);
    ui->peakTableView->setItemDelegateForColumn(3, floatDelegate1);

    ui->peakTableView->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(ui->peakTableView, &QTableView::customContextMenuRequested,
            this, &PeakListWidget::createContextMenu);
    connect(ui->peakTableView->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &PeakListWidget::markSelectedPeaks);

}

#include "mainwindow.h"
#include "libcomune.h"
#include "wavedialog.h"
//#include "fourierpeaklistdialog.h"
#include "qt_utils.h"
#include "stringlistdialog.h"
#include "graphitem.h"

#include <QStandardPaths>
#include <QCoreApplication>
#include <QMessageBox>
#include <QApplication>
#include <QDebug>

extern MainWindow *mMainWindow;
extern "C" void wlist_get_title(int col, char *title);
extern "C" void wlist_get_item(int row, int col, char *item);
//extern "C" void rammfunc(int isel);

int jscreen = 0;
bool single_it = true;

// void fortran_al_lavoro(void)
// {
//    if(!jscreen)
//       return;
//    single_it = true;
// }

// void fine_lavoro(void)
// {
//    if(!jscreen)
//       return;
//    single_it = false;
// }

extern "C" void get_app_name_and_version(char *app_name, int *len_name, char *app_version, int *len_version)
{
   QString appName = qApp->applicationName();
   strncpy(app_name, appName.toStdString().c_str(), appName.length()+1);
   *len_name = appName.length();

   QString appVersion = qApp->applicationVersion();
   strncpy(app_version, appVersion.toStdString().c_str(), appVersion.length()+1);
   *len_version = appVersion.length();
}

// extern "C" int HomeLocationC(char *dirnam, int *ld)
// {
//     QString home = QStandardPaths::writableLocation(QStandardPaths::HomeLocation);
//     if (home.isEmpty()) return 1;
//     *ld = home.length();
//     strncpy(dirnam, home.toStdString().c_str(), home.length()+1);
//     return 0;
// }

// extern "C" void getProgramDir(char *exedir, int *l1)
// {
//     QString dir = QCoreApplication::applicationDirPath();
//     strncpy( exedir, dir.toStdString().c_str(), dir.length()+1);
//     *l1 = dir.length();
// }

// extern "C" void cbasename(char *filenam, char *basenam)
// {
//     QString name(filenam);
//     QString bname = QFileInfo(name).baseName();
//     strncpy( basenam, bname.toStdString().c_str(), bname.length()+1);
// }

// extern "C" void setCurrentFiles(char *fileIn, char *fileOut, char *fileType) {
//     //mMainWindow->setCurrentFile(QString::fromUtf8(fileIn),QString::fromUtf8(fileType));
//     //Recente files require filename completed with path
//     mMainWindow->setCurrentFile(QFileInfo(fileIn).absoluteFilePath(),QString::fromUtf8(fileType));
//     mMainWindow->setOutputFileName(QString::fromUtf8(fileOut));
//     mMainWindow->setWindowTitle(QString::fromUtf8(fileIn));
// }

// extern "C" void WriteMesg(char *mesg, int i, int kprocess) {
//     mMainWindow->getInfoTable()->writeMessage(QString::fromUtf8(mesg), i);
//     if (kprocess == 1) QCoreApplication::processEvents();
// }

// extern "C" void setRowCountMesgC(int nrow, int kprocess) {
//     mMainWindow->getInfoTable()->setRowCountMessage(nrow);
//     if (kprocess == 1) QCoreApplication::processEvents();
// }

// extern "C" void ClearMesg(int kprocess) {
//     mMainWindow->getInfoTable()->setRowCountMessage(0);
//     if (kprocess == 1) QCoreApplication::processEvents();
// }

// extern "C" void ClearRow(int row, int kprocess) {
//     mMainWindow->getInfoTable()->clearRow(row-1);
//     if (kprocess == 1) QCoreApplication::processEvents();
// }

// extern "C" int getRowCountMesg() {
//     return mMainWindow->getInfoTable()->rowCount();
// }

// extern "C" int rowFindMesg(char *mesg) {
//     return mMainWindow->getInfoTable()->rowFindMessage(QString::fromUtf8(mesg)) + 1;
// }

extern "C" void openwindow() {
    jscreen = 1;
}

extern "C" void enablesingleiteration(int z) {
    if (z == 0) {
        single_it = false;
    } else {
        single_it = true;
    }    
}

extern "C" void dosingleiteration() {
    if(!jscreen)
       return;
    QCoreApplication::processEvents();
}

extern "C" void c_set_status_message(const char *msg) {
    if (!mMainWindow) return;
    mMainWindow->setStatusMessage(QString::fromUtf8(msg));
}

// extern "C" void cprintmessage(char *string) {
//     if(!jscreen)
//        return;
//     if(single_it)
//        QCoreApplication::processEvents();

//     mMainWindow->setStatusMessage(QString::fromUtf8(string));
// }

// extern "C" void progressbar_set_visible(int vis, int kprocess) {
//     if(!jscreen)
//        return;

//     QProgressBar *progressBar = mMainWindow->getStatusProgressBar();

//     progressBar->setVisible(vis);
//     // restart range and value
//     progressBar->setRange(0,100);

//     if (kprocess == 1) QCoreApplication::processEvents();
// }

// extern "C" void progressbar_set_value(int x, int max, int kprocess) {
//     if(!jscreen)
//        return;
//     // if(single_it)
//     //    QCoreApplication::processEvents();

//     QProgressBar *progressBar = mMainWindow->getStatusProgressBar();

//     progressBar->setRange(0,max);
//     progressBar->setValue(x);

//     if (kprocess == 1) QCoreApplication::processEvents();
// }

// extern "C" void progressbar_set_message(char *string, int kprocess) {
//     if(!jscreen)
//        return;
//     // if(single_it)
//     //    QCoreApplication::processEvents();

//     QProgressBar *progressBar = mMainWindow->getStatusProgressBar();
//     progressBar->setFormat(QString::fromUtf8(string));

//     if (kprocess == 1) QCoreApplication::processEvents();
// }

// extern "C" void progressbar_reset_message_() {
//     if(!jscreen)
//         return;
//     if(single_it)
//         QCoreApplication::processEvents();

//     QProgressBar *progressBar = mMainWindow->getStatusProgressBar();
//     progressBar->resetFormat();
// }

// extern "C" void c_fine_lavoro(int flag)
// {
//     if(!jscreen)
//         return;

//     fine_lavoro();
// }

// extern "C" void c_fortran_al_lavoro(int flag)
// {
//     if(!jscreen)
//         return;

//     fortran_al_lavoro();
// }

// extern "C" void c_stop_fortran_visible(int visible) {
//     mMainWindow->setNextActionVisible(visible);
// }

// extern "C" void c_stop_fortran() {
//     mMainWindow->enableNextAction();
// }

extern "C" void c_enableActions(int action, int state) {
    mMainWindow->enableActions(static_cast<MainWindow::EnabledActions>(action), state);
}

extern "C" void set_graphic_area() {
    //mMainWindow->setGraphicArea();
    mMainWindow->xpdViewer()->setGraphicArea();
}

extern "C" void draw_graphic() {
    //mMainWindow->draw();
    mMainWindow->xpdViewer()->drawPlot();
}

extern "C" void add_plot(float x[], float y[], int num, int type, int visible, float wave, char *name) {
    graphItem::ItemType gtype = static_cast<graphItem::ItemType>(type);
    mMainWindow->xpdViewer()->addPlot(x, y, num, gtype, visible, wave, QString::fromUtf8(name));
}

// extern "C" void add_plot2(float x[], int num, int type, int visible, float wave) {
//     graphItem::ItemType gtype = static_cast<graphItem::ItemType>(type);
//     mMainWindow->xpdViewer()->addPlot(x, num, gtype, visible, wave);
// }

extern "C" void add_reflections(float x[], int h[], int k[], int l[], int num, int visible, float wave) {
    mMainWindow->xpdViewer()->addReflections(x, h, k, l, num, visible, wave);
}

// extern "C" void add_systematic_absences(int sysabs[], int nsysabs) {
//     QVector<int> sAbs(nsysabs);
//     for (int i = 0; i < nsysabs; i++) {
//         sAbs[i] = sysabs[i];
//     }
//     mMainWindow->xpdViewer()->setSystematicAbsences(sAbs);
//     mMainWindow->xpdViewer()->drawSystematicAbsences();
// }

extern "C" void enable_rescale(int rescale) {
    mMainWindow->xpdViewer()->enableRescalePlot(rescale);
}

extern "C" void openwave(int *radtype, int *nwave, float wave[], float ratio[]) {
    bool isWaitCursor = stopWaitCursor();
    WaveDialog dialog(mMainWindow);
    dialog.exec();
    *nwave = dialog.nWaves();
    *radtype = dialog.radiationType()+1;
    wave[0] = dialog.wave1();
    wave[1] = dialog.wave2();
    ratio[0] = 1.0;
    ratio[1] = dialog.ratio();
    if (isWaitCursor) QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
}

// extern "C" void cell_parameters_dlg(double cell[6], int *sgIndex, char *spg, int *lenspg, char *content, int *lencont, int *kfindspace, int *kreturn) {

//     QString spgSymbol = QString::fromUtf8(spg);
//     QString cont = QString::fromUtf8(content);
//     bool isWaitCursor = stopWaitCursor();
//     mMainWindow->openSpgDialog(cell, *sgIndex, spgSymbol, cont, *kfindspace, *kreturn);
//     strncpy(spg, spgSymbol.toStdString().c_str(), spgSymbol.length()+1);
//     *lenspg = spgSymbol.length();
//     strncpy(content, cont.toStdString().c_str(), cont.length()+1);
//     *lencont = cont.length();
//     if (isWaitCursor) QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
// }

// extern "C" void open_cell_dialog(int ncell, cell_info_type cell[], int *selected_cell){
//     //selected_cell starts from 1
//     mMainWindow->openCellDialog(cell, ncell, *selected_cell);
// }

// extern "C" void c_open_spacegroup(spgext_info_type spglist[], int listSize, int *kselect) {
//     bool isWaitCursor = stopWaitCursor();
//     int selectedSpace = 0;
//     mMainWindow->openSpaceGroupListDialog(spglist, listSize, selectedSpace);
//     *kselect = ++selectedSpace;
//     if (isWaitCursor) QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
// }

// extern "C" void open_restart_dlg(int kproc, int saf, int nset, int selset, trialInfo tinfo[]) {
//     mMainWindow->openExploreTrialsDialog(kproc, saf, nset, selset, tinfo);
// }

// extern "C" void aprisannelwin() {
//     mMainWindow->openGlobalOptDialog(false);
// }

// extern "C" void fourier_peak_list(FourierPeakInfo fPeaks[], int nPeaks, int nOldPeaks) {
//     mMainWindow->openFourierPeakDialog(fPeaks, nPeaks, nOldPeaks);
// }

extern "C" void update_peak_list() {
    mMainWindow->updatePeakListTable();
}

// extern "C" void openHklList(c_hkl_info hklInfo[], int nHkl, int vis[]) {
//     mMainWindow->openHklListDialog(hklInfo, nHkl, vis);
// }

// extern "C" void open_rietdlg(refine_condition_type *rcond) {
//     mMainWindow->openRietveldDialog(*rcond);
// }

extern "C" void MsgWinErrC1(char *inte, char *msg, int tipo, int *exitC) {
    enum MessageType {
        INFO_WINDOW = 1, QUEST_WINDOW, QUEST_WINDOW_R,
        QUEST_WINDOW_YES_NO, QUEST_WINDOW_NO_YES, THREE_BUTTONS, WARN_WINDOW,
        ERR_WINDOW, SEVERE_ERR_WINDOW
    };

    QMainWindow *parent = mMainWindow;

    bool isWaitCursor = stopWaitCursor();

    QMessageBox::StandardButton ret = QMessageBox::NoButton;
    MessageType mType = static_cast<MessageType>(tipo);
    switch (mType) {
    case INFO_WINDOW:
        QMessageBox::information(parent,QString::fromUtf8(inte),QString::fromUtf8(msg));
        break;
    case QUEST_WINDOW:
        ret = QMessageBox::question(parent,QString::fromUtf8(inte),QString::fromUtf8(msg),
                                    QMessageBox::Ok | QMessageBox::Cancel, QMessageBox::Ok);
        break;
    case QUEST_WINDOW_R:
        ret = QMessageBox::question(parent,QString::fromUtf8(inte),QString::fromUtf8(msg),
                                    QMessageBox::Ok | QMessageBox::Cancel, QMessageBox::Cancel);
        break;
    case QUEST_WINDOW_YES_NO:
        ret = QMessageBox::question(parent,QString::fromUtf8(inte),QString::fromUtf8(msg),
                                    QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);
        break;
    case QUEST_WINDOW_NO_YES:
        ret = QMessageBox::question(parent,QString::fromUtf8(inte),QString::fromUtf8(msg),
                                    QMessageBox::Yes | QMessageBox::No, QMessageBox::No);
        break;
    case THREE_BUTTONS:
        //Obsolete message, must be removed!
        break;
    case WARN_WINDOW:
        QMessageBox::warning(parent,QString::fromUtf8(inte),QString::fromUtf8(msg));
        break;
    case ERR_WINDOW:
    case SEVERE_ERR_WINDOW:
        QMessageBox::critical(parent,QString::fromUtf8(inte),QString::fromUtf8(msg));
        break;
    }

    switch (ret) {
    case QMessageBox::Ok:
    case QMessageBox::Yes:
        *exitC = 1;
        break;
    case QMessageBox::Cancel:
    case QMessageBox::No:
        *exitC = 2;
        break;
    default:
        *exitC = 0;
        break;
    }
    if (isWaitCursor) QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
}

// extern "C" void clear_plot2_() {
//     CustomPlotZoom *plot2 = mMainWindow->getPlot2();
//     plot2->clearGraphs();
//     plot2->axisRect(0)->setVisible(false);
// }

// extern "C" void init_plot2(char xLabel[], char yLabel[]) {
//     CustomPlotZoom *plot2 = mMainWindow->getPlot2();
//     mMainWindow->clearPlot2();
//     plot2->xAxis->setLabel(QString::fromUtf8(xLabel));
//     plot2->yAxis->setLabel(QString::fromUtf8(yLabel));
// }

// extern "C" void add_point_to_plot2(int graph, float x, float y) {
//     CustomPlotZoom *plot2 = mMainWindow->getPlot2();
//     if (plot2->graphCount() < graph) {
//         plot2->addGraph();
//         QColor color = graphItem::getPaletteColor(graph);
//         plot2->graph()->setPen(QPen(color));
//         //plot2->graph()->setScatterStyle(QCPScatterStyle::ssCircle);
//     }
//     //    double xx = x;
//     //    double yy = y;
//     //    plot2->graph()->addData(xx,yy);
//     plot2->graph()->addData(x,y);
//     plot2->rescaleAxes();
//     plot2->replot();
// }

// extern "C" void view_wilson(double xw[], double yw[], int nw, double xw1[2], double yw1[2], float bt) {
//     CustomPlotZoom *plot2 = mMainWindow->getPlot2();
//     mMainWindow->clearPlot2();
//     plot2->axisRect(0)->setVisible(true);

//     // set title of plot:
//     plot2->plotLayout()->insertRow(0);
//     plot2->plotLayout()->addElement(0, 0, new QCPTextElement(plot2, "Wilson Plot", QFont("sans", 10, QFont::Bold)));

//     // Draw curve
//     QVector<double> xwil(nw),ywil(nw);
//     std::copy(xw,xw+nw,xwil.begin());
//     std::copy(yw,yw+nw,ywil.begin());
//     plot2->addGraph();
//     plot2->graph()->setPen(QColor(Qt::blue));
//     plot2->graph()->setData(xwil,ywil,true);
//     plot2->xAxis->setLabel("<s"+QString(QChar(0x00B2))+">");
//     plot2->yAxis->setLabel("log(<I>/"+QString(QChar(0x03A3))+")");

//     // Draw straight line
//     QVector<double> xwil1(2),ywil1(2);
//     std::copy(xw1,xw1+2,xwil1.begin());
//     std::copy(yw1,yw1+2,ywil1.begin());
//     plot2->addGraph();
//     plot2->graph()->setPen(QColor(Qt::red));
//     plot2->graph()->setData(xwil1,ywil1,false);

//     // Draw text
//     QCPItemText *bText = new QCPItemText(plot2);
//     bText->setPositionAlignment(Qt::AlignTop|Qt::AlignHCenter);
//     bText->position->setType(QCPItemPosition::ptAxisRectRatio);
//     bText->position->setCoords(0.5, 0.05);
//     bText->setFont(QFont("sans", 10));
//     if (bt > 0) {
//         bText->setText(QString("Thermal factor (B) = %1").arg(bt));
//     } else {
//         bText->setText(QString("Thermal factor (B) = %1. Set to 0.001").arg(bt));
//     }

//     plot2->rescaleAxes();
//     plot2->replot();
// }

extern "C" void open_string_list_dlg(char *title, char *label, int nrow, int ncol, int connect, int *selection) {
    auto listDlg = new StringListDialog(mMainWindow);
    listDlg->setWindowTitle(QString::fromUtf8(title));
    listDlg->setLabel(label);
    listDlg->setTableSize(nrow,ncol);
    listDlg->setSelection(*selection - 1);

    //Set column titles
    char coltitle[80];
    QStringList columTitle;
    for (int i = 0; i < ncol; i++) {
        wlist_get_title(i+1, coltitle);
        columTitle.append(QString::fromUtf8(coltitle));
    }
    listDlg->setColumTitle(columTitle);

    //Set items for the table
    char sitem[80];
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            wlist_get_item(i+1, j+1, sitem);
            listDlg->setTableItem(i, j, QString::fromUtf8(sitem));
        }
    }

    //Connect signal for RAMM procedure
//    if (connect == 1) {
//        QObject::connect(listDlg, &StringListDialog::newRowSelected, [](int row) {
//            rammfunc(row+1);
//        });
//    }

    int ret = listDlg->exec();
    if (ret == QDialog::Accepted) {
        *selection = listDlg->getSelection() + 1;
    } else {
        *selection = 0;
    }
}

extern "C" void WaitCursorOn() {
    QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
}

extern "C" void WaitCursorOff() {
    QApplication::restoreOverrideCursor();
}

//----------------------------------------------------------------------
// Fortran interop: crystal info from CIF
//----------------------------------------------------------------------

extern "C" void init_qualx_tables(const char* exepath, int len_path, int* ier);

bool initQualxTables(const QString &exePath)
{
    QByteArray path = exePath.toLocal8Bit();
    int ier = 0;
    init_qualx_tables(path.constData(), path.size(), &ier);
    return (ier == 0);
}

extern "C" void get_crystal_info_from_cif(
    const char* cif_file,
    const int*  inorganic_only,
    int*   nat,    float* cellpar, int*   icell,
    float* vol,    float* dens,    int*   zval,
    float* mu,     int*   nrefl,   float* rir,    float* wavelen,
    char*  sform,  char*  subfile, char*  spg_sym, char* crysys,
    int*   refl_h, int*   refl_k,  int*   refl_l,
    float* refl_tth, float* refl_d, int* refl_mult,
    float* refl_lp,  float* refl_fc2, float* refl_inte, float* refl_ipct,
    int*   nrefl_print,
    int*   nelem,  char   specie_label[][3],
    char*  chem_name, char* mineral_name,
    int*   ier
);

bool readCrystalInfoFromCif(const QString &filePath, CifCrystalInfo &info, bool inorganicOnly)
{
    QByteArray path = filePath.toLocal8Bit();
    int ier = 0;
    int inorgFlag = inorganicOnly ? 1 : 0;
    get_crystal_info_from_cif(
        path.constData(),
        &inorgFlag,
        &info.nat, info.cellpar, info.icell, &info.vol, &info.dens,
        &info.zval, &info.mu, &info.nrefl, &info.rir, &info.wavelen,
        info.sform, info.subfile, info.spg_sym, info.crysys,
        info.refl_h, info.refl_k, info.refl_l,
        info.refl_tth, info.refl_d, info.refl_mult,
        info.refl_lp, info.refl_fc2, info.refl_inte, info.refl_ipct,
        &info.nrefl_print,
        &info.nelem, info.specie_label,
        info.chem_name, info.mineral_name,
        &ier
    );
    return (ier == 0);
}

//void test_crystal_info_from_cif()
//{
//    const char* cif_file = "/home/corrado/test_expo/1000099.cif";
//
//    int   nat, icell[6], zval, nrefl, nrefl_print, nelem, ier;
//    float cellpar[6], vol, dens, mu, rir, wavelen;
//    char  sform[256], subfile[32], spg_sym[64], crysys[64];
//    char  specie_label[100][3];
//    int   refl_h[500], refl_k[500], refl_l[500], refl_mult[500];
//    float refl_tth[500], refl_d[500], refl_lp[500];
//    float refl_fc2[500], refl_inte[500], refl_ipct[500];
//
//    get_crystal_info_from_cif(
//        cif_file,
//        &nat, cellpar, icell, &vol, &dens, &zval, &mu,
//        &nrefl, &rir, &wavelen,
//        sform, subfile, spg_sym, crysys,
//        refl_h, refl_k, refl_l,
//        refl_tth, refl_d, refl_mult,
//        refl_lp, refl_fc2, refl_inte, refl_ipct,
//        &nrefl_print,
//        &nelem, specie_label,
//        &ier
//    );
//
//    if (ier != 0) {
//        printf("get_crystal_info_from_cif error: ier = %d\n", ier);
//        return;
//    }
//
//    // Header info (same format as write_intensity_file)
//    printf(" FORMULA: %s\n", sform);
//    printf(" SUBFILE: %s\n", subfile);
//    printf(" CELL:    ");
//    for (int i = 0; i < 3; i++) printf(" %.4f", cellpar[i]);
//    for (int i = 3; i < 6; i++) printf(" %.3f", cellpar[i]);
//    printf("\n");
//    printf(" WRITE CELL: %6d%6d%6d%6d%6d%6d\n",
//           icell[0], icell[1], icell[2], icell[3], icell[4], icell[5]);
//    printf(" SPG:     %s\n", spg_sym);
//    printf(" CRY SYS: %s\n", crysys);
//    printf(" VOLUME : %.3f\n", vol);
//    printf(" Density: %.3f g cm-3\n", dens);
//    printf(" Z : %5d\n", zval);
//    printf(" mu(CuKa):%.3f cm-1\n", mu);
//    printf(" NAtoms: %d\n", nat);
//    printf(" NReflections: %d\n", nrefl);
//    printf(" RIR     :%.3f\n", rir);
//    printf(" ----------------------------------------------------------------------------- \n");
//    printf("   Remarks: Diffraction pattern calculated by EXPO from COD database cif file\n");
//    printf("   Remarks: RIR calculated by EXPO     \n");
//    printf(" ----------------------------------------------------------------------------- \n");
//
//    // Reflection table (same format as write_reflections code=5)
//    printf("  h   k  l      2theta    d      mult           LP"
//           "               Fc^2             Intensity                   Intensity(%%)\n");
//    for (int i = 0; i < nrefl_print; i++) {
//        printf("%4d%4d%4d%10.4f%10.4f%4d%20.4f%20.4f%20.4f%20.4f\n",
//               refl_h[i], refl_k[i], refl_l[i],
//               refl_tth[i], refl_d[i], refl_mult[i],
//               refl_lp[i], refl_fc2[i], refl_inte[i], refl_ipct[i]);
//    }
//}


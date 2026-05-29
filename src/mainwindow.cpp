#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "appstate.h"
#include "experimentalpeaks.h"
#include "peakassoc.h"
#include "dbquerybuilder.h"
#include "managedatabasesdialog.h"
#include "progkeysettings.h"
#include "restraintsdialog.h"
#include "savedialog.h"
#include "searchoptionsdialog.h"
#include "fileutils.h"
#include "xpdutils.h"
#include "dbresultswidget.h"

#include <QApplication>
#include <QDebug>
#include <QInputDialog>
#include <QMessageBox>

#include <algorithm>

MainWindow *mMainWindow;
QString MainWindow::pathDataFiles = "";

extern "C" void esportanew(int iCod, const char *filename, int len);
extern "C" void open_diffraction_patt(const char *fileIn, int lenIn, const char *fileOut, int lenOut, int addData, int *err);
extern "C" void run_peaksearchwin();
extern "C" void LoadPeaksC(const char *filename, int length, int tipo, int *ier);
extern "C" void SavePeaksC(const char *filename, int length, int tipo);
extern "C" void delete_peaksC(int pkvet[], int npeak);
extern "C" void process_action_points(int kaction, double xp, double yp,int *ier);
extern "C" void apply_background_subtraction();
extern "C" int peak_number();
extern "C" void get_d_delta_values(float dval[], float deltadval[], float tthval[], float intval[], float fwhmval[], double *wave);
extern "C" void computeFOM(double tth[], double intensity[], int tsize, double *fomd,
                           double w2thetad, double w_intensity, double w_phases, double delta2theta,
                           double *fompeakpos_out, double *fomintensity_out, double *scale_out,
                           double exp_tth[], double exp_intensity[], int exp_size);


MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , mAction(NoAction)
    , savedZoomAction(mAction)
{
    ui->setupUi(this);
    ui->resultsWidget->setEntryToolBar(ui->toolBarEntry);
    ui->resultsWidget->setContextMenuActions({
        ui->actionLoad_Add,
        nullptr,  // separator
        ui->actionAccept_Selected_Entries,
        ui->actionDelete,
        ui->actionChange_Color
    });

    tabifyDockWidget(ui->peakDockWidget, ui->dockWidgetCompare);
    tabifyDockWidget(ui->dockWidgetCompare, ui->dockWidgetCard);
    tabifyDockWidget(ui->dockWidgetCard, ui->dockWidgetQuant);
    tabifyDockWidget(ui->dockWidgetQuant, ui->dockWidgetReport);
    ui->peakDockWidget->raise();

    setWindowTitle(qApp->applicationDisplayName()+"-"+qApp->applicationVersion());

    // set objects for the statusbar
    statusLabel1 = new QLabel(this);
    statusLabel1->setFrameStyle(QFrame::Panel | QFrame::Sunken);
    statusProgressBar = new QProgressBar(this);
    statusProgressBar->setTextVisible(true);
    statusProgressBar->hide();
    ui->statusBar->addPermanentWidget(statusLabel1,3);
    ui->statusBar->addPermanentWidget(statusProgressBar,1);
    // send statustip on statusLabel1
    connect(ui->statusBar, &QStatusBar::messageChanged, this, [=](){
        statusLabel1->setText(ui->statusBar->currentMessage());
    });

    createActionGroup();
    actionsSetup();
    readAction();

    xpdViewer()->connectLabels(ui->plotLabel1, ui->plotLabel2, ui->plotLabel3);
    xpdViewer()->connectHorizontalScrollBar(ui->horizontalScrollBar);
    xpdViewer()->connectAddDeletePoints();
    xpdViewer()->connectSelectionChanged();
    connect(xpdViewer(), &XpdViewWidget::deleteSelectedPeaksSignal, this, &MainWindow::deleteSelectedPeaks);
    connect(xpdViewer(), &XpdViewWidget::addDeletePointSignal, this, &MainWindow::addDeleteSelectedPoint);
    connect(xpdViewer(), &XpdViewWidget::fileDropped, this, &MainWindow::onActionFileDropped);
    ui->actionLegend->setChecked(xpdViewer()->pSettings().legendVisible);

    createDialogs();

    //currentDatabase = "/home/corrado/temp/cod/cod2205/cod2205";
    // currentDatabase = "/home/corrado/temp/cod/cod2509/cod2509";
    // if (!AppState::db().openDatabases(currentDatabase)) {
    //     qCritical() << "Error opening databases";
    // }

    readSettings();
    createRecentActions();
    enableActions(InitAction);

    mMainWindow = this;
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::updatePeakListTable()
{
    if (ui->peakListWidget->isVisible())
        ui->peakListWidget->updatePeakListTable();
}

void MainWindow::enableActions(EnabledActions action, bool state)
{
    //qInfo() << "Enable Actions: " << action << " State: " << state;
    switch (action) {
    // case InitAction:  //Init
    //     enableMenu(ui->menuExportData, false);
    //     ui->actionSave_Project->setEnabled(false);
    //     ui->actionSave_Project_As->setEnabled(false);
    //     enableMenu(ui->menuPattern, false);
    //     enableMenu(ui->menuView, false);
    //     enableMenu(ui->menuInfo, false);
    //     break;
    // case PatternAction:   //Only Pattern
    //     enableMenu(ui->menuExportData, true);
    //     ui->actionSave_Project->setEnabled(true);
    //     ui->actionSave_Project_As->setEnabled(true);
    //     enableMenu(ui->menuPattern, true);
    //     ui->actionIntervals->setEnabled(false);
    //     // ui->actionAdd_Background_Point->setEnabled(true);
    //     // ui->actionDelete_Background_Point->setEnabled(true);
    //     enableMenu(ui->menuView, true);
    //     ui->actionDirect_Methods->setEnabled(true);
    //     ui->actionExplore_Trials->setEnabled(false);
    //     ui->actionSimulated_Annealing->setEnabled(true);
    //     ui->menuSuperflip->setEnabled(true);
    //     enableMenu(ui->menuSuperflip, true);
    //     ui->actionRAMM_Procedure->setEnabled(false);
    //     ui->actionRecycle_in_Extra->setEnabled(is_extraction_active());
    //     enableMenu(ui->menuRefine, false);
    //     ui->actionRietveld->setEnabled(true);
    //     enableMenu(ui->menuInfo, true);
    //     ui->actionProfile->setEnabled(false);
    //     break;
    // case DialogOpenAction:
    //     enableMenu(ui->menuFile, false);
    //     enableMenu(ui->menuPattern, false);
    //     // ui->actionAdd_Background_Point->setEnabled(false);
    //     // ui->actionDelete_Background_Point->setEnabled(false);
    //     enableMenu(ui->menuSolve, false);
    //     enableMenu(ui->menuRefine, false);
    //     ui->crystalWidget->updateModify(3);
    //     ui->toolBarRun->setEnabled(false);
    //     break;
    // case RunAction:     //Run procedure
    //     ui->toolBarRun->setEnabled(state);
    //     ui->actionSkip->setEnabled(false);
    //     break;
    // case RunSkipAction:  //Run/Skip procedure
    //     ui->toolBarRun->setEnabled(state);
    //     ui->actionSkip->setEnabled(state);
    //     break;
    case SaveAction:
        saveEnabledActions();
        break;
    case RestoreAction:
        restoreEnabledActions();
        break;
    case PeaksAction: //unused!
        //ui->toolBarPattern
        //qInfo() << "Enable Pattern Menu";
        enableMenu(ui->menuPattern, true);
        //ui->actionIntervals->setEnabled(false);
        break;
    default:
        break;
    }
}

void MainWindow::enableMenu(QMenu *menu, bool enab)
{
    QList<QAction*> actions = menu->actions();
    for (int i = 0; i < actions.size(); ++i)
        actions.at(i)->setEnabled(enab);
    QList<QMenu*> menus = menu->findChildren<QMenu*>();
    for (int i = 0; i < menus.size(); ++i) {
        menus.at(i)->setEnabled(enab);
        enableMenu(menus.at(i), enab); //to disable actions on toolbar
    }
}

void MainWindow::enumerateEnabledActionsMenu(QMenu *menu)
{
    foreach (QAction *action, menu->actions()) {
        if (action->isSeparator()) {

        } else if (action->menu()) {
            stateActions[action] = action->isEnabled();
            if (action->isEnabled()) enumerateEnabledActionsMenu(action->menu());
        } else {
            stateActions[action] = action->isEnabled();
        }
    }
}

QString MainWindow::getPathDataFiles()
{
    return pathDataFiles;
}

void MainWindow::setPathDataFiles(const QString &newPathDataFiles)
{
    pathDataFiles = newPathDataFiles;
}

void MainWindow::saveEnabledActions()
{
    stateActions.clear();
    const QList<QAction *> actions = ui->menubar->actions();  //QMenuBar::actions();
    foreach (auto action, actions) {
        stateActions[action] = action->isEnabled();
        if (action->isEnabled()) enumerateEnabledActionsMenu(action->menu());
    }
}

void MainWindow::restoreEnabledActions()
{
    QMutableMapIterator<QAction *, bool> i(stateActions);
    while (i.hasNext()) {
        i.next();
        if(i.key()->menu()) {
            i.key()->menu()->setEnabled(i.value());
        } else {
            i.key()->setEnabled(i.value());
        }
    }
}

XpdViewWidget *MainWindow::xpdViewer() const
{
    return ui->xpdWidget;
}

void MainWindow::createDialogs()
{
    peakSearchDialog   = new PeakSearchDialog(this);
    backgroundDialog   = new BackgroundDialog(this);
    m_restraintsDialog = new RestraintsDialog(this);
    smoothingDialog    = new SmoothingDialog(this);

    connect(ui->resultsWidget, &DbResultsWidget::hasResultsChanged,
            m_restraintsDialog, &RestraintsDialog::setMergeEnabled);

    connect(m_restraintsDialog, &RestraintsDialog::loadCardsRequested,
            this, [this]() { onRestraintsSearch(false); });
    connect(m_restraintsDialog, &RestraintsDialog::loadAndMergeCardsRequested,
            this, [this]() { onRestraintsSearch(true); });
    connect(m_restraintsDialog, &RestraintsDialog::searchWithRestraintsRequested,
            this, &MainWindow::onRestraintsSearchMatch);

    connect(ui->resultsWidget, &DbResultsWidget::cardSelected,
            this, &MainWindow::onCardSelected);

    connect(ui->resultsWidget, &DbResultsWidget::cardDataSelected,
            this, [this](const CardType &card) {
        ui->peakCompareWidget->setSelectedCard(
            card, card.getId(), SearchOptionsDialog::savedDelta2theta());
    });

    connect(ui->resultsWidget, &DbResultsWidget::phaseAccepted,
            this, [this](const CardType &card) {
        ui->quantWidget->addPhase(card);
        ui->peakCompareWidget->addAcceptedPhase(card);
        ui->reportWidget->updateQuantitative(
            ui->quantWidget->phases(), ui->quantWidget->quantPercentages());
        ui->dockWidgetQuant->show();
        ui->dockWidgetQuant->raise();
        if (SearchOptionsDialog::savedResidualSearching())
            performResidualSearch(card);
    });
}

void MainWindow::actionsSetup()
{
    //File menu
    connect(ui->actionImportDiffractionPattern, &QAction::triggered, this, &MainWindow::onActionImportDiffractionPatternTriggered);
    connect(ui->actionExit, &QAction::triggered, qApp, &QApplication::quit);

    //Pattern menu
    connect(ui->actionBackground, &QAction::triggered, this, &MainWindow::onActionBackgroundTriggered);
    connect(ui->actionExport_Background, &QAction::triggered, this, &MainWindow::onActionBackgroundExportTriggered);
    connect(ui->actionSubtract_Background, &QAction::triggered, this, &MainWindow::onActionSubtractBackgroundTriggered);
    connect(ui->actionSmoothing, &QAction::triggered, this, &MainWindow::onActionSmoothingTriggered);
    connect(ui->actionPeak_Search, &QAction::triggered, this, &MainWindow::onActionPeakSearchTriggered);
    connect(ui->actionLoad_Peaks, &QAction::triggered, this, &MainWindow::onActionLoadPeaksTriggered);
    connect(ui->actionSave_Peaks, &QAction::triggered, this, &MainWindow::onActionSavePeaksTriggered);
    connect(ui->actionPeak_Search_Conditions, &QAction::triggered, this, &MainWindow::onActionPeakSearchConditionsTriggered);

    //View menu
    connect(ui->actionPlot_Style, &QAction::triggered, this, &MainWindow::onActionPlotStyleTriggered);
    connect(ui->actionReset_Zoom, &QAction::triggered, this, &MainWindow::onActionResetZoomTriggered);
    connect(ui->actionAutoscale, &QAction::triggered, this, &MainWindow::onActionAutoscaleTriggered);
    connect(ui->actionLegend, &QAction::triggered, this, [=](bool state){
        xpdViewer()->setLegendVisible(state);
    });

    //Search menu
    connect(ui->actionSearch_Match, &QAction::triggered, this, &MainWindow::onActionSearchMatchTriggered);
    connect(ui->actionSearch_Match_Options, &QAction::triggered, this, &MainWindow::onActionSearchMatchOptionsTriggered);
    connect(ui->actionRestraints, &QAction::triggered, this, &MainWindow::actionRestraintsTriggered);
    connect(ui->actionTestDatabase, &QAction::triggered, this, &MainWindow::onActionTestDatabaseTriggered);
    connect(ui->actionDatabaseInfo, &QAction::triggered, this, &MainWindow::onActionDatabaseInfoTriggered);
    //connect(ui->actionGetCard, &QAction::triggered, this, &MainWindow::onActionGetCardTriggered);
    connect(ui->actionLoad_Add, &QAction::triggered, this, &MainWindow::onActionLoadAddTriggered);
    connect(ui->actionManage_Databases, &QAction::triggered, this, &MainWindow::actionManageDatabasesTriggered);

    //Entry menu
    ui->actionAccept_Selected_Entries->setEnabled(false);
    ui->actionDelete->setEnabled(false);
    ui->actionChange_Color->setEnabled(false);

    auto updateEntryActions = [this]() {
        const bool on = ui->resultsWidget->hasResults() && ui->resultsWidget->hasSelection();
        ui->actionAccept_Selected_Entries->setEnabled(on);
        ui->actionDelete->setEnabled(on);
        ui->actionChange_Color->setEnabled(on);
    };
    connect(ui->resultsWidget, &DbResultsWidget::hasResultsChanged,
            this, [updateEntryActions](bool) { updateEntryActions(); });
    connect(ui->resultsWidget, &DbResultsWidget::entrySelectionChanged,
            this, [updateEntryActions](bool) { updateEntryActions(); });

    connect(ui->actionAccept_Selected_Entries, &QAction::triggered, this, [this]() {
        ui->resultsWidget->acceptSelectedCards();
    });
    connect(ui->actionChange_Color, &QAction::triggered, this, [this]() {
        ui->resultsWidget->changeSelectedCardColor();
    });
    connect(ui->resultsWidget, &DbResultsWidget::cardColorChanged,
            this, [this](const QString &id) {
        onCardSelected(id);
        if (ui->dockWidgetCompare->isVisible())
            ui->peakCompareWidget->refresh();
    });
    connect(ui->actionDelete, &QAction::triggered, this, [this]() {
        ui->resultsWidget->deleteSelectedCards();
        setStatusMessage(tr("%1 card(s)").arg(ui->resultsWidget->allCards().size()));
    });
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    QString titolo = "Exit " + qApp->applicationName();
    QString testo = QString("Do you really want \nstop the program %1?").arg(qApp->applicationName());
    if(QMessageBox::No == QMessageBox::question(this, titolo, testo,
                                                 QMessageBox::Yes | QMessageBox::No))
    {
        event->ignore();
        return;
    }

    writeSettings();

    //std::cout << "\nThanks for using " << qPrintable(qApp->applicationName()) << "\nBye!\n\n";

    AppState::db().closeDatabeses();

    QApplication::quit();

    //Force close of dialog windows in a loop, kill the fortran computation
#ifndef Q_OS_WIN
    exit(EXIT_SUCCESS);
#endif
}

void MainWindow::writeSettings()
{
    QSettings settings;
    settings.setValue(QUALX_GEOMETRY_KEY, saveGeometry());
    settings.setValue(QUALX_STATE_KEY, saveState());
    xpdViewer()->pSettings().write();
}

void MainWindow::readSettings()
{
    QSettings settings;
    const QByteArray geometry = settings.value(QUALX_GEOMETRY_KEY, QByteArray()).toByteArray();
    if (geometry.isEmpty()) {
        const QRect availableGeometry = QGuiApplication::primaryScreen()->availableGeometry();
        resize(availableGeometry.width() * 0.80, availableGeometry.height() * 0.80);
        move((availableGeometry.width() - width()) / 2,
             (availableGeometry.height() - height()) / 2);
        //resizeMyDocks();

    } else {
        restoreGeometry(geometry);
    }

    restoreState(settings.value(QUALX_STATE_KEY).toByteArray());
    //ui->splitter1->restoreState(settings.value(EXPO_SPLITSIZE_KEY1).toByteArray());
    //ui->splitter2->restoreState(settings.value(EXPO_SPLITSIZE_KEY2).toByteArray());
}

void MainWindow::setStatusMessage(const QString &message)
{
    statusLabel1->setText(message);
}

void MainWindow::clearStatusMessage()
{
    statusLabel1->clear();
}

QProgressBar *MainWindow::getStatusProgressBar() const
{
    return statusProgressBar;
}

//
//  File Menu
//

void MainWindow::openRecentFile()
{
    QAction *action = qobject_cast<QAction *>(sender());
    if (action) {
        QString fileIn = action->data().toString();
        if (!QFile::exists(fileIn)) {
            QMessageBox::critical(this,"Error",fileIn + " does not exist.");
            return;
        }

        //Extract the fileType from action text and settings
        int index = action->text().section(".",0,0).toInt()-1;
        QSettings settings;
        settings.beginReadArray(QUALX_RECENT_FILES_KEY);
        settings.setArrayIndex(index);
        QString fileType = settings.value("FileType").toString();
        settings.endArray();

        QMetaEnum metaEnum = QMetaEnum::fromType<RecentFileType>();
        int type = metaEnum.keyToValue(fileType.toStdString().c_str());
        int err;
        switch (type) {
        case RecentFileType::Input:
            break;

        case RecentFileType::Data:
        {
            fileIn = QDir::toNativeSeparators(fileIn);
            QString fileOut = fileutils::removeExtension(fileIn)+".out";
            fileutils::setFileForWritable(fileOut);
            fileutils::setCurrentDirFromFile(fileIn);
            open_diffraction_patt(fileIn.toLocal8Bit().constData(), fileIn.toLocal8Bit().size(),
                                  fileOut.toLocal8Bit().constData(), fileOut.toLocal8Bit().size(),
                                  0, &err);
            if (!err) {
                currentFile = fileIn;
//                outputFileName = fileOut;
                setWindowTitle(currentFile);
            }
            break;
        }
        case RecentFileType::Structure:
            break;

        case RecentFileType::Project:
            break;
        }
    }
}

void MainWindow::loadDiffractionPatterns(QStringList files)
{
    QSettings settings;
    if (!files.isEmpty()) {
        QFileInfo info(files.at(0));
        QString outFile = info.path() + QDir::separator() + info.baseName() + ".out";
        fileutils::setFileForWritable(outFile);
        outFile = QDir::toNativeSeparators(outFile);
        int nerr = 0;
        fileutils::setCurrentDirFromFile(files.at(0));
        for (int i = 0; i < files.size(); i++) {
            int err;
            QString filename = QDir::toNativeSeparators(files.at(i));

            open_diffraction_patt(filename.toLocal8Bit().constData(), filename.toLocal8Bit().size(),
                                  outFile.toLocal8Bit().constData(), outFile.toLocal8Bit().size(),
                                  i, &err);
            if (err) {
                nerr++;
            }
        }
        if (nerr < files.size()) {
            QString filename = QDir::toNativeSeparators(files.at(0));
            settings.setValue(DEFAULT_DIR_KEY,QFileInfo(filename).absolutePath());
//            outputFileName = outFile;
            setCurrentFile(filename,QVariant::fromValue(RecentFileType::Data).toString());
        }
    }
}

void MainWindow::onActionImportDiffractionPatternTriggered()
{
    QSettings settings;
    QString selectedFilter = "All Powder Diffraction Data (*.dat *.xy *.rtv *.gda *.xye *.xrdml *.pow)";
    QString exts = "All files (*.*);;"
                   "All Powder Diffraction Data (*.dat *.xy *.rtv *.gda *.xye *.xrdml *.pow);;"
                   "ASCII profile [start,step,end,intensities] (*.dat *.pow);;"
                   "XY profile [2theta and intensities in two colomuns] (*.xy *.pow);;"
                   "GSAS data (*.gda);;"
                   "CIF powder data (*.rtv *.cif);;"
                   "CCDC Mercury data (*.xye);;"
                   "Siemens data (*.uxd);;"
                   "Sietronics Sieray data (*.cpi);;"
                   "XDD data (*.xdd);;"
                   "DBWS data (*.dbw);;"
                   "XDA data (*.xda);;"
                   "Philips UDF data (*.udf);;"
                   "PANalytical XRDML data (*.xrdml)";

    QStringList files = QFileDialog::getOpenFileNames(this,
                                                      tr("Import Diffraction Pattern From"),
                                                      settings.value(DEFAULT_DIR_KEY).toString(),
                                                      exts, &selectedFilter);

    loadDiffractionPatterns(files);
}

void MainWindow::createRecentActions()
{
    QAction* recentFileAction;
    for(int i = 0; i < maxFileNr; i++) {
        recentFileAction = new QAction(this);
        recentFileAction->setVisible(false);
        connect(recentFileAction,SIGNAL(triggered()),this,SLOT(openRecentFile()));
        //ui->menuRecent->addAction(recentFileAction);
        ui->menuRecent->addAction(recentFileAction);
        recentFileActionList.append(recentFileAction);
    }

    updateRecentFileActions();
}

void MainWindow::updateRecentFileActions()
{
    QSettings settings;

    QVector<RecentFileInfo> recentFiles;
    int size = settings.beginReadArray(QUALX_RECENT_FILES_KEY);
    for (int i = 0; i < size; i++) {
        settings.setArrayIndex(i);
        RecentFileInfo info;
        info.fileName = settings.value("FileName").toString();
        info.fileType = settings.value("FileType").toString();
        recentFiles.append(info);
    }
    settings.endArray();

    int numRecentFiles = qMin(recentFiles.size(),static_cast<int>(maxFileNr));

    for(int i = 0; i < numRecentFiles; ++i) {
        recentFileActionList.at(i)->setText(QString::number(i+1)+". "+recentFiles.at(i).fileName);
        recentFileActionList.at(i)->setData(recentFiles.at(i).fileName);
        recentFileActionList.at(i)->setVisible(true);
    }

    for(int i = numRecentFiles; i < maxFileNr; i++) {
        recentFileActionList.at(i)->setVisible(false);
    }
}

void MainWindow::setRecentFiles(const QString &fullFileName, const QString &fileType)
{
    QSettings settings;
    QVector<RecentFileInfo> recentFiles;
    int size = settings.beginReadArray(QUALX_RECENT_FILES_KEY);
    for (int i = 0; i < size; i++) {
        settings.setArrayIndex(i);
        RecentFileInfo info;
        info.fileName = settings.value("FileName").toString();
        info.fileType = settings.value("FileType").toString();
        if (info.fileName != fullFileName) {
            recentFiles.append(info);
        }
    }
    settings.endArray();
    recentFiles.prepend({fullFileName,fileType});
    while (recentFiles.size() > maxFileNr)
        recentFiles.removeLast();

    settings.beginWriteArray(QUALX_RECENT_FILES_KEY);
    for (int i = 0; i < recentFiles.size(); i++) {
        settings.setArrayIndex(i);
        settings.setValue("FileName", recentFiles.at(i).fileName);
        settings.setValue("FileType", recentFiles.at(i).fileType);
    }
    settings.endArray();
    //

    // if you have several istances of Mainwindow present,
    // call updateRecentFileActions on all top-level windows and
    // all recent files menu will be equal
    foreach (QWidget *widget, QApplication::topLevelWidgets()) {
        QMainWindow *mainwindow = qobject_cast<QMainWindow *>(widget);
        if (mainwindow)
            updateRecentFileActions();
    }
}

void MainWindow::setCurrentFile(const QString &fullFileName, const QString &fileType)
{
    if (!fullFileName.isEmpty()) {
        currentFile = fullFileName;
        setWindowTitle(currentFile);

        setRecentFiles(fullFileName, fileType);
    }
}

void MainWindow::onActionFileDropped(const QStringList &fileList)
{
    QStringList xpdExtensions = {"dat","xy","rtv","gda","xye","xrdml","pow"};

    for (int i = 0; i < fileList.size(); i++) {
        QString fileIn = fileList.at(i);
        QFileInfo fi(fileIn);
        QString ext = fi.suffix().toLower();
        if (xpdExtensions.contains(ext)) {
            loadDiffractionPatterns(fileList);
            break;
        } else if (ext == "expo") {
//FIX PROJECT LATER            loadProject(fileIn);
            break;
        } else {
            QMessageBox::critical(this,"Error",fileIn + " is not a valid file type.");
        }
    }
}

//
//  Pattern Menu
//

void MainWindow::onActionBackgroundTriggered()
{
    backgroundDialog->setBackground();
    backgroundDialog->show();
}

void MainWindow::onActionBackgroundExportTriggered()
{
    QSettings settings;
    QString selectedFilter = "xy";
    int code = 0;
    QString exts = "XY files (*.xy);; CIF files (*.cif)";
    QString fileName = SaveDialog::run(this,
                                       tr("Export Background As"),
                                       settings.value(DEFAULT_DIR_KEY).toString(),
                                       QFileInfo(currentFile).baseName(),
                                       exts,
                                       "xy",
                                       selectedFilter,
                                       &code);
    if (!fileName.isEmpty()) {
        settings.setValue(DEFAULT_DIR_KEY,QFileInfo(fileName).absolutePath());
        if (selectedFilter.contains("cif", Qt::CaseInsensitive)) {
            // Export Background as CIF
            esportanew(16, fileName.toLocal8Bit().constData(), fileName.toLocal8Bit().size());
        } else {
            // Export Background as XY
            esportanew(19, fileName.toLocal8Bit().constData(), fileName.toLocal8Bit().size());
        }
    }
}

void MainWindow::onActionSubtractBackgroundTriggered()
{
    apply_background_subtraction();
}

void MainWindow::onActionSmoothingTriggered()
{
    smoothingDialog->setSmoothing();
    smoothingDialog->show();
}

void MainWindow::deleteSelectedPeaks(const QVector<int> &selected)
{
    int *peaks = new int[selected.size()];
    for (int i = 0; i < selected.size(); i++) {
        peaks[i] = selected.at(i);
    }
    delete_peaksC(peaks, selected.size());
    delete [] peaks;
}

void MainWindow::addDeleteSelectedPoint(int action, double xp, double yp, int &ier)
{
    process_action_points(action, xp, yp, &ier);
}

void MainWindow::onActionPeakSearchTriggered()
{
    run_peaksearchwin();
}

void MainWindow::onActionPeakSearchConditionsTriggered()
{
    peakSearchDialog->setOptions();
    peakSearchDialog->show();
}

void MainWindow::onActionLoadPeaksTriggered()
{
    QSettings settings;
    QString selectedFilter = "2 theta values (*.dat *.txt *.pea)";
    QString exts = "2 theta values (*.dat *.txt *.pea);;"
                   "d values (*.dat *.txt *.pea)";
    QString fileName = QFileDialog::getOpenFileName(this,
                                                    tr("Load File"), settings.value(DEFAULT_DIR_KEY).toString(),
                                                    exts, &selectedFilter);

    if (!fileName.isEmpty()) {
        int tipo = (selectedFilter.startsWith("2 theta")) ? 1 : 2;
        int ier;
        LoadPeaksC(fileName.toStdString().c_str(), fileName.length(), tipo, &ier);
    }
}

void MainWindow::onActionSavePeaksTriggered()
{
    QSettings settings;
    QString selectedFilter = "2 theta values (*.dat *.txt *.pea)";
    QString exts = "2 theta values (*.dat *.txt *.pea);;"
                   "d values (*.dat *.txt *.pea);;"
                   "Input File for DICVOL (*.dat);;"
                   "Input File for McMaille (*.dat);;"
                   "Peak list (*.txt)";
    QString fileName = SaveDialog::run(this,tr("Save Peaks As"),
                                       settings.value(DEFAULT_DIR_KEY).toString(),
                                       QFileInfo(currentFile).baseName(),exts,
                                       "txt",selectedFilter);
    if (!fileName.isEmpty()) {
        QStringList list = exts.split(";;");
        int tipo = list.lastIndexOf(selectedFilter) + 1;
        SavePeaksC(fileName.toStdString().c_str(), fileName.length(),tipo);
    }
}

//
//  View Menu
//

void MainWindow::onActionPlotStyleTriggered()
{
    // plotStyleDialog->setOptions(xpdViewer());
    // plotStyleDialog->show();
}

void MainWindow::onActionResetZoomTriggered()
{
    xpdViewer()->xAxis->rescale();
    xpdViewer()->redrawPlot(true);
}

void MainWindow::onActionAutoscaleTriggered()
{
    xpdViewer()->applyAutoScale();
}

void MainWindow::createActionGroup()
{
    QActionGroup *zoomGroup = new QActionGroup(this);
    zoomGroup->addAction(ui->actionSelection_Mode);
    zoomGroup->addAction(ui->actionHorizontal_Zoom);
    zoomGroup->addAction(ui->actionRectangle_Zoom);
    zoomGroup->addAction(ui->actionPan);
    zoomGroup->addAction(ui->actionAdd_Background_Point);
    zoomGroup->addAction(ui->actionDelete_Background_Point);
    zoomGroup->addAction(ui->actionAdd_Peak);
    zoomGroup->addAction(ui->actionDelete_Peak);
    connect(zoomGroup,&QActionGroup::triggered, this, &MainWindow::zoomGroupTriggered);
}

void MainWindow::zoomGroupTriggered(QAction *action)
{
    if (action == ui->actionSelection_Mode) {
        if (mAction != NoZoom) setAction(NoZoom);
    } else if (action == ui->actionHorizontal_Zoom) {
        if (mAction != HorizontalZoom) setAction(HorizontalZoom);
    } else if (action == ui->actionRectangle_Zoom) {
        if (mAction != RectangleZoom) setAction(RectangleZoom);
    } else if (action == ui->actionPan) {
        if (mAction != Pan) setAction(Pan);
    } else if (action == ui->actionAdd_Background_Point) {
        if (mAction != AddBackgroundPoint) setAction(AddBackgroundPoint, false);
    } else if (action == ui->actionDelete_Background_Point) {
        if (mAction != DeleteBackgroundPoint) setAction(DeleteBackgroundPoint, false);
    } else if (action == ui->actionAdd_Peak) {
        if (mAction != AddPeak) setAction(AddPeak, false);
    } else if (action == ui->actionDelete_Peak) {
        if (mAction != DeletePeak) setAction(DeletePeak, false);
    }
}

void MainWindow::checkAction(MouseAction action)
{
    if (action == mAction) return;

    switch (action) {
    case NoZoom:
        ui->actionSelection_Mode->setChecked(true);
        break;
    case RectangleZoom:
        ui->actionRectangle_Zoom->setChecked(true);
        break;
    case HorizontalZoom:
        ui->actionHorizontal_Zoom->setChecked(true);
        break;
    case Pan:
        ui->actionPan->setChecked(true);
        break;
    default:
        break;
    }
    setAction(action, false);
}

void MainWindow::saveAction()
{
    savedAction = mAction;
}

void MainWindow::restoreAction()
{
    checkAction(savedAction);
}

void MainWindow::setZoomAction()
{
    if (mAction != savedZoomAction)
        checkAction(savedZoomAction);
}

void MainWindow::readAction()
{
    QSettings settings;
    if (settings.contains(XPDVIEW_ACTION_KEY)) {
        QString sAction = settings.value(XPDVIEW_ACTION_KEY).toString();
        QMetaEnum metaEnum = QMetaEnum::fromType<MouseAction>();
        int action = metaEnum.keyToValue(sAction.toStdString().c_str());
        checkAction(static_cast<MouseAction>(action));
    } else {
        checkAction(HorizontalZoom);
    }
}

void MainWindow::setAction(const MouseAction &action, bool writeConfig)
{
    XpdViewWidget::MouseAction xpdAction = static_cast<XpdViewWidget::MouseAction>(action);
    xpdViewer()->setAction(xpdAction);

    if (writeConfig) {
        QSettings settings;
        QMetaEnum metaEnum = QMetaEnum::fromType<MouseAction>();
        const char *key = metaEnum.valueToKey(action);
        settings.setValue(XPDVIEW_ACTION_KEY,key);
    }
}

//
//  Search Menu
//

// Reads experimental peaks from Fortran and stores them in AppState::peaks().
// Returns the number of peaks (0 if none).
static int loadExperimentalPeaks()
{
    const int n = peak_number();
    if (n <= 0) {
        AppState::peaks().clear();
        return 0;
    }
    float *dval      = new float[n];
    float *deltadval = new float[n];
    float *tthval    = new float[n];
    float *intval    = new float[n];
    float *fwhmval   = new float[n];
    double wave;
    get_d_delta_values(dval, deltadval, tthval, intval, fwhmval, &wave);

    ExperimentalPeaks &ep = AppState::peaks();
    ep.d.resize(n); ep.deltaD.resize(n);
    ep.tth.resize(n); ep.intensity.resize(n); ep.intensityOrig.resize(n); ep.fwhm.resize(n);
    ep.wave  = wave;
    ep.valid = true;
    for (int i = 0; i < n; ++i) {
        ep.d[i]             = dval[i];
        ep.deltaD[i]        = deltadval[i];
        ep.tth[i]           = tthval[i];
        ep.intensity[i]     = intval[i];
        ep.intensityOrig[i] = intval[i]; // original value, never subtracted
        ep.fwhm[i]          = fwhmval[i];
    }
    delete[] dval; delete[] deltadval; delete[] tthval; delete[] intval; delete[] fwhmval;
    return n;
}

void MainWindow::onActionSearchMatchTriggered()
{
    int npeaks = peak_number();
    if (npeaks == 0) {
        run_peaksearchwin();
    }
    npeaks = peak_number();
    if (npeaks == 0) {
        QMessageBox::warning(this, tr("No Peaks Found"), tr("Please run Peak Search first."));
        return;
    }

    loadExperimentalPeaks();
    const ExperimentalPeaks &ep0 = AppState::peaks();

    DbQueryBuilder builder;
    builder.setPrintEnabled(true);
    builder.setDValues(ep0.d, ep0.deltaD);
    builder.setWave(ep0.wave);
    builder.setCalcFom(true);
    builder.setMinFom(SearchOptionsDialog::savedMinFom());
    builder.setWeight2thetaD(SearchOptionsDialog::savedWeight2thetaD());
    builder.setWeightIntensity(SearchOptionsDialog::savedWeightIntensity());
    builder.setWeightPhases(SearchOptionsDialog::savedWeightPhases());
    builder.setDelta2theta(SearchOptionsDialog::savedDelta2theta());
    applyDialogRestraints(builder);
    builder.buildQuery();

    setStatusMessage(m_restraintsDialog->hasRestraints()
                         ? tr("Searching database with restraints...")
                         : tr("Searching database..."));
    statusProgressBar->setRange(0, 100);
    statusProgressBar->setValue(0);
    statusProgressBar->show();
    QApplication::setOverrideCursor(Qt::WaitCursor);
    QApplication::processEvents();

    auto progress = [this](int current, int total) {
        statusProgressBar->setValue(total > 0 ? (100 * current) / total : 0);
        QApplication::processEvents();
    };

    QVector<CardType> acceptedCards;
    if (SearchOptionsDialog::savedCheckStrongest())
        AppState::db().makeQueryStrongest(builder, acceptedCards, progress);
    else
        AppState::db().makeQueryWithoutStrongest(builder, acceptedCards, progress);

    QApplication::restoreOverrideCursor();
    statusProgressBar->hide();
    clearStatusMessage();

    // Keep only the top maxEntries cards by descending FOM
    const int maxEntries = SearchOptionsDialog::savedMaxEntries();
    if (acceptedCards.size() > maxEntries) {
        std::sort(acceptedCards.begin(), acceptedCards.end(),
                  [](const CardType &a, const CardType &b) {
                      return a.getFom() > b.getFom();
                  });
        acceptedCards.resize(maxEntries);
    }

    setStatusMessage(tr("Found %1 card(s)").arg(acceptedCards.size()));
    ui->resultsWidget->setResults(acceptedCards);
    ui->peakCompareWidget->clearAcceptedPhases();
    ui->peakCompareWidget->clearCard();
    ui->peakCompareWidget->setExperimentalPeaks(AppState::peaks());
    ui->reportWidget->clearQuantitative();
    ui->reportWidget->updateReport(AppState::peaks(), ui->resultsWidget->allCards());
    if (ui->resultsWidget->hasResults()) {
        ui->resultsWidget->selectFirstCard();
        ui->dockWidgetCompare->show();
        ui->dockWidgetCompare->raise();
    }
}

void MainWindow::onActionSearchMatchOptionsTriggered()
{
    SearchOptionsDialog dlg(this);
    dlg.exec();
}

void MainWindow::executeSearch(DbQueryBuilder &builder, bool merge)
{
    builder.buildQuery();

    setStatusMessage(tr("Querying database..."));
    statusProgressBar->setRange(0, 100);
    statusProgressBar->setValue(0);
    statusProgressBar->show();
    QApplication::setOverrideCursor(Qt::WaitCursor);
    QApplication::processEvents();

    auto progress = [this](int current, int total) {
        statusProgressBar->setValue(total > 0 ? (100 * current) / total : 0);
        QApplication::processEvents();
    };

    QVector<CardType> cards = AppState::db().makeQuery(builder, progress);

    QApplication::restoreOverrideCursor();
    statusProgressBar->hide();
    setStatusMessage(tr("Found %1 card(s)").arg(cards.size()));

    if (merge)
        ui->resultsWidget->mergeResults(cards);
    else
        ui->resultsWidget->setResults(cards);

    // Refresh the peak compare widget with the current experimental peaks
    ui->peakCompareWidget->clearAcceptedPhases();
    ui->peakCompareWidget->clearCard();
    ui->peakCompareWidget->setExperimentalPeaks(AppState::peaks());
    ui->reportWidget->clearQuantitative();
    ui->reportWidget->updateReport(AppState::peaks(), ui->resultsWidget->allCards());
    if (ui->resultsWidget->hasResults()) {
        ui->resultsWidget->selectFirstCard();
        ui->dockWidgetCompare->show();
        ui->dockWidgetCompare->raise();
    }
}

void MainWindow::actionRestraintsTriggered()
{
    m_restraintsDialog->setMergeEnabled(ui->resultsWidget->hasResults());
    m_restraintsDialog->setSearchEnabled(peak_number() > 0);
    m_restraintsDialog->show();
    m_restraintsDialog->raise();
    m_restraintsDialog->activateWindow();
}

void MainWindow::applyDialogRestraints(DbQueryBuilder &builder)
{
    builder.enableDeleted(SearchOptionsDialog::savedCheckDeleted());

    const RestraintsDialog *dlg = m_restraintsDialog;

    const QString formula = dlg->compositionFormula();
    if (!formula.isEmpty()) {
        builder.setElements(formula);
        if (dlg->isExactComposition())
            builder.setBOperator(DbQueryBuilder::ONLY_OP);
        else if (dlg->isContainsAny())
            builder.setBOperator(DbQueryBuilder::JUST_OP);
    }

    const QString chemName = dlg->chemicalName();
    if (!chemName.isEmpty())
        builder.setNames(chemName);

    const QStringList subfiles = dlg->subfilesCodes();
    if (!subfiles.isEmpty())
        builder.setSubfiles(subfiles);

    const QStringList csys = dlg->crystalSystemStrings();
    if (!csys.isEmpty())
        builder.setCsysString(csys);

    const QStringList spg = dlg->spaceGroupStrings();
    if (!spg.isEmpty())
        builder.setSpgString(spg);

    const auto cell = dlg->cellQuery();
    for (int i = 0; i < 6; ++i) {
        if (cell.values[i] > 0.0) {
            const double tol = (i < 3) ? cell.lenTol : cell.angTol;
            builder.setCellParameter(i, cell.values[i] - tol, cell.values[i] + tol);
        }
    }

    const QStringList ids = dlg->entryIds();
    if (!ids.isEmpty())
        builder.setIdEntry(ids);

    const QStringList colors = dlg->colorStrings();
    if (!colors.isEmpty())
        builder.setColorString(colors);

    const double densTol = dlg->densityTolerance();
    const double densCalc = dlg->densityCalc();
    if (densCalc > 0.0)
        builder.setDensityCalc(densCalc - densTol, densCalc + densTol);
    const double densMeas = dlg->densityMeas();
    if (densMeas > 0.0)
        builder.setDensityMeas(densMeas - densTol, densMeas + densTol);
}

void MainWindow::onRestraintsSearch(bool merge)
{
    if (!m_restraintsDialog->hasRestraints()) return;

    DbQueryBuilder builder;
    builder.setPrintEnabled(true);
    applyDialogRestraints(builder);

    if (loadExperimentalPeaks() > 0) {
        const ExperimentalPeaks &ep1 = AppState::peaks();
        builder.setDValues(ep1.d, ep1.deltaD);
        builder.setWave(ep1.wave);
        builder.setCalcFom(true);
        builder.setMinFom(-1.0);
    } else {
        builder.setCalcFom(false);
    }

    executeSearch(builder, merge);
}

void MainWindow::onRestraintsSearchMatch()
{
    if (!m_restraintsDialog->hasRestraints()) return;

    const int npeaks = peak_number();
    if (npeaks == 0) return;

    loadExperimentalPeaks();
    const ExperimentalPeaks &ep2 = AppState::peaks();

    DbQueryBuilder builder;
    builder.setPrintEnabled(true);
    builder.setDValues(ep2.d, ep2.deltaD);
    builder.setWave(ep2.wave);
    builder.setCalcFom(true);
    builder.setMinFom(SearchOptionsDialog::savedMinFom());
    applyDialogRestraints(builder);
    builder.buildQuery();

    setStatusMessage(tr("Searching database with restraints..."));
    statusProgressBar->setRange(0, 100);
    statusProgressBar->setValue(0);
    statusProgressBar->show();
    QApplication::setOverrideCursor(Qt::WaitCursor);
    QApplication::processEvents();

    auto progress = [this](int current, int total) {
        statusProgressBar->setValue(total > 0 ? (100 * current) / total : 0);
        QApplication::processEvents();
    };

    QVector<CardType> acceptedCards;
    AppState::db().makeQueryStrongest(builder, acceptedCards, progress);

    QApplication::restoreOverrideCursor();
    statusProgressBar->hide();
    clearStatusMessage();

    const int maxEntries = SearchOptionsDialog::savedMaxEntries();
    if (acceptedCards.size() > maxEntries) {
        std::sort(acceptedCards.begin(), acceptedCards.end(),
                  [](const CardType &a, const CardType &b) {
                      return a.getFom() > b.getFom();
                  });
        acceptedCards.resize(maxEntries);
    }

    ui->resultsWidget->setResults(acceptedCards);
    ui->peakCompareWidget->clearAcceptedPhases();
    ui->peakCompareWidget->clearCard();
    ui->peakCompareWidget->setExperimentalPeaks(AppState::peaks());
    ui->reportWidget->clearQuantitative();
    ui->reportWidget->updateReport(AppState::peaks(), ui->resultsWidget->allCards());
    if (ui->resultsWidget->hasResults()) {
        ui->resultsWidget->selectFirstCard();
        ui->dockWidgetCompare->show();
        ui->dockWidgetCompare->raise();
    }
}

void MainWindow::runSearch(const SearchOptions &opts)
{
    DbQueryBuilder builder;
    builder.setPrintEnabled(true);
    builder.setCalcFom(false);

    if (!opts.composition.isEmpty()) {
        builder.setElements(opts.composition);
        if (opts.exactComposition)
            builder.setBOperator(DbQueryBuilder::ONLY_OP);
        else if (opts.containsAny)
            builder.setBOperator(DbQueryBuilder::JUST_OP);
        // else: AND_OR_OP is the default
    }

    executeSearch(builder);
}

void MainWindow::onActionDatabaseInfoTriggered()
{
    int ncard;
    QString type;
    AppState::db().getInfo(ncard, type);
    qInfo() << "Ncard: " << ncard << "Type: " << type;
}

void MainWindow::onActionTestDatabaseTriggered()
{
    DbQueryBuilder builder;

    builder.setPrintEnabled(true);

    testSelection(builder, 22);

    builder.buildQuery();
    QVector<CardType> cards = AppState::db().makeQuery(builder);
    qInfo() << "Number of cards found: " << cards.size();
    ui->resultsWidget->setResults(cards);
}

void MainWindow::actionManageDatabasesTriggered()
{
    ManageDatabasesDialog dlg(this);
    dlg.setDatabases(AppState::databases());
    if (dlg.exec() == QDialog::Accepted)
        AppState::setDatabases(dlg.databases());
}

void MainWindow::onCardSelected(const QString &id)
{
    const CardInfo info = AppState::db().queryCard(id);
    if (!info.valid) return;

    auto row = [](const QString &label, const QString &value) -> QString {
        if (value.isEmpty() || value == "0" ) return QString();
        return QString("<tr><td><b>%1</b></td><td>%2</td></tr>").arg(label, value);
    };
    auto rowD = [](const QString &label, double value, int decimals = 4) -> QString {
        if (value == 0.0) return QString();
        return QString("<tr><td><b>%1</b></td><td>%2</td></tr>").arg(label).arg(value, 0, 'f', decimals);
    };

    QString cellParams;
    if (info.a != 0.0)
        cellParams = QString("a=%1 b=%2 c=%3 &alpha;=%4 &beta;=%5 &gamma;=%6")
                         .arg(info.a, 0, 'f', 4).arg(info.b, 0, 'f', 4).arg(info.c, 0, 'f', 4)
                         .arg(info.alpha, 0, 'f', 3).arg(info.beta, 0, 'f', 3).arg(info.gamma, 0, 'f', 3);

    QString ref;
    if (!info.authors.isEmpty())
        ref = info.authors;
    if (!info.journal.isEmpty()) {
        if (!ref.isEmpty()) ref += "; ";
        ref += info.journal;
        if (!info.journalVolume.isEmpty()) ref += " <b>" + info.journalVolume + "</b>";
        if (info.journalYear > 0)   ref += QString(" (%1)").arg(info.journalYear);
        if (!info.pageStart.isEmpty()) ref += " " + info.pageStart;
        if (!info.pageEnd.isEmpty())   ref += "-" + info.pageEnd;
    }

    QString html = "<html><body style='font-family:sans-serif;font-size:9pt'>";
    const QString idColor = cardColor(info.id).name();
    html += QString("<h3 style='margin:4px 0;color:%1'>%2</h3>").arg(idColor, info.id);
    html += "<table cellspacing='2' cellpadding='2'>";
    html += row("Name",             info.name);
    html += row("Mineral Name",     info.mineralName);
    html += row("Formula",          info.chemicalFormula);
    html += row("Space Group",      info.spaceGroup);
    html += row("Quality",          info.quality);
    html += row("RIR",              info.rir);
    if (!cellParams.isEmpty())
        html += "<tr><td><b>Cell</b></td><td>" + cellParams + "</td></tr>";
    html += rowD("Volume (&Aring;&sup3;)", info.volume, 2);
    html += rowD("Z",               static_cast<double>(info.z), 0);
    html += rowD("Density",         info.density, 3);
    html += rowD("Calc. Density",   info.crystalDensity, 3);
    html += rowD("&mu;(CuK&alpha;)",info.muCuKa, 3);
    html += row("Color",            info.color);
    html += row("Type",             info.type);
    if (!ref.isEmpty())
        html += "<tr><td><b>Reference</b></td><td>" + ref + "</td></tr>";
    html += "</table>";

    // --- Reflections table ---
    const QVector<double> &dv = info.dvalues;
    if (!dv.isEmpty()) {
        const QVector<double> &pw = xpdViewer()->plotWave;
        const double wave = pw.isEmpty() ? 1.54056 : pw.first();

        const QVector<double> tth = xpdutils::tthvalue_safe(dv, wave);

        const bool hasI   = (info.intensities.size() == dv.size());
        const bool hasHKL = (info.h.size() == dv.size() &&
                             info.k.size() == dv.size() &&
                             info.l.size() == dv.size());

        html += QString("<h4 style='margin:6px 0 2px 0'>Reflections "
                        "(&lambda; = %1 &Aring;)</h4>").arg(wave, 0, 'f', 5);
        html += "<table border='1' cellspacing='0' cellpadding='3' "
                "style='border-collapse:collapse;font-size:8pt'>";
        html += "<tr style='background:#ddd'>"
                "<th>#</th><th>2&theta;</th><th>d (&Aring;)</th><th>I (%)</th>";
        if (hasHKL) html += "<th>h</th><th>k</th><th>l</th>";
        html += "</tr>";

        for (int i = 0; i < dv.size(); ++i) {
            html += "<tr>";
            html += "<td align='right'>"  + QString::number(i + 1)                     + "</td>";
            html += "<td align='right'>"  + (i < tth.size()
                        ? QString::number(tth[i], 'f', 3) : QStringLiteral("-"))       + "</td>";
            html += "<td align='right'>"  + QString::number(dv[i], 'f', 4)             + "</td>";
            html += "<td align='right'>"  + (hasI
                        ? QString::number(info.intensities[i], 'f', 1) : QStringLiteral("-")) + "</td>";
            if (hasHKL) {
                html += "<td align='center'>" + QString::number(info.h[i]) + "</td>";
                html += "<td align='center'>" + QString::number(info.k[i]) + "</td>";
                html += "<td align='center'>" + QString::number(info.l[i]) + "</td>";
            }
            html += "</tr>";
        }
        html += "</table>";
    }

    html += "</body></html>";

    ui->cardBrowser->setHtml(html);
    ui->dockWidgetCard->setWindowTitle(id);
}

void MainWindow::performResidualSearch(const CardType &acceptedCard)
{
    ExperimentalPeaks &ep = AppState::peaks();
    if (!ep.valid || ep.tth.isEmpty()) return;

    const double delta   = SearchOptionsDialog::savedDelta2theta();
    const double wIntens = SearchOptionsDialog::savedWeightIntensity();

    // ── Step 1: associate experimental peaks with the accepted card ──
    // Scale the accepted card intensities to represent the actual pattern contribution.
    const QVector<double> &rawI  = acceptedCard.getIntensity();
    const double           cscale = acceptedCard.getScale();
    QVector<double> scaledI(rawI.size());
    for (int j = 0; j < rawI.size(); ++j)
        scaledI[j] = rawI[j] * cscale;

    const QVector<PeakAssociation> assoc = associatePeaks(
        ep.tth, ep.intensity, acceptedCard.getTth(), scaledI, delta);

    // ── Step 2: subtract the accepted card's contribution from experimental intensities ──
    // ranI defines the noise threshold: peaks whose residual is within ranI of the
    // original intensity are treated as fully explained and zeroed.
    // ranI defines the noise threshold: peaks whose residual is within ranI of the
    // ORIGINAL intensity (ep.intensityOrig, set at load time) are treated as fully explained.
    const double ranI = 0.2 * (1.0 - wIntens);

    for (int i = 0; i < ep.intensity.size(); ++i) {
        if (assoc[i].dbPeakIndex < 0) continue;
        ep.intensity[i] -= scaledI[assoc[i].dbPeakIndex];
        if (ep.intensity[i] < 0.0) ep.intensity[i] = 0.0;
        // Zero out residual peaks within the user-defined noise range
        if (ep.intensityOrig[i] > 0.0 && ep.intensity[i] / ep.intensityOrig[i] <= ranI)
            ep.intensity[i] = 0.0;
    }

    // ── Step 3: remove experimental peaks with intensity <= 0 ──
    // All parallel arrays (including intensityOrig and fwhm) are filtered in sync
    // so that index i always refers to the same peak across all arrays.
    QVector<double> newTth, newI, newIOrig, newD, newDD, newFwhm;
    for (int i = 0; i < ep.intensity.size(); ++i) {
        if (ep.intensity[i] > 0.0) {
            newTth.append(ep.tth[i]);
            newI.append(ep.intensity[i]);
            newIOrig.append(ep.intensityOrig[i]);
            newD.append(ep.d[i]);
            newDD.append(ep.deltaD[i]);
            newFwhm.append(ep.fwhm[i]);
        }
    }
    ep.tth = newTth; ep.intensity = newI; ep.intensityOrig = newIOrig;
    ep.d = newD; ep.deltaD = newDD; ep.fwhm = newFwhm;

    // ── Step 4: recompute FOMs for all remaining cards ──
    QVector<CardType> cards = ui->resultsWidget->allCards();
    const int expSize = ep.tth.size();

    for (CardType &card : cards) {
        if (expSize == 0) {
            // No experimental peaks remain: FOM is meaningless
            card.setFom(0.0);
            continue;
        }
        const int sz = card.getTth().size();
        if (sz == 0) { card.setFom(0.0); continue; }

        double fom = 0.0, fompeakpos = 0.0, fomintensity = 0.0, cardscale = 0.0;
        computeFOM(card.getTth().data(), card.getIntensity().data(), sz, &fom,
                   SearchOptionsDialog::savedWeight2thetaD(), wIntens,
                   SearchOptionsDialog::savedWeightPhases(), delta,
                   &fompeakpos, &fomintensity, &cardscale,
                   ep.tth.data(), ep.intensity.data(), expSize);
        card.setFom(fom);
        card.setFomPeakPos(fompeakpos);
        card.setFomIntensity(fomintensity);
        // Note: card scale is intentionally not updated here
    }

    // ── Step 5: discard cards whose FOM is <= 0 ──
    cards.erase(std::remove_if(cards.begin(), cards.end(),
                               [](const CardType &c){ return c.getFom() <= 0.0; }),
                cards.end());

    // ── Step 6: update the results widget and statusbar ──
    std::sort(cards.begin(), cards.end(),
              [](const CardType &a, const CardType &b){ return a.getFom() > b.getFom(); });
    ui->resultsWidget->setResults(cards);
    setStatusMessage(tr("%1 card(s) after residual search").arg(cards.size()));
}

void MainWindow::onActionLoadAddTriggered()
{
    bool ok = false;
    const QString id = QInputDialog::getText(this, tr("Load/Add Card by ID"),
                                             tr("Card ID:"), QLineEdit::Normal,
                                             QString(), &ok).trimmed();
    if (!ok || id.isEmpty()) return;

    const CardInfo info = AppState::db().queryCard(id);
    if (!info.valid) {
        QMessageBox::warning(this, tr("Card Not Found"),
                             tr("No card with ID \"%1\" found in the database.").arg(id));
        return;
    }

    // Build CardType from CardInfo
    const QVector<double> &pw = xpdViewer()->plotWave;
    const double wave = pw.isEmpty() ? 1.54056 : pw.first();

    CardType card;
    card.setId(info.id);
    card.setChemicalName(info.name);
    card.setMineralName(info.mineralName);
    card.setChemicalFormula(info.chemicalFormula);
    card.setQuality(info.quality);
    card.setRIR(info.rir);
    card.setSpaceGroup(info.spaceGroup);
    card.setD(info.dvalues, wave);
    card.setIntensity(info.intensities);

    // Compute FOM if experimental peaks are available
    ExperimentalPeaks &ep = AppState::peaks();
    if (ep.valid && !ep.tth.isEmpty() && !card.getTth().isEmpty()) {
        double fom = 0.0, fompeakpos = 0.0, fomintensity = 0.0, cardscale = 0.0;
        computeFOM(card.getTth().data(), card.getIntensity().data(), card.getTth().size(),
                   &fom,
                   SearchOptionsDialog::savedWeight2thetaD(),
                   SearchOptionsDialog::savedWeightIntensity(),
                   SearchOptionsDialog::savedWeightPhases(),
                   SearchOptionsDialog::savedDelta2theta(),
                   &fompeakpos, &fomintensity, &cardscale,
                   ep.tth.data(), ep.intensity.data(), ep.tth.size());
        card.setFom(fom);
        card.setFomPeakPos(fompeakpos);
        card.setFomIntensity(fomintensity);
        card.setScale(cardscale);
    }

    ui->resultsWidget->addCard(card);
    setStatusMessage(tr("%1 card(s)").arg(ui->resultsWidget->allCards().size()));

    // Force the card info dock visible
    ui->dockWidgetCard->show();
    ui->dockWidgetCard->raise();
}

void MainWindow::testSelection(DbQueryBuilder &builder, int testCase)
{
    switch (testCase) {
    case 1:
        builder.setNames("nickel");
        builder.enableDeleted(false);
        break;
    case 2:
        builder.setNames("nickel");
        builder.setSubfiles({"I"});
        break;
    case 3:
        builder.setNames("silicon oxide");
        builder.setSubfiles({"O"});
        break;
    case 4:
        builder.setNames("carbon");
        builder.setSubfiles({"I"});
        break;
    case 5:
        builder.setNames("copper");
        break;
    case 6:
        builder.setElements("Al");
        builder.setNames("copper");
        break;
    case 7:
        builder.setElements("Al and P and O");
        break;
    case 8:
        builder.setElements("Al or C");
        break;
    case 9:
        builder.setElements("Al or P and C");
        break;
    case 10:
        builder.setElements("Al and Si and P or C or O");
        break;
    case 11:
        builder.setBOperator(DbQueryBuilder::ONLY_OP);
        builder.setElements("Al");
        break;
    case 12:
        builder.setBOperator(DbQueryBuilder::ONLY_OP);
        builder.setElements("Al and O");
        break;
    case 13:
        builder.setBOperator(DbQueryBuilder::ONLY_OP);
        builder.setElements("Al and O and P");
        break;
    case 14:
        builder.setBOperator(DbQueryBuilder::ONLY_OP);
        builder.setElements("Al and O and P and Si");
        break;
    case 15:
        builder.setBOperator(DbQueryBuilder::JUST_OP);
        builder.setElements("Al");
        break;
    case 16:
        builder.setBOperator(DbQueryBuilder::JUST_OP);
        builder.setElements("Al and O");
        break;
    case 17:
        builder.setBOperator(DbQueryBuilder::JUST_OP);
        builder.setElements("Al and O and P");
        break;
    case 18:
        builder.setNames("potassium");
        builder.setSubfiles({"I"});
        builder.setBOperator(DbQueryBuilder::ONLY_OP);
        builder.setElements("Al and P");
        break;
    case 19:
        builder.setCsysString({"Cubic"});
        break;
    case 20:
        builder.setCsysString({"Cubic","Tetragonal"});
        break;
    case 21:
        builder.setCsysString({"hexagonal"});
        builder.setElements("Al");
        break;
    case 22:
        builder.setSpgString({"P 1 2 1"});
        break;
    case 23:
        builder.setSpgString({"P 1 1 2", "P 2 1 1"});
        break;
    case 24:
        builder.setSpgString({"P 1 1 2", "P 2 1 1"});
        builder.setCsysString({"Cubic"});
        break;
    case 25:
        builder.setCellParameter(0, 5.00, 5.01);
        builder.setCellParameter(1, 5.00, 5.01);
        break;
    case 26:
        builder.setCellParameter(0, 10.00, 11.00);
        builder.setCellParameter(1, 10.00, 11.00);
        builder.setCsysString({"Monoclinic"});
        break;
    case 27:
        builder.setCellParameter(0, 10.00, 11.00);
        builder.setCellParameter(1, 10.00, 11.00);
        builder.setCsysString({"Monoclinic"});
        builder.setSubfiles({"I"});
        break;
    case 28:
        builder.setCellParameter(0, 10.00, 11.00);
        builder.setCellParameter(1, 10.00, 11.00);
        builder.setCsysString({"Monoclinic"});
        builder.setSubfiles({"I"});
        builder.setElements("Al");
        break;
    case 29:
        builder.setIdEntry({"1000000","1000017"});
        builder.setSubfiles({"I"});
        builder.setElements("Al");
        builder.setCsysString({"Monoclinic"});
        break;
    }
}

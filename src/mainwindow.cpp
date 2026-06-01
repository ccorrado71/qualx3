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
#include <QDateTime>
#include <QDebug>
#include <QFile>
#include <QInputDialog>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
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
extern "C" void get_diffraction_data_size(int *ndata, int *nyb, int *nwave);
extern "C" void get_diffraction_data(float x[], float y[], float yb[], int nyb,
                                     bool *has_back, bool *back_subtracted,
                                     float wave[], float ratio[], int nwave,
                                     int *radtype);
extern "C" void set_diffraction_data(float x[], float y[], int ndata, float yb[], int nyb,
                                     bool has_back, bool back_subtracted,
                                     float wave[], float ratio[], int nwave,
                                     int radtype,
                                     const char *filename, int filename_len);


MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , mAction(NoAction)
    , savedZoomAction(mAction)
    , projectFileSaved(false)
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
    createDialogs();
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

void MainWindow::createDialogs()
{
    peakSearchDialog   = new PeakSearchDialog(this);
    backgroundDialog   = new BackgroundDialog(this);
    m_restraintsDialog = new RestraintsDialog(this);
    smoothingDialog    = new SmoothingDialog(this);
    plotStyleDialog    = new PlotStyleDialog(this);

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
    connect(ui->actionOpen_Project, &QAction::triggered, this, &MainWindow::onActionLoadProjectTriggered);
    connect(ui->actionSave_Project, &QAction::triggered, this, &MainWindow::onActionSaveProjectTriggered);
    connect(ui->actionSave_Project_As, &QAction::triggered, this, &MainWindow::onActionSaveProjectAsTriggered);
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
    connect(plotStyleDialog, &PlotStyleDialog::dialogClosed, this, &MainWindow::plotStyleClosed);
    connect(plotStyleDialog, &PlotStyleDialog::applyOffsetRequested, this, [this](double offset){ xpdViewer()->applyOffset(offset); });
    connect(plotStyleDialog, &PlotStyleDialog::redrawRequested, this, [this](){ xpdViewer()->redrawPlot(); });

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

    connect(ui->resultsWidget, &DbResultsWidget::selectedCardsChanged,
            this, [this](const QVector<CardType> &cards) {
        QVector<CardPeakData> peaks;
        peaks.reserve(cards.size());
        for (const CardType &card : cards) {
            if (card.getTth().isEmpty()) continue;
            CardPeakData cpd;
            const QVector<double> &pw = xpdViewer()->plotWave;
            cpd.id        = card.getId();
            cpd.color     = cardColor(card.getId());
            cpd.tth       = card.getTth();
            cpd.d         = card.getD();
            cpd.intensityAbsolute = (card.getScale() > 0.0);
            cpd.intensity = card.getScaledIntensity();
            cpd.wave      = pw.isEmpty() ? 1.54056 : pw.first();
            peaks.append(cpd);
        }
        xpdViewer()->setCardPeaks(peaks);
    });
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

void MainWindow::loadProject(QString fileName)
{
    QSettings settings;
    if (fileName.isEmpty()) return;

    fileName = QDir::toNativeSeparators(fileName);
    settings.setValue(DEFAULT_DIR_KEY, QFileInfo(fileName).absolutePath());

    // Helper: QJsonArray → QVector<double>
    auto toVec = [](const QJsonArray &a) {
        QVector<double> v;
        v.reserve(a.size());
        for (const QJsonValue &val : a) v.append(val.toDouble());
        return v;
    };

    // Helper: build CardType from a JSON object
    auto cardFromJson = [&](const QJsonObject &o, double wave) {
        CardType c;
        c.setId(o["id"].toString());
        c.setChemicalName(o["name"].toString());
        c.setMineralName(o["mineral"].toString());
        c.setChemicalFormula(o["formula"].toString());
        c.setQuality(o["quality"].toString());
        c.setRIR(o["rir"].toString());
        c.setSpaceGroup(o["spg"].toString());
        c.setD(toVec(o["d"].toArray()), wave);
        c.setIntensity(toVec(o["intensity"].toArray()));
        if (o["fom_calculated"].toBool()) {
            c.setFom(o["fom"].toDouble());
            c.setFomPeakPos(o["fom_peakpos"].toDouble());
            c.setFomIntensity(o["fom_intensity"].toDouble());
            c.setScale(o["scale"].toDouble());
        }
        return c;
    };

    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        QMessageBox::warning(this, tr("Open Project"),
                             tr("Cannot read file:\n%1").arg(fileName));
        return;
    }
    QJsonParseError parseErr;
    const QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &parseErr);
    if (doc.isNull()) {
        QMessageBox::warning(this, tr("Open Project"),
                             tr("Invalid project file:\n%1").arg(parseErr.errorString()));
        return;
    }
    const QJsonObject root = doc.object();

    // ── Diffraction data (x, y, background, wavelengths) ────────────
    if (root.contains("diffraction_data")) {
        const QJsonObject dd = root["diffraction_data"].toObject();

        auto jsonToFloatVec = [](const QJsonArray &a) {
            QVector<float> v(a.size());
            for (int i = 0; i < a.size(); ++i) v[i] = static_cast<float>(a[i].toDouble());
            return v;
        };

        QVector<float> xv    = jsonToFloatVec(dd["x"].toArray());
        QVector<float> yv    = jsonToFloatVec(dd["y"].toArray());
        const int ndata = xv.size();
        if (ndata > 0) {
            QVector<float> ybv    = dd.contains("yb")    ? jsonToFloatVec(dd["yb"].toArray())    : QVector<float>();
            QVector<float> wavev  = dd.contains("wave")  ? jsonToFloatVec(dd["wave"].toArray())  : QVector<float>();
            QVector<float> ratiov = dd.contains("ratio") ? jsonToFloatVec(dd["ratio"].toArray()) : QVector<float>();
            const bool has_back        = dd["has_back"].toBool();
            const bool back_subtracted = dd["back_subtracted"].toBool();
            const int  radtype         = dd["radtype"].toInt(0);
            const int  nyb             = ybv.size();
            const int  nwave           = wavev.size();
            const QByteArray fnBytes   = dd["filename"].toString().toLocal8Bit();

            // Dummy placeholder for optional zero-size parameters
            static float dummy = 0.0f;
            set_diffraction_data(xv.data(), yv.data(), ndata,
                                 nyb   > 0 ? ybv.data()    : &dummy, nyb,
                                 has_back, back_subtracted,
                                 nwave > 0 ? wavev.data()  : &dummy,
                                 nwave > 0 ? ratiov.data() : &dummy, nwave,
                                 radtype,
                                 fnBytes.constData(), fnBytes.size());
        }
    }

    // ── Experimental peaks ───────────────────────────────────────────
    double wave = 1.54056;
    if (root.contains("peaks")) {
        const QJsonObject pk = root["peaks"].toObject();
        ExperimentalPeaks &ep = AppState::peaks();
        ep.wave          = pk["wave"].toDouble(1.54056);
        ep.tth           = toVec(pk["tth"].toArray());
        ep.d             = toVec(pk["d"].toArray());
        ep.deltaD        = toVec(pk["delta_d"].toArray());
        ep.intensityOrig = toVec(pk["intensity"].toArray());
        ep.intensity     = ep.intensityOrig;
        ep.fwhm          = toVec(pk["fwhm"].toArray());
        ep.valid         = !ep.tth.isEmpty();
        wave             = ep.wave;
        updatePeakListTable();
        ui->peakCompareWidget->setExperimentalPeaks(ep);
    }

    // ── Search results ───────────────────────────────────────────────
    if (root.contains("results")) {
        QVector<CardType> cards;
        for (const QJsonValue &val : root["results"].toArray())
            cards.append(cardFromJson(val.toObject(), wave));
        ui->resultsWidget->setResults(cards);
        ui->reportWidget->updateReport(AppState::peaks(), cards);
        if (ui->resultsWidget->hasResults()) {
            ui->resultsWidget->selectFirstCard();
            ui->dockWidgetCompare->show();
            ui->dockWidgetCompare->raise();
        }
    }

    // ── Accepted phases ──────────────────────────────────────────────
    if (root.contains("phases")) {
        ui->quantWidget->clearPhases();
        ui->peakCompareWidget->clearAcceptedPhases();
        for (const QJsonValue &val : root["phases"].toArray()) {
            const CardType card = cardFromJson(val.toObject(), wave);
            ui->quantWidget->addPhase(card);
            ui->peakCompareWidget->addAcceptedPhase(card);
        }
        ui->reportWidget->updateQuantitative(
            ui->quantWidget->phases(), ui->quantWidget->quantPercentages());
        ui->dockWidgetQuant->show();
        ui->dockWidgetQuant->raise();
    }

    setCurrentFile(fileName, QVariant::fromValue(RecentFileType::Project).toString());
    setStatusMessage(tr("Project loaded: %1").arg(QFileInfo(fileName).fileName()));
}

void MainWindow::onActionLoadProjectTriggered()
{
    QSettings settings;
    QString selectedFilter = "Qualx Project (*.qxp)";
    QString exts = "Qualx Project (*.qxp)";

    QString fileName = QFileDialog::getOpenFileName(this,
                                                    tr("Open Project"), settings.value(DEFAULT_DIR_KEY).toString(),
                                                    exts, &selectedFilter);
    loadProject(fileName);
}

void MainWindow::saveProject(const QString &fileName)
{
    // Helper: QVector<double> → QJsonArray
    auto toJsonArray = [](const QVector<double> &v) {
        QJsonArray a;
        for (double x : v) a.append(x);
        return a;
    };

    QJsonObject root;
    root["version"] = 1;
    root["created"] = QDateTime::currentDateTime().toString(Qt::ISODate);

    // ── Diffraction data (x, y, background, wavelengths) ────────────
    int ndata = 0, nyb = 0, nwave = 0;
    get_diffraction_data_size(&ndata, &nyb, &nwave);
    if (ndata > 0) {
        QVector<float> xv(ndata), yv(ndata);
        QVector<float> ybv   (nyb   > 0 ? nyb   : 1);
        QVector<float> wavev (nwave > 0 ? nwave : 1);
        QVector<float> ratiov(nwave > 0 ? nwave : 1);
        bool has_back = false, back_subtracted = false;
        int radtype = 0;
        get_diffraction_data(xv.data(), yv.data(), ybv.data(), nyb,
                             &has_back, &back_subtracted,
                             wavev.data(), ratiov.data(), nwave,
                             &radtype);

        auto floatToJsonArray = [](const QVector<float> &v, int n) {
            QJsonArray a;
            for (int i = 0; i < n; ++i) a.append(static_cast<double>(v[i]));
            return a;
        };

        QJsonObject dd;
        dd["x"]               = floatToJsonArray(xv,  ndata);
        dd["y"]               = floatToJsonArray(yv,  ndata);
        dd["has_back"]        = has_back;
        dd["back_subtracted"] = back_subtracted;
        dd["radtype"]         = radtype;
        dd["filename"]        = currentFile;
        if (nyb   > 0) dd["yb"]    = floatToJsonArray(ybv,   nyb);
        if (nwave > 0) dd["wave"]  = floatToJsonArray(wavev, nwave);
        if (nwave > 0) dd["ratio"] = floatToJsonArray(ratiov,nwave);
        root["diffraction_data"] = dd;
    }

    // ── Experimental peaks ──────────────────────────────────────────
    const ExperimentalPeaks &ep = AppState::peaks();
    if (ep.valid && !ep.tth.isEmpty()) {
        QJsonObject pk;
        pk["wave"]      = ep.wave;
        pk["tth"]       = toJsonArray(ep.tth);
        pk["d"]         = toJsonArray(ep.d);
        pk["delta_d"]   = toJsonArray(ep.deltaD);
        pk["intensity"] = toJsonArray(ep.intensityOrig);
        pk["fwhm"]      = toJsonArray(ep.fwhm);
        root["peaks"] = pk;
    }

    // ── Search results ───────────────────────────────────────────────
    const QVector<CardType> cards = ui->resultsWidget->allCards();
    if (!cards.isEmpty()) {
        QJsonArray arr;
        for (const CardType &c : cards) {
            QJsonObject o;
            o["id"]             = c.getId();
            o["name"]           = c.getChemicalName();
            o["mineral"]        = c.getMineralName();
            o["formula"]        = c.getChemicalFormula();
            o["quality"]        = c.getQuality();
            o["rir"]            = c.getRIR();
            o["spg"]            = c.getSpaceGroup();
            o["fom_calculated"] = c.isFomCalculated();
            o["fom"]            = c.getFom();
            o["fom_peakpos"]    = c.getFomPeakPos();
            o["fom_intensity"]  = c.getFomIntensity();
            o["scale"]          = c.getScale();
            o["d"]              = toJsonArray(c.getD());
            o["intensity"]      = toJsonArray(c.getIntensity());
            arr.append(o);
        }
        root["results"] = arr;
    }

    // ── Accepted phases ──────────────────────────────────────────────
    const QVector<CardType> &phases = ui->quantWidget->phases();
    const QVector<double>   &quant  = ui->quantWidget->quantPercentages();
    if (!phases.isEmpty()) {
        QJsonArray arr;
        for (int i = 0; i < phases.size(); ++i) {
            QJsonObject o;
            o["id"]             = phases[i].getId();
            o["name"]           = phases[i].getChemicalName();
            o["mineral"]        = phases[i].getMineralName();
            o["formula"]        = phases[i].getChemicalFormula();
            o["quality"]        = phases[i].getQuality();
            o["rir"]            = phases[i].getRIR();
            o["spg"]            = phases[i].getSpaceGroup();
            o["fom_calculated"] = phases[i].isFomCalculated();
            o["fom"]            = phases[i].getFom();
            o["fom_peakpos"]    = phases[i].getFomPeakPos();
            o["fom_intensity"]  = phases[i].getFomIntensity();
            o["scale"]          = phases[i].getScale();
            o["d"]              = toJsonArray(phases[i].getD());
            o["intensity"]      = toJsonArray(phases[i].getIntensity());
            o["weight_percent"] = (i < quant.size()) ? quant[i] : 0.0;
            arr.append(o);
        }
        root["phases"] = arr;
    }

    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QMessageBox::warning(this, tr("Save Project"),
                             tr("Cannot write file:\n%1").arg(fileName));
        return;
    }
    file.write(QJsonDocument(root).toJson());
    projectFileSaved = true;
}

void MainWindow::onActionSaveProjectTriggered()
{
    if (currentProjectFile.isEmpty())
        onActionSaveProjectAsTriggered();
    else
        saveProject(currentProjectFile);
}

void MainWindow::onActionSaveProjectAsTriggered()
{
    QSettings settings;
    QString selectedFilter = "expo";
    QString exts = "Qualx Project (*.qxp)";
    QString fileName = SaveDialog::run(this,tr("Save Project As"),
                                       settings.value(DEFAULT_DIR_KEY).toString(),
                                       QFileInfo(currentFile).baseName(),exts,
                                       "qxp",selectedFilter);
    if (!fileName.isEmpty()) {
        fileName = QDir::toNativeSeparators(fileName);
        currentProjectFile = fileName;
        saveProject(fileName);
    }
}

void MainWindow::clearProjectFile()
{
    projectFileSaved = false;
    currentProjectFile.clear();
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
    plotStyleDialog->setOptions(xpdViewer());
    plotStyleDialog->show();
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

void MainWindow::plotStyleClosed(QDialogButtonBox::StandardButton button)
{
    if (button == QDialogButtonBox::Apply) {
        plotStyleDialog->apply(xpdViewer());
    } else if (button == QDialogButtonBox::Ok) {
        if (plotStyleDialog->anyChangeToApply()) {
            plotStyleDialog->apply(xpdViewer());
        }

        if (plotStyleDialog->anyChangeApplied()) {
            plotStyleConfig();
        }

    } else if (button == QDialogButtonBox::Cancel) {
        plotStyleDialog->cancel(xpdViewer());
    } else if (button == QDialogButtonBox::RestoreDefaults) {
        for (int i = 0; i < plotStyleDialog->obsPen.count(); i++) {
            plotStyleDialog->obsPen[i] = xpdViewer()->obs.at(i).getDefaultPen(i);
            plotStyleDialog->obsScatter[i] = xpdViewer()->obs.at(i).getDefaultScatter(i);
            plotStyleDialog->obsVisible[i] = true;
        }
        QPen backPen = xpdViewer()->back.getDefaultPen();
        QCPScatterStyle backScatter = xpdViewer()->bpoints.getDefaultScatter();
        QPen calcPen = xpdViewer()->calc.getDefaultPen();
        QPen diffPen = xpdViewer()->diff.getDefaultPen();
        QPen cDiffPen = xpdViewer()->cdiff.getDefaultPen();
        QPen peaksPen = xpdViewer()->peaks.getDefaultPen();
        for (int i = 0; i < plotStyleDialog->reflPen.count(); i++) {
            plotStyleDialog->reflPen[i] = xpdViewer()->refl.at(i).getDefaultPen(i);
            plotStyleDialog->reflVisible[i] = true;
        }
        PlotSettings defaults;
        defaults.restoreDefaults();
        plotStyleDialog->setWidgets(false, backPen, backScatter, calcPen, diffPen, cDiffPen, peaksPen, defaults,
                                    xpdViewer()->back.isVisible(), xpdViewer()->bpoints.isVisible(), xpdViewer()->calc.isVisible(),
                                    xpdViewer()->diff.isVisible(), xpdViewer()->cdiff.isVisible(), xpdViewer()->peaks.isVisible());
    }
}

void MainWindow::plotStyleConfig()
{
    QSettings settings;

    for (int i = 0; i < plotStyleDialog->obsPen.count(); i++ ) {
        settings.setValue(xpdViewer()->obs[i].getKeyString("pen", i),xpdViewer()->obs.at(i).getPen());
        settings.setValue(xpdViewer()->obs[i].getKeyString("scatterPen", i), xpdViewer()->obs.at(i).getScatter().pen());
        settings.setValue(xpdViewer()->obs[i].getKeyString("scatterStyle", i), xpdViewer()->obs.at(i).getScatter().shape());
    }

    settings.setValue(xpdViewer()->back.getKeyString("pen", -1),xpdViewer()->back.getPen());
    settings.setValue(xpdViewer()->bpoints.getKeyString("scatterPen", -1),xpdViewer()->bpoints.getScatter().pen());
    settings.setValue(xpdViewer()->bpoints.getKeyString("scatterStyle", -1),xpdViewer()->bpoints.getScatter().shape());

    settings.setValue(xpdViewer()->calc.getKeyString("pen", -1),xpdViewer()->calc.getPen());
    settings.setValue(xpdViewer()->diff.getKeyString("pen", -1),xpdViewer()->diff.getPen());
    settings.setValue(xpdViewer()->cdiff.getKeyString("pen", -1),xpdViewer()->cdiff.getPen());
    settings.setValue(xpdViewer()->peaks.getKeyString("pen", -1),xpdViewer()->peaks.getPen());

    for (int i = 0; i < plotStyleDialog->reflPen.count(); i++) {
        settings.setValue(xpdViewer()->refl[i].getKeyString("pen", i),xpdViewer()->refl.at(i).getPen());
    }
    xpdViewer()->pSettings().write();
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
    apply_background_subtraction();

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

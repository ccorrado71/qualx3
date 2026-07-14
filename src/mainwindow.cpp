#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "appstate.h"
#include "experimentalpeaks.h"
#include "searchmatch.h"
#include "peakassoc.h"
#include "dbquerybuilder.h"
#include "managedatabasesdialog.h"
#include "progkeysettings.h"
#include "restraintsdialog.h"
#include "savedialog.h"
#include "searchoptionsdialog.h"
#include "imgsettingsdialog.h"
#include "fileutils.h"
#include "xpdutils.h"
#include "dbresultswidget.h"

#include <QApplication>
#include <QDateTime>
#include <QDesktopServices>
#include "updater.h"
#include "maintenancetool.h"
#include "printdialog.h"
#include "reportwidget.h"
#include <QPrinter>
#include <QPrintDialog>
#include <QTextDocument>
#include <QFileInfo>
#include <QUrl>
#include <QDebug>
#include <QFile>
#include <QInputDialog>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMenu>
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
extern "C" void kalpha2_stripping();
extern "C" int peak_number();
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
extern "C" void delete_all_peaks();


MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , mAction(NoAction)
    , savedZoomAction(mAction)
    , projectFileSaved(true)
{
    ui->setupUi(this);
    ui->cardBrowser->setOpenExternalLinks(true);
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

    ui->xpdWidget->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(ui->xpdWidget, &QWidget::customContextMenuRequested, this, [this](const QPoint &pos) {
        QMenu menu(this);
        menu.addActions(ui->menuView->actions());
        menu.exec(ui->xpdWidget->mapToGlobal(pos));
    });

    defaultState = saveState();
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
    rangeDialog        = new RangeDialog(this);
    plotStyleDialog    = new PlotStyleDialog(this);
    aboutDialog = new AboutDialog(this);
    aboutDialog->setWebsiteUrl("https://www.ba.ic.cnr.it/softwareic/qualx/");
    aboutDialog->setContactsUrl("https://www.ba.ic.cnr.it/softwareic/expo/contact-us/");
    aboutDialog->setCitationUrl("https://doi.org/10.1017/S0885715617000240");

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

    connect(ui->quantWidget, &QuantWidget::cardSelected,
            this, &MainWindow::onCardSelected);

    connect(ui->resultsWidget, &DbResultsWidget::cardDataSelected,
            this, [this](const CardType &card) {
                ui->peakCompareWidget->setSelectedCard(
                    card, card.getId(), SearchOptionsDialog::savedDelta2theta());
            });

    connect(ui->resultsWidget, &DbResultsWidget::entrySelectionChanged,
            this, [this](bool hasSelection) {
                if (!hasSelection)
                    ui->peakCompareWidget->clearCard();
            });

    connect(ui->resultsWidget, &DbResultsWidget::phaseAccepted,
            this, [this](const CardType &card) {
                ui->quantWidget->addPhase(card);
                ui->peakCompareWidget->addAcceptedPhase(card);
                ui->reportWidget->updateQuantitative(
                    ui->quantWidget->phases(), ui->quantWidget->quantPercentages());
                xpdViewer()->addPhaseReflections(card, cardColor(card.getId()));
                ui->dockWidgetQuant->show();
                ui->dockWidgetQuant->raise();
                if (SearchOptionsDialog::savedResidualSearching())
                    performResidualSearch(card);
            });

    connect(ui->quantWidget, &QuantWidget::phaseRemoved,
            this, [this](const CardType &card) {
                ui->resultsWidget->addCard(card);
                ui->peakCompareWidget->removeAcceptedPhase(card.getId());
                ui->reportWidget->updateQuantitative(
                    ui->quantWidget->phases(), ui->quantWidget->quantPercentages());
                xpdViewer()->removePhaseReflections(card.getId());
            });

    connect(ui->peakCompareWidget, &PeakCompareWidget::selectedComparePointsChanged,
            this, [this](const QVector<double> &tth,
                         const QVector<double> &intensity,
                         const QVector<QColor> &colors) {
                xpdViewer()->drawSelectedComparePoints(tth, intensity, colors);
            });
}

void MainWindow::actionsSetup()
{
    //File menu
    connect(ui->actionNew, &QAction::triggered, this, &MainWindow::onActionNewTriggered);
    connect(ui->actionImportDiffractionPattern, &QAction::triggered, this, &MainWindow::onActionImportDiffractionPatternTriggered);
    connect(ui->actionOpen_Project, &QAction::triggered, this, &MainWindow::onActionLoadProjectTriggered);
    connect(ui->actionSave_Project, &QAction::triggered, this, &MainWindow::onActionSaveProjectTriggered);
    connect(ui->actionSave_Project_As, &QAction::triggered, this, &MainWindow::onActionSaveProjectAsTriggered);
    connect(ui->actionExport_Diffraction_Pattern, &QAction::triggered, this, &MainWindow::onActionExportDiffractionPattern);
    connect(ui->actionImage_Powder_Pattern, &QAction::triggered, this, &MainWindow::onActionImagePowderPatternTriggered);
    connect(ui->actionExit, &QAction::triggered, qApp, &QApplication::quit);

    //Pattern menu
    connect(ui->actionRange, &QAction::triggered, this, &MainWindow::onActionRangeTriggered);
    connect(ui->actionBackground, &QAction::triggered, this, &MainWindow::onActionBackgroundTriggered);
    connect(ui->actionExport_Background, &QAction::triggered, this, &MainWindow::onActionBackgroundExportTriggered);
    connect(ui->actionSubtract_Background, &QAction::triggered, this, &MainWindow::onActionSubtractBackgroundTriggered);
    connect(ui->actionSmoothing, &QAction::triggered, this, &MainWindow::onActionSmoothingTriggered);
    connect(ui->actionK_alpha2_Stripping, &QAction::triggered, this, &MainWindow::onActionKAlpha2StrippingTriggered);
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
    connect(ui->actionCancel_Search, &QAction::triggered, this, &MainWindow::onActionCancelSearchTriggered);
    //connect(ui->actionGetCard, &QAction::triggered, this, &MainWindow::onActionGetCardTriggered);
    connect(ui->actionLoad_Add, &QAction::triggered, this, &MainWindow::onActionLoadAddTriggered);
    connect(ui->actionManage_Databases, &QAction::triggered, this, &MainWindow::actionManageDatabasesTriggered);

    ui->actionRecalculate_FOM->setEnabled(false);
    connect(ui->resultsWidget, &DbResultsWidget::hasResultsChanged,
            ui->actionRecalculate_FOM, &QAction::setEnabled);
    connect(ui->actionRecalculate_FOM, &QAction::triggered,
            this, &MainWindow::onActionRecalculateFomTriggered);

    //Window menu
    QList<QAction *> actions = createPopupMenu()->actions();
    foreach (QAction *action, actions) {
        ui->menuWindow->addAction(action);
    }
    connect(ui->actionDefault_Layout, &QAction::triggered, this, &MainWindow::onActionDefaultLayoutTriggered);

    //Help menu
    connect(ui->actionDocumentation_HTML,   &QAction::triggered, this, &MainWindow::onActionDocumentationHtmlTriggered);
    connect(ui->actionDocumentation_PDF,    &QAction::triggered, this, &MainWindow::onActionDocumentationPdfTriggered);
    connect(ui->actionPrint,                &QAction::triggered, this, &MainWindow::onActionPrintTriggered);
    connect(ui->actionCheck_for_Updates,    &QAction::triggered, this, &MainWindow::onActionCheckForUpdatesTriggered);
    connect(ui->actionAbout,                &QAction::triggered, this, &MainWindow::onActionAboutTriggered);

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

    auto buildAndShowPeaks = [this](const QVector<CardType> &cards) {
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
    };

    connect(ui->resultsWidget, &DbResultsWidget::selectedCardsChanged,
            this, buildAndShowPeaks);

    connect(ui->quantWidget, &QuantWidget::selectedPhasesChanged,
            this, buildAndShowPeaks);
}

static QStringList buildActiveRestraints(RestraintsDialog *dlg)
{
    if (!dlg) return {};
    QStringList list;
    if (!dlg->compositionFormula().isEmpty())     list << "Composition";
    if (!dlg->chemicalName().isEmpty())           list << "Chemical name";
    if (!dlg->crystalSystemStrings().isEmpty())   list << "Crystal system";
    if (!dlg->spaceGroupStrings().isEmpty())       list << "Space group";
    if (!dlg->subfilesCodes().isEmpty())           list << "Subfiles";
    if (!dlg->entryIds().isEmpty())                list << "Entry IDs";
    const auto cell = dlg->cellQuery();
    for (double v : cell.values)
        if (v >= 0) { list << "Unit cell"; break; }
    return list;
}

void MainWindow::updateReport()
{
    if (peakSearchDialog)
        ui->reportWidget->setPeakSearchSettings(peakSearchDialog->settings());
    const QStringList activeR = buildActiveRestraints(m_restraintsDialog);
    ui->reportWidget->setRestraintsInfo(!activeR.isEmpty(), activeR);
    ui->reportWidget->updateReport(AppState::peaks(), ui->resultsWidget->allCards());
}

void MainWindow::updatePeakListTable()
{
    if (ui->peakListWidget->isVisible())
        ui->peakListWidget->updatePeakListTable();
    loadExperimentalPeaks();
    updateReport();
}

void MainWindow::enableActions(EnabledActions action, bool state)
{
    //qInfo() << "Enable Actions: " << action << " State: " << state;
    switch (action) {
    case InitAction:  //Init
        ui->actionSave_Project->setEnabled(false);
        ui->actionSave_Project_As->setEnabled(false);
        ui->actionExport_Diffraction_Pattern->setEnabled(false);
        ui->actionImage_Powder_Pattern->setEnabled(false);
        ui->actionSearch_Match->setEnabled(false);
        enableMenu(ui->menuPattern, false);
        enableMenu(ui->menuView, false);
        break;
    case PatternAction:   //Only Pattern
        ui->actionSave_Project->setEnabled(true);
        ui->actionSave_Project_As->setEnabled(true);
        ui->actionExport_Diffraction_Pattern->setEnabled(true);
        ui->actionImage_Powder_Pattern->setEnabled(true);
        ui->actionSearch_Match->setEnabled(true);
        enableMenu(ui->menuPattern, true);
        enableMenu(ui->menuView, true);
        break;
    case DialogOpenAction:
        enableMenu(ui->menuFile, false);
        enableMenu(ui->menuPattern, false);
        ui->actionSearch_Match->setEnabled(false);
        break;
    case RunAction:
        enableMenu(ui->menuFile, false);
        enableMenu(ui->menuPattern, false);
        enableMenu(ui->menuDatabase, false);
        enableMenu(ui->menuEntry, false);
        ui->actionCancel_Search->setEnabled(true);
        break;
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
    ui->peakDockWidget->raise();
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

void MainWindow::onActionNewTriggered()
{
    if (!projectFileSaved) {
        QMessageBox::StandardButton reply = QMessageBox::question(this, tr("New"),
                                                 tr("The current project has unsaved changes.\nDo you want to save it before restarting from scratch?"),
                                                 QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        if (reply == QMessageBox::Cancel)
            return;
        if (reply == QMessageBox::Save) {
            onActionSaveProjectTriggered();
            if (!projectFileSaved)
                return;
        }
    }

    xpdViewer()->setGraphicArea();
    xpdViewer()->hideGraphicArea();
    xpdViewer()->replot();

    ui->plotLabel1->clear();
    ui->plotLabel2->clear();
    ui->plotLabel3->clear();

    ui->resultsWidget->setResults({});
    ui->peakListWidget->peakListModel->setRowCount(0);
    ui->peakCompareWidget->clearAcceptedPhases();
    ui->peakCompareWidget->clearCard();
    ui->peakCompareWidget->setExperimentalPeaks(ExperimentalPeaks());
    ui->quantWidget->clearPhases();
    ui->reportWidget->clearQuantitative();
    ui->reportWidget->updateReport(ExperimentalPeaks(), {});
    ui->cardBrowser->clear();

    AppState::peaks().clear();
    delete_all_peaks();

    clearProjectFile();
    currentFile.clear();
    setWindowTitle(qApp->applicationDisplayName()+"-"+qApp->applicationVersion());

    enableActions(InitAction);
}

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
                markProjectModified();
                updateReport();
                enableActions(PatternAction);
            }
            break;
        }
        case RecentFileType::Structure:
            break;

        case RecentFileType::Project:
            loadProject(fileIn);
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
            markProjectModified();
            updateReport();
            enableActions(PatternAction);
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
        if (ui->resultsWidget->hasResults()) {
            ui->resultsWidget->selectFirstCard();
            ui->dockWidgetCompare->show();
            ui->dockWidgetCompare->raise();
        }
    }
    updateReport();

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

    enableActions(PatternAction);
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

void MainWindow::onActionExportDiffractionPattern()
{
    QSettings settings;
    QString selectedFilter = "xy";
    int code = 0;
    QString exts = "XY files (*.xy);; CIF files (*.cif)";
    QString fileName = SaveDialog::run(this,
                                       tr("Export Data As"),
                                       settings.value(DEFAULT_DIR_KEY).toString(),
                                       QFileInfo(currentFile).baseName(),
                                       exts,
                                       "xy",
                                       selectedFilter,
                                       &code);
    if (!fileName.isEmpty()) {
        settings.setValue(DEFAULT_DIR_KEY,QFileInfo(fileName).absolutePath());
        if (selectedFilter.contains("cif", Qt::CaseInsensitive)) {
            // Export as CIF
            esportanew(16, fileName.toLocal8Bit().constData(), fileName.toLocal8Bit().size());
        } else {
            // Export as XY
            esportanew(17, fileName.toLocal8Bit().constData(), fileName.toLocal8Bit().size());
        }
    }
}

void MainWindow::onActionImagePowderPatternTriggered()
{
    QSettings settings;
    QString selectedFilter = settings.value(QUALX_IMAGE_FILTER_KEY).toString();
    int code = 0;
    QString exts = "PNG (*.png);;"
                   "PDF (*.pdf);;"
                   "BMP (*.bmp);;"
                   "JPEG (*.jpeg *jpg *jpe)";
    QString fileName = SaveDialog::run(this,
                                       tr("Export Image As"),
                                       settings.value(DEFAULT_DIR_KEY).toString(),
                                       QFileInfo(currentFile).baseName(),
                                       exts,
                                       "png",
                                       selectedFilter,
                                       &code);
    settings.setValue(QUALX_IMAGE_FILTER_KEY,selectedFilter);
    if (!fileName.isEmpty()) {
        ImgSettingsDialog dialog(this, QFileInfo(fileName).suffix());
        if (dialog.exec()) {
            settings.setValue(DEFAULT_DIR_KEY,QFileInfo(fileName).absolutePath());
            if (dialog.isTransparent()) {
                // remove background for transparency
                xpdViewer()->setBackground(QBrush(Qt::NoBrush));
                xpdViewer()->axisRect()->setBackground(QBrush(Qt::NoBrush));
            }
            if( fileName.endsWith(".png") ){
                xpdViewer()->savePng( fileName, xpdViewer()->width(), xpdViewer()->height(), dialog.scale() );
            }
            if( fileName.endsWith(".jpg") || fileName.endsWith(".jpeg") || fileName.endsWith(".jpe")){
                xpdViewer()->saveJpg( fileName, xpdViewer()->width(), xpdViewer()->height(), dialog.scale() );
            }
            if( fileName.endsWith(".pdf") ){
                xpdViewer()->savePdf( fileName, xpdViewer()->width(), xpdViewer()->height());
            }
            if (dialog.isTransparent()) {
                // add background
                xpdViewer()->pSettings().applyToBackground(xpdViewer());
                xpdViewer()->pSettings().applyToLayer(xpdViewer());
            }
        }
    }
}

void MainWindow::clearProjectFile()
{
    projectFileSaved = true;
    currentProjectFile.clear();
}

void MainWindow::markProjectModified()
{
    projectFileSaved = false;
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
        } else if (ext == "qxp") {
            loadProject(fileIn);
            break;
        } else {
            QMessageBox::critical(this,"Error",fileIn + " is not a valid file type.");
        }
    }
}

static QString modelToHtml(QAbstractItemModel *model,
                           const QString &title,
                           const QList<int> &skipCols = {})
{
    QString html =
        "<html><head><style>"
        "body{font-family:sans-serif;font-size:10pt;}"
        "h2{color:#003366;}"
        "table{border-collapse:collapse;width:100%;}"
        "th{background:#003366;color:white;padding:4px 8px;text-align:left;}"
        "td{border:1px solid #ccc;padding:3px 8px;}"
        "tr:nth-child(even){background:#f0f4ff;}"
        "</style></head><body>";
    html += "<h2>" + title.toHtmlEscaped() + "</h2><table><tr>";

    for (int c = 0; c < model->columnCount(); ++c) {
        if (skipCols.contains(c)) continue;
        html += "<th>" + model->headerData(c, Qt::Horizontal).toString().toHtmlEscaped() + "</th>";
    }
    html += "</tr>";

    for (int r = 0; r < model->rowCount(); ++r) {
        html += "<tr>";
        for (int c = 0; c < model->columnCount(); ++c) {
            if (skipCols.contains(c)) continue;
            html += "<td>" + model->data(model->index(r, c)).toString().toHtmlEscaped() + "</td>";
        }
        html += "</tr>";
    }

    html += "</table></body></html>";
    return html;
}

static void printHtml(QPrinter *printer, const QString &html)
{
    QTextDocument doc;
    doc.setHtml(html);
    doc.print(printer);
}

static void printWidget(QPrinter *printer, QWidget *widget)
{
    QPixmap pix = widget->grab();
    QPainter painter(printer);
    QRect pr = printer->pageLayout().paintRectPixels(printer->resolution());
    QSize scaled = pix.size().scaled(pr.size(), Qt::KeepAspectRatio);
    QRect target(pr.topLeft(), scaled);
    painter.drawPixmap(target, pix);
}

void MainWindow::onActionPrintTriggered()
{
    PrintDialog dlg(this);
    if (dlg.exec() != QDialog::Accepted)
        return;

    QPrinter printer(QPrinter::HighResolution);
    QPrintDialog printDlg(&printer, this);
    if (printDlg.exec() != QDialog::Accepted)
        return;

    switch (dlg.selectedArea()) {

    case PrintDialog::Report:
        ui->reportWidget->print(&printer);
        break;

    case PrintDialog::ResultList:
        printHtml(&printer,
                  modelToHtml(ui->resultsWidget->sourceModel,
                              tr("Result List"),
                              {0}));   // skip checkbox column
        break;

    case PrintDialog::PeakList:
        printHtml(&printer,
                  modelToHtml(ui->peakListWidget->peakListModel,
                              tr("Peak List")));
        break;

    case PrintDialog::Pattern: {
        QCPPainter cpPainter(&printer);
        QRect pr = printer.pageLayout().paintRectPixels(printer.resolution());
        ui->xpdWidget->toPainter(&cpPainter, pr.width(), pr.height());
        break;
    }

    case PrintDialog::Card:
        ui->cardBrowser->print(&printer);
        break;

    case PrintDialog::Quantitative:
        printWidget(&printer, ui->quantWidget);
        break;

    case PrintDialog::Compare:
        printHtml(&printer,
                  modelToHtml(ui->peakCompareWidget->m_model,
                              tr("Compare")));
        break;
    }
}

//
//  Pattern Menu
//

void MainWindow::onActionRangeTriggered()
{
    rangeDialog->show();
    rangeDialog->setRange(xpdViewer());
}

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
    updateReport();
}

void MainWindow::onActionSmoothingTriggered()
{
    smoothingDialog->setSmoothing();
    smoothingDialog->show();
}

void MainWindow::onActionKAlpha2StrippingTriggered()
{
    kalpha2_stripping();
    updateReport();
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
    ui->peakDockWidget->show();
    ui->peakDockWidget->raise();
    run_peaksearchwin();
    updateReport();
}

void MainWindow::onActionPeakSearchConditionsTriggered()
{
    ui->peakDockWidget->show();
    ui->peakDockWidget->raise();
    peakSearchDialog->setOptions();
    peakSearchDialog->show();
}

void MainWindow::onActionLoadPeaksTriggered()
{
    ui->peakDockWidget->show();
    ui->peakDockWidget->raise();
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
        updateReport();
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
        ui->peakDockWidget->show();
        ui->peakDockWidget->raise();
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

void MainWindow::onActionSearchMatchTriggered()
{
    int npeaks = ensurePeaksFound();
    if (npeaks == 0) {
        QMessageBox::warning(this, tr("No Peaks Found"), tr("Please run Peak Search first."));
        return;
    }

    loadExperimentalPeaks();
    const ExperimentalPeaks &ep0 = AppState::peaks();

    DbQueryBuilder builder = buildSearchMatchQuery(ep0);
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
    updateReport();
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

void MainWindow::onActionCancelSearchTriggered()
{
    AppState::db().cancelSearch();
    setStatusMessage(tr("Search canceled"));
}

void MainWindow::executeSearch(DbQueryBuilder &builder, bool merge)
{
    builder.buildQuery();

    enableActions(SaveAction);
    enableActions(RunAction);

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

    enableActions(RestoreAction);

    QApplication::restoreOverrideCursor();
    statusProgressBar->hide();
    setStatusMessage(tr("Found %1 card(s)").arg(cards.size()));

    if (merge)
        ui->resultsWidget->mergeResults(cards);
    else
        ui->resultsWidget->setResults(cards);

    markProjectModified();

    // Refresh the peak compare widget with the current experimental peaks
    ui->peakCompareWidget->clearAcceptedPhases();
    ui->peakCompareWidget->clearCard();
    ui->peakCompareWidget->setExperimentalPeaks(AppState::peaks());
    ui->reportWidget->clearQuantitative();
    updateReport();
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
    updateReport();
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

void MainWindow::runSearchMatch()
{
    onActionSearchMatchTriggered();
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

    int dbNcard;
    QString dbType;
    AppState::db().getInfo(dbNcard, dbType);
    if (dbType == "COD") {
        const QString codUrl = QString("https://www.crystallography.net/cod/%1.html").arg(info.id);
        html += QString("<tr><td><b>COD Entry</b></td><td><a href='%1'>%1</a></td></tr>").arg(codUrl);
    }
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

    // ── Step 4: recompute FOMs for all remaining cards, discard non-matching ones ──
    recalculateFOMs(tr(" after residual search"));
}

// Recomputes the FOM of every card currently in the results widget against the
// experimental peaks in AppState::peaks(), using the search options currently saved
// in SearchOptionsDialog. Cards whose recomputed FOM is <= 0 or below the saved
// minimum FOM are dropped, the remaining ones are re-sorted by descending FOM and
// truncated to the saved max entries, mirroring the filtering applied right after a
// database search. Used both after a residual search (performResidualSearch) and
// when the user explicitly asks to recalculate FOMs (e.g. after editing peaks or
// changing search-match options).
void MainWindow::recalculateFOMs(const QString &statusSuffix)
{
    ExperimentalPeaks &ep = AppState::peaks();
    const double delta   = SearchOptionsDialog::savedDelta2theta();
    const double wIntens = SearchOptionsDialog::savedWeightIntensity();
    const int expSize = ep.tth.size();

    QVector<CardType> cards = ui->resultsWidget->allCards();

    for (CardType &card : cards) {
        if (expSize == 0) {
            // No experimental peaks: FOM is meaningless
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

    // Discard cards whose FOM is <= 0 or below the saved minimum FOM
    const double minFom = SearchOptionsDialog::savedMinFom();
    cards.erase(std::remove_if(cards.begin(), cards.end(),
                               [minFom](const CardType &c){ return c.getFom() <= 0.0 || c.getFom() < minFom; }),
                cards.end());

    std::sort(cards.begin(), cards.end(),
              [](const CardType &a, const CardType &b){ return a.getFom() > b.getFom(); });

    // Keep only the top maxEntries cards by descending FOM
    const int maxEntries = SearchOptionsDialog::savedMaxEntries();
    if (cards.size() > maxEntries)
        cards.resize(maxEntries);

    ui->resultsWidget->setResults(cards);
    setStatusMessage(tr("%1 card(s)%2").arg(cards.size()).arg(statusSuffix));
}

void MainWindow::onActionRecalculateFomTriggered()
{
    recalculateFOMs();
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

//
//  Help Menu
//

void MainWindow::onActionDocumentationHtmlTriggered()
{
    const QDir appDir(QCoreApplication::applicationDirPath());
    // Try development build layout: exe directly in build_*/ (single-config generators)
    // or nested in build_*/<config>/ (multi-config generators), then installed layout.
    const QStringList candidates = {
        appDir.filePath("../docs/site/index.html"),
        appDir.filePath("../../docs/site/index.html"),
        appDir.filePath("../share/qualx/docs/index.html"),
        appDir.filePath("docs/index.html"),
    };
    for (const QString &path : candidates) {
        if (QFile::exists(path)) {
            QDesktopServices::openUrl(QUrl::fromLocalFile(QFileInfo(path).absoluteFilePath()));
            return;
        }
    }
    QMessageBox::information(this, tr("Documentation"),
        tr("HTML documentation not found.\n"
           "Run 'mkdocs build' inside the docs/ folder to generate it."));
}

void MainWindow::onActionDocumentationPdfTriggered()
{
    const QDir appDir(QCoreApplication::applicationDirPath());
    const QStringList candidates = {
        appDir.filePath("../docs/qualx_manual.pdf"),
        appDir.filePath("../../docs/qualx_manual.pdf"),
        appDir.filePath("../share/qualx/docs/qualx_manual.pdf"),
        appDir.filePath("docs/qualx_manual.pdf"),
    };
    for (const QString &path : candidates) {
        if (QFile::exists(path)) {
            QDesktopServices::openUrl(QUrl::fromLocalFile(QFileInfo(path).absoluteFilePath()));
            return;
        }
    }
    QMessageBox::information(this, tr("Documentation"),
        tr("PDF documentation not found.\n"
           "Run 'bash docs/make_pdf.sh' to generate it."));
}

//
//  Window Menu
//

void MainWindow::onActionDefaultLayoutTriggered()
{
    //bool nextVisible = ui->toolBarNext->isVisible();
    restoreState(defaultState);
    // resizeDocks({ui->dockWidget,ui->dockWidget_2},{50,50},Qt::Horizontal);
    // qInfo() << "Next is visible: " << nextVisible;
    // ui->toolBarNext->setVisible(nextVisible);
}

//
// Helpers
//

void MainWindow::onActionCheckForUpdatesTriggered()
{
#if defined(Q_OS_WIN)
    auto m_updater = new MaintenanceTool(this);
    connect(m_updater, &MaintenanceTool::stateChanged, [=](MaintenanceTool::ProcessState state){
        if (state == MaintenanceTool::NotRunning && m_updater->hasUpdate()) {
            m_updater->startMaintenanceTool(MaintenanceTool::Updater);
        }
    });

    m_updater->checkUpdate();
#else
    auto updater = new Updater(this);
    updater->setUrl(QStringLiteral("https://www.ba.ic.cnr.it/content/old/qualx3/updates.json"));
    updater->setNotifyOnFinish(true);
    updater->checkForUpdates();
#endif
}

void MainWindow::onActionAboutTriggered()
{
    aboutDialog->show();
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

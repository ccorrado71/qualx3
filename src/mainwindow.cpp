#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "dbquerybuilder.h"
#include "progkeysettings.h"
#include "savedialog.h"
#include "fileutils.h"

#include <QDebug>
#include <QMessageBox>

MainWindow *mMainWindow;

extern "C" void open_diffraction_patt(const char *fileIn, int lenIn, const char *fileOut, int lenOut, int addData, int *err);
extern "C" void run_peaksearchwin();
extern "C" void LoadPeaksC(const char *filename, int length, int tipo, int *ier);
extern "C" void SavePeaksC(const char *filename, int length, int tipo);
extern "C" void apply_background_subtraction();
extern "C" int peak_number();
extern "C" void get_d_delta_values(float dval[], float deltadval[], double *wave);

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , mAction(NoAction)
    , savedZoomAction(mAction)
{
    ui->setupUi(this);

    actionsSetup();
    createDialogs();

    //currentDatabase = "/home/corrado/temp/cod/cod2205/cod2205";
    currentDatabase = "/home/corrado/temp/cod/cod2509/cod2509";
    if (!qualxDb.openDatabases(currentDatabase)) {
        qCritical() << "Error opening databases";
    }

    readSettings();
    mMainWindow = this;
}

MainWindow::~MainWindow()
{
    delete ui;
}

XpdViewWidget *MainWindow::xpdViewer() const
{
    return ui->xpdWidget;
}

void MainWindow::setAction(const MouseAction &action, bool writeConfig)
{
    XpdViewWidget::MouseAction xpdAction = static_cast<XpdViewWidget::MouseAction>(action);
    xpdViewer()->setAction(xpdAction);

    // FIX THIS LATER
    // if (writeConfig) {
    //     QSettings settings;
    //     QMetaEnum metaEnum = QMetaEnum::fromType<MouseAction>();
    //     const char *key = metaEnum.valueToKey(action);
    //     settings.setValue(EXPO_ACTION_KEY,key);
    // }
}

void MainWindow::checkAction(MouseAction action)
{
    if (action == mAction) return;
    // FIX THIS LATER
    // switch (action) {
    // case NoZoom:
    //     ui->actionSelection_Mode->setChecked(true);
    //     break;
    // case RectangleZoom:
    //     ui->actionRectangle_Zoom->setChecked(true);
    //     break;
    // case HorizontalZoom:
    //     ui->actionHorizontal_Zoom->setChecked(true);
    //     break;
    // case Pan:
    //     ui->actionPan->setChecked(true);
    //     break;
    // default:
    //     break;
    // }
    setAction(action, false);
}

void MainWindow::setZoomAction()
{
    if (mAction != savedZoomAction)
        checkAction(savedZoomAction);
}

void MainWindow::enableActions(EnabledActions action, bool state)
{
    qInfo() << "FIX LATER enableActions";
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

void MainWindow::createDialogs()
{
    peakSearchDialog = new PeakSearchDialog(this);
    backgroundDialog = new BackgroundDialog(this);
}

void MainWindow::actionsSetup()
{
    //File menu
    connect(ui->actionImportDiffractionPattern, &QAction::triggered, this, &MainWindow::onActionImportDiffractionPatternTriggered);
    connect(ui->actionExit, &QAction::triggered, qApp, &QApplication::quit);

    //Pattern menu
    connect(ui->actionBackground, &QAction::triggered, this, &MainWindow::onActionBackgroundTriggered);
    connect(ui->actionSubtract_Background, &QAction::triggered, this, &MainWindow::onActionSubtractBackgroundTriggered);
    connect(ui->actionPeak_Search, &QAction::triggered, this, &MainWindow::onActionPeakSearchTriggered);
    connect(ui->actionLoad_Peaks, &QAction::triggered, this, &MainWindow::onActionLoadPeaksTriggered);
    connect(ui->actionSave_Peaks, &QAction::triggered, this, &MainWindow::onActionSavePeaksTriggered);
    connect(ui->actionPeak_Search_Conditions, &QAction::triggered, this, &MainWindow::onActionPeakSearchConditionsTriggered);

    //Search menu
    connect(ui->actionSearch_Match, &QAction::triggered, this, &MainWindow::onActionSearchMatchTriggered);
    connect(ui->actionTestDatabase, &QAction::triggered, this, &MainWindow::onActionTestDatabaseTriggered);
    connect(ui->actionDatabaseInfo, &QAction::triggered, this, &MainWindow::onActionDatabaseInfoTriggered);
    connect(ui->actionGetCard, &QAction::triggered, this, &MainWindow::onActionGetCardTriggered);
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
    //settings.setValue(QUALX_SPLITSIZE_KEY1, ui->splitter1->saveState());
    //settings.setValue(EXPO_SPLITSIZE_KEY2, ui->splitter2->saveState());
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
        builder.setSpgString({"P 1 1 2"});
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

//
//  File Menu
//

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

    qInfo() << "Import Diffraction Pattern From";

    QStringList files = QFileDialog::getOpenFileNames(this,
                                                      tr("Import Diffraction Pattern From"),
                                                      settings.value(DEFAULT_DIR_KEY).toString(),
                                                      exts, &selectedFilter);

    if (!files.isEmpty()) {
        QFileInfo info(files.at(0));
        QString outFile = info.path() + QDir::separator() + info.baseName() + ".out";
        outFile = QDir::toNativeSeparators(outFile);
        int nerr = 0;
        fileutils::setCurrentDirFromFile(files.at(0));
        for (int i = 0; i < files.size(); i++) {
            int err;
            QString filename = QDir::toNativeSeparators(files.at(i));
            open_diffraction_patt(filename.toStdString().c_str(), filename.length(),
                               outFile.toStdString().c_str(), outFile.length(),
                                   i, &err);
            if (err) {
                nerr++;
            }
        }
    //     if (nerr < files.size()) {
    //         QString filename = QDir::toNativeSeparators(files.at(0));
    //         settings.setValue(DEFAULT_DIR_KEY,QFileInfo(filename).absolutePath());
    //         outputFileName = outFile;
    //         setCurrentFile(filename,QVariant::fromValue(RecentFileType::Data).toString());
    //     }
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

void MainWindow::onActionSubtractBackgroundTriggered()
{
    apply_background_subtraction();
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

void MainWindow::updatePeakListTable()
{
    if (ui->peakListWidget->isVisible())
        ui->peakListWidget->updatePeakListTable();
}

//
//  Search Menu
//

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

    float *dval = new float[npeaks];
    float *deltadval = new float[npeaks];
    double wave;
    get_d_delta_values(dval, deltadval, &wave);
    qInfo() << "wave:" << wave;

    QVector<double> dValues(npeaks);
    QVector<double> deltaValues(npeaks);
    for (int i = 0; i < npeaks; i++) {
        dValues[i] = dval[i];
        deltaValues[i] = deltadval[i];
    }

    delete [] dval;
    delete [] deltadval;

    DbQueryBuilder builder;
    builder.setPrintEnabled(true);
    builder.setDValues(dValues, deltaValues);
    builder.setWave(wave);
    QVector<CardType> acceptedCards;
    qualxDb.makeQueryStrongest(builder, acceptedCards);
    qInfo() << "Number of accepted cards: " << acceptedCards.size();
    ui->resultsWidget->setResults(acceptedCards);
}

void MainWindow::onActionDatabaseInfoTriggered()
{
    int ncard;
    QString type;
    qualxDb.getInfo(ncard, type);
    qInfo() << "Ncard: " << ncard << "Type: " << type;
}

void MainWindow::onActionTestDatabaseTriggered()
{
    DbQueryBuilder builder;

    builder.setPrintEnabled(true);

    testSelection(builder, 1);

    builder.buildQuery();
    qualxDb.makeQuery(builder);
}

void MainWindow::onActionGetCardTriggered()
{
    QString idCard = "2300375";
    //QString idCard = "230037"; //uncomment this to get error in case of wrong card number
    qualxDb.getCardInfo(idCard);
    qualxDb.getCardAdditionalInfo(idCard);
}

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

//#include "dbmanager.h"
#include "peaksearchdialog.h"
#include "backgrounddialog.h"
#include "qualxdbmanager.h"
#include "xpdviewwidget.h"
#include "managedatabasesdialog.h"

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    enum MouseAction {
        NoZoom,
        HorizontalZoom,
        RectangleZoom,
        Pan,
        AddBackgroundPoint,
        DeleteBackgroundPoint,
        AddPeak,
        DeletePeak,
        NoAction
    };
    Q_ENUM(MouseAction)

    enum EnabledActions {
        InitAction,
        PatternAction,
        StructureAction,
        PatternAndStructureAction,
        DialogOpenAction,
        RunAction,
        RunSkipAction,
        NextAction,
        PhaseAction,
        SaveAction,
        RestoreAction,
        IntervalsAction,
        ExtraAction,
        ExtraBackgroundAction,
        ProfileAction,
        CycleAction,
        PeaksAction
    };
    Q_ENUM(EnabledActions)

    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
    XpdViewWidget *xpdViewer() const;
    void setAction(const MouseAction &action, bool writeConfig = true);
    void updatePeakListTable();
    void checkAction(MouseAction action);
    void setZoomAction();
    void enableActions(MainWindow::EnabledActions action, bool state=false);
    void saveEnabledActions();
    void restoreEnabledActions();

    static QString getPathDataFiles();
    static void setPathDataFiles(const QString &newPathDataFiles);

private slots:
    void closeEvent(QCloseEvent *event);

    //File
    void onActionImportDiffractionPatternTriggered();

    //Pattern
    void onActionBackgroundTriggered();
    void onActionSubtractBackgroundTriggered();
    void onActionPeakSearchTriggered();
    void onActionPeakSearchConditionsTriggered();
    void onActionLoadPeaksTriggered();
    void onActionSavePeaksTriggered();

    void onActionSearchMatchTriggered();
    void onActionTestDatabaseTriggered();
    void onActionDatabaseInfoTriggered();
    void onActionGetCardTriggered();

    //Search
    void actionManageDatabasesTriggered();

private:
    void createDialogs();
    void actionsSetup();
    void writeSettings();
    void readSettings();

    Ui::MainWindow *ui;
    MouseAction mAction;
    MouseAction savedZoomAction;
    QMap<QAction *, bool> stateActions;
    void enumerateEnabledActionsMenu(QMenu *menu);

    //Dialog Windows
    PeakSearchDialog *peakSearchDialog;
    BackgroundDialog *backgroundDialog;

    //Files
    QString currentFile;
    static QString pathDataFiles;

    //Database
    QString currentDatabase;
    QualxDbManager qualxDb;
    void testSelection(DbQueryBuilder &builder, int testCase);
};
#endif // MAINWINDOW_H

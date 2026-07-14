#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "plotstyledialog.h"
#include "peaksearchdialog.h"
#include "backgrounddialog.h"
#include "restraintsdialog.h"
#include "dbquerybuilder.h"
#include "cardtype.h"
#include "xpdviewwidget.h"
#include "managedatabasesdialog.h"
#include "commandline.h"
#include "aboutdialog.h"
#include "smoothingdialog.h"
#include "rangedialog.h"

#include <QMainWindow>

typedef struct {
    QString fileName;
    QString fileType;
} RecentFileInfo;

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

    enum RecentFileType {Input, Data, Structure, Project};
    Q_ENUM(RecentFileType)

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
    void setCurrentFile(const QString &fullFileName, const QString &fileType);
    void setStatusMessage(const QString &message);
    void clearStatusMessage();
    QProgressBar *getStatusProgressBar() const;
    void runSearch(const SearchOptions &opts);
    void runSearchMatch();
    void updatePeakListTable();
    void checkAction(MouseAction action);
    void saveAction();
    void restoreAction();
    void setZoomAction();
    void enableActions(MainWindow::EnabledActions action, bool state=false);
    void saveEnabledActions();
    void restoreEnabledActions();
    void enableMenu(QMenu *menu, bool enab);

    static QString getPathDataFiles();
    static void setPathDataFiles(const QString &newPathDataFiles);

private slots:
    void closeEvent(QCloseEvent *event);
    void zoomGroupTriggered(QAction *action);
    void plotStyleClosed(QDialogButtonBox::StandardButton button);

    //File
    void onActionNewTriggered();
    void openRecentFile();
    void onActionImportDiffractionPatternTriggered();
    void onActionLoadProjectTriggered();
    void onActionSaveProjectTriggered();
    void onActionSaveProjectAsTriggered();
    void onActionExportDiffractionPattern();
    void onActionImagePowderPatternTriggered();
    void onActionPrintTriggered();
    void onActionFileDropped(const QStringList &fileList);

    //Pattern
    void onActionRangeTriggered();
    void onActionBackgroundTriggered();
    void onActionBackgroundExportTriggered();
    void onActionSubtractBackgroundTriggered();
    void onActionSmoothingTriggered();
    void onActionKAlpha2StrippingTriggered();
    void onActionPeakSearchTriggered();
    void onActionPeakSearchConditionsTriggered();
    void onActionLoadPeaksTriggered();
    void onActionSavePeaksTriggered();

    //View
    void onActionPlotStyleTriggered();
    void onActionResetZoomTriggered();
    void onActionAutoscaleTriggered();

    //Search
    void onActionSearchMatchTriggered();
    void onActionSearchMatchOptionsTriggered();
    void actionRestraintsTriggered();
    void actionManageDatabasesTriggered();
    void onActionCancelSearchTriggered();
    void onActionTestDatabaseTriggered();

    //Entry
    void onActionLoadAddTriggered();
    void onActionRecalculateFomTriggered();

    //Window
    void onActionDefaultLayoutTriggered();

    //Help
    void onActionDocumentationHtmlTriggered();
    void onActionDocumentationPdfTriggered();
    void onActionCheckForUpdatesTriggered();
    void onActionAboutTriggered();

private:
    void createDialogs();
    void createActionGroup();
    void actionsSetup();
    void updateReport();
    void writeSettings();
    void readSettings();
    void readAction();
    void plotStyleConfig();
    void deleteSelectedPeaks(const QVector<int> &selected);
    void addDeleteSelectedPoint(int action, double xp, double yp, int &ier);

    Ui::MainWindow *ui;
    QLabel *statusLabel1;
    QProgressBar *statusProgressBar;
    MouseAction mAction;
    MouseAction savedAction;
    MouseAction savedZoomAction;
    QMap<QAction *, bool> stateActions;
    void enumerateEnabledActionsMenu(QMenu *menu);

    //Dialog Windows
    PeakSearchDialog  *peakSearchDialog  = nullptr;
    PlotStyleDialog *plotStyleDialog = nullptr;
    BackgroundDialog  *backgroundDialog  = nullptr;
    RestraintsDialog  *m_restraintsDialog = nullptr;
    SmoothingDialog *smoothingDialog = nullptr;
    AboutDialog *aboutDialog = nullptr;
    RangeDialog *rangeDialog = nullptr;

    //Files
    QString currentFile;
    QString currentProjectFile;
    bool projectFileSaved;
    void clearProjectFile();
    void markProjectModified();
    static QString pathDataFiles;
    enum { maxFileNr = 10 };
    QList<QAction*> recentFileActionList;
    void saveProject(const QString &fileName);
    void createRecentActions();
    void updateRecentFileActions();
    void setRecentFiles(const QString &fullFileName, const QString &fileType);
    QByteArray defaultState;

    void testSelection(DbQueryBuilder &builder, int testCase);
    void executeSearch(DbQueryBuilder &builder, bool merge = false);
    void onRestraintsSearch(bool merge);
    void onRestraintsSearchMatch();
    void onCardSelected(const QString &id);
    void applyDialogRestraints(DbQueryBuilder &builder);
    void performResidualSearch(const CardType &acceptedCard);
    void recalculateFOMs(const QString &statusSuffix = QString());
    void loadDiffractionPatterns(QStringList files);
    void loadProject(QString fileName);
};
#endif // MAINWINDOW_H

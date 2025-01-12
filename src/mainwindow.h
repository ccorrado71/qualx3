#ifndef MAINWINDOW_H
#define MAINWINDOW_H

//#include "dbmanager.h"
#include "qualxdbmanager.h"
#include "xpdviewwidget.h"

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
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
    void enableActions(MainWindow::EnabledActions action, bool state=false);

private slots:

    //File
    void onActionImportDiffractionPatternTriggered();

    void on_actionGet_Card_triggered();
    void on_actionQueryName_triggered();
    void on_actionDatabaseInfo_triggered();

private:
    void actionsSetup();

    Ui::MainWindow *ui;    
    QString currentDatabase;
    QualxDbManager qualxDb;
    //DbManager db, dbInfo, dbInfoStat;
    void testSelection(DbQueryBuilder &builder, int testCase);
};
#endif // MAINWINDOW_H

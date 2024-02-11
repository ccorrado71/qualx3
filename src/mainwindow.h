#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "dbmanager.h"

#include <QMainWindow>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:    

    void on_actionGet_Card_triggered();
    void on_actionQueryName_triggered();
    void on_actionDatabaseInfo_triggered();

private:
    Ui::MainWindow *ui;    
    QString currentDatabase;
    DbManager db, dbInfo;
    void testSelection(DbQueryBuilder &builder, int testCase);
};
#endif // MAINWINDOW_H

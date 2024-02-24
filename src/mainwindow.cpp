#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "dbquerybuilder.h"

//#include "dbmanager.h"

#include <QDebug>
#include <QMessageBox>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    //currentDatabase = "/home/corrado/temp/codino/cod2205ino.sq";
    currentDatabase = "/home/corrado/temp/cod/cod2205.sq";

    if (!db.openDb(currentDatabase)) return;
    if (!dbInfo.openDb(currentDatabase+".info")) return;
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::on_actionDatabaseInfo_triggered()
{
    int ncard;
    QString type;
    db.getInfo(ncard, type);
    qInfo() << "Ncard: " << ncard << "Type: " << type;
}

void MainWindow::on_actionGet_Card_triggered()
{
    QString idCard = "2300375";
    //QString idCard = "230037"; //uncomment this to get error in case of wrong card number
    db.getCardInfo(idCard);
    dbInfo.getCardAdditionalInfo(idCard);
}

void MainWindow::testSelection(DbQueryBuilder &builder, int testCase)
{
    switch (testCase) {
    case 1:
        builder.setNames("nickel");
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
    }
}

void MainWindow::on_actionQueryName_triggered()
{
    DbQueryBuilder builder;

    builder.setPrintEnabled(true);

    testSelection(builder, 16);

    builder.buildQuery();
    db.makeQuery(builder);
}

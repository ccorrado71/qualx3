#ifndef QT_UTILS_H
#define QT_UTILS_H

#include <QString>
#include <QLayout>
#include <QLayoutItem>
#include <QTextStream>
#include <QStatusBar>
#include <QLabel>
#include <QStandardItem>
#include <QStandardItemModel>
#include <QDirIterator>

typedef struct {
    QString data;
    QString arch;
    QString compilersString;
    QString qtVersionInfo;
} AppVersionInfo;

bool isJAV();
QString get_user();
QString WelcomeMessage();
void ErrMsg(QString str);
QTextStream& qStdOut();
void removeChildren(QLayout *layout);
QString LabelStyle(QColor bg, QColor fg);
char *ToCharacter(QString str);
void Attesa();
void Attesa(QString Msg, QStatusBar *w);
void Attesa(QString Msg, QLabel *w);
void FineAttesa();
void FineAttesa(QStatusBar *w);
void FineAttesa(QLabel *w);
QString Aring();
QString Degree();
QList<QStandardItem *> leggiRiga(QStandardItemModel *model, int row);
QString formattedMessage(const QString& message, int ampl);
void setEnabledWidgetsInLayout(QLayout *layout, bool enabled);
bool isChildWidgetOfAnyLayout(QLayout *layout, QWidget *widget);
bool stopWaitCursor();
AppVersionInfo getVersionInfo();

#endif // QT_UTILS_H


#include <QApplication>
#include <QTime>
#include <QFileDialog>
#include <QMessageBox>
#include <QRegularExpression>
#include <QDebug>
#include "qt_utils.h"
#if USE_CONFIG_H
#include "config.h"
#endif

bool isJAV()
{
    QString nome = qApp->applicationName();
    return (nome.contains("JAV", Qt::CaseInsensitive));
}

QString get_user()
{
    QByteArray name = qgetenv("USER"); // get the user name in Linux and macOS
    if (name.isEmpty()) name = qgetenv("USERNAME"); // get the user name for windows
    return QString::fromLocal8Bit(name);
}

QString WelcomeMessage()
{
    int hh = QTime::currentTime().toString("H").toInt();  //hours 0-23
    QString msg;
    if((hh >= 14) && (hh < 18))
       msg = QString("Good afternoon %1. Welcome to %2").arg(get_user()).arg(qApp->applicationName());
    else if((hh >= 18) && (hh <= 23))
       msg = QString("Good evening %1. Welcome to %2").arg(get_user()).arg(qApp->applicationName());
    else if(hh >= 3)
       msg = QString("Good morning %1. Welcome to %2").arg(get_user()).arg(qApp->applicationName());
    else
       msg = QString("Good night %1. Welcome to %2").arg(get_user()).arg(qApp->applicationName());

    return msg;
}

void ErrMsg(QString str)
{
    QMessageBox msgbox;
    msgbox.setIcon(QMessageBox::Critical);
    msgbox.setText(str);
    msgbox.exec();
}

QTextStream& qStdOut()
{
    static QTextStream ts( stdout );
    return ts;
}

void removeChildren(QLayout *layout)
{
    QLayoutItem* child;
    while ( layout->count() != 0 ) {
        child = layout->takeAt ( 0 );
        if ( child->layout() != 0 ) {
            removeChildren ( child->layout() );
        } else if ( child->widget() != 0 ) {
            delete child->widget();
        }
        delete child;
    }	
}

QString LabelStyle(QColor bg, QColor fg)
{
    return("QLabel {background-color: '"+bg.name() +
               "'; font color: '" + fg.name() + "'}");
}

char *ToCharacter(QString str)
{
    QByteArray array = str.toLocal8Bit();
    return array.data();
}

void Attesa()
{
    QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
}

void FineAttesa()
{
    QApplication::restoreOverrideCursor();
}

void Attesa(QString Msg, QStatusBar *w)
{
    w->showMessage(Msg);
    int i=0;
    while(i<100) {
        QCoreApplication::processEvents();
        i++;
    }
    Attesa();
}

void FineAttesa(QStatusBar *w)
{
    w->clearMessage();
    FineAttesa();
}

void Attesa(QString Msg, QLabel *w)
{
    w->setText(Msg);
    int i=0;
    while(i<100) {
        QCoreApplication::processEvents();
        i++;
    }
    Attesa();
}

void FineAttesa(QLabel *w)
{
    w->clear();
    FineAttesa();
}

QString Aring()
{
    //return QString(0x0c5).toUtf8();  In Qt6 QString doesn't accept int
    return QString(QChar(0x0c5)).toUtf8();
}

QString Degree()
{
    //return QString(0x0b0).toUtf8();  In Qt6 QString doesn't accept int
    return QString(QChar(0x0b0)).toUtf8();
}

QList<QStandardItem *> leggiRiga(QStandardItemModel *model, int row)
{
    QList<QStandardItem *> lista;
    int ncol = model->columnCount();
    for (int k=0; k<ncol; k++)
    {
        QModelIndex index = model->index(row,k);
        QStandardItem *item = model->item(index.row(), index.column());
        lista.append(item);
    }
    return lista;
}

QString formattedMessage(const QString& message, int ampl) {
   QString ast("*");
   //QStringList lines = message.split(QRegExp("\n|\r\n|\r")); // not Qt6 compatible
   static QRegularExpression regex("\n|\r\n|\r");
   QStringList lines = message.split(regex);
   QString fmessage = ' ' + ast.repeated(ampl) + "\r\n";

   for (int i = 0; i < lines.size(); i++) {
       int spaces = ampl-2-lines.at(i).length();
       int spacesl = spaces/2;
       int spacesr = spaces-spacesl;
       fmessage = fmessage + ' ' + ast + QString(spacesl,' ') + lines.at(i) + QString(spacesr,' ') + ast + "\r\n";
       if (i != lines.size()-1) fmessage = fmessage +  ' ' + ast + QString(ampl-2,' ') + ast + "\r\n"; //add empty line
   }

   fmessage = fmessage + ' ' + ast.repeated(ampl) + "\r\n";
   return fmessage;
}

void setEnabledWidgetsInLayout(QLayout *layout, bool enabled)
{
   if (layout == NULL)
      return;

   QWidget *pw = layout->parentWidget();
   if (pw == NULL)
      return;

   foreach(QWidget *w, pw->findChildren<QWidget*>())
   {
      if (isChildWidgetOfAnyLayout(layout,w))
         w->setEnabled(enabled);
   }
}

bool isChildWidgetOfAnyLayout(QLayout *layout, QWidget *widget)
{
   if (layout == NULL || widget == NULL)
      return false;

   if (layout->indexOf(widget) >= 0)
      return true;

   foreach(QObject *o, layout->children())
   {
      if (isChildWidgetOfAnyLayout((QLayout*)o,widget))
         return true;
   }

   return false;
}

bool stopWaitCursor() {
    bool isWaitCursor = false;
    if (QApplication::overrideCursor()) {
        isWaitCursor = (QApplication::overrideCursor()->shape() == Qt::WaitCursor);
    }
    if (isWaitCursor) QApplication::restoreOverrideCursor();

    return isWaitCursor;
}

AppVersionInfo getVersionInfo()
{
    AppVersionInfo info;
    info.data = QFileInfo(QCoreApplication::applicationFilePath()).lastModified().toString(Qt::TextDate);
    info.arch = QSysInfo::buildCpuArchitecture();
    info.compilersString = QString("Compiled on %1").arg(info.arch);
#if USE_CONFIG_H
    info.compilersString += QString(" (%1, %2, %3)").arg(BUILD_TYPE, QFileInfo(CPP_COMPILER).baseName(), QFileInfo(FORTRAN_COMPILER).baseName());
#endif
    info.qtVersionInfo = QString("Graphical User Interface based on Qt libraries (V.%1)").arg(QT_VERSION_STR);
    return info;
}

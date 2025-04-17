#include "savedialog.h"

#include <QFileDialog>
#include <QPointer>
#include <QGridLayout>
#include <QRegularExpression>
#include <QDebug>

int SaveDialog::mChecked=0;

void SaveDialog::updateDefaultSuffix()
{
  //QString filter = selectedNameFilter();
  QString suffix;

  // Qt6 not compatible
//  QRegExp filter_regex(QLatin1String("(?:^\\*\\.(?!.*\\()|\\(\\*\\.)(\\w+)"));
//  if (filter_regex.indexIn(selectedNameFilter()) != -1) {
//      suffix = filter_regex.cap(1);
//  }
  // Qt6/Qt5 compatible
  //static QRegularExpression filterRegex(QLatin1String(R"(^(?:\*\.(?!.*\()|\(\*\.\)(\w+))"));
  static QRegularExpression filterRegex(QLatin1String("(?:^\\*\\.(?!.*\\()|\\(\\*\\.)(\\w+)"));
  auto match = filterRegex.match(selectedNameFilter());
  if (match.hasMatch()) {
      suffix = match.captured(1);
  }


// metodo 2 used in molsketch
//  // remove part of filter up "*."
//  int index = filter.indexOf(QRegExp("\\*."));
//  if (index > 0) {
//      filter = filter.remove(0, index + 2);

//      // truncate from "*" or ")"
//      index = filter.indexOf(QRegExp("( \\*.)|(\\))"));
//      if (index > 0) {
//          filter.truncate(index);
//          suffix = filter;
//      }
//  }
// metodo 1 used in Avogadro
//  const QString filter = selectedNameFilter();
//  QString suffix;
//  int i = filter.indexOf("*.");
//  if(i != -1)
//  {
//    // FIXME somebody who knows regexps should make this use a QRegExp.
//    int j;
//    const QString separators(" )");
//    for(j = i; j < filter.size() && !separators.contains(filter[j]); j++) {}
//    if(j < filter.size())
//    {
//      suffix = filter.mid(i+2, j-i-2);
//    }
//  }
  if(suffix.isEmpty()) suffix = m_defaultSuffix;
  setDefaultSuffix(suffix);
  //qDebug() << "SUFFIX: " << suffix;
  //qDebug() << "FILE: " << selectedFiles().isEmpty();
  if (!selectedFiles().isEmpty())
      emit currentChanged(selectedFiles().first());
 // if (!selectedFiles().isEmpty()) qDebug() <<selectedFiles().first();
}

SaveDialog::SaveDialog(QWidget *widget,
                         const QString& windowTitle,
                         const QString& defaultDirectory,
                         const QString& defaultFileName,
                         const QString& filters,
                         const QString& defaultSuffix,
                         int check_type)
    : QFileDialog(widget, windowTitle, defaultDirectory, filters), m_defaultSuffix(defaultSuffix)
  {
    //check_type = 0   nothing to add
    //check_type = 1   add transparency check for png
    //check_type = 2   add diaplayed atoms check

    setOption(QFileDialog::DontUseNativeDialog);
    setWindowTitle(windowTitle);
    if(!(defaultDirectory.isEmpty())) setDirectory(defaultDirectory);
    if(!(defaultFileName.isEmpty())) {
        //Force suffix, without suffix setDefaultSuffix doesn't work
        if (QFileInfo(defaultFileName).suffix().isEmpty())
            selectFile(defaultFileName+'.'+defaultSuffix);
        else
            selectFile(defaultFileName);
    }
    setFileMode(QFileDialog::AnyFile);
    setAcceptMode(QFileDialog::AcceptSave);
    //setConfirmOverwrite(true);
    setOption(DontConfirmOverwrite, false);
    setLabelText(QFileDialog::Accept, tr("Save"));
    checkButton = nullptr;
    if(check_type != 0)
    {
        QGridLayout* mainLayout = dynamic_cast <QGridLayout*>(this->layout());
        if( !mainLayout){
            qDebug()<<"mainLayout is NULL";
        }else{
            QHBoxLayout *hbl =new QHBoxLayout(0);
            if(check_type == 1)
                checkButton = new QCheckBox("Transparent", this);
            else
                checkButton = new QCheckBox("Displayed atoms", this);
            hbl->addWidget(checkButton);
            int num_rows = mainLayout->rowCount();
            mainLayout->addLayout(hbl, num_rows, 0);
            connect(checkButton, SIGNAL(stateChanged(int)), this,
                    SLOT(checkChanged(int)));
        }
    }
    connect(this, SIGNAL(filterSelected(const QString &)), this, SLOT(updateDefaultSuffix()));
    updateDefaultSuffix();
  }

const QString SaveDialog::run(QWidget *widget,
                              const QString& windowTitle,
                              const QString& defaultDirectory,
                              const QString& defaultFileName,
                              const QString& filters,
                              const QString& defaultSuffix,
                              QString &defaultFilter,
                              int *check_type)
{
  QString result;
  int tipo = 0;
  if(check_type != nullptr)
      tipo = *check_type;

  // Make sure we always have something for a file name
  QString fileName(defaultFileName);
  if (fileName.isEmpty())
    fileName = tr("untitled");

#if defined (Q_WS_MAC) || defined (Q_WS_WIN)
  if(check_type != nullptr)
  {
// The Mac and Windows Qt/Native dialog already update extensions for us.
// So we'll call the static version.
  result = QFileDialog::getSaveFileName(widget,
                                        windowTitle,
                                        defaultDirectory + '/' + fileName,
                                        filters, &defaultFilter);
  return result;
  }
#endif
  QPointer<SaveDialog> dialog = new SaveDialog(widget, windowTitle, defaultDirectory, fileName, filters, defaultSuffix, tipo);
  dialog->selectNameFilter(defaultFilter);
  dialog->updateDefaultSuffix();
  mChecked = 0;
  if(dialog->exec())
  {
    result = dialog->selectedFiles().first();
    defaultFilter = dialog->selectedNameFilter();
    if(check_type != nullptr)
       *check_type = dialog->mChecked;
  }
  delete dialog;

  return result;
}

void SaveDialog::checkChanged(int stato)
{
    mChecked = stato;
}



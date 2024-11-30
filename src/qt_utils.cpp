#include <QApplication>

bool stopWaitCursor() {
    bool isWaitCursor = false;
    if (QApplication::overrideCursor()) {
        isWaitCursor = (QApplication::overrideCursor()->shape() == Qt::WaitCursor);
    }
    if (isWaitCursor) QApplication::restoreOverrideCursor();

    return isWaitCursor;
}

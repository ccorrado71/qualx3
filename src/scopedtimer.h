#pragma once

#include <QElapsedTimer>
#include <QDebug>

class ScopedTimer {
public:
    ScopedTimer(const QString& label = QString())
        : m_label(label)
    {
        m_timer.start();
    }

    ~ScopedTimer()
    {
        qint64 elapsed = m_timer.elapsed();
        if (m_label.isEmpty())
            qInfo() << "Elapsed time:" << elapsed << "ms";
        else
            qInfo() << m_label << "elapsed time:" << elapsed << "ms";
    }

private:
    QString m_label;
    QElapsedTimer m_timer;
};

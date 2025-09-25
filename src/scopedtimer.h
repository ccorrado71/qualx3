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
        qint64 elapsed_ms = m_timer.elapsed();
        double elapsed_sec = elapsed_ms / 1000.0;
        if (m_label.isEmpty())
            qInfo() << "Elapsed time:" << QString::number(elapsed_sec, 'f', 3) << "sec";
        else
            qInfo() << m_label << "elapsed time:" << QString::number(elapsed_sec, 'f', 3) << "sec";
    }

private:
    QString m_label;
    QElapsedTimer m_timer;
};

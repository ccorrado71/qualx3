#ifndef PLOTSTYLEDIALOG_H
#define PLOTSTYLEDIALOG_H

//#include "graphitem.h"
#include "plotsettings.h"
#include "linestylewidget.h"
#include "xpdviewwidget.h"

namespace Ui {
class PlotStyleDialog;
}

class PlotStyleDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PlotStyleDialog(QWidget *parent = nullptr);
    ~PlotStyleDialog();

    void setOptions(const XpdViewWidget *plot);
    void apply(XpdViewWidget *plot, bool update = true);
    // void apply(XpdViewWidget *plot, QVector<graphItem> &obs, graphItem &back, graphItem &bpoints, graphItem &calc,
    //            graphItem &diff, graphItem &cdiff, graphItem &peaks, QVector<graphItem> &refl,
    //            const QVector<double> waves, bool update = true);
    void setWidgets(bool init, const QPen &backPen, const QCPScatterStyle &backScatter, const QPen &calcPen,
                    const QPen &diffPen, const QPen &cDiffPen, const QPen &peaksPen, const PlotSettings &psettings,
                    bool backVisible, bool backpVisible, bool calcVisible, bool diffVisible, bool cDiffVisible, bool peaksVisible);
    void cancel(XpdViewWidget *plot);
    // void cancel(XpdViewWidget *plot, QVector<graphItem> &obs, graphItem &back, graphItem &bpoints, graphItem &calc,
    //             graphItem &diff, graphItem &cdiff, graphItem &peaks, QVector<graphItem> &refl, const QVector<double> waves);
    bool anyChangeToApply() const;
    bool anyChangeApplied() const;    
    void reject();

    QVector<QPen> obsPen, reflPen;
    QVector<QCPScatterStyle> obsScatter;
    QVector<bool> obsVisible;
    QVector<bool> reflVisible;

signals:
    void dialogClosed(QDialogButtonBox::StandardButton button);
    void applyOffsetRequested(double offset);
    void redrawRequested();

private slots:
    void on_buttonBox_clicked(QAbstractButton *button);
    void on_observedComboBox_currentIndexChanged(int index);
    void on_reflComboBox_currentIndexChanged(int index);
    void enableApplyButton();

private:
    Ui::PlotStyleDialog *ui;    

    bool backVisible0, backpVisible0, calcVisible0;
    bool diffVisible0, cDiffVisible0, peaksVisible0;
    QVector<bool> obsVisible0;
    QVector<bool> reflVisible0;
    PlotSettings settings0;
    QVector<QPen> obsPen0, reflPen0;
    QVector<QCPScatterStyle> obsScatter0;
    QPen backPen0, calcPen0, diffPen0, cDiffPen0, peaksPen0;
    QCPScatterStyle backScatter0;

    bool changeApplied;    
    xpdutils::xAbscissaType mAbscissa;
    double mYOffset;
    QCPAxis::ScaleType mYScaleType;

    void connectLineWidget(LineStyleWidget *lsWidget);
    void setObserved(int index);
    void setReflections(int index);
    xpdutils::xAbscissaType getAbscissaState() const;
};

#endif // PLOTSTYLEDIALOG_H

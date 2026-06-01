#ifndef XPDVIEWWIDGET_H
#define XPDVIEWWIDGET_H

#include "customplotzoom.h"
#include "graphitem.h"
#include "plotsettings.h"

typedef struct {
    int hkl[3];
    double x;
} refInfo;

typedef struct {
    QVector<refInfo> ref;
    int visible;
    float wave;
} reflectionSet;

struct CardPeakData {
    QString id;
    QColor  color;
    QVector<double> tth;
    QVector<double> d;
    QVector<double> intensity;        // 0-100, or absolute counts if intensityAbsolute=true
    bool            intensityAbsolute = false;
    double          wave = 0.0;
};

class XpdViewWidget : public CustomPlotZoom
{
    Q_OBJECT
public:
    enum MouseAction {
        NoZoom,
        HorizontalZoom,
        RectangleZoom,
        Pan,
        AddBackgroundPoint,
        DeleteBackgroundPoint,
        AddPeak,
        DeletePeak,
        NoAction
    };
    Q_ENUM(MouseAction)

    explicit XpdViewWidget(QWidget *parent = nullptr);
    void setGraphicArea();
    void addPlot(const QVector<double> &xvet0, const QVector<double> &yvet0, graphItem::ItemType type,
                 int visible, float wave, const QString &name);
    void addPlot(const QVector<double> &xvet0, graphItem::ItemType type,
                 int visible, float wave);
    void addPlot(float xvet[], float yvet[], int num, graphItem::ItemType type,
                 int visible, float wave, const QString &name);
    void addPlot(float xvet[], int num, graphItem::ItemType type,
                 int visible, float wave);
    void addReflections(float xvet[], int h[], int k[], int l[], int num, int visible, float wave);
    void drawPlot();
    void redrawPlot(bool computeLimits = true);
    void setAction(const MouseAction &action);
    void drawPeaks();
    void drawSelectedPeaks(const QVector<int> &selected);
    void deleteSelectedPeaks();
    void clearSelectedPeaks();
    void drawSelectedRef();
    void clearSelectedRef();
    void setSelectedRef(const QVector<int> &selected);
    void setSystematicAbsences(const QVector<int> &refIndex);
    void drawSystematicAbsences();
    void clearSystematicAbsences();
    void setLegendVisible(bool visible);
    void setCardPeaks(const QVector<CardPeakData> &peaks);
    void applyOffset(double yOffset = 0);
    void applyAutoScale();
    void enableRescalePlot(bool value);
    void connectLabels(QLabel *newLabel1, QLabel *newLabel2, QLabel *newLabel3);
    void connectHorizontalScrollBar(QScrollBar *newHorizontalScrollBar);
    void connectAddDeletePoints();
    void connectSelectionChanged();
    PlotSettings pSettings() const;
    void setPlotSettings(const PlotSettings &newPlotSettings);
    void drawGraphicItem(graphItem &item, const QVector<double> &xvet, const QVector<double> &yvet, double wave);
    void clearGraphSelection(graphItem &item);

    QVector<graphItem> obs;
    graphItem calc;
    graphItem back;
    graphItem bpoints;
    graphItem diff;
    graphItem cdiff;
    graphItem peaks;
    QVector<graphItem> refl;
    QVector<reflectionSet> refSet;
    QVector<double> plotWave;

signals:
    void deleteSelectedPeaksSignal(const QVector<int> &selected);
    void addDeletePointSignal(int action, double xp, double yp, int &ier);
    void fileDropped(const QStringList &fileList);

private slots:
    void myMoveEvent(QMouseEvent *event);
    void xAxisChanged(QCPRange range);
    void horzScrollBarChanged(int value);
    void addDeletePoint(QPoint mousePos);
    void selectionChanged();

protected:
    void dragEnterEvent(QDragEnterEvent *event) override;
    void dropEvent(QDropEvent *event) override;

private:
    QVector<graphItem> profCurves;
    graphItem smooth;
    graphItem undpeaks;
    graphItem sysAbs;
    graphItem selectedRef;
    graphItem selectedPeak;
    graphItem intervalLimit;
    int nObserved;
    int nReflections;
    int nProfCurves;    
    double yLowerRange, yUpperRange;
    double yLowerRangeNoRef;
    double mLastYOffset;
    bool rescalePlot;
    MouseAction mAction;
    MouseAction savedZoomAction;
    PlotSettings plotSettings;
    const int scrollBarLen = 1000000000; //Length of the scrollbar. It can be any large value.

    void makePlot(const QVector<double> &xvet, const QVector<double> &yvet, graphItem &item);
    void addObserved(const QVector<double> &xvet, const QVector<double> &yvet,
                     int visible, float wave, const QString &name);
    void getYLimits(double &yMin, double &yMax);
    void drawReflections(const QVector<double>& x, double y, double length, QPen pen, int &itemStart, int &itemEnd);
    void drawReflections(const QVector<refInfo> &ref, double y, double length, QPen pen, int &itemStart, int &itemEnd);
    void drawIntervals();
    void reDrawReflections(const QVector<refInfo> &refSet, const graphItem &ref, double y, double length);
    void drawGraphicItem_old(graphItem &item);
    bool rescalePlotEnabled() const;
    void makeReflections(const QVector<double> &xvet);
    int findRefLocation(const QVector<refInfo> &ref, double xval);
    QLabel *labelWidget1, *labelWidget2, *labelWidget3;
    QScrollBar *horizontalScrollBar;

    QVector<CardPeakData> m_cardPeaks;
    QVector<QCPGraph *>   m_cardPeakGraphs;
    void drawCardPeaks();
};

#endif // XPDVIEWWIDGET_H

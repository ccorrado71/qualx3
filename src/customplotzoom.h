#ifndef CUSTOMPLOTZOOM_H
#define CUSTOMPLOTZOOM_H

#include "qcustomplot.h"

class QRubberBand;
class QMouseEvent;
class QWidget;

class CustomPlotZoom : public QCustomPlot
{
    Q_OBJECT

public:
    enum ZoomMode {
        NoZoom,
        RectangleZoom,
        VerticalZoom,
        HorizontalZoom,
        PanZoom,
        AddDeletePoint
    };
    explicit CustomPlotZoom(QWidget *parent = nullptr);
    virtual ~CustomPlotZoom();  

    void setZoomMode(const ZoomMode &zoomMode);
    ZoomMode zoomMode() const;    
    void setDraggingLegend(bool value);
    void setLegendPosition(float x, float y);
    void getMinMax(double &min, double &max) const;
    bool updateMinMax();
    QPointF closestDataPoint(const QPoint &point, int id);
    double closestDataPointX(double xValue, int id);
    void createTracer(const QColor &color);
    void disableTracer();
    void attachTracerTo(QCPGraph *tgraph);
    void setDoubleCursorPosition(double lpos, double rpos);
    void deleteDoubleCursor();

signals:
    void addDeletePointAction(QPoint mousePos);
    void leftCursorPositionChanged(double lpos);
    void rightCursorPositionChanged(double lpos);
    void customPlotResized();

protected:
    void mousePressEvent(QMouseEvent *event);
    void mouseReleaseEvent(QMouseEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
    void resizeEvent(QResizeEvent *event);

private:    
    ZoomMode mZoomMode;
    QRubberBand *mRubberBand;
    QPoint mOrigin;
    double minRect;
    double maxRect;
    bool draggingLegendEnabled;
    bool draggingLegend;
    QPointF dragLegendOrigin;
    double minKey, maxKey;    
    QPointer<QCPItemTracer> tracer;
    bool tracerEnabled;    
    QPointer<QCPItemStraightLine> leftCursor;
    QPointer<QCPItemStraightLine> rightCursor;
    QPointer<QCPLayer> cursorLayer;
    bool leftCursorDrag;
    bool rightCursorDrag;
};

#endif // CUSTOMPLOTZOOM_H

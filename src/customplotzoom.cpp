#include "customplotzoom.h"
#include <float.h>

CustomPlotZoom::CustomPlotZoom(QWidget *parent)
    : QCustomPlot(parent)
    , mZoomMode(NoZoom)
    , mRubberBand(new QRubberBand(QRubberBand::Rectangle, this))
    , draggingLegendEnabled(false)
    , draggingLegend(false)
    , minKey(-1000000)
    , maxKey(1000000)
    , tracerEnabled(false)
    , leftCursorDrag(false)
    , rightCursorDrag(false)
{

}

CustomPlotZoom::~CustomPlotZoom()
{
    delete mRubberBand;
}

void CustomPlotZoom::setZoomMode(const ZoomMode &zoomMode)
{
    mZoomMode = zoomMode;
}

void CustomPlotZoom::mousePressEvent(QMouseEvent *event)
{
    // Zoom actions
    if (mZoomMode != NoZoom && mZoomMode != PanZoom && mZoomMode != AddDeletePoint)
    {
        if (event->button() == Qt::LeftButton)
        {
            if (mZoomMode == RectangleZoom)
            {
                mOrigin = event->pos();
            }
            else if (mZoomMode == HorizontalZoom)
            {
                QCPRange keyRange = axisRect()->axis(QCPAxis::atLeft)->range();
                minRect = axisRect()->axis(QCPAxis::atLeft)->coordToPixel(keyRange.lower);
                double max = axisRect()->axis(QCPAxis::atLeft)->coordToPixel(keyRange.upper);
                mOrigin = QPoint(event->pos().x(),max);
            }
            else if (mZoomMode == VerticalZoom)
            {
                QCPRange keyRange = axisRect()->axis(QCPAxis::atBottom)->range();
                double min = axisRect()->axis(QCPAxis::atBottom)->coordToPixel(keyRange.lower);
                maxRect = axisRect()->axis(QCPAxis::atBottom)->coordToPixel(keyRange.upper);
                mOrigin = QPoint(min,event->pos().y());
            }

            mRubberBand->setGeometry(QRect(mOrigin,QSize()));
            mRubberBand->show();
            setCursor(Qt::CrossCursor);
        }
    }

    // Legend
    if (mZoomMode == NoZoom) {
        if (draggingLegendEnabled) {
            if (legend->selectTest(event->pos(), false) > 0) {
                draggingLegend = true;
                // since insetRect is in axisRect coordinates (0..1), we transform the mouse position:
                QPointF mousePoint((event->pos().x()-axisRect()->left())/(double)axisRect()->width(),
                                   (event->pos().y()-axisRect()->top())/(double)axisRect()->height());
                dragLegendOrigin = mousePoint-axisRect()->insetLayout()->insetRect(0).topLeft();
                setCursor(Qt::SizeAllCursor);
            }
        }
    }

    //Cursors
    if (leftCursor) {
        leftCursorDrag = false;
        double distance = leftCursor->selectTest(event->pos(), false);
        if (distance <= 5 && axisRect()->rect().contains(event->pos())) {
            leftCursorDrag = true;
        }
    }
    if (rightCursor) {
        rightCursorDrag = false;
        double distance = rightCursor->selectTest(event->pos(), false);
        if (distance <= 5 && axisRect()->rect().contains(event->pos())) {
            rightCursorDrag = true;
        }
    }

    if (mZoomMode == PanZoom && event->button() == Qt::LeftButton) {
        setCursor(Qt::OpenHandCursor);
    }

    if (mZoomMode == AddDeletePoint && event->button() == Qt::LeftButton) {
        if (axisRect()->rect().contains(event->pos())) {
            emit addDeletePointAction(event->pos());
        }
    }

    QCustomPlot::mousePressEvent(event);
}

void CustomPlotZoom::mouseMoveEvent(QMouseEvent *event)
{
    // Zoom actions
    if (mRubberBand->isVisible())
    {
        if (mZoomMode == RectangleZoom)
        {
            mRubberBand->setGeometry(QRect(mOrigin,event->pos()).normalized());
        }
        else if (mZoomMode == HorizontalZoom)
        {
            mRubberBand->setGeometry(QRect(mOrigin,QPoint(event->pos().x(),minRect)).normalized());
        }
        else if (mZoomMode == VerticalZoom)
        {
            mRubberBand->setGeometry(QRect(mOrigin,QPoint(maxRect,event->pos().y())).normalized());
        }
    }

    // Legend
    if (draggingLegend)
    {
        QRectF rect = axisRect()->insetLayout()->insetRect(0);
        // since insetRect is in axisRect coordinates (0..1), we transform the mouse position:
        QPointF mousePoint((event->pos().x()-axisRect()->left())/(double)axisRect()->width(),
                           (event->pos().y()-axisRect()->top())/(double)axisRect()->height());
        rect.moveTopLeft(mousePoint-dragLegendOrigin);
        axisRect()->insetLayout()->setInsetRect(0, rect);
        replot();
    }

    // Tracer
    if (tracerEnabled) {
        double x = xAxis->pixelToCoord(event->pos().x());
        tracer->setGraphKey(x);
        tracer->setInterpolating(true);
        tracer->updatePosition();
        replot();
    }

    //Cursors
    if (leftCursorDrag || rightCursorDrag) {
        double pixelx = event->pos().x();
        QCPRange keyRange = axisRect()->axis(QCPAxis::atBottom)->range();
        double min = axisRect()->axis(QCPAxis::atBottom)->coordToPixel(keyRange.lower);
        double max = axisRect()->axis(QCPAxis::atBottom)->coordToPixel(keyRange.upper);

        if (pixelx < min ) {
            pixelx = min;
        } else if (pixelx > max) {
            pixelx = max;
        }

        if (leftCursorDrag) {
            double rcursor = rightCursor->point1->key();
            double rcursorx = axisRect()->axis(QCPAxis::atBottom)->coordToPixel(rcursor);

            if (pixelx > rcursorx - 1) {
                pixelx = rcursorx - 1;
            }

            double lpos = xAxis->pixelToCoord(pixelx);
            leftCursor->point1->setCoords(lpos,0);
            leftCursor->point2->setCoords(lpos,1);
            leftCursorPositionChanged(lpos);
        }

        if (rightCursorDrag) {
            double lcursor = leftCursor->point1->key();
            double lcursorx = axisRect()->axis(QCPAxis::atBottom)->coordToPixel(lcursor);

            if (pixelx < lcursorx + 1) {
                pixelx = lcursorx + 1;
            }

            double rpos = xAxis->pixelToCoord(pixelx);
            rightCursor->point1->setCoords(rpos,0);
            rightCursor->point2->setCoords(rpos,1);
            rightCursorPositionChanged(rpos);
        }

        layer("cursorLayer")->replot();
    }

    QCustomPlot::mouseMoveEvent(event);
}

void CustomPlotZoom::resizeEvent(QResizeEvent *event)
{
    emit customPlotResized();
    QCustomPlot::resizeEvent(event);
}

void CustomPlotZoom::setDraggingLegend(bool value)
{
    draggingLegendEnabled = value;
    if (value) {
        // set the placement of the legend (index 0 in the axis rect's inset layout) to not be
        // border-aligned (default), but freely, so we can reposition it anywhere:
        axisRect()->insetLayout()->setInsetPlacement(0, QCPLayoutInset::ipFree);
        legend->setMaximumSize(legend->minimumOuterSizeHint());
    } else {
        axisRect()->insetLayout()->setInsetPlacement(0, QCPLayoutInset::ipBorderAligned);
    }    
}

void CustomPlotZoom::setLegendPosition(float x, float y)
{
    QRectF rect = axisRect()->insetLayout()->insetRect(0);
    rect.moveTopRight(QPointF(x,y));
    axisRect()->insetLayout()->setInsetRect(0, rect);
}

void CustomPlotZoom::getMinMax(double &min, double &max) const
{
    min = minKey;
    max = maxKey;
}

bool CustomPlotZoom::updateMinMax()
{
    double min = DBL_MAX;
    double max  = -DBL_MIN;
//    QCPRange keyrange = axisRect()->axis(QCPAxis::atBottom)->range();
//    double min = keyrange.lower;
//    double max = keyrange.upper;
    for (int i = 0; i < graphCount(); i++) {
        if (!graph(i)->data()->isEmpty()) {
            if (min > graph(i)->data()->at(0)->key) min = graph(i)->data()->at(0)->key;
            auto nc = graph(i)->data()->size();
            if (max < graph(i)->data()->at(nc-1)->key) max = graph(i)->data()->at(nc-1)->key;
        }
    }
    if (min < max) {
        minKey = min;
        maxKey = max;
    }
    return min < max;
}

QPointF CustomPlotZoom::closestDataPoint(const QPoint &point, int id)
{
    if (id < 0 || id >= graphCount()) return QPointF();

    QCPGraphDataContainer::const_iterator it; // = graph(id)->data()->constEnd();
    QVariant details;
    if (graph(id)->selectTest(point, false, &details)) {
        QCPDataSelection dataPoints = details.value<QCPDataSelection>();
        if (dataPoints.dataPointCount() > 0) {
            it = graph(id)->data()->at(dataPoints.dataRange().begin());
            return QPointF(it->key,it->value);
        }
    }

    return QPointF();
}

double CustomPlotZoom::closestDataPointX(double xValue, int id)
{
    if (id < 0 || id >= graphCount()) return -1;

    //Binary search
    int j,ju,jm,jl;
    bool ascnd;

    int n=graph(id)->data()->size();
    jl=-1;
    ju=n;
    ascnd=(graph(id)->data()->at(n-1)->key >= graph(id)->data()->at(0)->key);
    while (ju-jl > 1) {
        jm=(ju+jl) >> 1;
        if (xValue >= graph(id)->data()->at(jm)->key == ascnd)
            jl=jm;
        else
            ju=jm;
    }
    if (xValue == graph(id)->data()->at(0)->key) j=0;
    else if (xValue == graph(id)->data()->at(n-1)->key) j=n-2;
    else j=jl;

    //Locate the closest value
    if (j == -1) return graph(id)->data()->at(0)->key; // out of range to the left
    if (j == n - 1) return graph(id)->data()->at(j)->key; // out of range to the right
    if (abs(xValue - graph(id)->data()->at(j)->key) < abs(xValue - graph(id)->data()->at(j+1)->key)) return graph(id)->data()->at(j)->key;   // xx[j] < x < xx[j+1]
    return graph(id)->data()->at(++j)->key;
}

CustomPlotZoom::ZoomMode CustomPlotZoom::zoomMode() const
{
    return mZoomMode;
}

void CustomPlotZoom::mouseReleaseEvent(QMouseEvent *event)
{
    if (mRubberBand->isVisible())
    {
        mRubberBand->hide();
        unsetCursor();
        const QRect zoomRect = mRubberBand->geometry();        
        if (zoomRect.width() > 3 && zoomRect.height() > 3) {
            int xp1, yp1, xp2, yp2;
            zoomRect.getCoords(&xp1,&yp1,&xp2,&yp2);
            bool outsideRange = false;
            if (mZoomMode == RectangleZoom) {
                double x1, x2, y1, y2;
                if (xAxis->rangeReversed()) {
                    x1 = xAxis->pixelToCoord(xp2);
                    x2 = xAxis->pixelToCoord(xp1);
                    y1 = yAxis->pixelToCoord(yp1);
                    y2 = yAxis->pixelToCoord(yp2);
                } else {
                    x1 = xAxis->pixelToCoord(xp1);
                    x2 = xAxis->pixelToCoord(xp2);
                    y1 = yAxis->pixelToCoord(yp1);
                    y2 = yAxis->pixelToCoord(yp2);
                }
                outsideRange = x2 < xAxis->range().lower || x1 > xAxis->range().upper ||
                        y1 < yAxis->range().lower || y2 > yAxis->range().upper;
                qInfo() << "OUT: " << x1 << x2 << y1 << y2 << yAxis->range().lower << yAxis->range().upper << outsideRange ;
                qInfo() << "1: " << (x2 < xAxis->range().lower);
                qInfo() << "2: " << (x1 > xAxis->range().upper);
                qInfo() << "3: " << (y1 < yAxis->range().lower);
                qInfo() << "4: " << (y2 > yAxis->range().upper);
                if (!outsideRange) {
                    xAxis->setRange(x1, x2);
                    yAxis->setRange(y1, y2);
                }
            }
            else if (mZoomMode == HorizontalZoom)
            {
                double x1, x2;
                if (xAxis->rangeReversed()) {
                    x1 = xAxis->pixelToCoord(xp2);
                    x2 = xAxis->pixelToCoord(xp1);
                } else {
                    x1 = xAxis->pixelToCoord(xp1);
                    x2 = xAxis->pixelToCoord(xp2);
                }
                outsideRange = x2 < xAxis->range().lower || x1 > xAxis->range().upper;                
                if (!outsideRange) xAxis->setRange(x1, x2);
            }
            else if (mZoomMode == VerticalZoom)
            {
                double y1 = yAxis->pixelToCoord(yp1);
                double y2 = yAxis->pixelToCoord(yp2);
                outsideRange = y1 < yAxis->range().lower || y2 > yAxis->range().upper;
                if (!outsideRange) yAxis->setRange(y1, y2);
            }
            if (!outsideRange) replot();
        }
    }

    // Legend
    if (draggingLegend) {
        draggingLegend = false;
        unsetCursor();
    }

    // Cursors
    if (leftCursorDrag) {
        leftCursorDrag = false;
    }
    if (rightCursorDrag) {
        rightCursorDrag = false;
    }

    if (mZoomMode == PanZoom) {
        unsetCursor();
    }
    QCustomPlot::mouseReleaseEvent(event);
}

void CustomPlotZoom::createTracer(const QColor &color)
{
    if (!tracer) tracer = new QCPItemTracer(this);
    tracer->setPen(QPen(color));
    tracer->setBrush(QBrush(color));
    tracer->setStyle(QCPItemTracer::tsCrosshair);
    tracer->setSize(20.0);
    tracer->setVisible(true);
    tracerEnabled = false;
}

void CustomPlotZoom::disableTracer()
{    
    if (tracer && tracerEnabled) {
        tracer->setVisible(false);
        replot();
        tracerEnabled = false;
    }
}

void CustomPlotZoom::attachTracerTo(QCPGraph *tgraph)
{
    if (tracer) {
        tracer->setGraph(tgraph);
        tracerEnabled = true;
    }
}

void CustomPlotZoom::setDoubleCursorPosition(double lpos, double rpos)
{
    if (!cursorLayer) {
        addLayer("cursorLayer", 0, QCustomPlot::limAbove);
        cursorLayer = this->layer("cursorLayer");
    }

    if (!leftCursor) {
        leftCursor = new QCPItemStraightLine(this);
        leftCursor->setPen(QPen(Qt::red));
        leftCursor->setLayer("cursorLayer");
    }
    if (!rightCursor) {
        rightCursor = new QCPItemStraightLine(this);
        rightCursor->setPen(QPen(Qt::red));
        rightCursor->setLayer("cursorLayer");
    }

    leftCursor->point1->setCoords(lpos,0);
    leftCursor->point2->setCoords(lpos,1);
    rightCursor->point1->setCoords(rpos,0);
    rightCursor->point2->setCoords(rpos,1);

    layer("cursorLayer")->replot();    
}

void CustomPlotZoom::deleteDoubleCursor()
{
    if (leftCursor) removeItem(leftCursor);
    if (rightCursor) removeItem(rightCursor);
    if (cursorLayer) {
        layer("cursorLayer")->replot();
        removeLayer(cursorLayer);
    }
    leftCursorDrag = false;
    rightCursorDrag = false;
}

#include "xpdviewwidget.h"
#include "libcomune.h"
#include "xpdutils.h"
#include "nr.h"

#include <cfloat>
#include <QLabel>

XpdViewWidget::XpdViewWidget(QWidget *parent)
    : CustomPlotZoom(parent)
    , nObserved(0)
    , yLowerRange(DBL_MAX)
    , yUpperRange(-DBL_MAX)
    , yLowerRangeNoRef(0.0)
    , mLastYOffset(0)
    , rescalePlot(true)
    , mAction(NoAction)
    , savedZoomAction(mAction)
{
    //hide empty plot
    axisRect(0)->setVisible(false);
    axisRect(0)->setVisible(false);

    // TODO
    // remove parameter idcolor from setColorLine
    // fix default idcolor in setColor (ex. 3 for back and bpoints)
    // setDefaultLine and scatter with no par, default idcolor shoud be written in graphItem.cpp
    calc.setGtype(graphItem::Calculated);
    calc.setDefaultIDColor(2);
    calc.setColorLine();
    calc.setName();
    back.setGtype(graphItem::Background);
    back.setDefaultIDColor(3);
    back.setColorLine();
    back.setName();
    bpoints.setGtype(graphItem::Background_Points);
    bpoints.setDefaultIDColor(3);
    bpoints.setColorLine();
    bpoints.setName();
    bpoints.setLineStyle(Qt::NoPen);
    bpoints.setLineConnectionType(QCPGraph::lsNone);
    diff.setGtype(graphItem::Difference);
    diff.setDefaultIDColor(4);
    diff.setColorLine();
    diff.setName();
    cdiff.setGtype(graphItem::Cumulative);
    cdiff.setDefaultIDColor(5);
    cdiff.setColorLine();
    cdiff.setName();
    peaks.setGtype(graphItem::Peaks);
    peaks.setDefaultIDColor(6);
    peaks.setColorLine();
    peaks.setName();
    peaks.setLineConnectionType(QCPGraph::lsImpulse);
    smooth.setGtype(graphItem::Smoothing);
    smooth.setName();
    smooth.setPen(QPen(Qt::green,1,Qt::DashLine));
    undpeaks.setGtype(graphItem::Unindexed_Peaks);
    undpeaks.setName();
    QColor colorUndPeaks = peaks.getPen().color();
    QCPScatterStyle scatter(QCPScatterStyle::ssTriangleInverted,colorUndPeaks,colorUndPeaks,10);
    undpeaks.setScatter(scatter);
    undpeaks.setLineStyle(Qt::NoPen);
    undpeaks.setLineConnectionType(QCPGraph::lsNone);
    sysAbs.setGtype(graphItem::Systematic_Absences);
    sysAbs.setName();
    sysAbs.setLineStyle(Qt::NoPen);
    sysAbs.setLineConnectionType(QCPGraph::lsNone);
    selectedRef.setGtype(graphItem::Selected_Reflections);
    selectedRef.setName();
    selectedRef.setLineStyle(Qt::NoPen);
    selectedRef.setLineConnectionType(QCPGraph::lsNone);
    selectedPeak.setGtype(graphItem::Selected_Peaks);
    selectedPeak.setName();
    selectedPeak.setLineStyle(Qt::NoPen);
    selectedPeak.setLineConnectionType(QCPGraph::lsNone);
    intervalLimit.setGtype(graphItem::Intervals);
    intervalLimit.setName();

    setAcceptDrops(true);

    plotSettings.read();
}

void XpdViewWidget::setGraphicArea()
{
    disableTracer();
    clearGraphs();
    clearPlottables();
    clearItems();

    plotSettings.applyTo(this);

    axisRect(0)->setVisible(true);

    xAxis2->setVisible(true);
    xAxis2->setTickLabels(false);
    xAxis2->setTicks(false);

    yAxis2->setVisible(true);
    yAxis2->setTickLabels(false);
    yAxis2->setTicks(false);    
    yAxis->grid()->setZeroLinePen(Qt::NoPen); // hide zero-line

    nObserved = 0;
    nReflections = 0;
    refl.clear();
    refSet.clear();
    nProfCurves = 0;
    yLowerRange = DBL_MAX;
    yUpperRange = -DBL_MIN;
    for (int i = 0; i < obs.count(); i++) obs[i].setGraphIndex(-1);
    for (int i = 0; i < profCurves.count(); i++) profCurves[i].setGraphIndex(-1);
    back.setGraphIndex(-1);
    bpoints.setGraphIndex(-1);
    calc.setGraphIndex(-1);
    diff.setGraphIndex(-1);
    cdiff.setGraphIndex(-1);
    //for (int i = 0; i < refl.count(); i++) refl[i].setGraphIndex(-1);
    peaks.setGraphIndex(-1);
    smooth.setGraphIndex(-1);
    undpeaks.setGraphIndex(-1);
    sysAbs.setGraphIndex(-1);
    sysAbs.setVisible(false);
    selectedRef.setGraphIndex(-1);
    selectedPeak.setGraphIndex(-1);
    intervalLimit.setGraphIndex(-1);
    intervalLimit.setVisible(false);
    plotWave.clear();
}

void XpdViewWidget::makePlot(const QVector<double> &xvet, const QVector<double> &yvet, graphItem &item)
{
    double ymin = Minimo(yvet);
    double ymax = Massimo(yvet);
    item.setMin(ymin);
    item.setMax(ymax);
    item.setGraphIndex(graphCount());
    plotWave.append(item.wave);
    addGraph();
    graph()->setName(item.getName());
    graph()->setData(xvet,yvet,false);
    graph()->setPen(item.getPen());
    graph()->setLineStyle(item.getLineConnectionType());
    graph()->setScatterStyle(item.getScatter());
}

void XpdViewWidget::addPlot(const QVector<double> &xvet0, const QVector<double> &yvet0, graphItem::ItemType type, int visible, float wave, const QString &name)
{
    QVector<double> xvet = xvet0;
    QVector<double> yvet = yvet0;
    if (plotSettings.getAbscissa() == xpdutils::DVALUE) {
        xpdutils::tthetaToD(xvet,yvet,wave);
        xAxis->setRangeReversed(true);
    } else {
        xAxis->setRangeReversed(false);
    }

    switch (type) {
    case graphItem::Observed:
        addObserved(xvet,yvet,visible,wave,name);
        break;
    case graphItem::Calculated:
        calc.setVisible(visible);
        calc.wave = wave;
        makePlot(xvet,yvet,calc);
        break;

    case graphItem::Background:
        back.setVisible(visible);
        back.wave = wave;
        makePlot(xvet,yvet,back);
        break;

    case graphItem::Background_Points:
        bpoints.setVisible(visible);
        bpoints.wave = wave;
        makePlot(xvet,yvet,bpoints);
        break;

    case graphItem::Difference:
        diff.setVisible(visible);
        diff.wave = wave;
        makePlot(xvet,yvet,diff);
        break;

    case graphItem::Cumulative:
        cdiff.setVisible(visible);
        cdiff.wave = wave;
        makePlot(xvet,yvet,cdiff);
        break;

    case graphItem::Peaks:
        peaks.setVisible(visible);
        peaks.wave = wave;
        makePlot(xvet,yvet,peaks);
        break;

    case graphItem::Smoothing:
        smooth.setVisible(visible);
        smooth.wave = wave;
        makePlot(xvet,yvet,smooth);
        break;

    case graphItem::Unindexed_Peaks:
        undpeaks.setVisible(visible);
        undpeaks.wave = wave;
        makePlot(xvet,yvet,undpeaks);
        graph()->setScatterStyle(undpeaks.getScatter());
        enableRescalePlot(false);
        break;

    case graphItem::Profile_Curves:
        if (profCurves.size() <= nProfCurves) { //new graphic element required
            graphItem item;
            item.setColorLine(nProfCurves);
            item.setName(name);
            item.wave = wave;
            profCurves.push_back(item);
        }
        profCurves[nProfCurves].setData(xvet,yvet);
        profCurves[nProfCurves].setVisible(visible);
        makePlot(xvet,yvet,profCurves[nProfCurves]);
        graph()->setScatterStyle(profCurves[nProfCurves].getScatter());
        graph()->removeFromLegend();
        nProfCurves++;
        break;

    default:
        break;
    }
}

void XpdViewWidget::addPlot(const QVector<double> &xvet0, graphItem::ItemType type, int visible, float wave)
{
    // QVector<double> xvet = xvet0;
    // if (type == graphItem::Reflections) {
    //     if (refl.size() <= nReflections) { //new graphic element required
    //         graphItem item;
    //         item.setGtype(graphItem::Reflections);
    //         item.setColorLine(nReflections);
    //         item.setVisible(visible);
    //         item.wave = wave;
    //         refl.push_back(item);
    //     }
    //     refl[nReflections].setX(xvet);

    //     //Default Name: for more phases use Phase 1, Phase 2, ...
    //     if (nReflections == 0) {
    //         refl[0].setName();
    //     } else if (nReflections == 1) {
    //         refl[0].setName("Phase "+QString::number(1));
    //         refl[1].setName("Phase "+QString::number(2));
    //     } else {
    //         refl[nReflections].setName("Phase "+QString::number(nReflections+1));
    //     }
    //     //makeReflections(xvet);
    //     nReflections++;
    // } else if (type == graphItem::Intervals) {
    if (type == graphItem::Intervals) {
        QVector<double> xvet = xvet0;
        if (plotSettings.getAbscissa() == xpdutils::DVALUE) {
            xvet = xpdutils::dvalue(xvet,wave);
        }
        intervalLimit.setVisible(visible);
        intervalLimit.wave = wave;
        intervalLimit.setX(xvet);
    }
}

void XpdViewWidget::addPlot(float xvet[], float yvet[], int num, graphItem::ItemType type, int visible, float wave, const QString &name)
{
    QVector<double> xv(num);
    QVector<double> yv(num);
    for (int i = 0; i < num; i++) {
        xv[i] = xvet[i];
        yv[i] = yvet[i];
    }

    addPlot(xv,yv,type,visible,wave,name);
}

void XpdViewWidget::addPlot(float xvet[], int num, graphItem::ItemType type, int visible, float wave)
{
    QVector<double> xv(num);
    for (int i = 0; i < num; i++) {
        xv[i] = xvet[i];
    }

    addPlot(xv,type,visible,wave);
}

void XpdViewWidget::addReflections(float xvet[], int h[], int k[], int l[], int num, int visible, float wave)
{
//Print Reflections
    // for (int i = 0; i < num; i++) {
    //     qInfo() << "Reflection: " << xvet[i] << xpdutils::dvalue(xvet[i], wave) << h[i] << k[i] << l[i];
    // }

    //Fill refSet
    reflectionSet reflS;
    reflS.visible = visible;
    reflS.wave = wave;
    for (int i = 0; i < num; i++) {
        refInfo r;
        r.x = xvet[i];
        r.hkl[0] = h[i];
        r.hkl[1] = k[i];
        r.hkl[2] = l[i];
        reflS.ref.push_back(r);
    }
    if (plotSettings.getAbscissa() == xpdutils::DVALUE) {
        for (int i = 0; i < reflS.ref.size(); i++) {
            reflS.ref[i].x = xpdutils::dvalue(reflS.ref[i].x, wave);
        }
    }

    if (refl.size() <= nReflections) { //new graphic element required
        graphItem item;
        item.setGtype(graphItem::Reflections);
        item.setColorLine(nReflections);
        item.setVisible(visible);
        item.wave = wave;
        refl.push_back(item);
        refSet.push_back(reflS);
    } else {
        refSet[nReflections] = reflS;
    }

    // QVector<double> xv(num);
    // for (int i = 0; i < num; i++) {
    //     xv[i] = xvet[i];
    // }
    // refl[nReflections].setX(xv);

    //Default Name: for more phases use Phase 1, Phase 2, ...
    if (nReflections == 0) {
        refl[0].setName();
    } else if (nReflections == 1) {
        refl[0].setName("Phase "+QString::number(1));
        refl[1].setName("Phase "+QString::number(2));
    } else {
        refl[nReflections].setName("Phase "+QString::number(nReflections+1));
    }
    //makeReflections(xvet);
    nReflections++;
}

void XpdViewWidget::drawPlot()
{
    getYLimits(yLowerRangeNoRef, yUpperRange);

    //Compute space for reflections
    double spaceRef = (yUpperRange - yLowerRangeNoRef) * 0.04; //space for single set of reflections
    double lengthRef = spaceRef * 0.75; // length of the bar
    int nVisibleRef = 0;
    for (int i = 0; i < refl.size(); ++i) {
        if (refl.at(i).isVisible()) nVisibleRef++;
    }
    yLowerRange = yLowerRangeNoRef - nVisibleRef*spaceRef; // make space for bars

    //Draw Reflections
    for (int i = 0; i < refl.size(); i++) {
        QPen mPen(refl[i].getPen());
        refl[i].setGraphIndex(graphCount());
        plotWave.append(refl[i].wave);
        addGraph();
        graph()->setPen(mPen);
        graph()->setName(refl[i].getName());
        double ypos = yLowerRange + (refl.size()-i-1)*spaceRef;
        //drawReflections(refl[i].getX(),ypos,lengthRef,mPen, refl[i].itemIndexStart, refl[i].itemIndexEnd);
        drawReflections(refSet[i].ref,ypos,lengthRef,mPen, refl[i].itemIndexStart, refl[i].itemIndexEnd);
        refl[i].setYPos(ypos);
        refl[i].setLengthRef(lengthRef);

        //Create custom scatter for legend
        QPainterPath customScatterPath(QPointF(0,-10));
        customScatterPath.lineTo(0,10);
        graph()->setScatterStyle(QCPScatterStyle(customScatterPath, mPen));
        graph()->setLineStyle(QCPGraph::lsNone);
    }

    //Draw Intervals
    drawIntervals();

    if (rescalePlotEnabled()) {
        yAxis->setRange(yLowerRange,yUpperRange);
        updateMinMax();
        xAxis->rescale();
    } else {
        if (refl.size() > 0) yAxis->setRange(yLowerRange,yUpperRange);
    }

    setDraggingLegend(true);
    setLegendPosition(1.0,0.0);

    replot();
}

void XpdViewWidget::redrawPlot(bool computeLimits)
{
    if (computeLimits) getYLimits(yLowerRangeNoRef, yUpperRange);

    //Compute space for reflections
    double spaceRef = (yUpperRange - yLowerRangeNoRef) * 0.04; //space for single set of reflections
    //FIX: problem with log scale for negative value e long bar
    //    spaceRef = spaceRef/100;
    double lengthRef = spaceRef * 0.75; // length of the bar
    int nVisibleRef = 0;
    for (int i = 0; i < refl.size(); ++i) {
        if (refl.at(i).isVisible()) nVisibleRef++;
    }
    //yLowerRange -= nVisibleRef*spaceRef; // make space for bars
    yLowerRange = yLowerRangeNoRef - nVisibleRef*spaceRef; // make space for bars
    //qInfo() << "LOW: " << yLowerRange << yLowerRangeNoRef << spaceRef;

    //Redraw reflections
    int iVis = -1;
    for (int i = 0; i < refl.size(); i++) {
        if (refl.at(i).isVisible()) {
            ++iVis;
            double ypos = yLowerRange + (nVisibleRef-iVis-1)*spaceRef;
            reDrawReflections(refSet[i].ref,refl[i],ypos,lengthRef);
            refl[i].setYPos(ypos);
            refl[i].setLengthRef(lengthRef);
        }
    }

    yAxis->setRange(yLowerRange,yUpperRange);
    updateMinMax();

    replot();
}

void XpdViewWidget::setAction(const MouseAction &action)
{
    mAction = action;
    switch (action) {
    case NoZoom:
        setZoomMode(CustomPlotZoom::NoZoom);
        setInteractions(QCP::iRangeZoom | QCP::iSelectLegend | QCP::iSelectPlottables);
        axisRects().at(0)->setRangeZoom(Qt::Horizontal);
        unsetCursor();
        disableTracer();
        savedZoomAction = action;
        break;
    case RectangleZoom:
        setZoomMode(CustomPlotZoom::RectangleZoom);
        setInteractions(QCP::iRangeZoom);
        unsetCursor();
        disableTracer();
        savedZoomAction = action;
        break;
    case HorizontalZoom:
        setZoomMode(CustomPlotZoom::HorizontalZoom);
        setInteractions(QCP::iRangeZoom);
        axisRects().at(0)->setRangeZoom(Qt::Horizontal);
        unsetCursor();
        disableTracer();
        savedZoomAction = action;
        break;
    case Pan:
        setZoomMode(CustomPlotZoom::PanZoom);
        setInteractions(QCP::iRangeDrag | QCP::iRangeZoom);
        axisRects().at(0)->setRangeZoom(Qt::Horizontal);
        axisRects().at(0)->setRangeDrag(Qt::Horizontal);
        unsetCursor();
        disableTracer();
        savedZoomAction = action;
        break;
    case AddBackgroundPoint:
    case DeleteBackgroundPoint:
    case AddPeak:
    case DeletePeak:
        setZoomMode(CustomPlotZoom::AddDeletePoint);
        setInteractions(QCP::iRangeZoom);
        axisRects().at(0)->setRangeZoom(Qt::Horizontal);
        setCursor(Qt::CrossCursor);
        if (action == AddPeak || action == DeletePeak) {
            createTracer(peaks.getPen().color());
            attachTracerTo(graph(0));
        } else {
            disableTracer();
        }
        break;
    default:
        break;
    }
}

// void XpdViewWidget::drawPeaks()
// {
//     drawGraphicItem_old(peaks);
// }

void XpdViewWidget::drawSelectedPeaks(const QVector<int> &selected)
{
    if (selected.size() == 0) return;
    int idPeak = peaks.getGraphIndex();
    if (idPeak < 0) return;

    if (selectedPeak.getGraphIndex() < 0) {
        selectedPeak.setGraphIndex(graphCount());
        plotWave.append(-1);
        addGraph();
        graph()->setName(selectedPeak.getName());
        graph()->setPen(selectedPeak.getPen());
        QColor peakColor = peaks.getPen().color();
        QCPScatterStyle scatter = QCPScatterStyle(QCPScatterStyle::ssTriangleInverted,peakColor,peakColor,8);
        selectedPeak.setScatter(scatter);
        graph()->setScatterStyle(selectedPeak.getScatter());
    }

    selectedPeak.setX(selected);
    QVector<double> x(selected.size()), y(selected.size());
    for (int i = 0; i < selected.size(); i++) {
        x[i] = graph(idPeak)->data()->at(selected.at(i))->key;
        y[i] = graph(idPeak)->data()->at(selected.at(i))->value;
    }

    int id = selectedPeak.getGraphIndex();
    graph(id)->setData(x,y);

    //force resize of legend
    legend->setMaximumSize(legend->minimumOuterSizeHint());

    replot();
}

void XpdViewWidget::deleteSelectedPeaks()
{
    if (selectedPeak.ixSize() == 0) return;

    emit deleteSelectedPeaksSignal(selectedPeak.getIx());

    int idPeak = peaks.getGraphIndex();

    //Delete peaks from graph
    QVector<double> keys;
    for (int i = 0; i < selectedPeak.ixSize(); i++) {
        auto id = selectedPeak.getIx().at(i);
        keys.append(graph(idPeak)->data()->at(id)->key);
        // double key = graph(idPeak)->data()->at(id)->key;
        // qInfo() << "Delete: " << key << id;
        // graph(idPeak)->data()->remove(key);
    }

    //Delete peaks from graph
    for (int i = 0; i < keys.size(); i++) {
        graph(idPeak)->data()->remove(keys.at(i));
    }

    clearSelectedPeaks();
}

void XpdViewWidget::clearSelectedPeaks()
{
    clearGraphSelection(selectedPeak);
}

void XpdViewWidget::drawSelectedRef()
{
    //TOFIX: combine with setSelectedRef in one function
    if (selectedRef.xSize() > 0) {
        if (selectedRef.getGraphIndex() < 0) {
            selectedRef.setGraphIndex(graphCount());
            plotWave.append(-1);
            addGraph();
            graph()->setName(selectedRef.getName());
            graph()->setPen(selectedRef.getPen());
            QColor refColor = refl.at(0).getPen().color();
            QCPScatterStyle scatter = QCPScatterStyle(QCPScatterStyle::ssTriangleInverted,refColor,refColor,8);
            selectedRef.setScatter(scatter);
            graph()->setScatterStyle(selectedRef.getScatter());
        }
        int id = selectedRef.getGraphIndex();
        double ypos = refl.at(0).getYPos() + refl.at(0).getLengthRef();
        graph(id)->setData(selectedRef.getX(),QVector<double>(selectedRef.xSize(),ypos));

        replot();
    }
}

void XpdViewWidget::clearSelectedRef()
{
    clearGraphSelection(selectedRef);
}

void XpdViewWidget::setSystematicAbsences(const QVector<int> &refIndex)
{
    if (refl.size() == 0) return;

    QVector<double> x(refIndex.size());
    for (int i = 0; i < refIndex.size(); i++) {
        //x[i] = refl[0].getX(refIndex.at(i));
        x[i] = refSet[0].ref[refIndex.at(i)].x;
    }
    sysAbs.setX(x);
}

void XpdViewWidget::drawSystematicAbsences()
{
    if (refl.size() == 0) return;

    if (sysAbs.getGraphIndex() < 0) {
        sysAbs.setGraphIndex(graphCount());
        plotWave.append(-1);
        addGraph();
        graph()->setName("Systematic Absences");
        graph()->setPen(sysAbs.getPen());
        QColor refColor = refl.at(0).getPen().color();
        QCPScatterStyle scatter = QCPScatterStyle(QCPScatterStyle::ssDiamond,refColor,refColor,8);
        sysAbs.setScatter(scatter);
        graph()->setScatterStyle(sysAbs.getScatter());
        legend->setMaximumSize(legend->minimumOuterSizeHint()); //force resize of legend
    }
    int id = sysAbs.getGraphIndex();
    double ypos = refl.at(0).getYPos() + refl.at(0).getLengthRef()/2.0;
    graph(id)->setData(sysAbs.getX(),QVector<double>(sysAbs.xSize(),ypos));

    replot();
}

void XpdViewWidget::clearSystematicAbsences()
{
    int id = sysAbs.getGraphIndex();
    if (id < 0) return;

    removeGraph(id);
    replot();
    sysAbs.setGraphIndex(-1);
}

void XpdViewWidget::connectLabels(QLabel *newLabel1, QLabel *newLabel2, QLabel *newLabel3)
{
    labelWidget1 = newLabel1;
    labelWidget2 = newLabel2;
    labelWidget3 = newLabel3;
    connect(this, &CustomPlotZoom::mouseMove, this, &XpdViewWidget::myMoveEvent);
}

void XpdViewWidget::myMoveEvent(QMouseEvent *event)
{
    double x_val = xAxis->pixelToCoord(event->pos().x());
    double y_val = yAxis->pixelToCoord(event->pos().y());

    double x_end = xAxis->range().upper;

    int graphIndex = -1;
    int indexMin = 0;
    double distMin = DBL_MAX;

    //Traverse each curve, finding the closest point to the cursor point and the corresponding curve index
    for (int i = 0; i < xAxis->graphs().count(); i++) {
        if (!graph(i)->data()->isEmpty()) {
            //Find a key value index closest to the curve by x value
            int index = 0;
            if (x_val > x_end) {
                index = graph(i)->data()->size() - 1;
            } else {
                int index_left = graph(i)->findBegin(x_val, true);
                int index_right = index_left + 1; //ui->plot->graph(0)->findEnd(x_val, true);
                double dif_left = abs(graph(i)->data()->at(index_left)->key - x_val);
                double dif_right = abs(graph(i)->data()->at(index_right)->key - x_val);
                if (dif_left < dif_right)
                    index = index_left;
                else
                    index = index_right;
            }

            //Compute distance from the cursor position
            double dx = graph(i)->data()->at(index)->key - x_val;
            double dy = graph(i)->data()->at(index)->value - y_val;
            double dist = dx*dx + dy*dy;
            if (dist < distMin) {
                distMin = dist;
                indexMin = index;
                graphIndex = i;
            }
        }
    }

    const QString twoTheta = xpdutils::abscissaString(xpdutils::TTHETA);
    double wave = -1;
    if (graphIndex >= 0) {
        double xp_val = graph(graphIndex)->data()->at(indexMin)->key;
        double yp_val = graph(graphIndex)->data()->at(indexMin)->value;

        wave = plotWave.at(graphIndex);
        if (wave > 0) {
            QString xString1 = xpdutils::abscissaString(plotSettings.getAbscissa());
            QString xString2;
            double xp2_val;
            if (plotSettings.getAbscissa() == xpdutils::TTHETA) {
                xString2 = "d";
                xp2_val = xpdutils::dvalue(xp_val, wave);
            } else {
                xString2 = twoTheta;
                xp2_val = xpdutils::tthvalue(xp_val, wave, plotSettings.getAbscissa());
            }
            //xString2 = (plotSettings.getAbscissa() == xpdutils::TTHETA) ? twoTheta : "d";
            //xp2_val = xpdutils::tthvalue(xp_val, wave, plotSettings.getAbscissa());
            QString label1 = QString("%1: %2 I: %3").arg(xString1).arg(x_val).arg(y_val);
            labelWidget1->setText(fontMetrics().elidedText(label1,Qt::ElideRight,labelWidget1->size().width()-10));


            QString label2 = QString("Count: #%1 (%2, %3) %4: %5 Graph: %6").
                             arg(indexMin+1).
                             arg(xp_val).
                             arg(yp_val).
                             arg(xString2).
                             arg(xp2_val).
                             arg(graph(graphIndex)->name());
            labelWidget2->setText(fontMetrics().elidedText(label2,Qt::ElideRight,labelWidget2->size().width()-10));
            //ui->plotLabel1->setText(QString("%1: %2 I: %3").arg(xString1).arg(x_val).arg(y_val));
            // ui->plotLabel2->setText(QString("Count: #%1 (%2, %3) %4: %5 Graph: %6").
            //                         arg(indexMin+1).
            //                         arg(xp_val).
            //                         arg(yp_val).
            //                         arg(xString2).
            //                         arg(xp2_val).
            //                         arg(ui->plot->graph(graphIndex)->name()));
        }
    }

    //Find the closest reflection and the corrisponding phase
    int refIndex = -1;
    int phaseIndex = -1;
    if (refl.count() > 0) {
        QVector<double> yPos(refl.count());
        for (int i = 0; i < refl.count(); i++) yPos[i] = refl[i].getYPos();

        phaseIndex = NR::locateClosest(yPos, y_val);

        if (plotSettings.getAbscissa() == xpdutils::TTHETA) {
            //refIndex = refl[phaseIndex].findLocation(x_val);
            refIndex = findRefLocation(refSet[phaseIndex].ref, x_val);
            //qInfo() << "REFTT: " << refIndex << findRefLocation(refSet[phaseIndex].ref, x_val);
        } else {
            if (wave > 0) {
                double x_val2 = xpdutils::tthvalue(x_val, wave, plotSettings.getAbscissa());
                //It works because refl[i].x should be in 2theta
                //refIndex = refl[phaseIndex].findLocation(x_val2);
                refIndex = findRefLocation(refSet[phaseIndex].ref, x_val2);
                //qInfo() << "REFDD: " << refIndex << findRefLocation(refSet[phaseIndex].ref, x_val2);
            }
        }
    }

    if (refIndex >= 0) {
        int hkl[3];
        //int err;
        float tth, dval;
        for (int i = 0; i < 3; i++) hkl[i] = refSet[phaseIndex].ref[refIndex].hkl[i];
        tth = refSet[phaseIndex].ref[refIndex].x;
        dval = xpdutils::dvalue(tth, wave);
        // qInfo() << "REF: " << refSet[phaseIndex].ref[refIndex].hkl[0] << refSet[phaseIndex].ref[refIndex].hkl[1] << refSet[phaseIndex].ref[refIndex].hkl[2] <<
        //     refSet[phaseIndex].ref[refIndex].x << xpdutils::dvalue(refSet[phaseIndex].ref[refIndex].x, wave);
        // phaseIndex++;
        // refIndex++;
        // get_reflection_info(phaseIndex, refIndex, hkl, &tth, &dval, &err);
        // qInfo() << "REF: " << hkl[0] << hkl[1] << hkl[2] << tth << dval;

        QString label3 = QString("Refl: #%1 H: %2 K: %3 L: %4 %5: %6 d: %7 Phase: %8").
                         arg(refIndex).
                         arg(hkl[0]).arg(hkl[1]).arg(hkl[2]).
                         arg(twoTheta).
                         arg(tth).arg(dval).
                         arg(phaseIndex);
        labelWidget3->setText(fontMetrics().elidedText(label3,Qt::ElideRight,labelWidget3->size().width()-10));
        // ui->plotLabel3->setText(QString("Refl: #%1 H: %2 K: %3 L: %4 %5: %6 d: %7 Phase: %8").
        //                         arg(refIndex).
        //                         arg(hkl[0]).arg(hkl[1]).arg(hkl[2]).
        //                         arg(twoTheta).
        //                         arg(tth).arg(dval).
        //                         arg(phaseIndex));
    }
}

void XpdViewWidget::xAxisChanged(QCPRange range)
{
    //qInfo() << "ini================================ xAxisChanged";
    //Avoid zoom and drag outside the range
    double xUpperRange,xLowerRange;
    getMinMax(xLowerRange,xUpperRange);
    QCPRange boundedRange = range;
    //qInfo() << "xAxisChanged: " << boundedRange.lower << "-" << boundedRange.upper << xLowerRange << "-" << xUpperRange;

    if (boundedRange.size() > xUpperRange - xLowerRange) {
        boundedRange = QCPRange(xLowerRange,xUpperRange);
    } else {
        double oldSize = boundedRange.size();
        if (boundedRange.lower < xLowerRange) {
            boundedRange.lower = xLowerRange;
            boundedRange.upper = xLowerRange + oldSize;
        }
        if (boundedRange.upper > xUpperRange) {
            boundedRange.upper = xUpperRange;
            boundedRange.lower = xUpperRange - oldSize;
        }
    }

    xAxis->setRange(boundedRange);

    double plotWidth = xUpperRange - xLowerRange;
    double viewportWidth = boundedRange.size() / plotWidth;

    //if (viewportWidth > 1.0)
    if (viewportWidth > 0.9999) {
        horizontalScrollBar->hide();
    }
    else
    {
        //qInfo() << "Viewport: " << viewportWidth << " plotWidth: " << plotWidth << " xLowerRange: " << xLowerRange;
        horizontalScrollBar->show();
        horizontalScrollBar->setPageStep(qRound(scrollBarLen*viewportWidth));
        horizontalScrollBar->setSingleStep((std::min)(scrollBarLen / 100, horizontalScrollBar->pageStep()));
        horizontalScrollBar->setRange(0,scrollBarLen-horizontalScrollBar->pageStep());
        //qInfo() << "Step: " << ui->horizontalScrollBar->pageStep() << "Range: " << boundedRange.size();
        double value;
        if (xAxis->rangeReversed()) {
            value = scrollBarLen*(xUpperRange - boundedRange.upper)/plotWidth;
        } else {
            value = scrollBarLen*(boundedRange.lower - xLowerRange)/plotWidth;
        }
        //qInfo() << "Value: " << value;
        horizontalScrollBar->setValue(qRound(value));
    }

    //qInfo() << "Value: " << ui->horizontalScrollBar->value() << "L: " << boundedRange.lower << "Center: " << boundedRange.center() << "U: " << range.upper;
    //qInfo() << "end================================ xAxisChanged";
}

void XpdViewWidget::horzScrollBarChanged(int value)
{
    //qInfo() << "ini================================ horzScrollChange";
    double xUpperRange,xLowerRange;
    getMinMax(xLowerRange,xUpperRange);
    double plotWidth = xUpperRange - xLowerRange;
    double lower;
    if (xAxis->rangeReversed()) {
        lower = xUpperRange - plotWidth * value /scrollBarLen - xAxis->range().size();
    } else {
        lower = plotWidth * value /scrollBarLen + xLowerRange;
    }
    //qInfo() << "HScrollChanged: " << value << "lower: " << lower << "xLower: " << xLowerRange << "range.lower: " << ui->plot->xAxis->range().lower;
    if (qAbs(xAxis->range().lower - lower) > 0.01) // if user is dragging plot, we don't want to replot twice
    {
        //qInfo() << "horzScrollBarChanged: " << value;
        double upper = lower + xAxis->range().size();
        //qInfo() << "HBar Changed, value: " << value << "L: " << lower << "U: " << upper;
        xAxis->setRange(lower,upper);
        replot();
    }
    //qInfo() << "end================================ horzScrollChange";
}

void XpdViewWidget::addDeletePoint(QPoint mousePos)
{
    int ier;

    if (mAction == AddBackgroundPoint || mAction == AddPeak) {
        double xval = xAxis->pixelToCoord(mousePos.x());
        double yval = yAxis->pixelToCoord(mousePos.y());
        //process_action_points(mAction, xval, yval, &ier);
        emit addDeletePointSignal(mAction, xval, yval, ier);
        if (mAction == AddPeak) {
            createTracer(peaks.getPen().color()); //reattach tracer previously deleted in vedi
            attachTracerTo(graph(0));
        }

    } else if (mAction == DeleteBackgroundPoint) {
        int id = bpoints.getGraphIndex();
        //double radius = qMax(5.0,bpoints.getScatter().size());
        QPointF closePoint = closestDataPoint(mousePos,id);
        if (closePoint.isNull()) return;

        // int xp = xAxis->coordToPixel(closePoint.x());
        // int yp = yAxis->coordToPixel(closePoint.y());
        //QPointF dist = mousePos - QPointF(xp,yp);

        //if (dist.manhattanLength() < radius) {
            //process_action_points(mAction, closePoint.x(), closePoint.y(), &ier);
        emit addDeletePointSignal(mAction, closePoint.x(), closePoint.y(), ier);
        //}

    } else if (mAction == DeletePeak) {
        int id = peaks.getGraphIndex();
        if (id >= 0) {
            double xMouseCoord = xAxis->pixelToCoord(mousePos.x());
            double closePoint = closestDataPointX(xMouseCoord,id);

            // int xp = xAxis->coordToPixel(closePoint);
            // if (abs(mousePos.x() - xp) < 5) {
                //process_action_points(mAction, closePoint, 0, &ier);
            emit addDeletePointSignal(mAction, closePoint, 0, ier);
                createTracer(peaks.getPen().color()); //reattach tracer previously deleted in vedi
                attachTracerTo(graph(0));
            //}
        }

    }
}

void XpdViewWidget::selectionChanged()
{
    // synchronize selection of graphs with selection of corresponding legend items:
    for (int i=0; i<graphCount(); ++i)
    {
        QCPGraph *gr = graph(i);
        if (gr->visible()) {
            QCPPlottableLegendItem *item = legend->itemWithPlottable(gr);
            if (item->selected() || gr->selected())
            {
                item->setSelected(true);
                gr->setSelection(QCPDataSelection(gr->data()->dataRange()));
            }
        }
    }
}

void XpdViewWidget::dragEnterEvent(QDragEnterEvent *event)
{
    if (event->mimeData()->hasUrls()) {
        event->acceptProposedAction();
    }
}

void XpdViewWidget::dropEvent(QDropEvent *event)
{
    const QMimeData *mimeData = event->mimeData();
    if (mimeData->hasUrls()) {
        QList<QUrl> urlList = mimeData->urls();
        if (!urlList.isEmpty()) {
            QStringList fileList;
            fileList.reserve(urlList.size());

            for (auto it = urlList.cbegin(); it != urlList.cend(); ++it) {
                fileList.append(it->toLocalFile());
            }

            //QTimer allows Qt's event loop to complete the drop operation and make the file icon disappear
            QTimer::singleShot(0, this, [this, fileList]() { emit fileDropped(fileList); });
        }
    }
}

void XpdViewWidget::setPlotSettings(const PlotSettings &newPlotSettings)
{
    plotSettings = newPlotSettings;
}

PlotSettings XpdViewWidget::pSettings() const
{
    return plotSettings;
}

void XpdViewWidget::addObserved(const QVector<double> &xvet, const QVector<double> &yvet,
                                int visible, float wave, const QString &name)
{
    if (obs.size() <= nObserved) { //new graphic element required
        graphItem item;
        item.setColorLine(nObserved);
        item.wave = wave;
        obs.push_back(item);
    }
    obs[nObserved].setData(xvet,yvet);
    obs[nObserved].setVisible(visible);
    obs[nObserved].setName(name);
    makePlot(xvet,yvet,obs[nObserved]);
    graph()->setScatterStyle(obs[nObserved].getScatter());
    nObserved++;
}

void XpdViewWidget::getYLimits(double &yMin, double &yMax)
{
    yMin = DBL_MAX;
    yMax = -DBL_MAX;

    QVector<graphItem>::const_iterator i;
    for (i = obs.constBegin(); i != obs.constEnd(); ++i) {
        if (i->getGraphIndex() >= 0 && i->isVisible()) {
            if (i->getMin() < yMin) yMin = i->getMin();
            if (i->getMax() > yMax) yMax = i->getMax();
        }
    }

    if (back.getGraphIndex() >= 0 && back.isVisible()) {
        if (back.getMin() < yMin) yMin = back.getMin();
        if (back.getMax() > yMax) yMax = back.getMax();
    }

    if (peaks.getGraphIndex() >= 0 && peaks.isVisible()) {
        if (peaks.getMin() < yMin) yMin = peaks.getMin();
        if (peaks.getMax() > yMax) yMax = peaks.getMax();
    }

    if (calc.getGraphIndex() >= 0 && calc.isVisible()) {
        if (calc.getMin() < yMin) yMin = calc.getMin();
        if (calc.getMax() > yMax) yMax = calc.getMax();
    }

    if (diff.getGraphIndex() >= 0 && diff.isVisible()) {
        if (diff.getMin() < yMin) yMin = diff.getMin();
        if (diff.getMax() > yMax) yMax = diff.getMax();
    }

    if (cdiff.getGraphIndex() >= 0 && cdiff.isVisible()) {
        if (cdiff.getMin() < yMin) yMin = cdiff.getMin();
        if (cdiff.getMax() > yMax) yMax = cdiff.getMax();
    }

    if (smooth.getGraphIndex() >= 0 && smooth.isVisible()) {
        if (smooth.getMin() < yMin) yMin = smooth.getMin();
        if (smooth.getMax() > yMax) yMax = smooth.getMax();
    }
}

void XpdViewWidget::drawReflections(const QVector<double> &x, double y, double length, QPen pen, int &itemStart, int &itemEnd)
{
    itemStart = itemCount();
    for (int i = 0; i < x.size(); i++) {
        QCPItemLine *line = new QCPItemLine(this);
        line->setPen(pen);
        line->setHead(QCPLineEnding::esNone);
        line->start->setCoords(x[i], y);
        line->end->setCoords(x[i], y+length);
    }
    itemEnd = x.size() + itemStart - 1;
}

void XpdViewWidget::drawReflections(const QVector<refInfo> &ref, double y, double length, QPen pen, int &itemStart, int &itemEnd)
{
    itemStart = itemCount();
    for (int i = 0; i < ref.size(); i++) {
        QCPItemLine *line = new QCPItemLine(this);
        line->setPen(pen);
        line->setHead(QCPLineEnding::esNone);
        line->start->setCoords(ref[i].x, y);
        line->end->setCoords(ref[i].x, y+length);
    }
    itemEnd = ref.size() + itemStart - 1;
}

void XpdViewWidget::drawIntervals()
{
    if (!intervalLimit.isVisible()) return;

    QPen pen1(Qt::gray);
    QPen pen2(Qt::black);
    int ip=0;
    double diff = yUpperRange - yLowerRangeNoRef;
    double offset;
    QVector<double> xval = intervalLimit.getX();
    for (int i = 0; i < xval.size(); i+=2) {
        QCPItemLine *line = new QCPItemLine(this);
        if (ip % 2 == 0) {
            line->setPen(pen1);
            offset = diff * 0.01;
        } else {
            line->setPen(pen2);
            offset = 0.0;
        }
        ip++;
        line->setHead(QCPLineEnding::esSpikeArrow);
        line->setTail(QCPLineEnding::esSpikeArrow);
        line->start->setCoords(xval[i],yLowerRangeNoRef + offset);
        line->end->setCoords(xval[i+1],yLowerRangeNoRef + offset);
    }
}

void XpdViewWidget::reDrawReflections(const QVector<refInfo> &refSet, const graphItem &ref, double y, double length)
{
    for (int ind = ref.itemIndexStart; ind <= ref.itemIndexEnd; ind++) {
        QCPItemLine *line = dynamic_cast<QCPItemLine *> (this->item(ind));
        int posRef = ref.itemIndexEnd - ind;
        //qInfo() << "Line: " << posRef << ref.getX().at(posRef) << y << y + length;
        line->start->setCoords(refSet[posRef].x, y);
        line->end->setCoords(refSet[posRef].x, y+length);
    }
}

// void XpdViewWidget::reDrawReflections(graphItem &ref, double y, double length)
// {
//     for (int ind = ref.itemIndexStart; ind <= ref.itemIndexEnd; ind++) {
//         QCPItemLine *line = dynamic_cast<QCPItemLine *> (this->item(ind));
//         int posRef = ref.itemIndexEnd - ind;
//         //qInfo() << "Line: " << posRef << ref.getX().at(posRef) << y << y + length;
//         line->start->setCoords(ref.getX().at(posRef), y);
//         line->end->setCoords(ref.getX().at(posRef), y+length);
//     }
// }

void XpdViewWidget::clearGraphSelection(graphItem &item)
{
    int id = item.getGraphIndex();
    if (id < 0) return;

    removeGraph(id);
    legend->setMaximumSize(legend->minimumOuterSizeHint());
    replot();
    item.setGraphIndex(-1);
}

// void XpdViewWidget::drawGraphicItem_old(graphItem &item)
// {
//     int npoints = get_plot_size(item.getGtype());

//     if (npoints > 0) {
//         float *xv = new float[npoints];
//         float *yv = new float[npoints];
//         float wave;
//         get_plot_xy(xv, yv, &wave, item.getGtype());

//         QVector<double> xvet(npoints), yvet(npoints);
//         for (int i = 0; i < npoints; i++) {
//             xvet[i] = xv[i];
//             yvet[i] = yv[i];
//         }
//         delete [] xv;
//         delete [] yv;
//         int id = item.getGraphIndex();
//         if (id < 0) {
//             item.wave = wave;
//             makePlot(xvet,yvet,item);
//         } else {
//             graph(id)->setData(xvet,yvet,true);
//         }

//         legend->setMaximumSize(legend->minimumOuterSizeHint());
//         replot();
//     } else {
//         clearGraphSelection(item);
//     }
// }

void XpdViewWidget::drawGraphicItem(graphItem &item, const QVector<double> &xvet, const QVector<double> &yvet, double wave)
{
    if (xvet.size() > 0) {
        int id = item.getGraphIndex();
        if (id < 0) {
            item.wave = wave;
            makePlot(xvet,yvet,item);
        } else {
            graph(id)->setData(xvet,yvet,true);
        }

        legend->setMaximumSize(legend->minimumOuterSizeHint());
        replot();
    } else {
        clearGraphSelection(item);
    }
}

void XpdViewWidget::setSelectedRef(const QVector<int> &selected)
{
    if (refl.size() == 0) return;

    QVector<double> x(selected.size());
    for (int i = 0; i < selected.size(); i++) {
        //x[i] = refl[0].getX(selected.at(i));
        x[i] = refSet[0].ref[selected.at(i)].x;
    }
    selectedRef.setX(x);
}

bool XpdViewWidget::rescalePlotEnabled() const
{
    return rescalePlot;
}

int XpdViewWidget::findRefLocation(const QVector<refInfo> &ref, double xval)
{
    // int ju,jm,jl;
    // bool ascnd;

    // int n=xx.size();
    // jl=-1;
    // ju=n;
    // ascnd=(xx[n-1] >= xx[0]);
    // while (ju-jl > 1) {
    //     jm=(ju+jl) >> 1;
    //     if ((x >= xx[jm]) == ascnd)
    //         jl=jm;
    //     else
    //         ju=jm;
    // }
    // if (x == xx[0]) j=0;
    // else if (x == xx[n-1]) j=n-2;
    // else j=jl;

    // if (j == -1) return 0; // out of range to the left
    // if (j == xx.count() - 1) return j; // out of range to the right
    // if (abs(x - xx[j]) < abs(x - xx[j+1])) return j;   // xx[j] < x < xx[j+1]
    // return ++j;

    int j,ju,jm,jl;
    bool ascnd;

    int n = ref.size();
    jl = -1;
    ju = n;
    ascnd = (ref[n-1].x >= ref[0].x);
    while (ju-jl > 1) {
        jm = (ju+jl) >> 1;
        if ((xval >= ref[jm].x) == ascnd)
            jl = jm;
        else
            ju = jm;
    }
    if (xval == ref[0].x) j = 0;
    else if (xval == ref[n-1].x) j = n-2;
    else j = jl;

    if (j == -1) return 0; // out of range to the left
    if (j == n - 1) return j; // out of range to the right
    if (abs(xval - ref[j].x) < abs(xval - ref[j+1].x)) return j;   // xx[j] < x < xx[j+1]
    return ++j;
}

// void XpdViewWidget::makeReflections(const QVector<double> &xvet)
// {
//     int ind = 0;
//     //refl[ind].setLineStyle(Qt::NoPen);
//     refl[ind].setLineConnectionType(QCPGraph::lsNone);
//     refl[ind].setGraphIndex(graphCount());
//     //refl[ind].setScatter(QCPScatterStyle(QCPScatterStyle::ssDisc));
//     plotWave.append(refl[ind].wave);
//     addGraph();
//     graph()->setName(refl[ind].getName());
//     QVector<double> yvet(xvet.size(),0.0);
//     graph()->setData(xvet,yvet,false);
//     graph()->setPen(refl[ind].getPen());
//     graph()->setLineStyle(refl[ind].getLineConnectionType());
//     //graph()->setScatterStyle(refl[ind].getScatter());
//     // int pix = 10;
//     // //Covert pix in graphic unit of qpainter
//     qreal pixelPoint = 10;
//     //qreal graphPoint = graph()->valueAxis()->pixelToCoord(pixelPoint);
//     qreal graphPoint = yAxis->pixelToCoord(pixelPoint);
//     qInfo() << "PIxel: " << pixelPoint << "Graph: " << graphPoint;

//     QPainterPath customScatterPath(QPointF(0,-pixelPoint));
//     customScatterPath.lineTo(0,pixelPoint);
//     graph()->setScatterStyle(QCPScatterStyle(customScatterPath, refl[ind].getPen(), Qt::black));
// }

void XpdViewWidget::enableRescalePlot(bool value)
{
    rescalePlot = value;
}

void XpdViewWidget::setLegendVisible(bool visible)
{
    plotSettings.legendVisible = visible;
    legend->setVisible(visible);
    replot();
}

void XpdViewWidget::applyOffset(double yOffset)
{
    //Unapply the last yoffset
    double newOffset = -mLastYOffset + yOffset;

    //No offset is applied to the first graph (i=0)
    double totOffset = 0;
    for (int i = 1; i < graphCount(); i++) {
        //totOffset += exp(i*newOffset);
        totOffset = i*newOffset;
        QCPGraphDataContainer *data = graph(i)->data().data();
        for(QCPGraphDataContainer::iterator it = data->begin(); it != data->end(); it++)
        {
            it->value += totOffset;
        }
    }

    //Apply offset to the y uppper range
    yUpperRange += totOffset;
    if (refl.size() > 0) {
        redrawPlot(false);
    } else {
        yAxis->setRange(yLowerRange,yUpperRange);
    }

    //Save last applied offset;
    mLastYOffset = yOffset;
}

void XpdViewWidget::applyAutoScale()
{
    // old method
    //Compute min and max along the yaxis in the visible range
    //Provare anche per ogni plot QCPRange range = ui->plot->graph(0)->getKeyRange(found);
    //    double yLowerRange = DBL_MAX;
    //    double yUpperRange = -DBL_MAX;
    //    double xLowerRange = ui->plot->xAxis->range().lower;
    //    double xUpperRange = ui->plot->xAxis->range().upper;
    //    for (int i = 0; i < ui->plot->graphCount(); i++) {
    //        QCPGraphDataContainer::const_iterator it = ui->plot->graph(i)->data()->constBegin();
    //        QCPGraphDataContainer::const_iterator end = ui->plot->graph(i)->data()->constEnd();;
    //        while (it != end)
    //        {
    //            if (it->key >= xLowerRange && it->key <= xUpperRange) {
    //                if (it->value < yLowerRange) yLowerRange = it->value;
    //                if (it->value > yUpperRange) yUpperRange = it->value;
    //            }
    //            it++;
    //        }
    //    }
    //    if (ui->plot->yAxis->range().upper != yUpperRange) {
    //        //double yLower = ui->plot->yAxis->range().lower;
    //        ui->plot->yAxis->setRange(yLower(),yUpperRange);
    //        ui->plot->replot();
    //        qInfo() << "Autoscale: " << yUpperRange;
    //    }

    //altro metodo
    double Lower = xAxis->range().lower;
    double Upper = xAxis->range().upper;
    //yLowerRange = Lower;
    double yUpperAutoScale = -DBL_MAX;
    double yLowerAutoScale = DBL_MAX;
    for (int i = 0; i < graphCount(); i++) {
        if (!graph(i)->data()->isEmpty() && graph(i)->visible()) {
            //int index_left = ui->plot->graph(i)->findBegin(Lower, false);
            //if (ui->plot->graph(0)->data()->at(index_left)->key < Lower) index_left++;
            //int index_right = ui->plot->graph(i)->findEnd(Upper, false) - 1;
            //double max = max_element(ui->plot->graph(i)->data()->at(index_left),ui->plot->graph(i)->data()->at(index_right));
            //qInfo() << "G: " << i << "Index L: " << index_left << "Index R: " << index_right << ui->plot->graph(i)->data()->size();
            QCPGraphDataContainer::const_iterator begin = graph(i)->data()->findBegin(Lower, false);
            QCPGraphDataContainer::const_iterator end = graph(i)->data()->findBegin(Upper, false) - 1;
            // end > begin for selection between two points (empty range!)
            if (begin <= end) {
                //warning
                //                const QCPGraphData* maxData = std::max_element(begin, end,
                //                                              [](QCPGraphData a, QCPGraphData b){return a.value < b.value;});
                const auto maxData = std::max_element(begin, end,
                                                      [](const QCPGraphData& a, const QCPGraphData& b){return a.value < b.value;});
                const auto minData = std::min_element(begin, end,
                                                      [](const QCPGraphData& a, const QCPGraphData& b){return a.value < b.value;});
                if (yUpperAutoScale < maxData->value) yUpperAutoScale = maxData->value;
                if (yLowerAutoScale > minData->value) yLowerAutoScale = minData->value;
            }
        }
    }
    if (yAxis->range().upper != yUpperAutoScale) {
        bool lowerChanged = (yAxis->range().lower != yLowerAutoScale);
        double yLowerRangeSav;
        if (lowerChanged) {
            yLowerRangeSav = yLowerRange;
            yLowerRangeNoRef = yLowerAutoScale;
        }
        //TOFIX: improve this part, yUpperRange and yLowerRange should not be changed to avoid problem at reset
        double yUppperRangeSav = yUpperRange;
        yUpperRange = yUpperAutoScale;
        redrawPlot(false);
        yUpperRange = yUppperRangeSav;
        if (lowerChanged) {
            yLowerRange = yLowerRangeSav;
        }
    }
}

void XpdViewWidget::connectHorizontalScrollBar(QScrollBar *newHorizontalScrollBar)
{
    horizontalScrollBar = newHorizontalScrollBar;
    horizontalScrollBar->setPageStep(scrollBarLen);
    horizontalScrollBar->hide();
    connect(horizontalScrollBar, &QScrollBar::valueChanged, this, &XpdViewWidget::horzScrollBarChanged);
    connect(xAxis, QOverload<const QCPRange &>::of(&QCPAxis::rangeChanged), this, &XpdViewWidget::xAxisChanged);
}

void XpdViewWidget::connectAddDeletePoints()
{
    connect(this, &CustomPlotZoom::addDeletePointAction, this, &XpdViewWidget::addDeletePoint);
}

void XpdViewWidget::connectSelectionChanged()
{
    connect(this, &CustomPlotZoom::selectionChangedByUser, this, &XpdViewWidget::selectionChanged);
}

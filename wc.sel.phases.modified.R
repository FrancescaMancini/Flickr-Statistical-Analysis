wc.sel.phases.modified<-function (WC, sel.period = NULL, sel.lower = NULL, sel.upper = NULL,
    only.coi = F, only.sig = T, which.sig = "wp", siglvl = 0.05,
    phase.cols = c("red", "blue"), show.Angle = T, use.sAngle = F,
    Angle.col = "black", show.legend = T, legend.coords = "topleft",
    legend.horiz = T, label.time.axis = T, show.date = F, date.format = NULL,
    timelab = NULL, label.phase.axis = T, phaselab = NULL, phaselim = c(-pi,
        pi + show.legend * ifelse(legend.horiz, 0.8, 2)), main = NULL,
    sub = NULL, verbose = F)
{
    if (verbose == T) {
        out <- function(...) {
            cat(...)
        }
    }
    else {
        out <- function(...) {
        }
    }
    nc = WC$nc
    nr = WC$nr
    series.data = WC$series
    if (names(series.data)[1] == "date") {
        my.series.names = names(series.data)[2:3]
    }
    if (names(series.data)[1] != "date") {
        my.series.names = names(series.data)[1:2]
    }
    Period = WC$Period
    if (length(sel.period) != 0) {
        sel.rnum = which(abs(Period - sel.period) == min(abs(Period -
            sel.period)))
    }
    if (length(sel.period) == 0) {
        if (length(sel.lower) == 0) {
            sel.lower = min(Period)
        }
        if (length(sel.upper) == 0) {
            sel.upper = max(Period)
        }
        if (sel.lower > sel.upper) {
            sel.lower.h = sel.lower
            sel.lower = sel.upper
            sel.upper = sel.lower.h
        }
        sel.rnum = which(((Period >= sel.lower) & (Period <=
            sel.upper)))
    }
    sel.period.band = Period[sel.rnum]
    sel.period.range = as.character(round(sel.period.band, 3))
    Phase.x = WC$Phase.x
    Phase.y = WC$Phase.y
    Angle = WC$Angle
    sAngle = WC$sAngle
    if (only.coi == T) {
        for (i in (1:nc)) {
            for (s.ind in seq_len(nr)) {
                if (WC$Scale[s.ind] > 2^WC$coi.2[i]) {
                  Phase.x[s.ind, i] = NA
                  Phase.y[s.ind, i] = NA
                  Angle[s.ind, i] = NA
                  sAngle[s.ind, i] = NA
                }
            }
        }
    }
    if (only.sig == T) {
        if ((which.sig == "wp") & !is.null(WC$Power.xy.pval)) {
            ind.sig = (WC$Power.xy.pval < siglvl)
            Phase.x[ind.sig == 0] = NA
            Phase.y[ind.sig == 0] = NA
            Angle[ind.sig == 0] = NA
            sAngle[ind.sig == 0] = NA
        }
        if ((which.sig == "wc") & !is.null(WC$Coherence.pval)) {
            ind.sig = (WC$Coherence.pval < siglvl)
            Phase.x[ind.sig == 0] = NA
            Phase.y[ind.sig == 0] = NA
            Angle[ind.sig == 0] = NA
            sAngle[ind.sig == 0] = NA
        }
        if ((which.sig == "wt") & !is.null(WC$Power.x.pval) &
            !is.null(WC$Power.y.pval)) {
            ind.sig = (WC$Power.x.pval < siglvl) * (WC$Power.y.pval <
                siglvl)
            ind.sig.x = (WC$Power.x.pval < siglvl)
            ind.sig.y = (WC$Power.y.pval < siglvl)
            Phase.x[ind.sig.x == 0] = NA
            Phase.y[ind.sig.y == 0] = NA
            Angle[ind.sig == 0] = NA
            sAngle[ind.sig == 0] = NA
        }
    }
    if (only.sig == F) {
        siglvl = NA
    }
    Phase.x = Phase.x[sel.rnum, ]
    Phase.y = Phase.y[sel.rnum, ]
    Angle = Angle[sel.rnum, ]
    sAngle = sAngle[sel.rnum, ]
    if (length(sel.rnum) > 1) {
        sel.period.range = paste(round(range(sel.period.band),
            3), collapse = " - ")
        Phase.x = colMeans(Phase.x, na.rm = T)
        Phase.y = colMeans(Phase.y, na.rm = T)
        Angle = colMeans(Angle, na.rm = T)
        sAngle = colMeans(sAngle, na.rm = T)
    }
    if (is.null(sub)) {
        sub = paste("selected period: ", sel.period.range, sep = "")
    }
    matplot(WC$axis.1, data.frame(Phase.x, Phase.y), type = "l",lwd=2,
        lty = 1, col = phase.cols, xaxs = "i", xaxt = "n", ylim = phaselim,
        yaxt = "n", xlab = "", ylab = "", main = main, sub = sub)
    legend.angle = NULL
    if (show.Angle == T) {
        if (use.sAngle == T) {
            lines(WC$axis.1, sAngle, lty = 2, lwd = 2, col = Angle.col)
            legend.angle = "phase difference\n(smoothed)"
        }
        if (use.sAngle == F) {
            lines(WC$axis.1, Angle, lty = 2, lwd = 2, col = Angle.col)
            legend.angle = "phase difference"
        }
    }
    if (show.legend == T) {
        legend(legend.coords, legend = c(my.series.names, legend.angle),
            horiz = legend.horiz, col = c(phase.cols, ifelse(show.Angle,
                Angle.col, NULL)), lty = c(1, 1, ifelse(show.Angle,
                2, NULL)), lwd = c(2, 2, ifelse(show.Angle, 2,
                NULL)), box.lwd = 0.25,cex=2)
    }
    if (label.phase.axis == T) {
        if (is.null(phaselab)) {
            phaselab = "phase"
            if (length(sel.rnum) > 1) {
                phaselab = "average phase"
            }
        }
        axis(2, lwd = 0.25, at = seq(-pi, pi, pi/2), labels = NA,
            tck = 0.02, tcl = 0.5,cex.axis=1.5)
        axis(4, lwd = 0.25, at = seq(-pi, pi, pi/2), labels = NA,
            tck = 0.02, tcl = 0.5,cex.axis=2)
        mtext(c(expression(-pi), expression(-pi/2), 0, expression(pi/2),
            expression(pi)), at = seq(-pi, pi, pi/2), side = 2,
            line = 0.5, las = 1, font = 1,cex=2)
        mtext(phaselab, side = 2, line = 4, font = 1,cex=2,mar=0)
    }
    my.date = NULL
    if (label.time.axis == T) {
        if (is.null(timelab)) {
            timelab = "time"
        }
        if (show.date == F) {
            A.1 = axis(1, lwd = 0.25, labels = NA, tck = 0,cex.axis=2,mar=10)
            mtext(A.1, side = 1, at = A.1, line = 0.5,cex=2,mar=10)
            mtext(timelab, side = 1, line = 2,cex=2,mar=10)
        }
        if (show.date == T) {
            if (is.element("date", names(series.data))) {
                my.date = series.data$date
            }
            else {
                my.date = rownames(series.data)
            }
            if (is.null(date.format)) {
                my.date = as.Date(my.date)
            }
            else {
                my.date = as.POSIXct(my.date, format = date.format)
            }
            par(new = TRUE)
            plot(my.date, rep(0, WC$nc), ylim = phaselim, type = "n",
                xaxs = "i", yaxt = "n",xaxt="n", xlab = "", ylab = "",
                lwd = 0.25, tck = 0.02, tcl = 0.5,cex.axis=1.5)
            axis(side=1,at=my.date[seq(from=1,to=72,12)],labels=F)
            text(x=my.date[seq(from=1,to=72,12)], y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
            labels= format(my.date[seq(from=1,to=72,12)],"%Y"), srt=45, adj=1, xpd=TRUE,cex=2)
            mtext(timelab, side = 1, line = 2.5,cex=2)
        }
    }
    output <- list(Period = sel.period.band, Phase.x = Phase.x,
        Phase.y = Phase.y, Angle = Angle, sAngle = sAngle, only.coi = only.coi,
        only.sig = only.sig, which.sig = which.sig, siglvl = siglvl,
        date = my.date, time.axis = WC$axis.1)
    class(output) = "sel.phases"
    out("Class attributes are accessible through following names:\n")
    out(names(output), "\n")
    return(invisible(output))
}










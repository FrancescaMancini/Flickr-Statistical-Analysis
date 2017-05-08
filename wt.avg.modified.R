wt.avg.modified<-function (WT, my.series = 1, show.siglvl = T, siglvl = c(0.05,
    0.1), sigcol = c("red", "blue"), sigpch = 20, label.avg.axis = T,
    averagelab = NULL, label.period.axis = T, periodlab = NULL,
    show.legend = T, legend.coords = "topright", main = NULL,
    lwd = 0.5, verbose = F)
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
    series.data = WT$series
    if (class(WT) == "analyze.wavelet") {
        out("Your input object class is 'analyze.wavelet'...\n")
        my.series = ifelse(names(series.data)[1] == "date", names(series.data)[2],
            names(series.data)[1])
        Power.avg = WT$Power.avg
        Power.avg.pval = WT$Power.avg.pval
    }
    if (class(WT) == "analyze.coherency") {
        out("Your input object class is 'analyze.coherency'...\n")
        if (is.numeric(my.series)) {
            if (!is.element(my.series, c(1, 2))) {
                stop("Please choose either series number 1 or 2!")
            }
            my.series = ifelse(names(series.data)[1] == "date",
                names(series.data)[my.series + 1], names(series.data)[my.series])
        }
        ind = which(names(series.data) == my.series)
        which.series.num = ifelse(names(series.data)[1] == "date",
            ind - 1, ind)
        if (!is.element(which.series.num, c(1, 2))) {
            stop("Your series name is not available, please check!")
        }
        if (which.series.num == 1) {
            Power.avg = WT$Power.x.avg
            Power.avg.pval = WT$Power.x.avg.pval
        }
        if (which.series.num == 2) {
            Power.avg = WT$Power.y.avg
            Power.avg.pval = WT$Power.y.avg.pval
        }
    }
    out(paste("Power averages across time of your time series '",
        my.series, "' will be plotted...", sep = ""), "\n")
    plot(Power.avg, log2(WT$Period), lwd = lwd, type = "l", axes = FALSE,
        ylab = "", xlab = "", yaxs = "i", main = main,cex.main=2)
    if ((show.siglvl == T) & (is.null(Power.avg.pval) == F)) {
        P.dat = data.frame(Pvalue = Power.avg.pval, Log.period = log2(WT$Period),
            Average = Power.avg)
        n.sig = length(siglvl)
        if (length(sigpch) == 1) {
            sigpch = rep(sigpch, n.sig)
        }
        if (n.sig != length(sigpch)) {
            sigpch = rep(20, n.sig)
        }
        if (n.sig != length(sigcol)) {
            sigcol = 1:n.sig
        }
        siglvl.order = order(siglvl, decreasing = T)
        sig.params = data.frame(siglvl = siglvl[siglvl.order],
            sigcol = sigcol[siglvl.order], sigpch = sigpch[siglvl.order])
        for (i in (1:length(siglvl))) {
            with(P.dat[P.dat$Pvalue < sig.params$siglvl[i], ],
                points(Average, Log.period, pch = sig.params$sigpch[i],
                  col = as.character(sig.params$sigcol[i]),cex=2))
        }
        if (show.legend == T) {
            legend(legend.coords, legend = siglvl, pch = sigpch,
                col = sigcol, horiz = F, box.lwd = 0.25,cex=2)
        }
    }
    box(lwd = 0.25)
    if (label.avg.axis == T) {
        if (is.null(averagelab)) {
            averagelab = "average wavelet power"
        }
        A.1 = axis(1, lwd = 0.25, labels = NA, tck = 0,cex.axis=2)
        text(x=A.1, y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
        labels= A.1, srt=45, adj=1, xpd=TRUE,cex=2)

    }
    if (label.period.axis == T) {
        if (is.null(periodlab)) {
            periodlab = "period"
        }
        period.tick <- unique(trunc(WT$axis.2))
        period.tick[period.tick < log2(WT$Period[1])] = NA
        period.tick = na.omit(period.tick)
        period.tick.label <- 2^(period.tick)
        axis(2, lwd = 0.25, at = period.tick, labels = NA, tck = 0.02,
            tcl = 0.5,cex.axis=2)
        axis(4, lwd = 0.25, at = period.tick, labels = NA, tck = 0.02,
            tcl = 0.5,cex.axis=2)
        mtext(period.tick.label, side = 2, at = period.tick,
            las = 1, line = 0.5,cex=2)
        mtext(periodlab, side = 2, line = 2.5, font = 1,cex=2)
    }
}

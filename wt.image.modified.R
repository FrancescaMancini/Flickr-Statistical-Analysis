wt.image.modified<-function (WT, my.series = 1, plot.coi = T, plot.contour = T,
    siglvl = 0.1, col.contour = "white", plot.ridge = T, lvl = 0,
    col.ridge = "black", color.key = "quantile", n.levels = 100,
    color.palette = "rainbow(n.levels, start=0, end=.7)", useRaster = T,
    max.contour.segments = 250000, plot.legend = T, legend.params = list(width = 1.2,
        shrink = 0.9, mar = 5.1, n.ticks = 6, label.digits = 1,
        label.format = "f", lab = NULL, lab.line = 2.5), label.time.axis = T,
    show.date = F, date.format = NULL, timelab = NULL, label.period.axis = T,
    periodlab = NULL, main = NULL, lwd = 2, graphics.reset = T,
    verbose = F)
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
    default.options = options()
    options(max.contour.segments = as.integer(max.contour.segments))
    axis.1 <- WT$axis.1
    axis.2 <- WT$axis.2
    lwd.axis = 0.25
    series.data = WT$series
    if (class(WT) == "analyze.wavelet") {
        out("Your input object class is 'analyze.wavelet'...\n")
        my.series = ifelse(names(series.data)[1] == "date", names(series.data)[2],
            names(series.data)[1])
        Power = WT$Power
        Power.pval = WT$Power.pval
        Ridge = WT$Ridge
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
            Power = WT$Power.x
            Power.pval = WT$Power.x.pval
            Ridge = WT$Ridge.x
        }
        if (which.series.num == 2) {
            Power = WT$Power.y
            Power.pval = WT$Power.y.pval
            Ridge = WT$Ridge.y
        }
    }
    out(paste("A wavelet power image of your time series '",
        my.series, "' will be plotted...", sep = ""), "\n")
    if (is.element(color.key, c("interval", "i"))) {
        wavelet.levels = seq(from = 0, to = max(Power), length.out = n.levels +
            1)
    }
    if (is.element(color.key, c("quantile", "q"))) {
        wavelet.levels = quantile(Power, probs = seq(from = 0,
            to = 1, length.out = n.levels + 1))
    }
    key.cols = rev(eval(parse(text = color.palette)))
    if (is.null(legend.params$width))
        legend.params$width = 1.2
    if (is.null(legend.params$shrink))
        legend.params$shrink = 0.9
    if (is.null(legend.params$mar))
        legend.params$mar = ifelse(is.null(legend.params$lab),
            5.1, 6.1)
    if (is.null(legend.params$n.ticks))
        legend.params$n.ticks = 6
    if (is.null(legend.params$label.digits))
        legend.params$label.digits = 1
    if (is.null(legend.params$label.format))
        legend.params$label.format = "f"
    if (is.null(legend.params$lab.line))
        legend.params$lab.line = 2.5
    op = par(no.readonly = TRUE)
    image.plt = par()$plt
    legend.plt = NULL
    if (plot.legend == T) {
        legend.plt = par()$plt
        char.size = par()$cin[1]/par()$din[1]
        hoffset = char.size * par()$mar[4]
        legend.width = char.size * legend.params$width
        legend.mar = char.size * legend.params$mar
        legend.plt[2] = 1 - legend.mar
        legend.plt[1] = legend.plt[2] - legend.width
        vmar = (legend.plt[4] - legend.plt[3]) * ((1 - legend.params$shrink)/2)
        legend.plt[4] = legend.plt[4] - vmar
        legend.plt[3] = legend.plt[3] + vmar
        image.plt[2] = min(image.plt[2], legend.plt[1] - hoffset)
        par(plt = legend.plt)
        key.marks = round(seq(from = 0, to = 1, length.out = legend.params$n.ticks) *
            n.levels)
        key.labels = formatC(as.numeric(wavelet.levels), digits = legend.params$label.digits,
            format = legend.params$label.format)[key.marks +
            1]
        image(1, seq(from = 0, to = n.levels), matrix(wavelet.levels,
            nrow = 1), col = key.cols, breaks = wavelet.levels,
            useRaster = T, xaxt = "n", yaxt = "n", xlab = "",
            ylab = "")
        axis(4, lwd = lwd.axis, at = key.marks, labels = NA,
            tck = 0.02, tcl = (par()$usr[2] - par()$usr[1]) *
                legend.params$width - 0.04,cex.axis=2)
        mtext(key.labels, side = 4, at = key.marks, line = 0.5,
            las = 2,cex=2)
        text(x = par()$usr[2] + (4 + legend.params$lab.line) *
            par()$cxy[1], y = n.levels/2, labels = legend.params$lab,
            xpd = NA, srt = 270,cex=2)
        box(lwd = lwd.axis)
        par(new = TRUE, plt = image.plt)
    }
    image(axis.1, axis.2, t(Power), col = key.cols, breaks = wavelet.levels,
        useRaster = useRaster, ylab = "", xlab = "", axes = FALSE,
        main = main,cex.main=2)
    if ((plot.contour == T) & (is.null(Power.pval) == F)) {
        contour(axis.1, axis.2, t(Power.pval) < siglvl, levels = 1,
            lwd = lwd, add = TRUE, col = col.contour, drawlabels = FALSE)
    }
    if (plot.ridge == T) {
        Ridge = Ridge * (Power >= lvl)
        contour(axis.1, axis.2, t(Ridge), levels = 1, lwd = lwd,
            add = TRUE, col = col.ridge, drawlabels = FALSE)
    }
    if (plot.coi == T) {
        polygon(WT$coi.1, WT$coi.2, border = NA, col = rgb(1,
            1, 1, 0.5))
    }
    box(lwd = lwd.axis)
    if (label.period.axis == T) {
        if (is.null(periodlab)) {
            periodlab = "period"
        }
        period.tick = unique(trunc(axis.2))
        period.tick[period.tick < log2(WT$Period[1])] = NA
        period.tick = na.omit(period.tick)
        period.tick.label = 2^(period.tick)
        axis(2, lwd = lwd.axis, at = period.tick, labels = NA,
            tck = 0.02, tcl = 0.5,cex.axis=2)
        axis(4, lwd = lwd.axis, at = period.tick, labels = NA,
            tck = 0.02, tcl = 0.5,cex.axis=2)
        mtext(period.tick.label, side = 2, at = period.tick,
            las = 1, line = 0.5,cex=2)
        mtext(periodlab, side = 2, line = 2.5,cex=2)
    }
    if (label.time.axis == T) {
        if (is.null(timelab)) {
            timelab = "time"
        }
        if (show.date == F) {
            A.1 = axis(1, lwd = lwd.axis, labels = NA, tck = 0,cex.axis=2,mar=10)
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
            plot(my.date, seq(min(axis.2), max(axis.2), length.out = WT$nc),
                type = "n", xaxs = "i", yaxs = "i", yaxt = "n", xaxt="n",
                xlab = "", ylab = "", lwd = lwd.axis, tck = 0.02,
                tcl = 0.5,cex.axis=1.5)
            axis(side=1,at=my.date[seq(from=1,to=72,12)],labels=F)
            text(x=my.date[seq(from=1,to=72,12)], y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]),
            labels= format(my.date[seq(from=1,to=72,12)],"%Y"), srt=45, adj=1, xpd=TRUE,cex=2)

            mtext(timelab, side = 1, line = 2.5,cex=2)
        }
    }
    options(default.options)
    if (graphics.reset == T) {
        par(op)
    }
    output = list(op = op, image.plt = image.plt, legend.plt = legend.plt)
    class(output) = "graphical parameters"
    out("Class attributes are accessible through following names:\n")
    out(names(output), "\n")
    return(invisible(output))
}

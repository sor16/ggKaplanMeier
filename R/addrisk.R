addrisk <- function(input){
    #require(reshape2)
    require(ggplot2)
    require(survival)
    require(gtable)
    list2env(input,envir=environment())
    if(!("gg" %in% class(plot))){
        stop("plot has to be of class ggplot")
    }
    xticks=ggplot_build(plot)$panel$ranges[[1]]$x.major_source
    xrange=ggplot_build(plot)$panel$ranges[[1]]$x.range
    risk.data <- data.frame(strata = summary(fit, times = xticks, extend = TRUE)$strata,
                            time = summary(fit, times = xticks, extend = TRUE)$time,
                            n.risk = summary(fit, times = xticks, extend = TRUE)$n.risk)

    tbl <- ggplot(risk.data, aes(x = time, y = factor(strata,levels=rev(levels(strata))), label=n.risk)) + coord_cartesian(xlim = xrange) +
        geom_text(size = 3.5)+theme_bw()+ylab("")+scale_y_discrete(breaks=levels(data$strata), labels=levels(data$strata))+ theme(
            panel.grid.major = element_blank(),
            legend.position = "none",
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            legend.position="none",
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks=element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_blank()
        )

    gb1=ggplot_build(plot)
    gb2=ggplot_build(tbl)

    n1 <- length(gb1$panel$ranges[[1]]$y.labels)
    n2 <- length(gb2$panel$ranges[[1]]$y.labels)

    gA <- ggplot_gtable(gb1)
    gB <- ggplot_gtable(gb2)

    # combine both plots (last should really be "pmax", it's an unfortunate bug)
    both <- gtable:::rbind_gtable(gA, gB, "last")

    # locate the panels in the gtable layout
    panels <- both$layout$t[grepl("panel", both$layout$name)]
    # assign new (relative) heights to the panels, based on the number of breaks
    #note that n1 and n2 was previously used, now replace by constants 1 and 8
    both$heights[panels] <- list(unit(8,"null"), unit(1, "null"))
    both <- gtable_add_rows(both, heights = unit(1,"line"), 8)
    both <- gtable_add_grob(both,
                            textGrob("Number at risk", hjust=0, x=0,gp = gpar(fontsize = 12)),
                            t=9, l=3, r=4)
    g <- arrangeGrob(plot, tbl,nrow=2)
#     grid.newpage()
#     grid.draw(both)
    return(plotobject=both)

}

addrisk <- function(input){
    require(ggplot2)
    require(survival)
    require(grid)
    require(gtable)
    require(gridExtra)
    list2env(input,envir=environment())
    if(!("gg" %in% class(plot))){
        stop("plot has to be of class ggplot")
    }
    
    risk.data$strata=factor(risk.data$strata,levels=rev(levels(risk.data$strata)))
    if(max(nchar(levels(risk.data$strata))) < 5){
        riskLabels=paste(levels(risk.data$strata),"       ")
    }else{
        riskLabels=levels(risk.data$strata)
    }
    tbl <- ggplot(risk.data, aes(x = time, y = strata, label=n.risk))  +
        geom_text(size = 3.5, na.rm=TRUE)+theme_bw()+ylab("")+scale_y_discrete(breaks=levels(risk.data$strata), labels=riskLabels) +
        theme(
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
            plot.title = element_blank(),
            plot.margin = unit(c(-0.2,1,1, 1),"cm")
      ) #+ xlim(min(f.frame$time),max(f.frame$time))

    gb1=ggplot_build(plot)
    gb2=ggplot_build(tbl)
    
    n1 <- length(gb1$panel$ranges[[1]]$y.labels)
    n2 <- length(gb2$panel$ranges[[1]]$y.labels)

    gA <- ggplot_gtable(gb1)
    gB <- ggplot_gtable(gb2)

    # combine both plots 
    #both <- rbind.gtable(gA, gB, "last")
    both <- gtable:::rbind_gtable(gA, gB, "last")

    # locate the panels in the gtable layout
    panels <- both$layout$t[grepl("panel", both$layout$name)]
    # assign new heights to the panels, based on the length of strata
    tableHeight=ifelse(length(f.frame$strata) < 4,1,2)
    both$heights[panels] <- unit.c(unit(8,"null"), unit(tableHeight, "null"))
    both <- gtable_add_rows(both, heights = unit(1,"line"), 8)
    both <- gtable_add_grob(both,
                             textGrob("Number at risk", hjust=0, x=0,gp = gpar(fontsize = 12)),
                             t=9, l=3, r=4)
    
    grid.newpage()
    grid.draw(both)                                                                                                                                                                                                                                                                                                                                                                                                                                             
    return(both)

}

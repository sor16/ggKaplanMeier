gg_KM_backup <- function(fit,title="",legend="none",confinterval=TRUE,startPoint=FALSE,background=TRUE,ticks="1x",ylabel="surv",xlabel="time",colors=c(),pval=data.frame(text="",x="",y="",stringsAsFactors = FALSE), namesOfStrata = c(), timeInYears = FALSE){
    require(scales)
    require(grid)
    require(ggplot2)
    require(survival)
    nrTicks=switch(ticks,"1x"=1,"2x"=2,"4x"=4)
    if(timeInYears) fit$time=fit$time/365.25
    if(length(namesOfStrata)!=0 && !is.null(fit$strata)) names(fit$strata) = namesOfStrata

    legend_position=switch(legend,"none"="none","top-right"=c(1,1),"bottom-right"=c(1,0),"top-left"=c(0,1),"bottom-left"=c(0,0))
    f.frame=as.data.frame(with(fit,cbind(time,n.risk,n.event,n.censor,surv,upper,lower)))
    if(!"strata" %in% names(fit)){
        f.frame$strata=factor("Overall")
        if(startPoint==FALSE){
            start=data.frame(time=c(0, f.frame$time[1]), n.risk=c(fit$n, fit$n), n.event=c(0,0), 
                                  n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1),strata=rep(levels(f.frame$strata),2))
            f.frame=rbind(start,f.frame)
        }
        g=ggplot(data=f.frame)+geom_step(aes(time,surv,colour=strata),direction="hv")+
            geom_point(data=subset(f.frame, n.censor > 0), aes(x=time, y=surv),shape=3)+theme_bw()+ggtitle(title)+
            ylab(ylabel)+xlab(xlabel)+
            theme(title = element_text(vjust=0),
                legend.position=legend_position,
                legend.title=element_blank(),
                legend.justification=legend_position,
                axis.title.x=element_text(vjust=0.7),
                plot.margin = unit(c(1,1,0.2, 1),"cm"))
        if(confinterval){
            g=g+geom_step(aes(time,upper,colour=strata),direction="hv",linetype="dashed")+
                geom_step(aes(time,lower,colour=strata),direction="hv",linetype="dashed") 
        }
    }else {
        strata=rep(names(fit$strata),fit$strata)
        f.frame$strata=factor(strata)
        if(startPoint==FALSE){
            zeros=rep(0,length(fit$strata))
            ones=rep(1,length(fit$strata))
            start=data.frame("time"=zeros,"n.risk"=fit$n,"n.event"=zeros,"n.censor"=zeros,"surv"=ones,"upper"=ones,"lower"=ones,"strata"=names(fit$strata))
            f.frame=rbind(start,f.frame)
        }
        f.frame=f.frame[with(f.frame,order(strata,time)),]
        g=ggplot(data=f.frame,aes(time,surv))+geom_step(aes(time,surv,colour=strata),direction="hv")+
            geom_point(data=subset(f.frame, n.censor > 0), aes(x=time, y=surv),shape=3)+theme_bw()+
            ggtitle(title)+ylab(ylabel)+xlab(xlabel)+
            theme(
                  title = element_text(vjust=2),
                  legend.position=legend_position,
                  legend.justification=legend_position,
                  legend.title=element_blank(),
                  axis.title.x=element_text(vjust=0),
                  axis.line = element_line(color = 'black'),
                  plot.margin = unit(c(1,1,0.2, 1),"cm"))

        
        if(confinterval){
            g=g+geom_step(aes(time,upper,colour=strata),direction="hv",linetype="dashed")+
                geom_step(aes(time,lower,colour=strata),direction="hv",linetype="dashed") 
        }

    }
    if(!background){
        g=g+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), axis.line = element_line(color = "black"), 
                  panel.border = element_blank()
        )
    }
    if(length(colors)!=0){
        g=g+scale_colour_manual(values=colors)
    }
    if(!is.na(as.numeric(pval$x)) && nchar(pval)!=0){
        g=g+geom_text(data=pval,aes(x,y,label=text))
    }
    
    xticks=ggplot_build(g)$panel$ranges[[1]]$x.major_source
    g=g+scale_x_continuous(breaks = pretty_breaks(n=length(xticks)*nrTicks))
    xticks=ggplot_build(g)$panel$ranges[[1]]$x.major_source
    
    if(!"strata" %in% names(fit)){
        summarystrata=rep(factor("Overall"),length(xticks))
    }else{
    summarystrata=summary(fit, times = xticks, extend = TRUE)$strata
    }
    risk.data <- data.frame(strata = summarystrata,
                            time = summary(fit, times = xticks, extend = TRUE)$time,
                            n.risk = summary(fit, times = xticks, extend = TRUE)$n.risk)
    
    return(list("plot"=g,"risk.data"=risk.data,"f.frame"=f.frame))
}
    
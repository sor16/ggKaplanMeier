gg_KM <- function(fit,title="",legend="none",confinterval=TRUE,startPoint=FALSE,cumIncidence=FALSE,background=TRUE,ticks="1x",ylabel="surv",xlabel="time",colors=c(),pval=data.frame(text="",x="",y="",stringsAsFactors = FALSE),x_limit=NULL namesOfStrata = c(), timeInYears = FALSE,legendTitle=""){
    require(scales)
    require(grid)
    require(ggplot2)
    require(survival)
    #bad solution, fix soon
    if(cumIncidence){
        fit[["surv"]]=1-fit[["surv"]]
        fit[["lower"]]=1-fit[["lower"]]
        fit[["upper"]]=1-fit[["upper"]]
    }
    nrTicks=switch(ticks,"1x"=1,"2x"=2,"4x"=4)
    if(timeInYears){
        fit$time=fit$time/365.25
    }
    
    legend_position=switch(legend,"none"="none","top-right"=c(1,1),"bottom-right"=c(1,0),"top-left"=c(0,1),"bottom-left"=c(0,0))
    
    f.frame=as.data.frame(with(fit,cbind(time,n.risk,n.event,n.censor,surv,upper,lower)))
    if(!"strata" %in% names(fit)){
        f.frame$strata=factor("Overall")
        namesOfStrata="Overall"
        #levels(fit$strata)="Overall"
    }else {
        if(length(namesOfStrata) == 0){
            namesOfStrata = names(fit$strata)
        }else{
            names(fit$strata)=namesOfStrata
        }
        strata=rep(namesOfStrata,fit$strata)
        f.frame$strata=factor(strata,levels=namesOfStrata,ordered = TRUE)
    }
    if(startPoint==FALSE){
        zeros=rep(0,nlevels(f.frame$strata))
        #Take into account if user wants to plot cumulative incidence 
        startValue=ifelse(cumIncidence,rep(0,nlevels(f.frame$strata)),rep(1,nlevels(f.frame$strata)))
        start=data.frame("time"=zeros,"n.risk"=fit$n,"n.event"=zeros,"n.censor"=zeros,"surv"=startValue,"upper"=startValue,"lower"=startValue,"strata"=factor(namesOfStrata,levels=namesOfStrata,ordered=TRUE))
        f.frame=rbind(start,f.frame)
    }
    f.frame=f.frame[with(f.frame,order(strata,time)),]
    g=ggplot(data=f.frame,aes(time,surv))+geom_step(aes(time,surv,colour=strata),direction="hv",na.rm=TRUE)+
        geom_point(data=subset(f.frame, n.censor > 0), aes(x=time, y=surv),shape=3,na.rm=TRUE)+theme_bw()+
        ggtitle(title)+ylab(ylabel)+xlab(xlabel)+ylim(0,1) + scale_color_discrete(name=legendTitle) +
        theme(
            title = element_text(vjust=2),
            legend.position=legend_position,
            legend.justification=legend_position,
            legend.background = element_rect(linetype = 1,colour="black",size=0.2),
            axis.title.x=element_text(vjust=0),
            axis.line = element_line(color = 'black'),
            plot.margin = unit(c(1,1,0.2, 1),"cm"))
    
    
    if(confinterval){
        g=g+geom_step(aes(time,upper,colour=strata),direction="hv",linetype="dashed",na.rm=TRUE)+
            geom_step(aes(time,lower,colour=strata),direction="hv",linetype="dashed",na.rm=TRUE) 
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
        g=g+geom_text(data=pval,aes(x,y,label=text),na.rm=TRUE)
    }
    if(is.null(x_limit)){
        x_limit=c(0,max(fit$time))
    }
    g=g+xlim(x_limit)
    xticks=ggplot_build(g)$panel$ranges[[1]]$x.major_source
    #Extract the right end of x axis
    xmax= ggplot_build(g)$panel$ranges[[1]]$x.range[2]
    #adding xticks and making sure they are non negative
    g=g+scale_x_continuous(breaks = pretty(c(min(fit$time),xmax),length(xticks)*nrTicks))
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
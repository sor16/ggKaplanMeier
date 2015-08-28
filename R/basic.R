basic <- function(fit,title="",legend="none",confinterval=TRUE){
    require(ggplot2)
    require(survival)
    legend_position=switch(legend,"none"="none","top-right"=c(1,1),"bottom-right"=c(1,0),"top-left"=c(0,1),"bottom-left"=c(0,0))
    f.frame=as.data.frame(with(fit,cbind(time,n.risk,n.event,n.censor,surv,upper,lower)))
    if(is.null(fit$strata)){
        rbind()
        g=ggplot(data=f.frame)+geom_step(aes(time,surv),direction="hv")+
            geom_point(data=subset(f.frame, n.censor > 0), aes(x=time, y=surv),shape=3)+theme_bw()+
        if(confinterval){
            g=g+geom_step(aes(time,upper),direction="hv",linetype="dashed")+
                geom_step(aes(time,lower),direction="hv",linetype="dashed") 
        }
    }else{
#         strata=rep(names(fit$strata),fit$strata)
#         f.frame$strata=factor(strata)
#         zeros=rep(0,length(fit$strata))
#         ones=rep(1,length(fit$strata))
#         start=data.frame("time"=zeros,"n.risk"=fit$n,"n.event"=zeros,"n.censor"=zeros,"surv"=ones,"upper"=ones,"lower"=ones,"strata"=names(fit$strata))
#         f.frame=rbind(start,f.frame)
        f.frame=f.frame[with(f.frame,order(strata,time)),]
        g=ggplot(data=f.frame,aes(time,surv))+geom_step(aes(time,surv,colour=strata),direction="hv")+
            geom_point(data=subset(f.frame, n.censor > 0), aes(x=time, y=surv),shape=3)+theme_bw()+
            ggtitle(title)+
            theme(#plot.background = element_blank(), 
                  #panel.grid.major = element_blank(),
                  #panel.grid.minor = element_blank(),
                  title = element_text(vjust=2),
                  #panel.border = element_blank(),
                  legend.position=legend_position,
                  legend.justification=legend_position,
                  #axis.line = element_line(color = 'black'),
                  plot.margin = unit(c(1,1,-0.2, 1),"cm"))
        if(confinterval){
            g=g+geom_step(aes(time,upper,colour=strata),direction="hv",linetype="dashed")+
                geom_step(aes(time,lower,colour=strata),direction="hv",linetype="dashed") 
        }
    }
    xticks=ggplot_build(g)$panel$ranges[[1]]$x.major_source
    risk.data <- data.frame(strata = summary(fit, times = xticks, extend = TRUE)$strata,
                            time = summary(fit, times = xticks, extend = TRUE)$time,
                            n.risk = summary(fit, times = xticks, extend = TRUE)$n.risk)
    
    return(list("plot"=g,"risk.data"=risk.data))
}
    
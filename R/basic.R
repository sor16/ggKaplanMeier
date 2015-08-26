basic <- function(fit,return=FALSE){
    require(ggplot2)
    require(survival)
    f.frame=as.data.frame(with(fit,cbind(time,n.risk,n.event,n.censor,surv,upper,lower)))
    if(is.null(fit$strata)){
        rbind()
        g=ggplot(data=f.frame)+geom_step(aes(time,surv),direction="hv")+
            geom_point(data=subset(f.frame, n.censor > 0), aes(x=time, y=surv),shape=3)+theme_bw()+
            geom_step(aes(time,upper),direction="hv",linetype="dashed")+geom_step(aes(time,lower),direction="hv",linetype="dashed")
        return(list("plot"=g,"data"=f.frame))
    }
    strata=rep(names(fit$strata),fit$strata)
    #factor(f.frame$strata,levels=rev(levels(f.frame$strata)))
    f.frame$strata=factor(strata)
    #zeros=rep(0,length(fit$strata))
    #ones=rep(1,length(fit$strata))
    #start=data.frame("time"=zeros,"n.risk"=fit$n,"n.event"=zeros,"n.censor"=zeros,"surv"=ones,"upper"=ones,"lower"=ones,"strata"=names(fit$strata))
    #f.frame=rbind(start,f.frame)
    f.frame=f.frame[with(f.frame,order(strata,time)),]
    g=ggplot(data=f.frame,aes(time,surv))+geom_step(aes(time,surv,colour=strata),direction="hv")+
        geom_point(data=subset(f.frame, n.censor > 0), aes(x=time, y=surv),shape=3)+theme_bw()+
        geom_step(aes(time,upper,colour=strata),direction="hv",linetype="dashed")+geom_step(aes(time,lower,colour=strata),direction="hv",linetype="dashed")+
        theme(plot.background = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              legend.position="none",
              axis.line = element_line(color = 'black'))
    
    return(list("plot"=g,"data"=f.frame))
}
    
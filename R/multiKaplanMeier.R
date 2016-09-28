multiKaplanMeier <- function(data,surv_object,wantedColumns,path,width=16,height=9.6,nrRisk=TRUE,...){
    require(survival)
    require(gridExtra)
    require(devtools)
    require(ggKaplanMeier)
    setwd(path)
    wantedColumns <- paste("^",wantedColumns,"$",sep="") %>% paste(collapse = "|")
    plotData <- data %>% select(matches(wantedColumns))
    lapply(1:ncol(plotData),function(i) {
        formula <- as.formula(paste("surv_object",names(plotData)[i],sep="~"))
        fit <- survfit(formula, data=plotData)
        
        if(nrRisk==TRUE){
            pdf(file=paste(names(plotData)[i],"pdf",sep="."), width =width , height = height,onefile=FALSE)
            grid.newpage()
            grid.draw(addrisk(gg_KM(fit,...)))
            dev.off()
        }else{
            pdf(file=paste(names(plotData)[i],"pdf",sep="."), width =width , height = height,onefile=FALSE)
            gg_KM(fit,...)$plot
            dev.off()
        }
        
    })
}
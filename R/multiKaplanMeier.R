multiKaplanMeier <- function(data,surv_object,wantedColumns,path,startAt=1,cumIncidence=TRUE...){
    require(survival)
    require(devtools)
    require(ggKaplanMeyer)
    setwd(path)
    wantedColumns <- paste("^",wantedColumns,"$",sep="") %>% paste(collapse = "|")
    plotData <- data %>% select(matches(wantedColumns))
    lapply(startAt:ncol(plotData),function(i) {
        formula <- as.formula(paste("surv_object",names(plotData)[i],sep="~"))
        fit <- survfit(formula, data=plotData)
        pdf(file=paste(names(plotData)[i],"pdf",sep="."), width =16 , height = 9.6,onefile=FALSE)
        grid.newpage()
        grid.draw(addrisk(gg_KM(fit,...)))
        dev.off()
    })
}
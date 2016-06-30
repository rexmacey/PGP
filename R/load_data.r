#' Load data from DFA CSV file
#' 
#' File should have Date in m/d/Y, followed by a risky and safe asset return in
#' decimal format example "1/1/1926",-0.0385,0.00338
#' 
#' @param fn Filename including path of csv file
#'   
#' @return list Risk is name of risky asset, Safe is name of safe.  data is
#'   dataframe containing RiskyRet, SafeRet, RiskyPx, and SafePx which are
#'   returns and prices.
#' @export
#' 
#' @examples load_dfa_csv("periodic returns")
load_dfa_csv<-function(fn){
    library(zoo)
    data<-read.csv(fn)
    temp<-as.Date(data$Date,"%m/%d/%Y")
    rownames(data)<-temp
    data$Date<-NULL
    out<-list()
    out$risky<-colnames(data)[1]
    out$safe<-colnames(data)[2]
    out$frequency<-"monthly"
    colnames(data)<-c("RiskyRet","SafeRet")
    data$RiskyPx<-cumprod(1+data$RiskyRet)
    data$SafePx<-cumprod(1+data$SafeRet)
    temp<-c(NA,NA,1,1)
    data<-rbind(temp,data)
    rownames(data)[1]<-format(as.Date(rownames(data)[2])-1,"%Y-%m-01")
    rownames(data)<-as.yearmon(rownames(data))
    out$data<-data
    class(out)<-"pgp"
    return(out)
}
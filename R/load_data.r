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
    # rownames(data)[1]<-format(as.Date(rownames(data)[2])-1,"%Y-%m-01")
    rownames(data)[1] <- format(as.Date(ISOdate(year(rownames(data)[2]),month(rownames(data)[2]),1))-1, "%Y-%m-%d")
    rownames(data)<-as.yearmon(rownames(data))
    out$data<-data
    class(out)<-"pgp"
    return(out)
}
# downloaded csv file from https://returnsweb.dimensional.com/
load_dfa_csv_w_bonds<-function(fn){
    library(readr)
    library(zoo)
    data <- read_csv(fn, skip = 7, col_types = "cnnnn")
    colnames(data) <- c("Date","RiskyRet", "SafeRet", "Notes", "Bonds")
    data <- data[!is.na(data$RiskyRet),]
    data$Date<-as.Date(data$Date,"%m/%d/%Y")
    data$BalRet <- 0.6*data$RiskyRet + 0.4*ifelse(is.na(data$Bonds), data$Notes, data$Bonds)
    data$RiskyPx<-cumprod(1+data$RiskyRet)
    data$SafePx<-cumprod(1+data$SafeRet)
    data$BalPx<-cumprod(1+data$BalRet)
    data<-rbind(data[1,],data)
    data[1,"Date"] <- format(as.Date(ISOdate(year(data$Date[1]),month(data$Date[1]),1))-1, "%Y-%m-%d")
    data[1,2:6] <- NA
    data[1,7:9] <- 1
    rownames(data)<-as.yearmon(data$Date)
    out<-list()
    out$risky<-colnames(data)[1]
    out$safe<-colnames(data)[2]
    out$balanced <- "60% Stocks / 40% Bonds"
    out$frequency<-"monthly"
    
    # rownames(data)[1]<-format(as.Date(rownames(data)[2])-1,"%Y-%m-01")
    #rownames(data)[1] <- format(as.Date(ISOdate(year(rownames(data)[2]),month(rownames(data)[2]),1))-1, "%Y-%m-%d")
    #rownames(data)<-as.yearmon(rownames(data))
    out$data<-data
    class(out)<-"pgp"
    return(out)
}
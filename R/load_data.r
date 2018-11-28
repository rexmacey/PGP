
#' Read csv file created by DFA online tool
#' Performs import, removes extra rows, makes column headings
#' Expects Date, Risky, Safe, Notes, Agg in that order
#' Downloaded csv file from https://returnsweb.dimensional.com/
#' @param fn name of file 
#'
#' @return a tbl.df object
#'
#' @examples read_dfa_csv(fn)
read_dfa_csv <- function(fn){
    library(readr)
    library(zoo)
    data <- read_csv(fn, skip = 8, col_types = "cnnnn",
        col_names = c("Date","RiskyRet", "SafeRet", "Notes", "Agg"))
    data <- data[!is.na(data$RiskyRet),]
    data$Date<-as.Date(data$Date,"%m/%d/%Y")
    return(data)
}

load_dfa_csv_w_bonds<-function(fn){
    data <- read_dfa_csv(fn)
    data$Bonds <- ifelse(is.na(data$Agg), data$Notes, data$Agg)
    # Create portfolio returns for all stock_wts
    out<-list()
    out$data <- data
    class(out)<-c("pgp", class(out))
    return(out)
}

#' Load data from DFA CSV file
#' Previous version without notes and bonds, just 2 columns here
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
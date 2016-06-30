#' Set Parameters of PGP object
#'
#' @param pgp 
#' @param width Number of prices in moving average
#' @param commission Commission rate in percent.
#'
#' @return pgp object
#' @export
#'
#' @examples set_params(pgp,11,0.1)
set_params<-function(pgp,width,commission){
    pgp$width<-width
    pgp$commission<-commission
    return(pgp)
}

#' pgp_backtest
#' 
#' Runs a backtest on the data.
#'
#' @param pgp A pgp object
#'
#' @return pgp updated pgp with results
#' @export
#'
#' @examples pgp_backtest(pgp)
pgp_backtest<-function(pgp){
    library(lubridate)
    library(xts)
    pgp<-avg_px(pgp)
    pgp<-signal(pgp)
    pgp<-period_returns(pgp)
    returns<-pgp$data[complete.cases(pgp$data),c("RiskyRet","SafeRet","Gross.ret","Net.ret")]
    d<-rownames(returns)
    d<-as.Date(as.yearmon(format(d),"%b %Y"))
    d<-d+days(days_in_month(d)-1)
    rownames(returns)<-as.Date(d)
    returns<-xts(returns,order.by = d)
    pgp$returns<-returns
    pgp$legs<-analyze_legs(pgp)
    return(pgp)
}

#' Average Price - adds a moving average of the prices
#'
#' @param pgp A PGP class object with at least a data frame with the RiskyPx column
#'
#' @return pgp object
#'
#' @examples avg_px(pgp)
avg_px<-function(pgp){
    library(zoo)
    out<-pgp
    temp<-rollmean(pgp$data$RiskyPx,k=pgp$width,fill=NA,align="right")
    out$data$AvgPx<-temp
    return(out)
}

#' Signal - Adds signal to pgp object.
#' 
#' The signal is true (hold risky) when RiskyPx >= AvgPx, otherwise false (hold
#' safe).  The signal is calculated at the end of the period and should be
#' applied to the following month.
#' 
#' @param pgp
#'   
#' @return pgp clas with signal column
#' 
#' @examples signal(pgp)
signal<-function(pgp){
    pgp$data$Signal<-pgp$data$RiskyPx>=pgp$data$AvgPx
    return(pgp)
}

#' Period Returns - generate periodic returns (monthly, daily depending on frequency of data)
#'
#' Calculates both gross and net returns using the commission rate.
#'
#' @param pgp pgp object
#'
#' @return pgp object
#'
#' @examples period_returns(pgp)
period_returns<-function(pgp){
    pgp$data$Gross.ret<-NA
    pgp$data$Net.ret<-NA
    signal.lag<-c(NA,pgp$data$Signal[1:(nrow(pgp$data)-1)])
    temp<-signal.lag*pgp$data$RiskyRet + (1-signal.lag)*pgp$data$SafeRet
    pgp$data$Gross.ret<-temp
    signal.lag2<-c(NA,NA,pgp$data$Signal[1:(nrow(pgp$data)-2)])
    pgp$data$Trade<-signal.lag!=signal.lag2
    pgp$data$Trade[pgp$width+1]<-FALSE
    pgp$data$Net.ret<-pgp$data$Gross.ret - pgp$data$Trade*pgp$commission/100
    return(pgp)
}


#' Analyze Legs
#' 
#' Useful for analyzing taxes and the degree of turnover.  A leg is a period
#' over which a position in either the risky or safe asset is held, that is from
#' the time it is bought until it is sold.
#' 
#' @param pgp  A pgp object
#'   
#' @return list A list with a detail of each leg including the start and end
#'   dates, the number of periods, the type (Risky or Safe) and the gross and
#'   net returns.  The Summary is a data frame with rows for the long term gains
#'   and losses, short-term gains and losses (for the Risky asset) and a row for
#'   the safe investments.  The summary for each row includes the number of
#'   legs, how many periods in total were invested in those legs and the
#'   cumulative gross and net returns.
#'   
#' @examples analyze_legs(pgp)
analyze_legs<-function(pgp){
    pi1y<-switch(pgp$frequency,monthly="12",daily=252)
    data<-pgp$data[complete.cases(pgp$data),]
    legs<-data.frame(Start=character(),End=character(),Periods=numeric(),Type=character(),Gross.ret=numeric(),
                     Net.ret=numeric())
    out<-list()
    legs.cnt<-1
    legs[legs.cnt,"Start"]<-rownames(data)[1]
    legs[legs.cnt,"Gross.ret"]<-1
    legs[legs.cnt,"Net.ret"]<-1
    legs[legs.cnt,"Type"]<-ifelse(data[1,"Signal"],"Risky","Safe")
    legs[legs.cnt,"Periods"]<-0
    for (i in 1:nrow(data)){
        if (data[i,"Trade"]){ 
            legs[legs.cnt,"End"]<-rownames(data)[i-1]
            legs.cnt<-legs.cnt+1
            legs[legs.cnt,"Start"]<-rownames(data)[i]
            legs[legs.cnt,"Gross.ret"]<-1+data[i,"Gross.ret"]
            legs[legs.cnt,"Net.ret"]<-1+data[i,"Net.ret"]
            legs[legs.cnt,"Type"]<-ifelse(data[i-1,"Signal"],"Risky","Safe")
            legs[legs.cnt,"Periods"]<-1
        } else {
            legs[legs.cnt,"Gross.ret"]<-legs[legs.cnt,"Gross.ret"]*(1+data[i,"Gross.ret"])
            legs[legs.cnt,"Net.ret"]<-legs[legs.cnt,"Net.ret"]*(1+data[i,"Net.ret"])
            legs[legs.cnt,"Periods"]<-legs[legs.cnt,"Periods"]+1
        }
    }
    legs[legs.cnt,"End"]<-rownames(data)[nrow(data)]
    legs.summary<-data.frame(NumLegs=numeric(5),NumPeriods=numeric(5),Gross.cumret=numeric(5),Net.cumret=numeric(5))
    rownames(legs.summary)<-c("LT Gains","LT Losses","ST Gains","ST Losses","Safe")
    idx <- legs$Type=="Risky" & legs$Periods>=pi1y & legs$Gross.ret>=1
    legs.summary["LT Gains","NumLegs"]<-sum(idx)
    legs.summary["LT Gains","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["LT Gains","Gross.cumret"]<-prod(legs[idx,"Gross.ret"])-1
    legs.summary["LT Gains","Net.cumret"]<-prod(legs[idx,"Net.ret"])-1
    idx <- legs$Type=="Risky" & legs$Periods<pi1y & legs$Gross.ret>=1
    legs.summary["ST Gains","NumLegs"]<-sum(idx)
    legs.summary["ST Gains","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["ST Gains","Gross.cumret"]<-prod(legs[idx,"Gross.ret"])-1
    legs.summary["ST Gains","Net.cumret"]<-prod(legs[idx,"Net.ret"])-1
    idx <- legs$Type=="Risky" & legs$Periods>=pi1y & legs$Gross.ret<1
    legs.summary["LT Losses","NumLegs"]<-sum(idx)
    legs.summary["LT Losses","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["LT Losses","Gross.cumret"]<-prod(legs[idx,"Gross.ret"])-1
    legs.summary["LT Losses","Net.cumret"]<-prod(legs[idx,"Net.ret"])-1
    idx <- legs$Type=="Risky" & legs$Periods<pi1y & legs$Gross.ret<1
    legs.summary["ST Losses","NumLegs"]<-sum(idx)
    legs.summary["ST Losses","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["ST Losses","Gross.cumret"]<-prod(legs[idx,"Gross.ret"])-1
    legs.summary["ST Losses","Net.cumret"]<-prod(legs[idx,"Net.ret"])-1
    idx <- legs$Type=="Safe"
    legs.summary["Safe","NumLegs"]<-sum(idx)
    legs.summary["Safe","NumPeriods"]<-sum(legs[idx,"Periods"])
    legs.summary["Safe","Gross.cumret"]<-prod(legs[idx,"Gross.ret"])-1
    legs.summary["Safe","Net.cumret"]<-prod(legs[idx,"Net.ret"])-1
    out$detail<-legs
    out$summary<-legs.summary
    return(out)
}


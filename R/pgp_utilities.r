#' Create a portfolio names
#' 
#' @param stock_wt Weights of stocks in portfolio decimal.
#'
#' @return character string "PortxSyB" where x is stock weight and y is bond weight
#'
#' @examples get_port_name(0.1)
get_port_name <- function(stock_wt){
    paste0("Port", round(stock_wt*100,0), "S", round(100-stock_wt*100,0), "B")
}

get_pgp_name <- function(pgp_wt){
    paste0("PGP", round(pgp_wt*100,0), "_Bond", round(100-pgp_wt*100,0))
}
add_port_returns_and_prices <- function(pgp){
    stock_wts <- pgp$params$stockwts
    data <- pgp$data
    for(i in stock_wts){
        data[,get_port_name(i)] <- i * data$RiskyRet + (1-i) * data$Bonds
    }
    # Create Price series (growth of 1) for all columns except date
    for(i in colnames(data)[!(colnames(data) %in% "Date")]){
        data[,paste0(i,"Px")] <- cumprod(1+data[,i])
    }
    data<-rbind(data[1,],data) # add a row at top; contents will be replaced
    data[1,"Date"] <- format(as.Date(ISOdate(year(data$Date[1]),month(data$Date[1]),1))-1, "%Y-%m-%d")
    data[1,2:(ncol(data)/2+1)] <- NA
    data[1,(ncol(data)/2+2):ncol(data)] <- 1
    pgp$data <- data
    return(pgp)
}

add_pgp_returns <- function(pgp){
    pgp_wts <- pgp$params$pgpwts
    data <- pgp$data
    for(i in pgp_wts){
        data[,get_pgp_name(i)] <- i * data$Net.ret + (1-i) * data$Bonds
    }
    pgp$data<-data
    return(pgp)
}

#' Set Parameters of PGP object
#'
#'
#' @return pgp object with list named params. Should contain at least width and commission
#' @export
#'
#' @examples set_params(pgp,11,0.1)
set_params<-function(pgp, ...){
    params <- list(...)
    if(!("width" %in% names(params))){
        params$width <- 12
        warning("width parameter missing. Setting to 12.")
    } 
    if(!("commission" %in% names(params))) {
        params$commission <- 0
        warning("commission parameter missing. Setting to 0.")
    }
    if(!("stockwts" %in% names(params))) {
        params$portwts <- seq(0,1,.01)
        warning("stockwts parameter missing. Setting to 0% to 100% by 10%.")
    }
    if(!("pgpwts" %in% names(params))) {
        params$pgpwts <- c(1, 0.8, 0.6, 0.5, 0.4)
        warning("pgpwts parameter missing. Setting to c(1, 0.8, 0.6, 0.5, 0.4).")
    }
    if(!("frequency" %in% names(params))) {
        params$frequency <- "monthly"
        warning("frequency parameter missing. Setting to monthly.")
    }
    pgp$params <- params
    # pgp$width<-width
    # pgp$commission<-commission
    return(pgp)
}

#' pgp_backtest
#' 
#' Runs a backtest on the data.
#'
#' @param pgp A pgp object which contains:
#'  data: a tibble with the monthly returns for raw data and portfolios, and PX values 
#'  frequency: monthly or daily
#'  width: 12 window of pgp
#'  commission for pgp trades e.g. 0.1 for 0.1%
#'  returns: monthly returns for pgp has lag
#'  legs: used for tax analysis
#'  g1d grows of $1, same periods as returns
#'  risky_dates for shading charts
#'
#' @return pgp updated pgp with results
#' @export
#'
#' @examples pgp_backtest(pgp)
pgp_backtest<-function(pgp){
    library(lubridate)
    library(xts)
    pgp<-add_port_returns_and_prices(pgp)
    pgp<-avg_px(pgp)
    pgp<-signal(pgp)
    pgp<-period_returns(pgp)
    pgp<-add_pgp_returns(pgp)
    # returns<-pgp$data[,c("Date","RiskyRet","SafeRet","Gross.ret","Net.ret")]
    returns<-pgp$data[, c("Date", "RiskyRet", "SafeRet", "Gross.ret", "Net.ret", 
                          names(pgp$data)[substr(names(pgp$data),1,4) %in% "Port"],
                          names(pgp$data)[substr(names(pgp$data),1,3) %in% "PGP"])]
    returns<-returns[complete.cases(returns),]
    #returns<-xts(data.frame(returns),order.by = returns$Date)
    pgp$returns<-returns
    pgp$legs<-analyze_legs(pgp)
    pgp$g1d <- pgp_g1d(pgp) 
    pgp$risky_dates <- get_risky_dates(pgp)
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
    pgp$data$AvgPx<-rollmean(pgp$data$RiskyRetPx,k=pgp$params$width,fill=NA,align="right")
    return(pgp)
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
    pgp$data$Signal<-pgp$data$RiskyRetPx>=pgp$data$AvgPx
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
    pgp$data$Trade[pgp$params$width+1]<-FALSE
    pgp$data$Net.ret<-pgp$data$Gross.ret - pgp$data$Trade*pgp$params$commission/100
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
    pi1y<-switch(pgp$params$frequency,monthly="12",daily=252)
    data<-pgp$data[complete.cases(pgp$data$Trade),]
    legs<-data.frame(Start=as.Date(character()),End=as.Date(character()),Periods=numeric(),Type=character(),Gross.ret=numeric(),
                     Net.ret=numeric())
    out<-list()
    legs.cnt<-1
    legs[legs.cnt,"Start"]<-data[1, "Date"]
    legs[legs.cnt,"Gross.ret"]<-1
    legs[legs.cnt,"Net.ret"]<-1
    legs[legs.cnt,"Type"]<-ifelse(data[1,"Signal"],"Risky","Safe")
    legs[legs.cnt,"Periods"]<-0
    for (i in 1:nrow(data)){
        if(as.logical(data[i,"Trade"])){ 
            #legs[legs.cnt,"End"]<-rownames(data)[i-1]
            legs[legs.cnt,"End"]<-data$Date[i-1]
            legs.cnt<-legs.cnt+1
            #legs[legs.cnt,"Start"]<-rownames(data)[i]
            legs[legs.cnt,"Start"]<-data$Date[i]
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
    legs[legs.cnt,"End"]<-data$Date[nrow(data)]
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

pgp_g1d <- function(pgp){
    d1 <- pgp$returns[1, "Date", drop=TRUE] %m-% months(1)
    x <- apply(1+pgp$returns[,!(colnames(pgp$returns) %in% "Date")], 2, cumprod)
    x <- rbind(1,x)
    x <- tibble::as.tibble(x)
    x <- cbind(Date=c(d1, pgp$returns$Date), x)
    return(x)
}

plot.pgp <- function(pgp){
    idx <- pgp$legs$detail$Type=="Risky"
    sdt <- as.Date(as.yearmon(pgp$legs$detail[idx,"Start"]),frac=1)
    edt <- as.Date(as.yearmon(pgp$legs$detail[idx,"End"]),frac=1)
    risky.dates <- data.frame(BegRisky=sdt, EndRisky=edt)
    
    ggplot(data = pgp$g1d) + geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        theme_bw() +
        geom_rect(data=risky.dates, aes(xmin=BegRisky, xmax=EndRisky, ymin=0.1, ymax=+Inf), fill="pink", alpha=1,
                  inherit.aes = TRUE) +
        geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=Net.ret, col="PGP")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=SafeRet, col="CASH")) +
        scale_y_log10() + theme_bw() +
        scale_color_manual(labels=c("Cash", "PGP", "S&P Index"), 
                           values = c("SP5"="red", "PGP"="blue","CASH"="green"),
                           name = "") +
        ggtitle("Long Term Growth of Stocks, Cash, and PGP") + xlab("") + ylab("Growth of $1") +
        theme(legend.position = "top", legend.direction = "horizontal")
}

pgp_backtest2<-function(pgp){
    library(lubridate)
    library(xts)
    pgp<-avg_px(pgp)
    pgp<-signal(pgp)
    pgp<-period_returns(pgp)
    cnames<-c("Date", "RiskyRet","SafeRet","Gross.ret","Net.ret", "BalRet")
    returns<-pgp$data[complete.cases(pgp$data[,cnames]),cnames]
    d <- returns$Date
    returns$Date <- NULL
    returns<-xts(returns,order.by = d)
    pgp$returns<-returns
    pgp$legs<-analyze_legs2(pgp)
    pgp$g1d <- pgp_g1d(pgp) 
    return(pgp)
}

analyze_legs2<-function(pgp){
    pi1y<-switch(pgp$params$frequency,monthly="12",daily=252)
    cnames<-c("Date", "RiskyRet","SafeRet","Gross.ret","Net.ret", "BalRet", "AvgPx", "Signal", "Trade",
              "RiskyPx", "SafePx", "BalPx")
    data<-pgp$data[complete.cases(pgp$data[,cnames]),cnames]
    legs<-data.frame(Start=character(),End=character(),Periods=numeric(),Type=character(),Gross.ret=numeric(),
                     Net.ret=numeric())
    out<-list()
    legs.cnt<-1
    legs[legs.cnt,"Start"]<-rownames(data)[1]
    legs[legs.cnt,"Gross.ret"]<-1
    legs[legs.cnt,"Net.ret"]<-1
    legs[legs.cnt,"Type"]<-ifelse(data[1,"Signal"],"Risky","Safe")
    legs[legs.cnt,"Periods"]<-0
    browser()
    for (i in 1:nrow(data)){
        if(data[i,"Trade"]){ 
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

plot.pgp2 <- function(pgp){
    risky.dates <- pgp$risky_dates
    ggplot(data = pgp$g1d) + geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        theme_bw() +
        geom_rect(data=risky.dates, aes(xmin=BegRisky, xmax=EndRisky, ymin=0.1, ymax=+Inf), fill="pink", alpha=1,
                  inherit.aes = TRUE) +
        geom_line(aes(x=Date, y=RiskyRet, col="SP5")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=Net.ret, col="PGP")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=SafeRet, col="CASH")) +
        geom_line(data = pgp$g1d, aes(x=Date, y=BalRet, col="BAL")) +
        scale_y_log10() + theme_bw() +
        scale_color_manual(labels=c("Balanced", "Cash", "PGP", "S&P Index"), 
                           values = c("SP5"="red", "PGP"="blue","CASH"="green", "BAL"="chartreuse"),
                           name = "") +
        ggtitle("Long Term Growth") + xlab("") + ylab("Growth of $1") +
        theme(legend.position = "top", legend.direction = "horizontal")
}

#' Get Risky Dates
#'
#' @param pgp 
#'
#' @return data frame with two columns, start date of risky, end date of risky
#' @export
#'
#' @examples get_risky_dates(pgp)
get_risky_dates <- function(pgp){
    idx <- pgp$legs$detail$Type=="Risky"
    sdt <- as.Date(as.yearmon(pgp$legs$detail[idx,"Start"]),frac=1)
    edt <- as.Date(as.yearmon(pgp$legs$detail[idx,"End"]),frac=1)
    return(data.frame(BegRisky=sdt, EndRisky=edt))
}


# start plotly related functions

#' Create Rectangle list object for use in shading G1D chart
#'
#' @param i The row number of the "risky_dates" data frame
#' @param shade_dates 
#'
#' @return list with information for shading
#'
#' @examples create_rectangle(1, risky_dates)
create_rectangle <- function(i, shade_dates){
    # shades dates is a data.frame. First column is start date, second is end. 
    list(type = "rect",
         fillcolor = "pink", line=list(color="pink"), opacity = 0.4,
         x0 = shade_dates[i,1], x1 = shade_dates[i,2], xref = "x",
         y0 = 0, y1=1, yref="paper")
}

#' Create G1D chart in plotly
#'
#' @param pgp 
#'
#' @return plotly chart
#' @export
#'
#' @examples create_g1d_plotly(pgp)
create_g1d_plotly <- function(pgp){
    p <- plot_ly(data = pgp$g1d, x = ~Date, y = ~RiskyRet, mode = "lines", type = "scatter",
                 name = "S&P Index",
                 line = list(color="red")) %>%
        add_trace(y = ~Net.ret, name = "PGP", line = list(color = "blue")) %>%
        add_trace(y = ~BalRet, name = "Balanced", line = list(color = "chartreuse")) %>%
        add_trace(y = ~SafeRet, name = "Cash", line = list(color = "green")) %>%
        layout(title = "Long Term Growth",
               yaxis = list(title = "Growth of $1", type="log"),
               xaxis = list(title = ""))
    
    temp <- lapply(1:nrow(risky_dates), create_rectangle, risky_dates=pgp$risky_dates)
    g1d <- layout(p, shapes = temp)
    return(g1d)
}

#' Create boxplot in plotly
#' This is a single time frame.  
#'
#' @param tbl_roll An xts object with SPIndex, PGP, and Balanced columns
#'
#' @return plotly chart
#' @export
#'
#' @examples create_plotly_boxplot(table_roll5)
create_boxplot_plotly <- function(tbl_roll){
    data = data.frame(coredata(tbl_roll[complete.cases(tbl_roll),]))
    p <- plot_ly(data = data, y = ~SPIndex, type="box",
                 name = "S&P Index", color="red") %>%
        add_trace(y = ~PGP, name="PGP", color="blue") %>%
        add_trace(y = ~Balanced, name="Balanced", color="chartreuse")
    return(p)
}    
# end plotly related functions


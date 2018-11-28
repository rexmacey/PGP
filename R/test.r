library(zoo)
library(lubridate)
library(PerformanceAnalytics)
library(ggplot2)
library(knitr)
library(kableExtra)
library(gridExtra)
library(RColorBrewer)
source(paste0(Sys.getenv("HOME"),"/Quant Trading/PGP/r/load_data.r"))
source(paste0(Sys.getenv("HOME"),"/Quant Trading/PGP/r/pgp_utilities.r"))
fn <- paste0(Sys.getenv("HOME"), "/Quant Trading/PGP/data/PeriodicReturnsWBonds201810.csv")
pgp<-load_dfa_csv_w_bonds(fn)
head(pgp$data)
tail(pgp$data)
pgp<-set_params(pgp,width=12,commission=0.1, stockwts=seq(0,1,.1))

#--------------------
pgp<-pgp_backtest(pgp)

head(pgp$data,20)
tail(pgp$data)

library(PerformanceAnalytics)
table.AnnualizedReturns(pgp$returns)


testf <- function(pgp, ...){
   list(...)
    #out<-list()
    #for(i in 1:length(arguments)){
    #    out[i] <- arguments[i]
    #    # names(out[i]) <- names(arguments[i])
    #}
    #names(out) <- names(arguments)
    #return(out)
}

test <- testf(pgp, width=12, commission=0.1, portwts=seq(0,1,.1))

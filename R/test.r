pgp<-load_dfa_csv("./data/periodicreturns.csv")
head(pgp$data)
tail(pgp$data)

pgp<-set_params(pgp,width=12,commission=0.1)
pgp<-pgp_backtest(pgp)

head(pgp$data,20)
tail(pgp$data)

library(PerformanceAnalytics)
table.AnnualizedReturns(pgp$returns)


library(tis)
nber <- nberDates()

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
    scale_color_manual(labels=c("Cash", "PGP", "S&P 500"), 
                       values = c("SP5"="red", "PGP"="blue","CASH"="green"),
                       name = "") +
    ggtitle("Long Term Growth of Stocks, Cash, and PGP") + xlab("") + ylab("Growth of $1") +
    theme(legend.position = "top", legend.direction = "horizontal")



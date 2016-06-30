pgp<-load_dfa_csv("./data/periodicreturns.csv")
head(pgp$data)
tail(pgp$data)

pgp<-set_params(pgp,width=11,commission=0.1)
pgp<-pgp_backtest(pgp)

head(pgp$data,20)
tail(pgp$data)

library(PerformanceAnalytics)
table.AnnualizedReturns(pgp$returns)

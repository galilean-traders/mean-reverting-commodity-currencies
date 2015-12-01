#!/usr/bin/env Rscript

library(data.table)
library(reshape2)
library(argparse)
library(TTR)

argument.parser <- ArgumentParser(description="perform the johansen test")
argument.parser$add_argument(
    "input",
    nargs="?",
    default="time_series.rds",
    help="input rds file")

args <- argument.parser$parse_args()
dt <- readRDS(args$input)

to.test <- data.table(dcast(dt, time ~ name, value.var="closeBid"))[,
                                                                    .(time, aud_eur, cad_eur)]
to.test2 <- data.table(dcast(dt, time ~ name, value.var="closeAsk"))[,
                                                                     .(time, aud_eur, cad_eur)]

merged <- merge(to.test, to.test2, by="time", suffixes=c("_bid", "_ask"))

portfolio.prices <- merged[, `:=`(
                                  portfolio_bid = aud_eur_bid - 2.28 * cad_eur_ask,
                                  portfolio_ask = aud_eur_ask - 2.28 * cad_eur_bid
                                  )
]

lookback <- 11428

portfolio <- portfolio.prices[complete.cases(portfolio.prices),
    units := (portfolio_bid - runMean(portfolio_bid, lookback)) / runSD(portfolio_bid, lookback)
    ]

portfolio <- portfolio[complete.cases(portfolio)][, ..I := .I]

portfolio$position <- c(NA, diff(portfolio$units))

calculate.pnl <- function(position, portfolio_bid, portfolio_ask) {
    if (position > 0) {
        return(position * portfolio_ask)
    }
    else {
        return(-position * portfolio_bid)
    }
}

portfolio <- portfolio[complete.cases(portfolio),
    pnl := calculate.pnl(position, portfolio_bid, portfolio_ask),
    by=..I]

print(portfolio)

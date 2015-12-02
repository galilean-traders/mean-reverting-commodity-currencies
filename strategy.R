#!/usr/bin/env Rscript

library(data.table)
library(reshape2)
library(argparse)
library(TTR)
library(ggplot2)

theme_set(theme_bw(base_size=12) + theme(
    legend.key.size=unit(1, 'lines'),
    text=element_text(face='plain', family='CM Roman'),
    legend.title=element_text(face='plain'),
    axis.line=element_line(color='black'),
    axis.title.y=element_text(vjust=0.1),
    axis.title.x=element_text(vjust=0.1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank(),
    panel.border = element_blank()
))

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

portfolio <- portfolio[complete.cases(portfolio)]

portfolio$trade <- c(portfolio[1, units], diff(portfolio$units))

calculate.cost <- function(trade, bid, ask) {
    if (trade > 0) {
        value <- -trade * ask
    }
    else {
        value <- -trade * bid
    }
    return(value)
}

portfolio[,
    cost := calculate.cost(trade, portfolio_bid, portfolio_ask),
    by=time]

portfolio[,
    account := cumsum(cost)
    ]

calculate.value = function(units, bid, ask) {
        if (units > 0) {
            return(units * bid)
        }
        else {
            return(units * ask)
        }
    }

portfolio[,
    value := calculate.value(units, portfolio_bid, portfolio_ask), by=time]

portfolio[, equity := value + account]

print(portfolio)

plot <- ggplot(portfolio, aes(x=time, y=equity)) + geom_line()

width = 10
factor = 0.618
height = width * factor
X11(width=width, height=height)
print(plot)
warnings()
invisible(readLines(con="stdin", 1))

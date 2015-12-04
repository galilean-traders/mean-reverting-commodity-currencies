#!/usr/bin/env Rscript

library(data.table)
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

argument.parser <- ArgumentParser(description="simulate the strategy")
argument.parser$add_argument(
    "--time_series",
    nargs="?",
    default="time_series.rds",
    help="input rds file"
)
argument.parser$add_argument(
    "--johansen",
    nargs="?",
    default="johansen.rds",
    help="input rds file"
)

args <- argument.parser$parse_args()
dt <- readRDS(args$time_series)
johansen <- readRDS(args$johansen)
lookback <- round(log(2) / johansen$lambda)

to.test <- dcast(dt,
          time ~ name,
          value.var=c("closeBid", "closeAsk", "openBid", "openAsk")
          )

column.names <- function(eigenvector, prefix, positive, negative) {
    names <- names(eigenvector)
    return(sapply(names, function(name) {
        value <- eigenvector[name]
        currency <- gsub(".l2", "", name)
        if (value > 0) {
            return(paste0(prefix, positive, "_", currency))
        }
        else {
            return(paste0(prefix, negative, "_", currency))
        }
    }))
}

eigenvector <- head(johansen$v, -1)
close_ask.names <- column.names(eigenvector, "close", "Ask", "Bid")
close_bid.names <- column.names(eigenvector, "close", "Bid", "Ask")
open_ask.names <- column.names(eigenvector, "open", "Ask", "Bid")
open_bid.names <- column.names(eigenvector, "open", "Bid", "Ask")

to.test[, portfolio_close_ask := as.matrix(.SD) %*% eigenvector,
        .SDcols=close_ask.names]
to.test[, portfolio_close_bid := as.matrix(.SD) %*% eigenvector,
        .SDcols=close_bid.names]
to.test[, portfolio_open_ask := as.matrix(.SD) %*% eigenvector,
        .SDcols=open_ask.names]
to.test[, portfolio_open_bid := as.matrix(.SD) %*% eigenvector,
        .SDcols=open_bid.names]

to.test[complete.cases(to.test),
    score := (portfolio_close_bid - runMean(portfolio_close_bid, lookback))
    / runSD(portfolio_close_bid, lookback)
    ]

entry.score <- 1
exit.score <- -0.9

trades <- to.test[,
    list(
    time=time,
    long.entry=(score > entry.score & shift(score) < entry.score),
    long.exit=(score < exit.score & shift(score) > exit.score),
    short.entry=(score < -entry.score & shift(score) > -entry.score),
    short.exit=(score > exit.score & shift(score) < exit.score)
    )
    ]

portfolio <- merge(to.test, trades, by="time")

portfolio[, trades := (
            - long.entry * shift(portfolio_open_ask, type="lead")
            + long.exit * shift(portfolio_open_bid, type="lead")
            - short.exit * shift(portfolio_open_ask, type="lead")
            + short.entry * shift(portfolio_open_bid, type="lead")
            )
]
portfolio[complete.cases(portfolio),
          position := (long.entry - short.entry + short.exit - long.exit)]

portfolio[
    complete.cases(portfolio),
    p.and.l := cumsum(trades)
        ]

print(portfolio)

plot <- ggplot(portfolio, aes(x=time, y=score)) + geom_line()

width = 15
factor = 0.618
height = width * factor
X11(width=width, height=height)
print(plot)
warnings()
invisible(readLines(con="stdin", 1))

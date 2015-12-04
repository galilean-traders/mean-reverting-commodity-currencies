#!/usr/bin/env Rscript

library(data.table)
library(reshape2)
library(argparse)
library(urca)

argument.parser <- ArgumentParser(description="perform the johansen test")
argument.parser$add_argument(
    "input",
    nargs="?",
    default="time_series.rds",
    help="input rds file")
argument.parser$add_argument(
    "output",
    nargs="?",
    default="johansen.rds",
    help="output rds file")

args <- argument.parser$parse_args()
dt <- readRDS(args$input)

dt[, closeMid := (closeBid + closeAsk) / 2]
to.test <- data.table(dcast(dt, time ~ name, value.var="closeMid"))[, time := NULL]

johansen.test <- ca.jo(to.test, ecdet="const", type="trace")

print(summary(johansen.test))

lambda <- johansen.test@lambda[1]
v <- johansen.test@V[, 1]

saveRDS(list(lambda=lambda, v=v), args$output)

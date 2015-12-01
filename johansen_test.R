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

args <- argument.parser$parse_args()
dt <- readRDS(args$input)

to.test <- data.table(dcast(dt, time ~ name, value.var="closeBid"))[, time :=
                                                                    NULL]
to.test <- to.test[, .(aud_eur, cad_eur)]

print(to.test)

johansen.test <- ca.jo(to.test, ecdet="const", type="trace")

print(summary(johansen.test))


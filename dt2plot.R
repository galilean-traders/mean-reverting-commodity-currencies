#!/usr/bin/env Rscript

library(ggplot2)
library(data.table)
library(argparse)

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

argument.parser <- ArgumentParser(description="plot the data table")
argument.parser$add_argument(
    "input",
    nargs="?",
    default="time_series.rds",
    help="input rds file")

args <- argument.parser$parse_args()
dt <- readRDS(args$input)

plot <- ggplot(dt, aes(x=time, y=(closeBid + closeAsk) / 2, colour=name)) +
geom_line(aes(group=name))

width = 7
factor = 0.618
height = width * factor
X11(width=width, height=height)
print(plot)
warnings()
invisible(readLines(con="stdin", 1))

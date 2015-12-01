#!/usr/bin/env Rscript

library(argparse)
library(jsonlite)
library(data.table)
library(tools)

argument.parser <- ArgumentParser(description="read json into data.table")
argument.parser$add_argument(
    "input",
    nargs="+",
    help="input json files")
argument.parser$add_argument(
    "--output",
    nargs="?",
    default="time_series.rds",
    help="input json files")

args <- argument.parser$parse_args()
files <- args$input

jsons <- lapply(files, function(filename) {
                data <- fromJSON(filename)
                table <- data.table(data)
                # invert quotes as we need prices in eur, not in the foreign
                # currency
                # xplaind.com/676985/indirect-quote
                base <- file_path_sans_ext(basename(filename))
                split <- strsplit(base, "_")[[1]]
                name <- paste0(split[2], "_", split[1])
                indirect <- table[, .(
                    openBid = 1 / openAsk,
                    lowBid = 1 / highAsk,
                    closeBid = 1 / closeAsk,
                    highBid = 1 / lowAsk,
                    openAsk = 1 / openBid,
                    lowAsk = 1 / highBid,
                    closeAsk = 1 / closeBid,
                    highAsk = 1 / lowBid,
                    name = name,
                    time = as.POSIXct(time,
                        format="%Y-%m-%dT%H:%M:%OSZ",
                        tz="UTC"),
                    volume = volume
                    )
                    ]
                return(indirect)
    }
)

merged <- rbindlist(jsons)
print(merged)

saveRDS(merged, args$output)

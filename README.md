# mean-reverting-commodity-currencies
test and trade a mean reverting portfolio of cad aud zar nok

# download data from oanda
```bash
make -j4 -B
```

# import the json files into a time series
```bash
./json2dt.R data/*.json --output time_series.rds
```

# plot the time series
```bash
./dt2plot.R --input time_series.rds
```

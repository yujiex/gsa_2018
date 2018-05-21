library("DBI")
library("tsoutliers")
library("dygraphs")
library("xts")
## data("hicp")
## tso(y = log(hicp[[1]]))

con = dbConnect(drv=RSQLite::SQLite(), dbname="csv_FY/db/interval_ion.db")
ids = dbGetQuery(con, 'SELECT DISTINCT Building_Number FROM electric')[['Building_Number']]
ids[1:1]

type = "AO"
for (x in ids[1:1]) {
    query = paste0(sprintf("SELECT Building_Number, CAST (SUM([Electric_(KWH)]) AS INT) AS Electric, Timestamp FROM electric WHERE Building_Number = '%s'", x), " GROUP BY STRFTIME('%Y', Timestamp), STRFTIME('%m', Timestamp), STRFTIME('%d', Timestamp), STRFTIME('%H', Timestamp);")
    p1 = dbGetQuery(con, query)
    p1 <- head(p1, n=1000)
    ## print(head(p1))
    ## ## print(nrow(p1))
    y = ts(p1$Electric)
    ## print(length(y))
    ## ## dygraph(y) %>% dyRangeSelector() %>% print()
    ## ## timeseries = xts(p1["Electric"], as.POSIXlt(p1$Timestamp))
    ## ## head(y)
    result = tso(y, types=c(type))
    print(plot.tsoutliers(result))
    ## dygraph(result) %>% dyRangeSelector() %>% print()
}

## type = "AO"
df = read.csv("/media/yujiex/work/ROCIS/ROCIS/DataBySensor/Dylos/chop_start/round_all/ARR_O_D950_7-15-2016_outside_unknown.log")
print(nrow(df))
df <- head(df, n=1000)
yr = ts(df$Small)
print(head(yr))
resultr = tso(yr)
png(sprintf("/home/yujiex/Desktop/%s.png", "All"), 1000, 500)
plot.tsoutliers(resultr)
dev.off()

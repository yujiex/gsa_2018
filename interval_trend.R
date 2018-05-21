library("DBI")
library("dygraphs")
library("xts")
library("htmlwidgets")
## hourly
con = dbConnect(drv=RSQLite::SQLite(), dbname="csv_FY/db/interval_ion.db")

hourTrend <- function(metertype, column, ylabel, status) {
    if (status == "raw") {
        filter = ""
        outdir = "plot_interval_hour"
    } else {
        filter = " AND outlier == '0'"
        outdir = "plot_interval_hour_clean"
    }
    ids = dbGetQuery(con, sprintf('SELECT DISTINCT Building_Number FROM %s_outlier_tag', metertype))[['Building_Number']]
    print(length(ids))
    for (x in ids) {
        query = paste0(sprintf("SELECT Building_Number, outlier, SUM([%s]) AS %s", column, metertype), ", STRFTIME('%Y-%m-%d %H:00:00', Timestamp) AS Hour ", sprintf("FROM %s_outlier_tag WHERE Building_Number = '%s'%s", metertype, x, filter), " GROUP BY Hour;")
        ## query = paste0(sprintf("SELECT Building_Number, SUM([%s]) AS %s", column, metertype), ", STRFTIME('%Y-%m-%d %H:00:00', Timestamp) AS Hour ", sprintf("FROM %s_outlier_tag WHERE Building_Number = '%s'", metertype, x), " GROUP BY Hour;")
        p1 = dbGetQuery(con, query)
        query2 = sprintf("SELECT Building_Number, timeZoneId FROM EUAS_timezone WHERE Building_Number = '%s'", x)
        p2 = dbGetQuery(con, query2)
        ## print(head(p2))
        tzid = p2[1,2]
        ## print(tzid)
        timeseries = xts(p1[metertype], as.POSIXct(p1$Hour))
        dygraph(timeseries, xlab='Time', ylab=ylabel, main=paste0(x, sprintf('%s_hourly', metertype))) %>% dyOptions(useDataTimezone = TRUE) %>% dyRangeSelector() %>% saveWidget(sprintf("%s/%s_%s.html", outdir, x, metertype), selfcontained=FALSE,libdir=outdir)
    }
}
## hourTrend("gas", "Gas_(CubicFeet)", "Cubic Feet", "raw")
hourTrend("gas", "Gas_(CubicFeet)", "Cubic Feet", "clean")
hourTrend("electric", "Electric_(KWH)", "KWH", "raw")
hourTrend("electric", "Electric_(KWH)", "KWH", "clean")

## original 15 min
con = dbConnect(drv=RSQLite::SQLite(), dbname="csv_FY/db/interval_ion.db")
ids = dbGetQuery(con, 'SELECT DISTINCT Building_Number FROM gas')[['Building_Number']]
for (x in ids[1]) {
    p1 = dbGetQuery(con,sprintf('SELECT * FROM gas WHERE Building_Number = \'%s\'', x)) ## original
    timeseries = xts(p1["Gas_(CubicFeet)"], as.POSIXlt(p1$Timestamp))
    dygraph(timeseries, xlab='Time', ylab='Cubic Feet', main=paste0(x, 'Gas')) %>% dyRangeSelector() %>% saveWidget(sprintf("plot_interval/%s_gas.html", x), selfcontained=FALSE,libdir="plot_interval")
}

ids = dbGetQuery(con, 'SELECT DISTINCT Building_Number FROM electric')[['Building_Number']]
for (x in ids) {
    p1 = dbGetQuery(con,sprintf('SELECT * FROM electric WHERE Building_Number = \'%s\'', x))
    timeseries = xts(p1["Electric_(KWH)"], as.POSIXlt(p1$Timestamp))
    dygraph(timeseries, xlab='Time', ylab='KWH', main=paste0(x, ' Electric')) %>% dyRangeSelector() %>% saveWidget(sprintf("plot_interval/%s_electric.html", x), selfcontained=FALSE,libdir="plot_interval")
}

## after remove outlier
con = dbConnect(drv=RSQLite::SQLite(), dbname="csv_FY/db/interval_ion.db")
ids = dbGetQuery(con, 'SELECT DISTINCT Building_Number FROM gas_outlier_tag')[['Building_Number']]
for (x in ids) {
    print(x)
    p1 = dbGetQuery(con,sprintf('SELECT * FROM gas_outlier_tag WHERE Building_Number = \'%s\' AND outlier == \'0\'', x)) ## original
    timeseries = xts(p1["Gas_(CubicFeet)"], as.POSIXlt(p1$Timestamp))
    dygraph(timeseries, xlab='Time', ylab='Cubic Feet', main=paste0(x, 'Gas')) %>% dyRangeSelector() %>% saveWidget(sprintf("remove_outlier_gas/%s_gas.html", x), selfcontained=FALSE,libdir="remove_outlier_gas")
}

ids = dbGetQuery(con, 'SELECT DISTINCT Building_Number FROM electric_outlier_tag')[['Building_Number']]
for (x in ids) {
    print(x)
    p1 = dbGetQuery(con,sprintf('SELECT * FROM electric_outlier_tag WHERE Building_Number = \'%s\' AND outlier = \'0\'', x))
    timeseries = xts(p1["Electric_(KWH)"], as.POSIXlt(p1$Timestamp))
    dygraph(timeseries, xlab='Time', ylab='KWH', main=paste0(x, ' Electric')) %>% dyRangeSelector() %>% saveWidget(sprintf("remove_outlier_elec/%s_electric.html", x), selfcontained=FALSE,libdir="remove_outlier_elec")
}

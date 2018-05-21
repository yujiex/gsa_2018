## hard coded removing 0 energy consumptions, might look into removing trailing = (baselineBill - actualBill) / baselineBill
## 0's

library("readxl")
library("dplyr")
library("locpol")
library("DBI")
library("RSQLite")
library("ggplot2")
library("mgcv")
library("zoo")
library("readr")

con <- dbConnect(RSQLite::SQLite(), "csv_FY/db/all.db")

alltables = dbListTables(con)

## building = projDate[1, "Building_Number"]
## projectDate = projDate[1, "Date"]

querystr = paste0("SELECT * FROM EUAS_monthly LIMIT 5")
dbGetQuery(con,  querystr) %>%
  as_data_frame() %>%
  {.}

querystr = paste0("SELECT DISTINCT [Building_Number] [Region_No.] FROM EUAS_monthly WHERE [Region_No.] = '9'")
dbGetQuery(con,  querystr) %>%
  as_data_frame() %>%
  readr::write_csv("csv_FY/region9buildings.csv")
  ## `eui_elec`, `eui_gas`) %>%

compute_roi <- function(building, projectDate) {
  querystr = paste0("SELECT * FROM EUAS_monthly_weather WHERE [Building_Number] = '", building, "'")
  ## querystr = paste0("SELECT * FROM EUAS_monthly_weather", "")
  df = dbGetQuery(con,  querystr) %>%
    as_data_frame() %>%
    dplyr::select(`Building_Number`, `year`, `month`, `hdd65`, `cdd65`, `ave`,
                  `Electric_(kBtu)`, `Gas_(kBtu)`) %>%
                  ## `eui_elec`, `eui_gas`) %>%
    dplyr::mutate(`Date`=zoo::as.Date(zoo::as.yearmon(paste(`year`, `month`), "%Y %m"))) %>%
    {.}

  queryarea = paste0("SELECT [Gross_Sq.Ft] FROM EUAS_area WHERE [Building_Number] = '", building, "' ORDER BY Fiscal_Year DESC LIMIT 1")
  area = dbGetQuery(con,  queryarea) %>%
    .$`Gross_Sq.Ft`

  dfElec = df %>%
    ## dplyr::select(-`eui_gas`) %>%
    dplyr::select(-`Gas_(kBtu)`) %>%
    dplyr::mutate(`Electric` = `Electric_(kBtu)`) %>%
    ## dplyr::mutate(`Electric` = `Electric_(kBtu)` / area) %>%
    ## dplyr::rename(`Electric` = `eui_elec`) %>%
    {.}

  if (building %in% c("FL0061ZZ", "SC0028ZZ", "UT0032ZZ")) {
    ## print(nrow(dfElec))
    dfElec = dfElec %>%
      dplyr::filter(`Electric` > 0) %>%
      {.}
    ## print(nrow(dfElec))
  }
  ## p = dfElec %>%
  ##   dplyr::mutate(`Date`=zoo::as.yearmon(paste(`year`, `month`), "%Y %m")) %>%
  ##   ggplot(aes(y=`Electric`, x=`Date`)) +
  ##   geom_point()
  ## print(p)

  dfPre = dfElec %>%
    dplyr::filter(`Date` < projectDate) %>%
    dplyr::mutate(`status` = "pre") %>%
    tail(n=36) %>%
    {.}
  dfPre %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_elec_pre.csv"))

  dfPost = dfElec %>%
    dplyr::filter(`Date` > projectDate) %>%
    dplyr::mutate(`status` = "post") %>%
    head(n=36) %>%
    {.}

  ## dfPre %>% ggplot(aes(y=`Electric`, x=`ave`)) +
  ##   geom_point() +
  ##   xlab("Monthly average temperature") +
  ##   geom_smooth(method="loess")

  le = loess(Electric ~ ave, data=dfPre)
  summary(le)

  dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Electric`) %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_elec_post.csv"))

  ## dfSaving = dfPost %>%
  ##   dplyr::mutate(`baseline`=predict(le, ave)) %>%
  ##   dplyr::rename(`actual`=`Electric`) %>%
  ## elecPercent = sum(dfSaving$actual) - sum

  temp = dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Electric`) %>%
    dplyr::select(`actual`, `baseline`, `ave`, `Date`) %>%
    na.omit() %>%
    reshape2::melt(id.vars=c("ave", "Date"), variable.name="period", value.name="Electric") %>%
    {.}

  dfDiff = temp %>% dplyr::group_by(period) %>%
    summarise(total=sum(Electric)) %>%
    {.}

  actualBill = (dfDiff %>% dplyr::filter(period=="actual") %>% dplyr::select(`total`))[[1]]
  baselineBill = (dfDiff %>% dplyr::filter(period=="baseline") %>% dplyr::select(`total`))[[1]]
  ## print(actualBill)
  ## print(baselineBill)
  ## elecSaving = (baselineBill - actualBill) / nrow(dfPost) * 12 * area
  ## annual reduction in kbtu
  elecSaving = (baselineBill - actualBill) / nrow(dfPost) * 12
  elecSavePercent = 100 * (baselineBill - actualBill) / baselineBill

  temp %>%
    ggplot(aes(y=Electric, x=Date, color=period)) +
    geom_point() +
    geom_line() +
    ## ylab("Electric (kBtu/sqft)") +
    ylab("Electric (kBtu)") +
    ggtitle(paste("Electric trend, ", building, "\nkBtu reduction: ", format(round(elecSaving, 2),big.mark=",",scientific=FALSE))) +
    theme()
  ggsave(file=paste0("images/", building, "_trend_elec_kbtu.png"), width=4, height=4, units="in")

  dplyr::bind_rows(dfPre, dfPost) %>%
    ggplot(aes(y=Electric, x=ave, color=status)) +
    geom_point() +
    geom_smooth(method = "loess") +
    xlab("Monthly average temperature") +
    ## ylab("Electric (kBtu/sqft)") +
    ylab("Electric (kBtu)") +
    ggtitle(paste("Electric regression fit, ", building)) +
    theme()
  ggsave(file=paste0("images/", building, "_reg_elec_kbtu.png"), width=4, height=4, units="in")

  ## temp %>%
  ##   ggplot(aes(y=Electric, x=ave, color=period)) +
  ##   geom_point() +
  ##   geom_smooth(method = "loess") +
  ##   xlab("Monthly average temperature") +
  ##   theme()

  ## gas saving

  dfGas = df %>%
    ## dplyr::select(-`eui_elec`) %>%
    ## dplyr::rename(`Gas` = `eui_gas`) %>%
    ## dplyr::mutate(`Gas` = `Gas_(kBtu)` / area) %>%
    dplyr::mutate(`Gas` = `Gas_(kBtu)`) %>%
    {.}
  if (building %in% c("SC0028ZZ", "UT0032ZZ")) {
    print(nrow(dfGas))
    dfGas = dfGas %>%
      dplyr::filter(`Gas` > 0) %>%
      {.}
    print(nrow(dfGas))
  }

  dfPre = dfGas %>%
    dplyr::filter(`Date` < projectDate) %>%
    tail(n=36) %>%
    dplyr::mutate(`status` = "pre") %>%
    {.}
  dfPre %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_gas_pre.csv"))

  dfPost = dfGas %>%
    dplyr::filter(`Date` > projectDate) %>%
    head(n=36) %>%
    dplyr::mutate(`status` = "post") %>%
    {.}
  gasPost = sum(dfPost$Gas) / nrow(dfPost) * 12

  ## dfPre %>% ggplot(aes(y=`Gas`, x=`ave`)) +
  ##   geom_point() +
  ##   xlab("Monthly average temperature") +
  ##   geom_smooth(method="loess")

  le = loess(Gas ~ ave, data=dfPre)
  summary(le)

  dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Gas`) %>%
    readr::write_csv(paste0("csv_FY/tempResultsInRoi/", building, "_gas_post.csv"))

  temp = dfPost %>%
    dplyr::mutate(`baseline`=predict(le, ave)) %>%
    dplyr::rename(`actual`=`Gas`) %>%
    dplyr::select(`actual`, `baseline`, `ave`, `Date`) %>%
    na.omit() %>%
    reshape2::melt(id.vars=c("ave", "Date"), variable.name="period", value.name="Gas") %>%
    {.}

  dfDiff = temp %>% dplyr::group_by(period) %>%
    summarise(total=sum(Gas)) %>%
    {.}

  actualBill = (dfDiff %>% dplyr::filter(period=="actual") %>% dplyr::select(`total`))[[1]]
  baselineBill = (dfDiff %>% dplyr::filter(period=="baseline") %>% dplyr::select(`total`))[[1]]
  ## gasSaving = (baselineBill - actualBill) / nrow(dfPost) * 12 * area
  gasSaving = (baselineBill - actualBill) / nrow(dfPost) * 12
  gasSavePercent = 100 * (baselineBill - actualBill) / baselineBill

  temp %>%
    ggplot(aes(y=Gas, x=Date, color=period)) +
    geom_point() +
    geom_line() +
    ## ylab("Gas (kBtu/sqft)") +
    ylab("Gas (kBtu)") +
    ggtitle(paste("Gas trend, ", building, "\nkBtu reduction: ", format(round(gasSaving, 2),big.mark=",",scientific=FALSE))) +
    theme(legend.position="bottom")
  ggsave(file=paste0("images/", building, "_trend_gas_ktbu.png"), width=4, height=4, units="in")

  dplyr::bind_rows(dfPre, dfPost) %>%
    ggplot(aes(y=Gas, x=ave, color=status)) +
    geom_point() +
    geom_smooth(method = "loess") +
    xlab("Monthly average temperature") +
    ylab("Gas (kBtu)") +
    ggtitle(paste("Gas regression fit, ", building)) +
    theme()
  ggsave(file=paste0("images/", building, "_reg_gas_kbtu.png"), width=4, height=4, units="in")
  return(list("elec"=elecSaving, "gas"=gasSaving, "elecPercent"=elecSavePercent,
              "gasPercent"=gasSavePercent, "area"=area))
}

setwd("~/Dropbox/gsa_2017/")
projDate = readr::read_csv("manual_excel/project_date.csv") %>%
  as.data.frame() %>%
  dplyr::mutate(`Date` = as.Date(`Date`, "%m/%d/%y")) %>%
  {.}

## building = "OK0063ZZ"
## projectDate = "2011 11"
## building = projDate[2, "Building_Number"]
## projectDate = projDate[2, "Date"]
## building = "NY0315ZZ"
nrow(projDate)

## for (i in 1:1) {
## for (i in 1:nrow(projDate)) {
for (i in 4:4) {
  building = projDate[i, "Building_Number"]
  projectDate = projDate[i, "Date"]
  result = compute_roi(building, projectDate)
  ## print(paste(building, projectDate, result$elec, result$elecPercent, result$gas, result$gasPercent, sep=","))
  print(sprintf("%s,%s,%.0f,%.2f,%.2f%%,%.2f,%.2f%%", building, projectDate, result$area, result$elec, result$elecPercent, result$gas, result$gasPercent, sep=","))
}

## savings results
## [1] "DC0028ZZ,2012-11-15,-3937794.40770917,0"
## [1] "LA0098ZZ,2011-09-16,243504.394107464,405764.914299781"
## [1] "NY0300ZZ,2012-09-27,289040.350357518,567581.957919364"
## [1] "NY0351ZZ,2011-11-18,5819232.95834125,2156425.0154802"
## [1] "NY0399ZZ,2012-02-27,-3192166.77904592,4769941.22619969"
## [1] "SC0028ZZ,2014-03-07,208798.164621339,143735.048798812"
## [1] "NC0028ZZ,2012-01-08,65920.7926125514,-759738.783234354"
## [1] "FL0061ZZ,2013-08-30,2184876.79847263,10348.6936640126"
## [1] "OK0063ZZ,2011-11-21,530963.363875254,204495.325790848"
## [1] "UT0032ZZ,2013-09-01,4130575.96202114,3198805.28720162"


## building="NY0399ZZ"
## querystr = paste0("SELECT * FROM EUAS_monthly_weather WHERE [Building_Number] = '", building, "'")

## take out the trailing 0 data

for building in c("DC0028ZZ" ,"LA0098ZZ" ,"NY0300ZZ" ,"NY0351ZZ" ,"NY0399ZZ" ,"SC0028ZZ" ,"NC0028ZZ" ,"FL0061ZZ" ,"OK0063ZZ" ,"UT0032ZZ") {
  queryarea = paste0("SELECT [Gross_Sq.Ft] FROM EUAS_area WHERE [Building_Number] = '", building, "' ORDER BY Fiscal_Year DESC LIMIT 1")
  dbGetQuery(con,  queryarea) %>% 
}

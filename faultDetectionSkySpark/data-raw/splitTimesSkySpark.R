library("dplyr")
library("readr")
library("tidyr")
library("readxl")

setwd("~/Dropbox/gsa_2017/faultDetectionSkySpark/data-raw/skyspark fault detection sparks download/")

files = list.files()

## get timestamp and duration
acc_rule = NULL
for (f in files) {
  b = substr(f, 1, 8)
  print(sprintf("---------------%s---------------", b))
  df = readr::read_csv(f, col_types=cols()) %>%
    tibble::as_data_frame() %>%
    {.}
  if (!("eCost" %in% names(df))) {
    df$eCost = NA
  }
  if (!("mCost" %in% names(df))) {
    df$mCost = NA
  }
  if (!("sCost" %in% names(df))) {
    df$sCost = NA
  }
  df <- df %>%
    dplyr::select(`Date`, `times`, `tz`, `ruleRef`, `Cost`, `eCost`, `mCost`, `sCost`, `equipRef`, `targetRef`) %>%
    dplyr::mutate(`rule`=substr(`ruleRef`, 32, nchar(`ruleRef`)),
                  `tz`=paste0("America/", tz)) %>%
    dplyr::select(-`ruleRef`) %>%
    dplyr::rowwise() %>%
    dplyr::do(cbind(., data.frame(`timeSplit`=strsplit(.$times, split=", ")[[1L]], check.names=FALSE, stringsAsFactors=FALSE))) %>%
    dplyr::select(-`times`) %>%
    dplyr::mutate(`timeStr`=substr(`timeSplit`, 1, gregexpr(" ", `timeSplit`, fixed=TRUE)[[1L]] - 1)) %>%
    ## make the end index very large so that it takes the string suffix
    dplyr::mutate(`durationStr`=substr(`timeSplit`, gregexpr(" ", `timeSplit`, fixed=TRUE)[[1L]] + 1, 30)) %>%
    dplyr::mutate(`len`=nchar(`timeSplit`)) %>%
    dplyr::mutate(`startStr`=paste0(as.character(`Date`), " ", `timeStr`),
                  ) %>%
    dplyr::mutate(tz=as.character(tz)) %>%
    dplyr::mutate(`startPosix`=as.POSIXct(`startStr`, tz=tz, format="%Y-%m-%d %I:%M%p")) %>%
    dplyr::mutate(`durationSecond`=ifelse(grepl("hr", `durationStr`, fixed=TRUE),
                                        as.numeric(substr(`durationStr`, 1, gregexpr("hr", `durationStr`)[[1L]] - 1)) * 3600.0,
                                        as.numeric(substr(`durationStr`, 1, gregexpr("min", `durationStr`)[[1L]] - 1)) * 60.0)) %>%
    dplyr::mutate(`endPosix`=`startPosix` + `durationSecond`) %>%
    dplyr::mutate(`building`=b) %>%
    dplyr::select(-`timeSplit`, -`timeStr`, -`durationStr`, -`len`, -`startStr`) %>%
    ## dplyr::select(`building`, `startPosix`, `endPosix`, `rule`, `tz`) %>%
    dplyr::mutate_at(vars(c("Cost", "eCost", "mCost", "sCost")),
                     funs(as.numeric(gsub("$", "", ., fixed=TRUE)))) %>%
    dplyr::group_by(Date, rule, equipRef, targetRef) %>%
    dplyr::mutate(Cost=Cost * durationSecond / sum(durationSecond),
                  eCost=eCost * durationSecond / sum(durationSecond),
                  sCost=sCost * durationSecond / sum(durationSecond),
                  mCost=mCost * durationSecond / sum(durationSecond)
                  ) %>%
    dplyr::ungroup() %>%
    {.}
  df %>%
    readr::write_csv(sprintf("../ruleStartEndByBuilding/%s", f))
  acc_rule = rbind(acc_rule, df)
}

save(acc_rule, file="faultStartEndByBuilding.rda")

load("faultStartEndByBuilding.rda")

## concat buildings with multiple files into one file
files = list.files("../ruleStartEndByBuilding", "*.csv")

to_combine = data.frame(filename=files) %>%
  dplyr::mutate(building=substr(filename, 1, 8)) %>%
  dplyr::group_by(building) %>%
  dplyr::filter(n() > 1) %>%
  dplyr::ungroup() %>%
  distinct(building) %>%
  .$building

head(files)

for (b in to_combine) {
  bfiles = files[which(substr(files, 1, 8)==b)]
  acc=lapply(bfiles, function(x) {
    readr::read_csv(sprintf("../ruleStartEndByBuilding/%s", x)) %>%
    tibble::as_data_frame() %>%
    {.}
  })
  df = do.call(rbind, acc)
  df %>%
    readr::write_csv(sprintf("../ruleStartEndByBuilding/%s_2018.csv", b))
}

to_remove = files[which(substr(files, 1, 8) %in% to_combine)]
to_remove = to_remove[which(nchar(to_remove) > 17)]

for (f in to_remove) {
  file.remove(sprintf("../ruleStartEndByBuilding/%s", f))
}

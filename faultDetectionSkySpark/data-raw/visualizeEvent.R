library("dplyr")
library("timevis")

## should I exclude the following categories
## to_exclude = c("Missing Data", "Sensor Out Of Range", "Sensor Failure", "Bad Energy Data")

b = "AK0031AA"

files = list.files(path="ruleStartEndByBuilding/", pattern=sprintf("%s*", b))

acc <- NULL
for (f in files) {
  df <- readr::read_csv(sprintf("ruleStartEndByBuilding/%s", f)) %>%
    ## dplyr::filter(!(`rule` %in% to_exclude)) %>%
    {.}
  acc <- rbind(acc, df)
}

acc <- acc %>%
  dplyr::rename(`start`=`startPosix`, `end`=`endPosix`, `content`=`rule`) %>%
  {.}

myTimeline = timevis(acc)

htmlwidgets::saveWidget(myTimeline, sprintf("%s.html", b), selfcontained = F)

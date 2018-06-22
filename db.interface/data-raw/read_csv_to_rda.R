library(readr)
library(dplyr)

oldwd = getwd()
## temporory set working directory
setwd("~/Dropbox/gsa_2017/db.interface/data-raw")
dfStateAbbrLookup = readr::read_csv("state_abbr.csv") %>%
  as.data.frame() %>%
  {.}
head(dfStateAbbrLookup)
devtools::use_data(dfStateAbbrLookup, overwrite = TRUE)
## restore working directory
setwd(oldwd)

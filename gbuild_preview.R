library(readxl)
library(dplyr)

setwd("/home/yujiex/Dropbox/gsa_2017/input/FY/ECM info/gbuild")

skipn=0
sheetid=2
for (sheetid in 2:16) {
    colnames = readxl::read_excel("gBUILD_PROD_DATA_20180129.xlsx", sheetid, skip=skipn) %>%
        as_data_frame() %>%
        names()
    for (n in colnames) {
        if (grepl("cost", n) || grepl("Cost", n)) {
            print(n)
        }
    }
}

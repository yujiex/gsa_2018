library("reshape2")
library("ggplot2")
library("dplyr")
library("readxl")

readr::read_csv("input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16_sheet2.csv", skip=2) %>%
    as_data_frame() %>%
    dplyr::filter(`Scope Type` %in% c("New/Replacement Roof", "Existing Roof R&A")) %>%
    ## dplyr::select(-indicator) %>%
    distinct(`Scope Type`)
    

read_excel("input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx", sheet=2, skip=3) %>%
    as_data_frame() %>%
    names()

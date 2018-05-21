library("DBI")
library("reshape2")
library("ggplot2")
library("dplyr")
library("readxl")
library("readr")
library("RColorBrewer")
library("stringr")

n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(3, 5, NA) 
df = data.frame(n, s, b)

## project_scope summary
excludeCols = c("Project Name", "Region", "CreatedIn",
                "id##Record ID", "Slope", "BA Code",
                "Comments", "Other, Please Specify")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx"
sheetid = 2
skipn = 2
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(-one_of(excludeCols)) %>%
    dplyr::select(-which(sapply(., function(col) {all(is.na(col))}))) %>%
    dplyr::group_by(`Project Type`, `Scope Type`, `Building ID`) %>%
    slice(1) %>%
    dplyr::group_by(`Project Type`, `Scope Type`) %>%
    summarize(n()) %>%
    write.csv("project_scope.csv", na="")

## scope attribute summary
excludeCols = c("Project Name", "Region", "CreatedIn", "Project Type",
                "Workflow Phase", "id##Record ID", "Slope", "BA Code",
                "ePM record #", "Comments", "Other, Please Specify")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx"
sheetid = 2
skipn = 2
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(-one_of(excludeCols)) %>%
    dplyr::select(-which(sapply(., function(col) {all(is.na(col))}))) %>%
    dplyr::select(-which(sapply(., function(col) {is.numeric(col)}))) %>%
    melt(id.vars=c("Building ID", "Scope Type"), variable.name="Attribute Name") %>%
    na.omit() %>%
    ## remove dup
    dplyr::group_by(`Building ID`, `Scope Type`, `Attribute Name`, `value`) %>%
    slice(1) %>%
    dplyr::group_by(`Scope Type`, `Attribute Name`, `value`) %>%
    summarize(n()) %>%
    write.csv("scope_attribute.csv", na="")


## excludeCols = c("Project Name", "Region", "CreatedIn", "Project Type",
##                 "Workflow Phase", "id##Record ID", "Slope", "BA Code",
##                 "ePM record #", "Comments", "Other, Please Specify")
excludeCols = c("Project name", "Comments", "Region", "Budget Activity", "Substantial Completion Date", "IRIS SCD", "gBUILD SCD")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 2
skipn = 3
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(-one_of(excludeCols)) %>%
    dplyr::select(-which(sapply(., function(col) {all(is.na(col))}))) %>%
    melt(id.vars=c("Building ID", "Project Type"), variable.name="Attribute Name") %>%
    na.omit() %>%
    ## remove dup
    dplyr::group_by(`Building ID`, `Project Type`, `Attribute Name`, `value`) %>%
    slice(1) %>%
    dplyr::group_by(`Project Type`, `Attribute Name`, `value`) %>%
    summarize(n()) %>%
    write.csv("project_attribute_led_indoor.csv", na="")

## scope attribute summary
excludeCols = c("Project Name", "Region", "CreatedIn", "Project Type",
                "Workflow Phase", "id##Record ID", "Slope", "BA Code",
                "ePM record #", "Comments", "Other, Please Specify")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx"
sheetid = 2
skipn = 2
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(-one_of(excludeCols)) %>%
    dplyr::filter(`Scope Type`="New Windows") %>%
    dplyr::filter(`Scope Type`="") %>%
    names()

    dplyr::select(-which(sapply(., function(col) {all(is.na(col))}))) %>%
    dplyr::select(-which(sapply(., function(col) {is.numeric(col)}))) %>%
    melt(id.vars=c("Building ID", "Scope Type"), variable.name="Attribute Name") %>%
    na.omit() %>%
    ## remove dup
    dplyr::group_by(`Building ID`, `Scope Type`, `Attribute Name`, `value`) %>%
    slice(1) %>%
    dplyr::group_by(`Scope Type`, `Attribute Name`, `value`) %>%
    summarize(n()) %>%
    write.csv("scope_attribute.csv", na="")

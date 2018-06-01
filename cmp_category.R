## VRF file
## which SCD to use:  IRIS SCD or gBUILD SCD
## not sure how to use sheet2

library("DBI")
library("reshape2")
library("ggplot2")
library("dplyr")
library("readxl")
library("readr")
library("RColorBrewer")

con <- dbConnect(RSQLite::SQLite(), "csv_FY/db/all.db")
alltables = dbListTables(con)
df_old = dbGetQuery(con, 'SELECT * FROM EUAS_ecm' ) %>%
    as_data_frame()

## df_gb = read_excel("input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx", skip=2, sheet=2) %>%
df_gb = readr::read_csv("input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16_sheet2.csv", skip=2) %>%
    as_data_frame() %>%
    # drop un-usable columns
    dplyr::select(-`Project Name`, -`CreatedIn`, -`BA Code`,
                  -`Workflow Phase`, -`ePM record #`, -`Comments`) %>%
    dplyr::mutate_at(vars(-`Region`, -`id##Record ID`, -`Building ID`,
                   -`Scope Type`, -`Project Type`),
              function(x) ifelse(is.na(x), x, 1)) %>%
    dplyr::mutate_at(vars(`Scope Type`), recode,
              "New/Replacement Roof" = "New_Roof",
              "Repair/Replace Existing Windows" = "Repairs_Windows",
              "Existing Facade Repair" = "Repairs_Facade",
              "New/Replaced Facade" = "New_Facade",
              "New Windows" = "New_Windows",
              "Existing Roof R&A" = "RandA_Roof") %>%
    dplyr::rename(`detail_level_ECM`=`Scope Type`,
           `Building_Number`=`Building ID`) %>%
    melt(id.vars = c("Region", "Building_Number", "Project Type",
                     "id##Record ID", "detail_level_ECM"),
         variable.name = "third_level_ECM", value.name="indicator",
         factorsAsStrings=T) %>%
    dplyr::filter(!is.na(indicator)) %>%
    dplyr::select(-indicator) %>%
    dplyr::mutate(high_level_ECM = "Building Envelope") %>%
    dplyr::mutate(source_third_level = "gBUILD Building Envelope Scope 11-23-16_sheet2") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    {.} 

df2 = read_excel("input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx", sheet=2, skip=3) %>%
    as_data_frame() %>%
    rename(`Building_Number`=`Building ID`,
           `Substantial_Completion_Date`=`Substantial Completion Date`) %>%
    # drop un-usable columns
    dplyr::select(-`Project name`, -`IRIS SCD`,
                  -`gBUILD SCD`, -`Budget Activity`, -`Comments`) %>%
    dplyr::mutate_at(vars(-`Region`, -`Building_Number`,
                   -`Substantial_Completion_Date`, -`Project Type`),
              function(x) ifelse(is.na(x), x, 1)) %>%
    melt(id.vars = c("Region", "Building_Number", "Project Type", 
                     "Substantial_Completion_Date"),
         variable.name = "third_level_ECM", value.name="indicator",
         factorsAsStrings=T) %>%
    dplyr::filter(!is.na(indicator)) %>%
    dplyr::select(-indicator) %>%
    dplyr::mutate(detail_level_ECM = "Indoor_Lighting") %>%
    dplyr::mutate(high_level_ECM = "Lighting") %>%
    dplyr::mutate(source_third_level = "LED projects in gBUILD with SCDs 6-15-2017_sheet2") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    {.}

df3 = read_excel("input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx", sheet=3, skip=3) %>%
    as_data_frame() %>%
    rename(`Building_Number`=`Building ID`,
           `Substantial_Completion_Date`=`Substantial Completion Date`) %>%
    dplyr::select(-`Project name`, -`IRIS SCD`,
                  -`gBUILD SCD`, -`Budget Activity`, -`Comment`) %>%
    dplyr::mutate_at(vars(-`Region`, -`Building_Number`, -`Project Type`,
                   -`Substantial_Completion_Date`),
              function(x) ifelse(is.na(x), x, 1)) %>%
    melt(id.vars = c("Region", "Building_Number", "Project Type",
                     "Substantial_Completion_Date"),
         variable.name = "third_level_ECM", value.name="indicator",
         factorsAsStrings=T) %>%
    dplyr::filter(!is.na(indicator)) %>%
    dplyr::select(-indicator) %>%
    dplyr::mutate(detail_level_ECM = "Outdoor_Lighting") %>%
    dplyr::mutate(high_level_ECM = "Lighting") %>%
    dplyr::mutate(source_third_level = "LED projects in gBUILD with SCDs 6-15-2017_sheet3") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    {.}

df_led = dplyr::bind_rows(df2, df3)

df_vrf = read_excel("input/FY/ECM info/fwdgbuildoutputs/VRF etc -- Mike Sullivan Corrected Equipment Ad Hoc 6.12.17.xlsx", sheet=1) %>% as_data_frame() %>%
    # drop un-usable columns
    dplyr::select(-`Project Name`, -`Building Name`, -`ASID`,
                  -`BA Code`, -`Workflow`, -`isdeleted##Deleted`,
                  -`ScopeDetailsCount__c##Scope Details Count`,
                  -`Name##PBSgBUILDSS`,
                  -`RecordTypeId##Record Type ID`,
                  -`RecordType.DeveloperName`,
                  -`createddate##Created Date`,
                  -`createdbyid##Created By ID`,
                  -`CreatedBy.Name`,
                  -`lastmodifieddate##Last Modified Date`,
                  -`lastmodifiedbyid##Last Modified By ID`,
                  -`LastModifiedBy.Name`,
                  -`systemmodstamp##System Modstamp`,
                  -`Rahd_ProjectBldgParentId__c##RAHD_Project Building Parent ID`,
                  -`Rahd_ProjectBldgParentId__r.Name`,
                  -`Comments`,
                  -`ScopeSysCode2__c##Scope System Code L2`,
                  -`ScopeSysCodeL1__c##Scope System Code L1`,
                  -`ScopeSysCodeL2__c##Scope System Code L2`,
                  -`ScopeSysCodeL3__c##Scope System Code L3`,
                  -`ScopeDetailsMaxSeq__c##Scope Details Max Sequence`,
                  -`ScopeSystemDetailRecordType__c##Scope System Detail Record Type`
                  ) %>%
    dplyr::rename(`Building_Number`=`Building Number`,
           ## `Substantial_Completion_Date`=`IRIS SCD`,
           `high_level_ECM`=`ScopeSysCode1__c##Scope System Code L1`) %>%
    # combine two date column, update NA in the first with the second
    dplyr::mutate(`Substantial_Completion_Date`=dplyr::coalesce(`IRIS SCD`, `gBUILD SCD`)) %>%
    dplyr::mutate_at(vars(`Type`), recode,
              "New/Replacement" = "New",
              "Repair/Alteration" = "Repair") %>%
    dplyr::mutate_at(vars(`high_level_ECM`), recode,
              "IndoorEnvironmentalQuality" = "IEQ") %>%
    dplyr::mutate(detail_level_ECM=paste0(`Type`, "_", `Category`)) %>%
    dplyr::select(-`Category`, -`Type`, -`IRIS SCD`, -`gBUILD SCD`) %>%
    melt(id.vars = c("Region", "Building_Number", "Project Type",
                     "Substantial_Completion_Date", "id##Record ID",
                     "high_level_ECM", "detail_level_ECM"),
         variable.name = "third_level_ECM", value.name="indicator",
         factorsAsStrings=T) %>%
    filter(!is.na(indicator)) %>%
    select(-indicator) %>%
    dplyr::mutate(source_third_level = "VRF etc -- Mike Sullivan Corrected Equipment Ad Hoc 6.12.17_sheet1") %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    {.}

df_new = dplyr::bind_rows(df_gb, df_led, df_vrf)

df_new <- df_new %>%
    dplyr::mutate_at(vars(`source_third_level`), recode,
              "gBUILD Building Envelope Scope 11-23-16_sheet2"="gBUILD",
              "LED projects in gBUILD with SCDs 6-15-2017_sheet2"="LED2",
              "LED projects in gBUILD with SCDs 6-15-2017_sheet3"="LED3",
              "VRF etc -- Mike Sullivan Corrected Equipment Ad Hoc 6.12.17_sheet1"="VRF1") %>%
    {.}

detail_order = read.csv("intermediate/detailECMorder.csv")

df_new %>%
    dplyr::arrange(high_level_ECM, detail_level_ECM, third_level_ECM) %>%
    dplyr::mutate(third_level_ECM = factor(third_level_ECM, unique(third_level_ECM))) %>%
    ggplot(aes(third_level_ECM, fill=detail_level_ECM)) +
    geom_bar() +
    facet_grid(. ~ source_third_level) +
    theme(axis.text.y = element_text(size=5)) +
    ggtitle("new ECM source record count") +
    ylab("record count") +
    coord_flip() +
    theme()
ggsave(file="writeup/images/record_cnt_newECM.pdf",
       width=8, height=4, units="in")

df_new %>%
    distinct(`Building_Number`, `source_third_level`) %>%
    ggplot(aes(source_third_level)) +
    geom_bar() +
    stat_count() + 
    ggtitle("new ECM source building count") +
    ylab("building count") +
    theme()
ggsave(file="writeup/images/building_cnt_newECM.pdf",
       width=8, height=4, units="in")

df_new <- df_new %>%
    dplyr::mutate(`Substantial_Completion_Date`=as.character(`Substantial_Completion_Date`)) %>%
    dplyr::mutate(`new_old` = "new")

df_old <- df_old %>%
    dplyr::mutate(`new_old` = "old")

df_all = dplyr::bind_rows(df_old, df_new)

df_all %>%
    ggplot(aes(high_level_ECM)) +
    geom_bar() +
    facet_grid(. ~ new_old) +
    coord_flip() +
    ggtitle("comparing high_level_ECM record count") +
    geom_text(stat='count', aes(label=..count..)) +
    ylab("record count") +
    theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
    theme()
ggsave(file="writeup/images/record_cnt_newOldECM_highlevel.pdf",
       width=8, height=4, units="in")

df_all %>%
    distinct(`Building_Number`, `high_level_ECM`, `new_old`) %>%
    ggplot(aes(high_level_ECM)) +
    geom_bar() +
    facet_grid(. ~ new_old) +
    coord_flip() +
    ggtitle("comparing high_level_ECM building count") +
    ylab("building count") +
    theme(axis.text.y = element_text(angle = 30, hjust = 1)) +
    theme()
ggsave(file="writeup/images/building_cnt_newOldECM_highlevel.pdf",
       width=8, height=4, units="in")

df_all %>%
    dplyr::arrange(high_level_ECM, detail_level_ECM, third_level_ECM) %>%
    dplyr::mutate(detail_level_ECM = factor(detail_level_ECM, unique(detail_level_ECM))) %>%
    ggplot(aes(detail_level_ECM, fill=high_level_ECM)) +
    geom_bar() +
    facet_grid(. ~ new_old) +
    coord_flip() +
    geom_text(stat='count', aes(label=..count..)) +
    ggtitle("comparing detail_level_ECM record count") +
    ylab("record count") +
    scale_fill_brewer(palette="Set2") +
    theme(axis.text.y = element_text(size=5)) +
    theme(legend.position="bottom") +
    theme()
ggsave(file="writeup/images/record_cnt_newOldECM_detail_level.pdf",
       width=8, height=6, units="in")

df_all %>%
    distinct(`Building_Number`, `high_level_ECM`, `detail_level_ECM`, `new_old`) %>%
    dplyr::arrange(high_level_ECM, detail_level_ECM) %>%
    dplyr::mutate(detail_level_ECM = factor(detail_level_ECM, unique(detail_level_ECM))) %>%
    ggplot(aes(detail_level_ECM, fill=high_level_ECM)) +
    geom_bar() +
    facet_grid(. ~ new_old) +
    coord_flip() +
    ggtitle("comparing detail_level_ECM building count") +
    ylab("building count") +
    scale_fill_brewer(palette="Set2") +
    theme(legend.position="bottom") +
    theme(axis.text.y = element_text(size=5)) +
    theme()
ggsave(file="writeup/images/building_cnt_newOldECM_detail_level.pdf",
       width=8, height=6, units="in")

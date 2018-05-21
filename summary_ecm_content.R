library("DBI")
library("reshape2")
library("ggplot2")
library("dplyr")
library("readxl")
library("readr")
library("RColorBrewer")
library("stringr")

con <- dbConnect(RSQLite::SQLite(), "csv_FY/db/all.db")
alltables = dbListTables(con)
df_area = dbGetQuery(con, 'SELECT * FROM EUAS_area' ) %>%
    as_data_frame() %>%
    group_by(`Building_Number`) %>%
    summarise(`Gross_Sq.Ft`=last(`Gross_Sq.Ft`)) %>%
    dplyr::rename(`Building ID`=`Building_Number`) %>%
    {.}

plot_fields_categorical = function(sourcefile, sheetid, skipn,
                                   excludeCols, prefix) {
    df_gb_factor = read_excel(sourcefile, sheetid, skip=skipn) %>%
        as_data_frame() %>%
        dplyr::mutate(Region=factor(Region)) %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        dplyr::select(which(sapply(.,is.factor))) %>%
        dplyr::select(-one_of(excludeCols)) %>%
        dplyr::select(which(sapply(., nlevels) > 1)) %>%
        {.}
    if ("Building Number" %in% names(df_gb_factor)) {
        names(df_gb_factor)[names(df_gb_factor) == "Building Number"] <- "Building ID"
    }
    names(df_gb_factor) = gsub(" ", "", names(df_gb_factor))
    names(df_gb_factor) = gsub("/", "or", names(df_gb_factor))
    names(df_gb_factor) = gsub("-", "", names(df_gb_factor))
    names(df_gb_factor) = gsub(",", "", names(df_gb_factor))
    names(df_gb_factor) = gsub("#", "", names(df_gb_factor))
    names(df_gb_factor) = gsub("(", "", names(df_gb_factor),
                               fixed=TRUE)
    names(df_gb_factor) = gsub(")", "", names(df_gb_factor),
                               fixed=TRUE)
    categorical_vars = names(df_gb_factor)
    categorical_vars = categorical_vars[categorical_vars != "BuildingID"]
    ## how to replace na with something!!!!!!!
    for (cat in categorical_vars) {
        plot.df = 
            df_gb_factor %>%
            dplyr::select(one_of(c(cat, "BuildingID"))) %>%
            dplyr::group_by_at(vars(one_of(c(cat, "BuildingID")))) %>%
            slice(1) %>%
            {.}
        print(plot.df)
        plot.df %>%
            ggplot() +
            geom_bar(aes_string(x=cat)) +
            geom_text(stat='count',
                      aes_string(x=cat, label="..count.."), vjust=-1) +
            ylab("Attribute Count (for unique buildings)") +
            ggtitle(paste0(prefix, "-- Attribute Count : ", cat, "\n(for unique buildings)")) +
            scale_x_discrete(labels =
                                 function(x) stringr::str_wrap(x, width = 40)) +
            theme(axis.text.y = element_text(size=20)) +
            theme(axis.text.x = element_text(size=20)) +
            ylim(0, 300) +
            coord_flip() +
            theme()
        ggsave(paste0("writeup/images/", prefix, "_", cat, "_count.png"))
    }
}

excludeCols = c("Project Name", "id##Record ID", "Slope",
                "Comments", "Other, Please Specify")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx"
sheetid = 2
skipn = 2
plot_fields_categorical(sourcefile, sheetid, skipn, excludeCols,
                        "gBUILD")

excludeCols = c("Project name", "Comments")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 2
skipn = 3
plot_fields_categorical(sourcefile, sheetid, skipn, excludeCols,
                        "LED2")

excludeCols = c("Project name", "Comment")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 3
skipn = 3
plot_fields_categorical(sourcefile, sheetid, skipn, excludeCols,
                        "LED3")

excludeCols = c("Project Name", "Building Name", "ASID",
                "id##Record ID",
                  "isdeleted##Deleted",
                  "ScopeDetailsCount__c##Scope Details Count",
                  "Name##PBSgBUILDSS",
                  "RecordTypeId##Record Type ID",
                  "RecordType.DeveloperName",
                  "createddate##Created Date",
                  "createdbyid##Created By ID",
                  "CreatedBy.Name",
                  "lastmodifieddate##Last Modified Date",
                  "lastmodifiedbyid##Last Modified By ID",
                  "LastModifiedBy.Name",
                  "systemmodstamp##System Modstamp",
                  "Rahd_ProjectBldgParentId__c##RAHD_Project Building Parent ID",
                  "Rahd_ProjectBldgParentId__r.Name",
                  "ScopeSysCode2__c##Scope System Code L2",
                  "ScopeSysCodeL1__c##Scope System Code L1",
                  "ScopeSysCodeL2__c##Scope System Code L2",
                  "ScopeSysCodeL3__c##Scope System Code L3",
                  "ScopeDetailsMaxSeq__c##Scope Details Max Sequence",
                  "ScopeSystemDetailRecordType__c##Scope System Detail Record Type",
                  "Comments")
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/VRF etc -- Mike Sullivan Corrected Equipment Ad Hoc 6.12.17.xlsx"
sheetid = 1
skipn = 0
plot_fields_categorical(sourcefile, sheetid, skipn, excludeCols,
                        "VRF1")

# how many roof cases
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx"
sheetid = 2
skipn = 2
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::mutate_at(vars(`Scope Type`), recode,
              "New/Replacement Roof" = "New_Roof",
              "Repair/Replace Existing Windows" = "Repairs_Windows",
              "Existing Facade Repair" = "Repairs_Facade",
              "New/Replaced Facade" = "New_Facade",
              "New Windows" = "New_Windows",
              "Existing Roof R&A" = "RandA_Roof") %>%
    dplyr::mutate(`Type`=gsub(".*_", "", `Scope Type`)) %>%
    dplyr::group_by(`Scope Type`, `Building ID`)) %>%
    slice(1) %>%
    ggplot(aes(x=Type)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    theme(axis.text.y = element_text(size=20)) +
    theme(axis.text.x = element_text(size=20)) +
    ggtitle(paste0("Roof Window Facade Project Count")) +
    ylab("percentage") +
    theme()
    ggsave(paste0("writeup/images/roofWindowFacade", "_percent.png"))

sourcefile = "input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx"
sheetid = 2
skipn = 2
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::mutate_at(vars(`Scope Type`), recode,
              "New/Replacement Roof" = "New_Roof",
              "Repair/Replace Existing Windows" = "Repairs_Windows",
              "Existing Facade Repair" = "Repairs_Facade",
              "New/Replaced Facade" = "New_Facade",
              "New Windows" = "New_Windows",
              "Existing Roof R&A" = "RandA_Roof") %>%
    dplyr::mutate(`Type`=gsub(".*_", "", `Scope Type`)) %>%
    ggplot(aes(x=Type)) +
    geom_bar(aes(y = ..count..)) +
    theme(axis.text.y = element_text(size=20)) +
    theme(axis.text.x = element_text(size=20)) +
    geom_text(stat='count', aes(label=..count..)) +
    ggtitle(paste0("Roof Window Facade Project Count")) +
    theme()
    ggsave(paste0("writeup/images/roofWindowFacade", "_count.png"))

## roof project area, didn't associate with area in the database
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/gBUILD Building Envelope Scope 11-23-16.xlsx"
sheetid = 2
skipn = 2
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    ## `id##Record ID` are distinct id for each row
    dplyr::select(`id##Record ID`, `Total Roof Square Footage`,
                  `Square Footage of Application`) %>%
    dplyr::rename(`Record ID`=`id##Record ID`) %>%
    ## dplyr::mutate(`Ratio`=`Square Footage of Application`/`Total Roof Square Footage`) %>%
    melt(id.vars="Record ID", variable.name="Roof Area Attribute",
         value.name="Square Footage") %>%
    ggplot() +
        geom_boxplot(aes(y = `Square Footage`, x = `Roof Area Attribute`)) +
        theme(axis.text.y = element_text(size=20)) +
        theme(axis.text.x = element_text(size=20)) +
        ylim(0, 1e+05) +
        scale_x_discrete(labels =
                                function(x) stringr::str_wrap(x, width = 15)) +
        theme()
    ggsave(paste0("writeup/images/roofarea", ".png"))

## roof project area ratio
read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    ## `id##Record ID` are distinct id for each row
    dplyr::select(`id##Record ID`, `Total Roof Square Footage`,
                  `Square Footage of Application`) %>%
    dplyr::mutate(`Ratio`=`Square Footage of Application`/`Total Roof Square Footage`) %>%
    ggplot() +
        geom_histogram(aes(x = `Ratio`)) +
        theme(axis.text.y = element_text(size=20)) +
        theme(axis.text.x = element_text(size=20)) +
        theme()
    ggsave(paste0("writeup/images/roofarea_ratio", ".png"))

## LED area
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 2
skipn = 3
df1 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Project name`, `LED Square Footage Illuminated`,
                  `Whole Application Sq Ft`) %>%
    ## summarize(n_distinct(`Project name`))
    melt(id.vars="Project name", variable.name="LED Area Attribute",
         value.name="Square Footage") %>%
    dplyr::mutate(`Type`="indoor") %>%
    {.}

sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 3
skipn = 3
df2 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Project name`, `LED Square Footage Illuminated`,
                  `Whole Application Sq Ft`) %>%
    ## summarize(n_distinct(`Project name`))
    melt(id.vars="Project name", variable.name="LED Area Attribute",
         value.name="Square Footage") %>%
    dplyr::mutate(`Type`="outdoor") %>%
    {.}

dplyr::bind_rows(df1, df2) %>%
    ggplot() +
        geom_boxplot(aes(y = `Square Footage`,
                         x = `LED Area Attribute`, fill=`Type`)) +
        theme(axis.text.y = element_text(size=20)) +
        theme(axis.text.x = element_text(size=20)) +
        ylim(0, 1e+06) +
        scale_x_discrete(labels =
                                function(x) stringr::str_wrap(x, width = 15)) +
        theme()
    ggsave(paste0("writeup/images/LEDarea", ".png"))

## LED cost
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 2
skipn = 3
plot_fields_categorical(sourcefile, sheetid, skipn, excludeCols,
                        "LED2")
df1 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Project name`, `System Cost ($)`) %>%
    ## summarize(n_distinct(`Project name`))
    group_by(`Project name`, `System Cost ($)`) %>%
    slice(1) %>%
    dplyr::mutate(`Type`="indoor") %>%
    {.}

sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 3
skipn = 3
df2 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Project name`, `System Cost ($)`) %>%
    ## summarize(n_distinct(`Project name`))
    group_by(`Project name`, `System Cost ($)`) %>%
    slice(1) %>%
    dplyr::mutate(`Type`="outdoor") %>%
    {.}

dplyr::bind_rows(df1, df2) %>%
    ggplot() +
        geom_boxplot(aes(y = `System Cost ($)`, x=`Type`)) +
        theme(axis.text.y = element_text(size=20)) +
        theme(axis.text.x = element_text(size=20)) +
        ylim(0, 1e+06) +
        theme()
    ggsave(paste0("writeup/images/LEDcost", ".png"))

## LED area ratio
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 2
skipn = 3
df1 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Building ID`, `Project name`,
                  `Whole Application Sq Ft`) %>%
    left_join(df_area, by="Building ID") %>%
    dplyr::mutate(`Application Sq Ft ratio`=`Whole Application Sq Ft`/`Gross_Sq.Ft`) %>%
    dplyr::mutate(`Type`="indoor") %>%
    {.}

sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 3
skipn = 3
df2 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Building ID`, `Project name`,
                  `Whole Application Sq Ft`) %>%
    left_join(df_area, by="Building ID") %>%
    dplyr::mutate(`Application Sq Ft ratio`=`Whole Application Sq Ft`/`Gross_Sq.Ft`) %>%
    dplyr::mutate(`Type`="outdoor") %>%
    {.}

df1 %>%
    ggplot() +
        geom_boxplot(aes(x="", y = `Application Sq Ft ratio`)) +
        theme(axis.text.y = element_text(size=20)) +
        theme(axis.text.x = element_text(size=20)) +
        xlab("indoor") +
        theme()
    ggsave(paste0("writeup/images/LEDarea_ratio", ".png"))

## LED time
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 2
skipn = 3
plot_fields_categorical(sourcefile, sheetid, skipn, excludeCols,
                        "LED2")
df1 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Building ID`, `Project name`,
                  `Substantial Completion Date`) %>%
    dplyr::mutate(`Type`="indoor") %>%
    {.}

sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 3
skipn = 3
df2 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Building ID`, `Project name`,
                  `Substantial Completion Date`) %>%
    dplyr::mutate(`Type`="outdoor") %>%
    {.}

dplyr::bind_rows(df1, df2) %>%
    ggplot() +
        geom_boxplot(aes(y = `Substantial Completion Date`,
                         x = `Type`)) +
        theme(axis.text.y = element_text(size=20)) +
        theme(axis.text.x = element_text(size=20)) +
        ggtitle("LED Project Substantial Completion Date") + 
        theme()

    ggsave(paste0("writeup/images/LEDtime", ".png"))

## LED project count
sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 2
skipn = 3
plot_fields_categorical(sourcefile, sheetid, skipn, excludeCols,
                        "LED2")
df1 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Building ID`, `Project name`,
                  `Substantial Completion Date`) %>%
    dplyr::mutate(`Type`="indoor") %>%
    {.}

sourcefile = "input/FY/ECM info/fwdgbuildoutputs/LED projects in gBUILD with SCDs 6-15-2017.xlsx"
sheetid = 3
skipn = 3
df2 = read_excel(sourcefile, sheetid, skip=skipn) %>%
    as_data_frame() %>%
    dplyr::select(`Building ID`, `Project name`,
                  `Substantial Completion Date`) %>%
    dplyr::mutate(`Type`="outdoor") %>%
    {.}

dplyr::bind_rows(df1, df2) %>%
    group_by(`Project name`) %>%
    slice(1) %>%
    ggplot() +
        geom_bar(aes(x=`Type`)) +
        geom_text(stat='count', aes(x=`Type`, label=..count..),
                  vjust=-1) +
        theme(axis.text.y = element_text(size=20)) +
        theme(axis.text.x = element_text(size=20)) +
        ggtitle("LED Project count") + 
        theme()
    ggsave(paste0("writeup/images/LED_count", ".png"))

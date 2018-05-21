library(ggplot2)
library(DBI)
library(RColorBrewer)
library(plyr)
library(stringr)
library(Rmisc)
library(dplyr)
library(reshape)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
con = dbConnect(drv=RSQLite::SQLite(), dbname="csv_FY/db/all.db")

## helpers start ##
stackbar <- function(df, xcol, fillcol, ylabel, tit, legendloc, pal) {
    dfcount = plyr::count(df, c(xcol, fillcol))
    dfcount <- transform(dfcount, mid_y = ave(dfcount$freq, dfcount[,xcol], FUN = function(val) cumsum(val) - (0.5 * val)))
    g = ggplot(dfcount, aes_string(x=xcol, y="freq", fill=fillcol, label="freq")) +
        geom_bar(stat="identity") +
        ylab(ylabel) +
        labs(title=tit) +
        geom_text(aes(y=mid_y), size=2.5)
    if (!missing(legendloc)) {
        g <- g + theme(legend.position=legendloc)
    }
    if (!missing(pal)) {
        g <- g + scale_fill_manual(values=pal)
    } else {
        g <- g + scale_fill_brewer(palette="Set3")
    }
    return(g)
}
## helpers end ##

## theme = "elec"
theme = "gas"
## title = "Electric"
title = "Gas"
## measure = "percent"
measure = "abs"
## cpop = "cap"
cpop = "op"
savingByActionCntErr <- function(theme, title, measure, cpop) {
    df = read.csv(sprintf("input_R/%s_action_save.csv", theme))
    if (measure == "percent") {
        oldname <- sprintf("%s_%s", title, "Saving")
    } else {
        oldname <- sprintf("%s_%s", title, measure)
    }
    print(oldname)
    for (i in 1:1) {
        dftemp <- df[df["number"] == i,]
        if (cpop == "cap") {
            dftemp <- dftemp[dftemp["high_level_ecm"] != "Advanced Metering",]
            dftemp <- dftemp[dftemp["high_level_ecm"] != "GSALink",]
        } else {
            dftemp <- dftemp[dftemp$high_level_ecm %in% c("Advanced Metering", "GSALink"),]
        }
        names(dftemp)[names(dftemp)==oldname] <- "Saving"
        dfs <- summarySE(dftemp, measurevar="Saving", groupvars=c("high_level_ecm"))
        ## comment out to remove groups with too few observations
        dfs <- dfs[dfs$N > 5,]
        dfs <- dfs[order(dfs$Saving, decreasing=TRUE),]
        dfs$high_level_ecm <- factor(dfs$high_level_ecm, levels=dfs$high_level_ecm)
        savewidth = ((nrow(dfs) %/% 3) + 1) * 4
        textwidth = ((nrow(dfs) %/% 3) + 1) * 30
        p <- ggplot(dfs, aes(x=high_level_ecm, y=Saving)) +
            geom_bar(stat="identity") +
            geom_errorbar(aes(ymin=Saving-ci, ymax=Saving+ci), width=.2, position=position_dodge(.9)) +
            geom_text(aes(y=-1, label=sprintf("n = %s", N))) +
            scale_x_discrete(labels = function(x) gsub(";", "\n", x))
        if (measure == "percent") {
            p <- p +
                geom_text(aes(y=Saving, label=sprintf("%.1f %s\n(%.1f%s, %.1f%s)", Saving, "%", Saving-ci, "%", Saving+ci, "%")),
                        nudge_x=-0.2, vjust=0, nudge_y=0.5) +
                ylab(sprintf("Average %s EUI Percent Saving", title)) +
                ggtitle(str_wrap(sprintf("Average %s EUI Percent Saving for Buildings with %s Action", title, i), width=textwidth))
        } else {
            p <- p +
                geom_text(aes(y=Saving, label=sprintf("%.1f\n(%.1f, %.1f)", Saving, Saving-ci, Saving+ci)),
                        nudge_x=-0.2, vjust=0, nudge_y=0.5) +
                ylab(sprintf("Average EUI Absolute Saving", title)) +
                ggtitle(str_wrap(sprintf("Average %s EUI Absolute Saving for Buildings with %s Action", title, i), width=textwidth))
        }
        print(p)
        ggsave(file=sprintf("plot_FY_annual/quant/%s_%s_%s_action_%s.png",
                            measure, theme, i, cpop),
            width=savewidth, height=4, units="in")
    }
}
savingByActionCntErr("elec", "Electric", "percent", "cap")
savingByActionCntErr("elec", "Electric", "abs", "cap")
savingByActionCntErr("gas", "Gas", "percent", "cap")
savingByActionCntErr("gas", "Gas", "abs", "cap")
savingByActionCntErr("elec", "Electric", "percent", "op")
savingByActionCntErr("elec", "Electric", "abs", "op")
savingByActionCntErr("gas", "Gas", "percent", "op")
savingByActionCntErr("gas", "Gas", "abs", "op")

df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat FROM EUAS_category')
df = merge(x=df1, y=df2, by="Building_Number", all.x=TRUE)
df <- df[df$Fiscal_Year < 2016,]
## qplot(factor(Fiscal_Year), data=df, geom="bar", fill=factor(Cat))
df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
g = stackbar(df, "Fiscal_Year", "Cat", "Building Count", "EUAS Building Count By Category")
print(g)
ggsave(file="plot_FY_annual/quant/building_by_cat_.png", width=8, height=4, units="in")
## df$Fiscal_Year <- factor(df$Fiscal_Year)
## df %>% dplyr::group_by(Fiscal_Year, Cat) %>%
##     summarise(Building_Number = n()) %>% cast(Cat ~ Fiscal_Year) %>%
##     write.csv("plot_FY_annual/quant_data/cat_cnt_by_year.csv", row.names=FALSE)
## df %>% dplyr::group_by(Cat) %>% summarise(F = count(Fiscal_Year))

## Building type count by year
colors = (c(brewer.pal(12, 'Set3'), "gray"))
## pie(rep(1,13), col=colors)
df1 = dbGetQuery(con, 'SELECT Building_Number, [Self-Selected_Primary_Function] as Type FROM EUAS_type')
df2 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly')
df = merge(x=df2, y=df1, by="Building_Number", all.x=TRUE)
df <- df[df$Fiscal_Year < 2016,]
df[is.na(df)] <- 'No Data'
df$Type <- factor(df$Type)
levels(df$Type)
df$Type <- factor(df$Type, levels(df$Type)[c(6, 1, 9, 2, 3, 5, 7, 8, 10:13, 4)])
levels(df$Type)
ggplot(df, aes(x=Fiscal_Year, fill=Type)) + geom_bar() + ylab("Building Count") + labs(title="EUAS Building Type Count by Year") + scale_fill_brewer(palette="Set3") + scale_fill_manual(values=colors)
ggsave(file="plot_FY_annual/quant/type_by_year.png", width=8, height=4, units="in")

## Building type count by cat
## colors = (c(brewer.pal(12, 'Set3'), "gray"))
## pie(rep(1,13), col=colors)
df1 = dbGetQuery(con, 'SELECT Building_Number, [Self-Selected_Primary_Function] as Type FROM EUAS_type')
df2 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, Cat FROM EUAS_monthly')
df = merge(x=df2, y=df1, by="Building_Number", all.x=TRUE)
df[is.na(df)] <- 'No Data'
df$Type <- factor(df$Type)
levels(df$Type)
df$Type <- factor(df$Type, levels(df$Type)[c(6, 1, 9, 2, 3, 5, 7, 8, 10:13, 4)])
levels(df$Type)
df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
ggplot(df, aes(x=Cat, fill=Type)) + geom_bar() + ylab("Building Count") + labs(title="EUAS Building Type Count By Category") + scale_fill_brewer(palette="Set3") + scale_fill_manual(values=colors)
ggsave(file="plot_FY_annual/quant/type_by_cat.png", width=8, height=4, units="in")

## Use the original count record
## library(ggplot2)
## library(DBI)
## library(RColorBrewer)
## con = dbConnect(drv=RSQLite::SQLite(), dbname="csv_FY/db/all.db")
## df = dbGetQuery(con, 'SELECT DISTINCT Building_Number, Fiscal_Year, Cat FROM EUAS_monthly')
## df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
## ggplot(df, aes(Fiscal_Year, fill=Cat)) + geom_bar() + ylab("Building Count") + labs(title="EUAS Building Count By Category") + scale_fill_brewer(palette="Set3")

df1 = dbGetQuery(con, 'SELECT Building_Number, Fiscal_Year, status FROM eui_by_fy')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat FROM EUAS_category')
df = merge(x=df1, y=df2, by="Building_Number", all.x=TRUE)
df <- df[df$Fiscal_Year < 2016,]
df$status <- factor(df$status, levels=c("Electric EUI >= 12 and Gas EUI >= 3", "Low Electric EUI", "Low Gas EUI", "Low Gas and Electric EUI"))
ggplot(df, aes(Fiscal_Year, fill=status)) + geom_bar() + ylab("Building Count") + labs(title="EUAS Gas and Electric EUI By Category") + scale_fill_brewer(palette="Set3") + theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/eui_byyear.png", width=8, height=4, units="in")

df1 = dbGetQuery(con, 'SELECT Building_Number, Fiscal_Year, status FROM eui_by_fy')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat FROM EUAS_category WHERE Cat in (\'A\', \'I\')')
df = merge(x=df1, y=df2, by="Building_Number")
df <- df[df$Fiscal_Year < 2016,]
df$status <- factor(df$status, levels=c("Electric EUI >= 12 and Gas EUI >= 3", "Low Electric EUI", "Low Gas EUI", "Low Gas and Electric EUI"))
ggplot(df, aes(Fiscal_Year, fill=status)) + geom_bar() + ylab("Building Count") + labs(title="Gas and Electric EUI of A + I Building in EUAS By Category") + scale_fill_brewer(palette="Set3") + theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/eui_byyear_ai.png", width=8, height=4, units="in")

df = read.csv("input_R/robust_energy.csv")
df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
ggplot(df, aes(Cat, fill=status)) + geom_bar() + ylab("Building Count") + labs(title="Robust energy by category") + scale_fill_brewer(palette="Set3") + theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/robust_energy_cnt.png", width=8, height=4, units="in")

df = read.csv("input_R/cap_op_cnt.csv")
df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly')
df = merge(df1, df, by="Building_Number", all.x=TRUE)
df <- df[df$Fiscal_Year < 2016,]
df$status <- factor(df$status, levels=c("Capital and Operational", "Capital Only", "Operational Only", "No Known Investment"))
ggplot(df, aes(Fiscal_Year, fill=status)) +
    geom_bar() + ylab("Building Count") + labs(title="Capital vs Operational Investment By Fiscal Year") + scale_fill_brewer(palette="Set3") + theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/co_cnt_byyear.png", width=8, height=4, units="in")
df %>% dplyr::group_by(Fiscal_Year, status) %>%
    summarise(Building_Number = n()) %>% cast(status ~ Fiscal_Year) %>%
    write.csv("plot_FY_annual/quant_data/co_cnt_byyear.csv", row.names=FALSE)

df = read.csv("input_R/cap_op_cnt.csv")
df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, Fiscal_Year FROM EUAS_monthly WHERE Cat in (\'A\', \'I\')')
df = merge(df1, df, by="Building_Number", all.x=TRUE)
df <- df[df$Fiscal_Year < 2016,]
## df <- df[df$status != "No Known Investment",]
df <- df[df$status != "With Investment",]
df$status <- factor(df$status, levels=c("Capital and Operational", "Capital Only", "Operational Only", "No Known Investment"))
df <- df[complete.cases(df),]
stackbar(df, "Fiscal_Year", "status", "Building Count", "Capital vs Operational Investment By Fiscal Year", "bottom", c("#8DD3C7", "#FFFFB3", "#BEBADA", "#D3D3D3"))
ggsave(file="plot_FY_annual/quant/co_cnt_byyear_with_.png", width=8, height=4, units="in")

df2 = df[df$Fiscal_Year == 2015,]
pie <- ggplot(df2, aes(x=factor(1), fill=status)) +
    geom_bar(width=1) +
    scale_fill_manual(values=c("#8DD3C7", "#FFFFB3",
                               "#BEBADA", "#D3D3D3")) +
    xlab("") +
    ylab("") +
    coord_polar(theta="y")
print(pie)
ggsave(file="plot_FY_annual/quant/pie_2015.png", width=8, height=4, units="in")

df = read.csv("input_R/cap_op_cnt.csv")
df1 = dbGetQuery(con, 'SELECT Building_Number, Cat FROM EUAS_category')
df = merge(df1, df, by="Building_Number", all.x=TRUE)
df$status <- factor(df$status, levels=c("Capital and Operational", "Capital Only", "Operational Only", "No Known Investment"))
df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
ggplot(df, aes(Cat, fill=status)) + geom_bar() + ylab("Building Count") + labs(title="Capital vs Operational Investment By Building Category") + scale_fill_brewer(palette="Set3") + theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/co_cnt_bycat.png", width=8, height=4, units="in")

df1 = read.csv("input_R/cap_op_cnt.csv")
df2 = read.csv("input_R/robust_energy.csv")
df2 <- rename(df2, c("status"="energy"))
df1 <- rename(df1, c("status"="investment"))
df = merge(df2, df1, by="Building_Number", all.x=TRUE)
df$investment <- factor(df$investment, levels=c("Capital and Operational", "Capital Only", "Operational Only"))
df <- df[complete.cases(df),]
ggplot(df, aes(investment, fill=energy)) + geom_bar() + ylab("Building Count") + labs(title="Capital vs Operational Investment Energy Data Quality") + scale_fill_brewer(palette="Set3") + theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/co_energy.png", width=8, height=4, units="in")

## separate out bad electric only and bad gas only
df1 = read.csv("input_R/cap_op_cnt.csv")
df2 = read.csv("input_R/robust_energy_sep.csv")
df2 <- rename(df2, c("status"="energy"))
df1 <- rename(df1, c("status"="investment"))
df = merge(df2, df1, by="Building_Number", all.x=TRUE)
df$investment <- factor(df$investment, levels=c("Capital and Operational", "Capital Only", "Operational Only"))
df <- df[complete.cases(df),]
df <- cbind(df[1:2], lapply(df[3], function(x) str_wrap(x, width = 30)), df[4])
g = stackbar(df, "investment", "energy", "Building Count",
             "Capital vs Operational Investment Energy Data Quality", "bottom")
print(g)
## ggplot(df, aes(investment, fill=energy)) +
##     geom_bar() +
##     ylab("Building Count") +
##     labs(title="Capital vs Operational Investment Energy Data Quality") +
##     scale_fill_brewer(palette="Set3") +
##     theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/co_energy_sep.png", width=8, height=4, units="in")

df1 = read.csv("input_R/cap_op_cnt.csv")
df2 = read.csv("input_R/robust_energy.csv")
df2 <- rename(df2, c("status"="energy"))
df1 <- rename(df1, c("status"="investment"))
df = merge(df2, df1, by="Building_Number", all.x=TRUE)
df$investment <- factor(df$investment, levels=c("With Investment", "No Known Investment"))
df <- df[complete.cases(df),]
ggplot(df, aes(investment, fill=energy)) + geom_bar() + ylab("Building Count") + labs(title="With vs Without Investment Energy Data Quality") + scale_fill_brewer(palette="Set3") + theme(legend.position="bottom")
ggsave(file="plot_FY_annual/quant/with_wout_energy.png", width=8, height=4, units="in")

getType <- function(s) {
    if (s %in% c("GSALink", "Advanced Metering", "LEED_EB", "GP", "first fuel", "Shave Energy", "E4")) {
        return("Operational")
    }
    else {
        return("Capital")
    }
}

give.n <- function(x){
    return(data.frame(y = 0, label = paste0("n=",length(x)))) 
}
# function for mean labels
mean.n <- function(x){
    return(c(y = median(x)*0.97, label = round(mean(x),2))) 
# experiment with the multiplier to find the perfect position
}
# function for median labels uncomment here to show both median and n
median.n <- function(x){
## return(c(y = median(x)*0.92, label = round(median(x),1))) 
return(c(y = median(x)*1.10, label = round(median(x),1))) 
# experiment with the multiplier to find the perfect position
}
actionCountEUIgb <- function(cat, plottype) {
    df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, high_level_ECM from EUAS_ecm WHERE high_level_ECM != \'GSALink\'')
    df2 = dbGetQuery(con, 'SELECT Building_Number, ECM_Program from EUAS_ecm_program')
    if (cat == "AI") {
        query = sprintf('SELECT Building_Number, Fiscal_Year, eui from eui_by_fy WHERE Cat in (%s) AND Fiscal_Year in (\'2003\', \'2015\')', '\'A\', \'I\'')
    } else if (cat == "AI-390") {
        query = sprintf('SELECT Building_Number, Fiscal_Year, eui from eui_by_fy_high_eui WHERE Cat in (%s) AND Fiscal_Year in (\'2003\', \'2015\')', '\'A\', \'I\'')
    }
    else {
        query = sprintf('SELECT Building_Number, Fiscal_Year, eui from eui_by_fy WHERE Fiscal_Year in (\'2003\', \'2015\')')
    }
    df3 = dbGetQuery(con, query)
    df1 <- df1[complete.cases(df1),]
    df2 <- df2[complete.cases(df2),]
    print(names(df1))
    print(names(df2))
    df1 <- plyr::rename(df1, c("high_level_ECM"="investment"))
    df2 <- plyr::rename(df2, c("ECM_program"="investment"))
    df = rbind(df1, df2)
    ## need to correct GP
    dfall = merge(df, df3, by="Building_Number", all.x=TRUE)
    dfall$type = sapply(dfall$investment, getType)
    dfall$type <-
        factor(dfall$type, levels=c("Capital", "Operational"))
    dfall$Fiscal_Year <- factor(dfall$Fiscal_Year)
    dfall$investment <-
    ## factor(dfall$investment, levels=c("LEED_EB","ESPC","GSALink",
    ##                                   "first fuel",
    ##                                   "Building Tuneup or Utility Improvements",
    ##                                   "HVAC","Lighting",
    ##                                   "Advanced Metering","GP","E4",
    ##                                   "Building Envelope",
    ##                                   "Shave Energy","LEED_NC"))
        factor(dfall$investment, levels=c("Advanced Metering",
                                          "GSALink",
                                          "E4","Shave Energy",
                                          "first fuel","LEED_EB","GP",
                                          "Building Tuneup or Utility Improvements",
                                          "HVAC","Lighting",
                                          "Building Envelope", "ESPC"))
    dfall %>% dplyr::group_by(investment, Fiscal_Year) %>% dplyr::summarize(median=median(eui)) %>% write.csv(file="/media/yujiex/work/GSA/merge/plot_FY_annual/quant_data/box03vs15.csv")
    dfall <- dfall[dfall$investment != "LEED_NC",]
    dfall <- dfall[complete.cases(dfall),]
    p <- ggplot(dfall, aes(x=investment, y=eui, fill=Fiscal_Year))
    if (plottype == "vio") {
        p <- p + geom_violin()
    }
    else if (plottype == "box") {
        p <- p + geom_boxplot(outlier.shape=NA)
    }
    p <- p + ylab("kBtu/sq.ft") +
        ## scale_fill_brewer(palette="Set3") +
        scale_fill_manual(values=c("#FB8072", "#8DD3C7")) +
        theme(legend.position="bottom") +
        ylim(-5, 140) +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        stat_summary(fun.data=give.n, geom="text", fun.y=median, position=position_dodge(width = 0.75), size=3) +
        stat_summary(fun.data=median.n, geom="text", fun.y=median, position=position_dodge(width = 0.75), size=3)
    if (cat == "AI-390") {
        p <- p + labs(title=sprintf("Electric + Gas EUI (n = 390) by Energy Investment %s Building in FY2003 vs FY2015", cat))
        ggsave(file=sprintf("plot_FY_annual/quant/invest_eui_1315_390.png", cat, plottype), width=12, height=6, units="in")
    } else {
        p <- p + labs(title=sprintf("Electric + Gas EUI (n = 650) by Energy Investment %s Building in FY2003 vs FY2015", cat))
        ggsave(file=sprintf("plot_FY_annual/quant/invest_eui_1315_.png", cat, plottype), width=12, height=6, units="in")
    }
    print(p)
}
actionCountEUIgb("AI", "box")
actionCountEUIgb("AI-390", "box")

# action vs no-action
year = 2015
give.n <- function(x){
    return(data.frame(y = 0, label = paste0("n=",length(x)))) 
}
# function for mean labels
mean.n <- function(x){
return(c(y = median(x)*0.97, label = round(mean(x),2))) 
# experiment with the multiplier to find the perfect position
}
# function for median labels
median.n <- function(x){
    return(c(y = median(x)*0.90, label = round(median(x),2))) 
# experiment with the multiplier to find the perfect position
}
plotset = "Electric EUI >= 3 kBtu/sq.ft and Gas EUI >= 3 kBtu/sq.ft FY2015"
titlestr = "Electric + Gas EUI kBtu/sq.ft/year by ECM Action vs Not"
query = sprintf('SELECT Building_Number, eui from eui_by_fy_high_eui WHERE Cat in (%s) AND Fiscal_Year = %s AND eui_elec >= 12 AND eui_gas >= 3', '\'A\', \'I\'', year)
df1 = dbGetQuery(con, query)
df2 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, high_level_ECM from EUAS_ecm WHERE high_level_ECM != \'GSALink\'')
df = merge(df1, df2, by="Building_Number", all.x=TRUE)
## df <- df[df["Cat"] %in% c("A", "I"),]
df$high_level_ECM[df$high_level_ECM == "Building Tuneup or Utility Improvements"] <- "Commissioning"
df$high_level_ECM[is.na(df$high_level_ECM)] <- "No Action"
## df$high_level_ECM <- factor(df$high_level_ECM, levels=levels(df$high_level_ECM)[c(1, 2, 4, 3, 5, 6)])
ggplot(df, aes(x=high_level_ECM, y=eui, fill=high_level_ECM)) +
    geom_boxplot() +
    ggtitle(bquote(atop(.(titlestr), atop(.(plotset), "")))) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    stat_summary(fun.data=give.n, geom="text", fun.y=median) +
    stat_summary(fun.data=median.n, geom="text", fun.y=median) +
    ylab("EUI kBtu/sq.ft") +
    ylim(0, 140)
    ggsave(file="plot_FY_annual/quant/ai_action.png", width=8, height=4, units="in")

give.n <- function(x){
return(c(y = median(x)*1.05, label = length(x))) 
}
# function for mean labels
mean.n <- function(x){
return(c(y = median(x)*0.97, label = round(mean(x),2))) 
# experiment with the multiplier to find the perfect position
}
# function for median labels
median.n <- function(x){
return(c(y = median(x)*0.90, label = round(median(x),2))) 
# experiment with the multiplier to find the perfect position
}
actionCountEUI <- function(cat, year, plottype) {
    df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, high_level_ECM from EUAS_ecm WHERE high_level_ECM != \'GSALink\'')
    df2 = dbGetQuery(con, 'SELECT Building_Number, ECM_Program from EUAS_ecm_program')
    if (cat == "AI") {
        if (year != "all") {
            query = sprintf('SELECT Building_Number, eui from eui_by_fy WHERE Cat in (%s) AND Fiscal_Year = %s', '\'A\', \'I\'', year)
        }
        else {
            query = sprintf('SELECT Building_Number, eui from eui_by_fy WHERE Cat in (%s)', '\'A\', \'I\'')
        }
    }
    else {
        if (year != "all") {
            query = sprintf('SELECT Building_Number, eui from eui_by_fy WHERE Fiscal_Year = %s', year)
        }
        else {
            query = sprintf('SELECT Building_Number, eui from eui_by_fy', year)
        }
    }
    df3 = dbGetQuery(con, query)
    df1 <- df1[complete.cases(df1),]
    df2 <- df2[complete.cases(df2),]
    df1 <- rename(df1, c("high_level_ECM"="investment"))
    df2 <- rename(df2, c("ECM_program"="investment"))
    df = rbind(df1, df2)
    ## need to correct GP
    dfall = merge(df, df3, by="Building_Number", all.x=TRUE)
    dfall$type = sapply(dfall$investment, getType)
    dfall$type <-
        factor(dfall$type, levels=c("Capital", "Operational"))
    dfall <- dfall[complete.cases(dfall),]
    summary(dfall)
    p <- ggplot(dfall, aes(x=investment, y=eui, fill=type))
    if (plottype == "vio") {
        p <- p + geom_violin()
    }
    else if (plottype == "box") {
        p <- p + geom_boxplot()
    }
    p <- p + ylab("kBtu/sq.ft") +
        labs(title=sprintf("Electric + Gas EUI by Energy Investment %s Building in FY%s", cat, year)) +
        scale_fill_brewer(palette="Accent") +
        theme(legend.position="bottom") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
        stat_summary(fun.data=give.n, geom="text", fun.y=median) +
        stat_summary(fun.data=median.n, geom="text", fun.y=median)
    ggsave(file=sprintf("plot_FY_annual/quant/invest_eui_%s_%s_%s.png", cat, year, plottype), width=12, height=6, units="in")
    if (year != "all") {
        p <- ggplot(df, aes(investment, fill=type))
    } else {
        p <- ggplot(dfall, aes(investment, fill=type))
    }
    p <- p + geom_bar() + ylab("Building Count") +
        labs(title=sprintf("Energy Investment %s Building in FY%s", cat, year)) +
        scale_fill_brewer(palette="Accent") +
        theme(legend.position="bottom") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    ggsave(file=sprintf("plot_FY_annual/quant/invest_cnt_%s_%s.png", cat, year, plottype), width=8, height=4, units="in")
}
actionCountEUI("all", "all", "box")
## actionCountEUI("AI", "2015", "box")
## actionCountEUI("AI", "2003", "box")
## actionCountEUI("AI", "2015", "vio")
## actionCountEUI("AI", "2003", "vio")

investCnt <- function(cat) {
    df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, high_level_ECM from EUAS_ecm WHERE high_level_ECM != \'GSALink\'')
    df2 = dbGetQuery(con, 'SELECT DISTINCT Building_Number, ECM_Program FROM EUAS_ecm_program WHERE ECM_Program != \'ESPC\'')
    if (cat == "AI") {
        df3 = dbGetQuery(con, 'SELECT Building_Number, Cat FROM EUAS_category WHERE Cat in (\'A\', \'I\')')
    } else {
        df3 = dbGetQuery(con, 'SELECT Building_Number, Cat FROM EUAS_category')
    }
    df1 <- df1[complete.cases(df1),]
    df2 <- df2[complete.cases(df2),]
    df1 <- rename(df1, c("high_level_ECM"="investment"))
    df2 <- rename(df2, c("ECM_program"="investment"))
    dfall = rbind(df1, df2)
    df = merge(dfall, df3, by="Building_Number")
    df$type = sapply(df$investment, getType)
    df$type <-
        factor(df$type, levels=c("Capital", "Operational"))
    df <- df[complete.cases(df),]
    df$type <- factor(df$type, levels=c("Capital", "Operational"))
    ## op = c("Advanced Metering", "E4", "first fuel", "GSALink", "GP", "LEED_EB", "Shave Energy")
    op = c("Advanced Metering", "first fuel", "GP", "GSALink", "Shave Energy", "LEED_EB", "E4")
    alltype = unique(df$investment)
    cap = alltype[!(alltype %in% op)]
    cap = c("HVAC", "Lighting", "Building Tuneup or Utility Improvements", "Building Envelope", "LEED_NC")
    ## print(cap)
    df$investment <- factor(df$investment, levels=c(op, cap))
    print(head(df))
    xcol = "investment"
    fillcol = "type"
    dfcount = plyr::count(df, c(xcol, fillcol))
    dfcount <- transform(dfcount, mid_y = ave(dfcount$freq, dfcount[,xcol], FUN = function(val) cumsum(val) - (0.5 * val)))
    p = ggplot(dfcount, aes_string(x=xcol, y="freq", fill=fillcol, label="freq")) +
        geom_bar(stat="identity") +
        geom_text(aes(y=mid_y), size=2.5) +
        ylab("Building Count")
    if (cat == "AI") {
        p <- p + labs(title=sprintf("Energy Investment A + I Building"))
    } else {
        p <- p + labs(title=sprintf("Energy Investment All Building"))
    }
    p <- p + scale_fill_brewer(palette="Accent") +
        theme(legend.position="bottom") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    print(p)
    ggsave(file=sprintf("plot_FY_annual/quant/invest_cnt_%s.png", cat), width=8, height=4, units="in")
    ## df %>% dplyr::group_by(investment) %>%
    ##     summarise(Building_Number = n()) %>%
    ##     write.csv(sprintf("plot_FY_annual/quant_data/pro_cnt_%s.csv", cat), row.names=FALSE)
}
investCnt("AI")
## investCnt("All")

## average square footage trend
library(Rmisc)
df = dbGetQuery(con, 'SELECT * from EUAS_area')
df <- df[df$Fiscal_Year < 2016,]
df$Fiscal_Year <- factor(df$Fiscal_Year)
dfs <- summarySE(df, measurevar="Gross_Sq.Ft", groupvars=c("Fiscal_Year"))
ggplot(dfs, aes(x=Fiscal_Year, y=Gross_Sq.Ft)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=Gross_Sq.Ft-ci, ymax=Gross_Sq.Ft+ci), width=.2, position=position_dodge(.9)) +
    ylab("Average Gross_Sq.Ft")
ggsave(file="plot_FY_annual/quant/ave_area.png", width=8, height=4, units="in")

# strip plot
df = dbGetQuery(con, 'SELECT * from EUAS_area')
df <- df[df$Fiscal_Year < 2016,]
df$Fiscal_Year <- factor(df$Fiscal_Year)
## dfs <- summarySE(df, measurevar="Gross_Sq.Ft", groupvars=c("Fiscal_Year"))
ggplot(df, aes(x=Fiscal_Year, y=Gross_Sq.Ft)) +
    geom_jitter()
##     geom_(stat="identity") +
##     geom_errorbar(aes(ymin=Gross_Sq.Ft-ci, ymax=Gross_Sq.Ft+ci), width=.2, position=position_dodge(.9)) +
##     ylab("Average Gross_Sq.Ft")
ggsave(file="plot_FY_annual/quant/ave_area_jitter.png", width=8, height=4, units="in")

## average square footage by category
library(Rmisc)
library(ggplot2)
library(DBI)
library(RColorBrewer)
con = dbConnect(drv=RSQLite::SQLite(), dbname="csv_FY/db/all.db")
df1 = dbGetQuery(con, 'SELECT * from EUAS_area')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat from EUAS_category')
df = merge(df1, df2, by="Building_Number", all.x=TRUE)
df <- df[df$Fiscal_Year < 2016,]
df$Fiscal_Year <- factor(df$Fiscal_Year)
head(df)
dfs <- summarySE(df, measurevar="Gross_Sq.Ft", groupvars=c("Cat", "Fiscal_Year"))
head(dfs)
pd <- position_dodge(0.1)
dfs$Cat <- factor(dfs$Cat, levels=c("A", "I", "C", "B", "D", "E"))
ggplot(dfs, aes(x=Cat, y=Gross_Sq.Ft, fill=Fiscal_Year)) + geom_bar(position=position_dodge(), stat="identity") + ylab("Average Gross_Sq.Ft")
## ggplot(dfs, aes(x=Fiscal_Year, y=Gross_Sq.Ft, color=Cat)) +
##     geom_line(aes(group=Cat), position=pd) +
##     expand_limits(y = 0) +
    ## geom_errorbar(aes(ymin=Gross_Sq.Ft-ci, ymax=Gross_Sq.Ft+ci),
    ##               width=.1, position=pd) +
    ## geom_point(position=pd)
ggsave(file="plot_FY_annual/quant/ave_area_bycat.png", width=8, height=4, units="in")
## ggsave(file="plot_FY_annual/quant/ave_area_bycat_line.png", width=8, height=4, units="in")

df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number from covered_facility')
iscovered <- function (x) {
    if (x %in% df1$Building_Number) {
        return("Covered")
    } else {
        return("Non-Covered")
    }
}
df2 = read.csv("input_R/cap_op_cnt.csv")
df2 <- df2[df2$status != "No Known Investment",]
df2 <- df2[df2$status != "With Investment",]
df2$isCovered = sapply(df2$Building_Number, iscovered)
g = stackbar(df2, "isCovered", "status", "Building Count", "Investment covered vs non-covered facility", "bottom")
print(g)
## with no label version
## ggplot(df2, aes(isCovered, fill=status)) +
##     geom_bar() +
##     ylab("Building Count") +
##     xlab("Is Covered") +
##     labs(title="Investment covered vs non-covered facility") +
##     theme(legend.position="bottom") +
##     scale_fill_brewer(palette="Set3")
ggsave(file="plot_FY_annual/quant/invest_cover_vs_no.png", width=8, height=4, units="in")

## covered by category
df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number from covered_facility')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat from EUAS_category')
df3 = dbGetQuery(con, 'SELECT Building_Number, [Self-Selected_Primary_Function] as Type FROM EUAS_type')
df = merge(df1, df2, on='Building_Number')
df <- merge(df, df3, on='Building_Number')
levels(df$Type)
## df[df["Type"]=="Data Center"] <- "Other"
df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
df$Type <- factor(df$Type)
df[(df=="Data Center")|(df=="Non-Refrigerated Warehouse")|(df=="Other - Public Services")] <- "Other"
levels(df$Type)
df$Type <- factor(df$Type, levels(df$Type)[c(4, 1, 5)])
levels(df$Type)
ggplot(df, aes(x=Cat, fill=Type)) + geom_bar() + ylab("Building Count") + xlab("Category") + labs(title="Covered facility by category and type") + scale_fill_brewer(palette="Set3")
ggsave(file="plot_FY_annual/quant/cover_bycat.png", width=8, height=4, units="in")
df %>% dplyr::group_by(Cat, Type) %>%
    summarise(Building_Number = n()) %>% cast(Cat ~ Type) %>%
    write.csv("plot_FY_annual/quant_data/covered_cat_type.csv", row.names=FALSE)

## Why is it treating it as continuous variable?
## covered by category with count label
df1 = dbGetQuery(con, 'SELECT DISTINCT Building_Number from covered_facility')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat from EUAS_category')
df3 = dbGetQuery(con, 'SELECT Building_Number, [Self-Selected_Primary_Function] as Type FROM EUAS_type')
df = merge(df1, df2, on='Building_Number')
df <- merge(df, df3, on='Building_Number')
levels(df$Type)
## df[df["Type"]=="Data Center"] <- "Other"
df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
df <- df[complete.cases(df),]
df$Type <- factor(df$Type)
df[(df=="Data Center")|(df=="Non-Refrigerated Warehouse")|(df=="Other - Public Services")] <- "Other"
levels(df$Type)
df$Type <- factor(df$Type, levels(df$Type)[c(4, 1, 5)])
levels(df$Type)
df$Cat <- factor(df$Cat, levels=c("A", "I", "C", "B", "D", "E"))
g = stackbar(df, "Cat", "Type", "Building Count", "Covered facility by category and type")
print(g)
ggsave(file="plot_FY_annual/quant/cover_bycat_.png", width=8, height=4, units="in")
## df %>% dplyr::group_by(Cat, Type) %>%
##     summarise(Building_Number = n()) %>% cast(Cat ~ Type) %>%
##     write.csv("plot_FY_annual/quant_data/covered_cat_type.csv", row.names=FALSE)

## area distribution by year cat I
## cat = "I"
cat = "A"
df1 = dbGetQuery(con, 'SELECT * from EUAS_area')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat from EUAS_category')
df = merge(df1, df2, by="Building_Number", all.x=TRUE)
nrow(df)
df <- df[df$Cat == cat,]
df$Fiscal_Year <- factor(df$Fiscal_Year)
nrow(df)
p <- ggplot(df, aes(x=Fiscal_Year, y=Gross_Sq.Ft))
p <- p + geom_boxplot()
print(p)
ggsave(file=sprintf("plot_FY_annual/quant/area_byyear_%s.png", cat), width=8, height=4, units="in")

# energy trend vs PNNL
## input = "aci_all"
input = "ai_good_eng"
title = "A + I"
eng = "robust energy"
df1 = read.csv(sprintf("input_R/%s.csv", input))
df2 = read.csv("input_R/pnnl.csv")
df1$Year <- (df1$Year)
df2$Year <- (df2$Year)
p1 <- ggplot(df1, aes(x=Year, y=EUI_Btu_per_SF)) +
    geom_line(color="red") +
    geom_point(color="red") +
    theme(legend.position="none") +
    ggtitle(sprintf("%s in EUAS data set (%s), EUI", title, eng)) +
    ylim(40000, 95000) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    geom_text(label=df1$EUI_Btu_per_SF, vjust=0, size=3.5) +
    ## geom_text(label=df1.n, vjust=0, size=3.5)
p2 <- ggplot(df2, aes(x=Year, y=EUI_Btu_per_SF)) +
    geom_line(color="red") +
    geom_point(color="red") +
    ggtitle("PNNL study, EUI") +
    theme(legend.position="none") +
    ylim(40000, 95000) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    geom_text(label=df2$EUI_Btu_per_SF, vjust=0, size=3.5)
p3 <- ggplot(df1, aes(x=Year, y=Energy_Use_BBtu)) +
    geom_line(color="blue") +
    geom_point(color="blue") +
    theme(legend.position="none") +
    ggtitle(sprintf("%s in EUAS data set (%s), Energy Use", title, eng)) +
    ylim(5000, 18000) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    geom_text(label=df1$Energy_Use_BBtu, vjust=0, size=3.5)
p4 <- ggplot(df2, aes(x=Year, y=Energy_Use_BBtu)) +
    geom_line(color="blue") +
    geom_point(color="blue") +
    theme(legend.position="none") +
    ggtitle("PNNL study, Energy Use") +
    ylim(5000, 18000) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    geom_text(label=df2$Energy_Use_BBtu, vjust=0, size=3.5)
p5 <- ggplot(df1, aes(x=Year, y=Floor_Space_kSF)) +
    geom_line(color="green") +
    geom_point(color="green") +
    theme(legend.position="none") +
    ggtitle(sprintf("%s in EUAS data set (%s), Square Footage", title, eng)) +
    ylim(80000, 200000) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    geom_text(label=df1$Floor_Space_kSF, vjust=0, size=3.5)
p6 <- ggplot(df2, aes(x=Year, y=Floor_Space_kSF)) +
    geom_line(color="green") +
    geom_point(color="green") +
    theme(legend.position="none") +
    ggtitle("PNNL study, Square Footage") +
    ylim(80000, 200000) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    geom_text(label=df2$Floor_Space_kSF, vjust=0, size=3.5)
png(file=sprintf("plot_FY_annual/quant/%s_trend.png", input), width=14, height=7, units="in", res=300)
multiplot(p1, p3, p5, p2, p4, p6, cols=2)
dev.off()

# capital vs operation median trend per degree day
df = read.csv("input_R/all_median_trend_perdd.csv")
df$status <-
    factor(df$status, levels=levels(df$status)[c(1, 4, 3, 6, 2, 5)])
ggplot(df,
       aes(x=Fiscal_Year, y=eui_perdd, color=status,
           label=Building_Number)) +
    geom_line() +
    geom_point() +
    geom_text(vjust=0, size=2.5, position=position_dodge(0.9)) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    ylab("Electric per cdd + Gas per hdd [kBtu/sq.ft * F * year]") +
    ggtitle("Median EUI per degree day trend") +
    scale_colour_brewer(palette="Set3")
ggsave(file="plot_FY_annual/quant/median_eui_trend_perdd.png", width=8, height=4, units="in")

# capital vs operation median trend
agg = "mean"
df = read.csv(sprintf("input_R/all_%s_trend.csv", agg))
df$status <-
    factor(df$status, levels=levels(df$status)[c(1, 4, 3, 6, 2, 5)])
ggplot(df,
       aes(x=Fiscal_Year,
           y=eui,
           color=status,
           label=Building_Number)) +
    geom_line() +
    geom_point() +
    geom_text(vjust=0, size=2.5, position=position_dodge(0.9)) +
    scale_x_continuous(breaks=seq(2003, 2015, 1)) +
    ylab("Electric + Gas [kBtu/sq.ft * year]") +
    ggtitle(sprintf("%s EUI trend", agg)) +
    scale_colour_brewer(palette="Set3")
ggsave(file=sprintf("plot_FY_annual/quant/%s_eui_trend.png", agg), width=8, height=4, units="in")

## very highest level
agg = "median"
df = read.csv(sprintf("input_R/all_%s_trend.csv", agg))
df <- df[df$Fiscal_Year %in% c(2003, 2015),]
df$eui <- sapply(df$eui, function(x) round(x, 1))
## df$status <-
##     factor(df$status, levels=levels(df$status)[c(1, 4, 3, 6, 2, 5)])
df$Fiscal_Year <- factor(df$Fiscal_Year)
df1 <- df[df$status == "A + I",]
p1 <- ggplot(df1,
       aes(x=Fiscal_Year, y=eui, label=eui)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("Median Electric + Gas [kBtu/sq.ft]") +
    ggtitle("Median EUI Reduction 2003 to 2015")
    geom_text(nudge_y=1)
print(p1)
ggsave(file=sprintf("plot_FY_annual/quant/AI_%s_eui_0315.png", agg), width=4, height=4, units="in")
df2 <- df[df$status %in% c("Capital_and_Operational",
                           "Capital_Only", "Operational_Only"),]
p2 <- ggplot(df2, aes(x=Fiscal_Year, y=eui, fill=status, label=eui)) +
    geom_bar(stat="identity", position="dodge") +
    ylab("Median Electric + Gas [kBtu/sq.ft]") +
    ggtitle("Median EUI Reduction 2003 to 2015") +
    geom_text(aes(y = eui + 1), position=position_dodge(0.9)) +
    theme(legend.position="bottom")
    scale_fill_brewer(palette="Set3")
print(p2)
ggsave(file=sprintf("plot_FY_annual/quant/AI_%s_eui_co_0315.png", agg), width=4, height=4, units="in")

## check weather distribution across year
df = dbGetQuery(con, 'SELECT Fiscal_Year, SUM(hdd65) AS hdd65, SUM(cdd65) AS cdd65 FROM EUAS_monthly_weather WHERE Fiscal_Year < 2016 GROUP BY Fiscal_Year')
p1 <- ggplot(df) +
    geom_line(aes(x=Fiscal_Year, y=hdd65)) +
    geom_point(aes(x=Fiscal_Year, y=hdd65)) +
    ggtitle("Total Degree Day for All Buildings") +
    scale_x_continuous(breaks=seq(2003, 2015, 1))
p2 <- ggplot(df) +
    geom_line(aes(x=Fiscal_Year, y=cdd65)) +
    geom_point(aes(x=Fiscal_Year, y=cdd65)) +
    scale_x_continuous(breaks=seq(2003, 2015, 1))
png(file="plot_FY_annual/quant/tot_degreeday.png", width=14, height=7, units="in", res=300)
multiplot(p1, p2, cols=1)
dev.off()

df1 = dbGetQuery(con, 'SELECT Building_Number, Fiscal_Year, Fiscal_Month, hdd65, cdd65 FROM EUAS_monthly_weather WHERE Fiscal_Year < 2016')
df2 = dbGetQuery(con, 'SELECT Building_Number, Cat FROM EUAS_category WHERE Cat in (\'A\', \'I\')')
print(nrow(df2))
print(nrow(df1))
dfall = merge(x=df1, y=df2, by="Building_Number", all.x=TRUE)
dfall <- dfall[complete.cases(dfall),]
toagg = dfall[, c("Fiscal_Year", "hdd65", "cdd65")]
df <- aggregate(toagg, by=list(toagg$Fiscal_Year), FUN=sum)
df <- df[, c("Group.1", "hdd65", "cdd65")]
df <- rename(df, c("Group.1"="Fiscal_Year"))
print(nrow(df))
p1 <- ggplot(df) +
    geom_line(aes(x=Fiscal_Year, y=hdd65)) +
    geom_point(aes(x=Fiscal_Year, y=hdd65)) +
    ggtitle("Total Degree Day for A + I Buildings") +
    scale_x_continuous(breaks=seq(2003, 2015, 1))
p2 <- ggplot(df) +
    geom_line(aes(x=Fiscal_Year, y=cdd65)) +
    geom_point(aes(x=Fiscal_Year, y=cdd65)) +
    scale_x_continuous(breaks=seq(2003, 2015, 1))
png(file="plot_FY_annual/quant/tot_degreeday_ai.png", width=14, height=7, units="in", res=300)
multiplot(p1, p2, cols=1)
dev.off()

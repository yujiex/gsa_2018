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
getType <- function(s) {
    if (s %in% c("GSALink", "Advanced Metering", "LEED_EB", "GP", "first fuel", "Shave Energy", "E4")) {
        return("Operational")
    }
    else {
        return("Capital")
    }
}

## no error bar version of number of action before vs after LEAN saving
## summary
theme = "elec"
## theme = "gas"
title = "Electric"
## title = "Gas"
measure = "percent"
## measure = "abs"
cpop = "cap"
## cpop = "op"
savingByActionCnt <- function(theme, title, measure, cpop) {
    labelsize = 4
    df = read.csv(sprintf("input_R/%s_action_save.csv", theme))
    if (measure == "percent") {
        oldname <- sprintf("%s_%s", title, "Saving")
    } else {
        oldname <- sprintf("%s_%s", title, measure)
    }
    print(oldname)
    for (i in 1:1) {
        dftemp <- df[df["number"] == i,]
        print(dftemp[dftemp["high_level_ecm"] == "Lighting",])
        if (cpop == "cap") {
            dftemp <- dftemp[dftemp["high_level_ecm"] != "Advanced Metering",]
            dftemp <- dftemp[dftemp["high_level_ecm"] != "GSALink",]
        } else {
            dftemp <- dftemp[dftemp$high_level_ecm %in% c("Advanced Metering", "GSALink"),]
        }
        names(dftemp)[names(dftemp)==oldname] <- "Saving"
        dfs <- summarySE(dftemp, measurevar="Saving", groupvars=c("high_level_ecm"))
        by_action <- dftemp %>% group_by(high_level_ecm)
        agg <- summarise(by_action,
                        count = n(),
                        min = min(Saving),
                        max = max(Saving),
                        Saving = mean(Saving))
        ## ## comment out to remove groups with too few observations
        agg <- agg[agg$count > 5,]
        agg <- agg[order(agg$Saving, decreasing=TRUE),]
        agg$high_level_ecm <- factor(agg$high_level_ecm, levels=agg$high_level_ecm)
        ## savewidth = ((nrow(agg) %/% 3) + 1) * 4
        ## textwidth = ((nrow(agg) %/% 3) + 1) * 30
        savewidth = nrow(agg) * 2
        textwidth = nrow(agg) * 30 
        p <- ggplot(agg, aes(x=high_level_ecm, y=Saving)) +
            geom_bar(stat="identity", width=0.5) +
            ## geom_errorbar(aes(ymin=Saving-ci, ymax=Saving+ci), width=.2, position=position_dodge(.9)) +
            geom_text(aes(y=-1, label=sprintf("n = %s", count))) +
            scale_x_discrete(labels = function(x) gsub(";", "\n", x))
        if (measure == "percent") {
            p <- p +
                ## geom_text(aes(y=Saving/2, label=sprintf("%.1f%s\n(%.1f%s, %.1f%s)", Saving, "%", min, "%", max, "%")),
                geom_text(aes(y=Saving/2, label=sprintf("%.1f%s", Saving, "%")),
                        size=labelsize, color="white") +
                ylab(sprintf("Average %s EUI Percent Saving", title)) +
                ggtitle(str_wrap(sprintf("Average %s EUI Percent Saving for Buildings with %s Action", title, i), width=textwidth))
        } else {
            p <- p +
                ## geom_text(aes(y=Saving/2, label=sprintf("%.1f\n(%.1f, %.1f)", Saving, min, max)),
                geom_text(aes(y=Saving/2, label=sprintf("%.1f", Saving)),
                          size=labelsize, color="white") +
                ylab(sprintf("Average EUI [kBtu/sq.ft./year] Absolute Saving", title)) +
                ggtitle(str_wrap(sprintf("Average %s EUI Absolute Saving for Buildings with %s Action", title, i), width=textwidth))
        }
        p <- p + theme(plot.title = element_text(size = 10))
        p <- p + ylim(-2, 35)
        print(p)
        ggsave(file=sprintf("plot_FY_annual/quant/%s_%s_%s_action_%s_noerr.png",
                            measure, theme, i, cpop),
            width=savewidth, height=4, units="in")
    }
    write.csv(dfs, file="plot_FY_annual/quant_data/saving_elec_1_action.csv")
}
savingByActionCnt("elec", "Electric", "percent", "cap")
savingByActionCnt("elec", "Electric", "abs", "cap")
savingByActionCnt("gas", "Gas", "percent", "cap")
savingByActionCnt("gas", "Gas", "abs", "cap")
savingByActionCnt("elec", "Electric", "percent", "op")
savingByActionCnt("elec", "Electric", "abs", "op")
savingByActionCnt("gas", "Gas", "percent", "op")
savingByActionCnt("gas", "Gas", "abs", "op")

## 3 action
savingByActionCnt3 <- function(theme, title, measure, cpop) {
    labelsize = 4
    titlesize = 7
    df = read.csv(sprintf("input_R/%s_action_save.csv", theme))
    if (measure == "percent") {
        oldname <- sprintf("%s_%s", title, "Saving")
    } else {
        oldname <- sprintf("%s_%s", title, measure)
    }
    print(oldname)
    for (i in 3:3) {
        dftemp <- df[df["number"] == i,]
        names(dftemp)[names(dftemp)==oldname] <- "Saving"
        dfs <- summarySE(dftemp, measurevar="Saving", groupvars=c("high_level_ecm"))
        by_action <- dftemp %>% group_by(high_level_ecm)
        agg <- summarise(by_action, count = n(), min = min(Saving),
                         max = max(Saving), Saving = mean(Saving))
        ## ## comment out to remove groups with too few observations
        agg <- agg[agg$count > 5,]
        agg <- agg[order(agg$Saving, decreasing=TRUE),]
        agg$high_level_ecm <- factor(agg$high_level_ecm, levels=agg$high_level_ecm)
        ## savewidth = ((nrow(agg) %/% 3) + 1) * 4
        ## textwidth = ((nrow(agg) %/% 3) + 1) * 30
        savewidth = nrow(agg) * 2
        textwidth = nrow(agg) * 30 
        p <- ggplot(agg, aes(x=high_level_ecm, y=Saving)) +
            geom_bar(stat="identity", width=0.5) +
            ## geom_errorbar(aes(ymin=Saving-ci, ymax=Saving+ci), width=.2, position=position_dodge(.9)) +
            geom_text(aes(y=-1, label=sprintf("n = %s", count), font=1)) +
            scale_x_discrete(labels = function(x) gsub(";", "\n", x))
        if (measure == "percent") {
            p <- p +
                ## geom_text(aes(y=Saving/2, label=sprintf("%.1f%s\n(%.1f%s, %.1f%s)", Saving, "%", min, "%", max, "%")),
                geom_text(aes(y=Saving/2, label=sprintf("%.1f%s", Saving, "%")),
                        size=labelsize, color="white") +
                ylab(sprintf("Average %s EUI Percent Saving", title)) +
                ggtitle(str_wrap(sprintf("Average %s EUI Percent Saving for Buildings with %s Action", title, i), width=textwidth))
        } else {
            p <- p +
                ## geom_text(aes(y=Saving/2, label=sprintf("%.1f\n(%.1f, %.1f)", Saving, min, max)),
                geom_text(aes(y=Saving/2, label=sprintf("%.1f", Saving)),
                          size=labelsize, color="white") +
                ylab(sprintf("Average EUI [kBtu/sq.ft./year] Absolute Saving", title)) +
                ggtitle(str_wrap(sprintf("Average %s EUI Absolute Saving for Buildings with %s Action", title, i), width=textwidth))
        }
        p <- p + theme(plot.title = element_text(size = titlesize))
        p <- p + ylim(-2, 35)
        p <- p + xlab("")
        print(p)
        ggsave(file=sprintf("plot_FY_annual/quant/%s_%s_%s_action_%s_noerr.png",
                            measure, theme, i, cpop), width=savewidth,
               height=4, units="in")
    }
    ## write.csv(dfs, file="plot_FY_annual/quant_data/saving_elec_1_action.csv")
}
## savingByActionCnt3("elec", "Electric", "percent")
## savingByActionCnt3("gas", "Gas", "percent")
## savingByActionCnt3("elec", "Electric", "abs", "cap")
## savingByActionCnt3("gas", "Gas", "abs", "cap")
savingByActionCnt3("elec", "Electric", "abs", "op")
savingByActionCnt3("gas", "Gas", "abs", "op")

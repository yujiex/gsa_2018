library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)

projectDir = "~/Dropbox/thesis/writeups/energy model/images"

df = read.csv('~/Dropbox/thesis/data/interval_building_info.csv')

p <- ggplot(df, aes(x=Year_Built, y=..density..)) +
    geom_histogram() +
    geom_density() +
    ylab("Number of Buildings") +
    ggtitle("Distribution of Year_Built") +
    scale_y_continuous(breaks=c(0,3,6,9))
    ggsave(file=sprintf("%s/dist_year_built.png", projectDir),
        width=4, height=4, units="in")
p

p <- ggplot(df, aes(x=Gross_Square_Feet_.GSF., y=..density..)) +
    geom_histogram() +
    geom_density() +
    ylab("Number of Buildings") +
    ggtitle("Distribution of gross square feet") +
    ggsave(file=sprintf("%s/dist_gsf.png", projectDir),
        width=4, height=4, units="in")
p

df = read.csv('~/Dropbox/thesis/data/energy_start_stop.csv')
df$energy_start <- format(as.Date(df$energy_start) ,format = "%Y-%m-%d %H:%M:%S")
df$energy_stop <- format(as.Date(df$energy_stop) ,format = "%Y-%m-%d %H:%M:%S")

df_long <- gather(df, status, time, energy_start:energy_stop)
df_long$month <- month(df_long$time)
df_long$year <- year(df_long$time)
df_long$ym <- as.Date(paste(df_long$year, "-", df_long$month, "-01 00:00:00", sep=""))

p <- ggplot(df_long, aes(x=ym, fill=status)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%y/%m")  +
    geom_histogram() +
    ggtitle("Distribution start and end month of energy recording") ##+
    ggsave(file=sprintf("%s/energy_start_stop.png", projectDir),
        width=8, height=4, units="in")
p

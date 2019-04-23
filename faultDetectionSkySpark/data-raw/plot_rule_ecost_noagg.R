library("dplyr")
library("readr")
library("ggplot2")
library("gridExtra")

## for each builidng, plot rule group by equipment, filter by type
namelookup = readr::read_csv("../data/gsalink_name_in_rulefile.csv") %>%
  tibble::as_data_frame() %>%
  {.}
component.group.jitter.scale = 0.1

energytype = "kWh Del Int"
## r = "Unoccupied Cooling Setpoint Out of Range"
r = NA
## b = "AK0031AA"

buildings = c("CA0167ZZ", "NV0304ZZ", "NC0002AE", "AL0039AB",
              "TX0302ZZ", "OH0192ZZ" , "IN0048ZZ", "IN0133ZZ",
              "GA1007ZZ", "UT0017ZZ")

for (b in buildings[1:1]) {

  b = buildings[1]
  name = paste0(namelookup[namelookup$building==b,]$name, " ")

  df = readr::read_csv(sprintf("ruleStartEndByBuilding/%s_2018.csv", b))
  nrow(df)
  df <- df %>%
    ## na.omit() %>%
    dplyr::mutate_at(vars(Cost, eCost, mCost, sCost), function(x) as.numeric(gsub("$", "", x, fixed=TRUE))) %>%
    {.}
  if (!is.na(r)) {
    df <- df %>%
      dplyr::filter(rule==r) %>%
      {.}
  }
  df <- df %>%
    dplyr::mutate(equipRef=substr(equipRef, 32, nchar(equipRef))) %>%
    dplyr::mutate(equipRef=gsub(name, "", equipRef)) %>%
    dplyr::mutate(`eCost/hour`=eCost/(durationSecond/3600)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`component.group`=(strsplit(equipRef, "[_ -]"))[[1]][1]) %>%
    ## dplyr::mutate(`eCost/hour`= `eCost/hour` + as.integer(as.factor(component.group))*component.group.jitter.scale
    ##               ## shift so that jitter amounts are centered (in some sense)
    ##               ## around 0:
    ##               - (length(unique(component.group))+1)/2*component.group.jitter.scale) %>%
    {.}

  ## print(head(df$`component.group`))

  mindate = min(df$startPosix)
  maxdate = max(df$endPosix)

  energy = readr::read_csv(sprintf("building_energy/%s_%s.csv", b, energytype)) %>%
    na.omit() %>%
    tibble::as_data_frame() %>%
    dplyr::mutate_if(is.integer, as.double) %>%
    {.}

  energy$Timestamp = as.POSIXct(energy$Timestamp,
                                format="%m/%d/%Y %I:%M:%S %p", tz="EST")

  energy <- energy %>%
    dplyr::filter(Timestamp <= maxdate, Timestamp >= mindate)

  p1 = df %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(aes(x=startPosix, xend=endPosix, y=`eCost/hour`,
                              yend=`eCost/hour`,
                              ## color=`rule`,
                              color=`component.group`,
                              ), size=2) +
    ## group=equipRef, color=equipRef), size=2) +
    ggplot2::ggtitle(label=sprintf("Building: %s", b),
                    subtitle = sprintf("Rule: %s", r)) +
    ggplot2::xlim(c(mindate, maxdate)) +
    ggplot2::xlab("Time") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  ## fixme
  devtools::load_all("~/Dropbox/gsa_2017/summarise.and.plot")
  summarise.and.plot::plot_event_start_end(df=df, event="rule", start="startPosix", end="endPosix", value="eCost/hour", time.min=mindate, time.max=maxdate, title=b)

  p2 = energy %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x=Timestamp, y=!!rlang::sym(energytype)))

  g <- grid.arrange(p1, p2, nrow=2)

  ggsave(filename=sprintf("../plots/%s_%s_%s.png", b, r, energytype), g)

}

library("magrittr")
library("pipeR")
library("readr")
library("ggplot2")
library("gridExtra")


setwd("~/Dropbox/gsa_2017/faultDetectionSkySpark/data-raw")

## for each builidng, plot rule group by equipment, filter by type
namelookup = readr::read_csv("../data/gsalink_name_in_rulefile.csv") %>%
  tibble::as_data_frame() %>%
  {.}

energytype = "kWh Del Int"
r = "Occupied Cooling Setpoint Out of Range"
## r = NA
## b = "AK0031AA"


buildings = c("CA0167ZZ", "NV0304ZZ", "NC0002AE", "AL0039AB",
              "TX0302ZZ", "OH0192ZZ" , "IN0048ZZ", "IN0133ZZ",
              "GA1007ZZ", "UT0017ZZ")

for (b in buildings[1:1]) {

  b = buildings[1]
  ## b = "IN0048ZZ"
  name = paste0(namelookup[namelookup$building==b,]$name, " ")
  df = readr::read_csv(sprintf("ruleStartEndByBuilding/%s_2018.csv", b)) %>%
    na.omit() %>%
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
    dplyr::mutate(`rule.component.group`=(strsplit(equipRef, "[_ -]"))[[1]][1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(startPosix, endPosix, rule, `eCost/hour`, equipRef, `rule.component.group`) %>%
    {.}

  print(tail(df$`rule.component.group`))

  time.min = min(df$startPosix)
  time.max = max(df$endPosix)
  ## time.min = as.POSIXct("2018-04-01", tz="America/Los_Angeles")
  ## time.max = as.POSIXct("2018-04-03", tz="America/Los_Angeles")
  ## time.min = as.POSIXct("2018-04-01")
  ## time.max = as.POSIXct("2018-04-03")
  time.epsilon = 0.1
  rule.component.group.jitter.scale = 0.01

  devtools::load_all("~/Dropbox/gsa_2017/summarise.and.plot")
  summarise.and.plot::scan_agg(df=df, start="startPosix", end="endPosix",
                               ## group=NULL, value="eCost/hour"
                               ## group=c("rule", "rule.component.group"), value="eCost/hour"
                               ## group="rule.component.group", value="eCost/hour"
                               group="rule.component.group", value="count", legend.ncol = 2,
                               group.jitter.scale=0.01
                               ## group="rule.component.group", value="eCost/hour"
                               )

  head(df)

  df$count = 1

  devtools::load_all("~/Dropbox/gsa_2017/summarise.and.plot")
  dfresult = summarise.and.plot::agg_interval(df=df, start="startPosix", end="endPosix",
                               ## group=NULL, value="eCost/hour"
                               ## group=c("rule", "rule.component.group"), value="eCost/hour"
                               ## group="rule.component.group", value="eCost/hour"
                               group="rule.component.group", value="count",
                               time.epsilon=0)

  dfleft = data.frame(time=seq(from=time.min, to=time.max, by="mins")) %>%
    {.}

  dfresult %>%
    dplyr::distinct(rule.component.group)

  extra.times = setdiff(dfresult[["time"]], dfleft[["time"]])
  if (length(extra.times) != 0L) {
    stop (paste0("The data had unexpected values in the time column; some are: ",
                 paste(head(extra.times), collapse=", ")))
  }

  dfresult %>%
    dplyr::filter(row.kind!="pre-delta") %>%
    tidyr::complete(rule.component.group, time=dfleft$time) %>%
    dplyr::group_by(rule.component.group) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(value.agg=zoo::na.locf0(value.agg)) %>%
    dplyr::mutate(value.agg=zoo::na.fill(value.agg, 0)) %>%
    dplyr::ungroup(rule.component.group) %>%
    ## readr::write_csv("filled.csv")
    ## {print(nrow(.));.} %>%
    ## {print(dplyr::distinct(.,time));.} %>%
    ## {print(dplyr::distinct(.,rule.component.group));.} %>%
    dplyr::filter(rule.component.group=="AH") %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x=time, y=value.agg))


    {.}

    head()

  ## energy = readr::read_csv(sprintf("building_energy/%s_%s.csv", b, energytype)) %>%
  ##   na.omit() %>%
  ##   tibble::as_data_frame() %>%
  ##   dplyr::mutate_if(is.integer, as.double) %>%
  ##   {.}

  ## energy$Timestamp = as.POSIXct(energy$Timestamp,
  ##                               format="%m/%d/%Y %I:%M:%S %p", tz="EST")

  ## energy <- energy %>%
  ##   dplyr::filter(Timestamp <= time.max, Timestamp >= time.min)

  rule.component.group.interval.sums =
    df %>>%
    ## tidyr::gather(type, time, startPosix:endPosix) %>%
    dplyr::filter(endPosix <= time.max, startPosix >= time.min) %>>%
    {
      dplyr::bind_rows(
               . %>>% dplyr::mutate(time=startPosix, delta=1),
               . %>>% dplyr::mutate(time=endPosix, delta=-1)
             )
    } %>>%
    ## dplyr::filter(time <= time.max, time >= time.min) %>%
    dplyr::mutate(`eCost/hour`=`eCost/hour` * delta) %>%
    dplyr::group_by(rule.component.group, time) %>>%
    dplyr::summarize(`eCost/hour`= sum(`eCost/hour`)) %>>%
    dplyr::ungroup() %>>%
    dplyr::group_by(`rule.component.group`) %>>%
    dplyr::arrange(time) %>>%
    dplyr::do({
      ## print(.)
      ## print(.$time)
      ## print(.$time - time.epsilon)
      ## print(. %>>% dplyr::mutate(time=.$time-time.epsilon) %>>% (time))
      ## print(tibble::tibble(time=.$time-time.epsilon) %>>% (time))
      ## print(class(.$time))
      ## print("AAA")
      ## print(time.min)
      ## print(.$time-time.epsilon)
      ## print(.$time+time.epsilon)
      ## print(time.max)
            asdf = tibble::tibble(
                      rule.component.group=.$rule.component.group[[1L]],
                      time=c(
                        time.min, # beginning of time
                        .$time - time.epsilon, # before delta
                        .$time + time.epsilon, # after delta
                        time.max # end of time
                      ),
                      value=c(
                        0, # assume 0 at beginning of time
                        cumsum(.$`eCost/hour`)-.$`eCost/hour`, # value before delta
                        cumsum(.$`eCost/hour`), # value after delta
                        0 # assume 0 at end of time
                      )
                    )
            ## print(asdf)
            asdf
          }) %>%
    dplyr::ungroup() %>>%
    ## For visualization: jitter values based on rule.component.group so rule.component.group time
    ## series don't overlap, e.g., when values are simultaneously 0
    dplyr::mutate(value=value # ...
                  ## number rule.component.groups 1..n, shift rule.component.group i by i*rule.component.group.jitter.scale:
                  + as.integer(as.factor(rule.component.group))*rule.component.group.jitter.scale
                  ## shift so that jitter amounts are centered (in some sense)
                  ## around 0:
                  - (length(unique(rule.component.group))+1)/2*rule.component.group.jitter.scale
                  ) %>>%
    {.}

  p1 <- rule.component.group.interval.sums %>>%
    ggplot2::ggplot(ggplot2::aes(time,value,colour=rule.component.group)) +
    ggplot2::geom_line(stat="identity") +
    ## ggplot2::geom_point() +
    ggplot2::ggtitle(label=sprintf("Building: %s", b),
                    subtitle = sprintf("Rule: %s", r)) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("eCost/hour") +
    ggplot2::theme_bw() +
    ggplot2::xlim(c(time.min, time.max)) +
    ggplot2::theme(legend.position = "bottom")
  print(p1)

  ## p2 = energy %>%
  ##   ggplot2::ggplot() +
  ##   ggplot2::geom_line(aes(x=Timestamp, y=!!rlang::sym(energytype)))

  ## g <- grid.arrange(p1, p2, nrow=2)

  ## ggsave(filename=sprintf("agg_%s_%s_%s.png", b, r, energytype), g)
  ## print(g)
}

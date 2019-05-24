library("dplyr")
library("readr")

energytype = "BTU - From Utility Int"

chopped = TRUE

if (chopped) {
  result.file = sprintf("reg_result_chopped/allrules_%s.csv", energytype)
} else {
  result.file = sprintf("reg_result/allrules_%s.csv", energytype)
}
reg.result = readr::read_csv(result.file) %>%
  tibble::as_tibble() %>%
  {.}

reg.result <- reg.result %>%
  dplyr::rename(rule=covariate) %>%
  tidyr::spread(occtype, coefficient) %>%
  dplyr::filter(!(rule %in% c("(Intercept)", "F"))) %>%
  {.}

buildings = reg.result %>%
  dplyr::distinct(building) %>%
  .$building

b = "OH0192ZZ"

b = buildings[1]

files = list.files(path="skyspark fault detection sparks download",
                   pattern=sprintf("%s*", b))

acc = NULL
for (b in buildings) {
  df = readr::read_csv(sprintf("ruleStartEndByBuilding/%s_2018.csv", b),
                       col_types = cols(durationSecond = col_double(),
                                        mCost=col_double(),
                                        sCost=col_double(),
                                        eCost=col_double(),
                                        Cost=col_double())) %>%
    {print(readr::problems(.));.} %>%
    tibble::as_tibble() %>%
    dplyr::select(startPosix, durationSecond, endPosix, tz, rule, eCost) %>%
    {.}
  timezone = df[["tz"]][1]
  df <- df %>%
    dplyr::mutate(`start.local.time`=as.POSIXct(format(startPosix, tz=timezone), tz="UTC")) %>%
    dplyr::mutate(`end.local.time`=as.POSIXct(format(endPosix, tz=timezone), tz="UTC")) %>%
    dplyr::mutate(day=format(start.local.time, "%A")) %>%
    dplyr::mutate(occ.start=as.POSIXct(format(start.local.time, "%Y-%m-%d 08:00:00"), tz="UTC")) %>%
    dplyr::mutate(occ.end=as.POSIXct(format(start.local.time, "%Y-%m-%d 17:00:00"), tz="UTC")) %>%
    dplyr::mutate(bound.end=pmin(end.local.time, occ.end)) %>%
    dplyr::mutate(bound.start=pmax(occ.start, start.local.time)) %>%
    dplyr::mutate(occ.duration.min = pmax(0, bound.end - bound.start)/60) %>%
    {.}
  df <- df %>%
    dplyr::select(-bound.start, -bound.end) %>%
    dplyr::mutate(duration.min = durationSecond / 60,
                  unocc.duration.min = duration.min - occ.duration.min,
                  building = b) %>%
    dplyr::left_join(reg.result, by=c("building", "rule")) %>%
    dplyr::select(-durationSecond) %>%
    dplyr::mutate(month=format(start.local.time, "%m")) %>%
    dplyr::mutate(season=ifelse(month %in% c("11", "12", "01", "02"), "winter",
                                ifelse(month %in% c("06", "07", "08"), "summer", "other"))) %>%
    dplyr::mutate(est.eCost.allday=ifelse((seasontype==season) | (seasontype=="allyear"),
                                          allday * duration.min, NA),
                  est.eCost.occ=ifelse((seasontype==season) | (seasontype=="allyear"),
                                      Occupied * occ.duration.min, NA),
                  est.eCost.unocc=ifelse((seasontype==season) | (seasontype=="allyear"),
                                      `Un-occupied` * unocc.duration.min, NA),
                  ) %>%
    dplyr::mutate(est.eCost.consider.occhour = est.eCost.occ + est.eCost.unocc) %>%
    {.}
  acc <- rbind(acc, df)
}

options(tibble.width = Inf)

acc %>%
  dplyr::filter(is.na(duration.min)) %>%
  distinct(building)


if (chopped) {
  acc %>%
    readr::write_csv(sprintf("compare_with_their_ecost_%s_chopped.csv", energytype))
} else {
  acc %>%
    readr::write_csv(sprintf("compare_with_their_ecost_%s.csv", energytype))
}

library("feather")

acc %>%
  feather::write_feather("compare_with_their_ecost.feather")

acc = feather::read_feather("compare_with_their_ecost.feather")

allrules = acc %>%
  distinct(rule) %>%
  .$rule

g.cols = scales::hue_pal()(3)
## c("red", "green", "blue")

## summary table comparing the total of the estimates
for (r in allrules) {
  print(r)
  toplot <- acc %>%
    dplyr::filter(rule == r) %>%
    dplyr::mutate(seasontype = factor(seasontype, levels=c("summer", "allyear", "winter"))) %>%
    dplyr::mutate(eCost = as.numeric(eCost)) %>%
    dplyr::filter(!is.na(eCost)) %>%
    {.}
  if (nrow(toplot) == 0) {
    next
  }
  toplot %>%
    dplyr::select(building, rule, seasontype, eCost, est.eCost.allday, est.eCost.consider.occhour) %>%
    na.omit() %>%
    tidyr::gather(status, ecost, eCost:est.eCost.consider.occhour) %>%
    dplyr::group_by(building, rule, seasontype, status) %>%
    dplyr::summarise(total.ecost = sum(ecost)) %>%
    ## dplyr::summarise(mean.ecost = mean(ecost)) %>%
    dplyr::ungroup() %>%
    ## tidyr::spread(status, total.ecost) %>%
    ggplot2::ggplot(ggplot2::aes(x=building, y=total.ecost, fill=seasontype)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::facet_grid(status ~ seasontype) +
    ggplot2::labs("Total eCost for all buildings",
                  subtitle = r) +
    ggplot2::scale_fill_manual(values=g.cols, drop=F) +
    ggplot2::theme(legend.position="bottom",
                   axis.text.x=ggplot2::element_text(angle=90, size=5))
    ggplot2::ggsave(sprintf("../plots/cmp_ecost_est_vs_report_chopped/%s.png", r), width=8, height=5)
}

  ## tidyr::spread(status, mean.ecost) %>%
  ## for BTU to dollar
  ## dplyr::mutate(ratio.allday = est.eCost.allday / eCost * 0.11 / 3412.14,
  ##               ratio.consider.occhour = est.eCost.consider.occhour / eCost * 0.11 / 3412.14) %>%
  ## for kWh to dollar
  ## dplyr::mutate(ratio.allday = est.eCost.allday * 0.11 / eCost,
  ##               ratio.consider.occhour = est.eCost.consider.occhour * 0.11 / eCost) %>%
  readr::write_csv("ratio_of_median_est_vs_report_chopped.csv")
## readr::write_csv("ratio_of_mean_est_vs_report_chopped.csv")
## readr::write_csv("ratio_of_mean_est_vs_report.csv")

toplot = readr::read_csv("ratio_of_mean_est_vs_report.csv") %>%
  dplyr::group_by(rule, seasontype) %>%
  ## filter for enough buildings
  dplyr::filter(n() > 15) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`eCost`, -est.eCost.allday, -est.eCost.consider.occhour) %>%
  tidyr::gather(status, ratio, ratio.allday:ratio.consider.occhour) %>%
  {.}

plotboundary = toplot %>%
  dplyr::filter(!(ratio %in% c(Inf, -Inf)),
                !is.na(ratio)) %>%
  dplyr::group_by(rule, seasontype, status) %>%
  dplyr::summarise(q1 = quantile(ratio, probs = c(0.25, 0.75))[1],
                   q3 = quantile(ratio, probs = c(0.25, 0.75))[2]) %>%
  dplyr::ungroup() %>%
  {.}

## lower = min(plotboundary$q1)
## upper = max(plotboundary$q3)

rules = toplot %>%
  dplyr::distinct(rule) %>%
  .$rule

for (r in rules) {
  boundary = plotboundary %>%
    dplyr::filter(rule==r) %>%
    {.}
  lower = min(boundary$q1)
  upper = max(1, max(boundary$q3))
  print(boundary$q1)
  print(boundary$q3)
  print(lower)
  print(upper)
  p <- toplot %>%
    dplyr::filter(rule==r) %>%
    ggplot2::ggplot(ggplot2::aes(x=seasontype, y=ratio, fill=status)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(. ~ status) +
    ggplot2::coord_cartesian(ylim=c(lower, upper)) +
    ggplot2::labs(title="Compare estimate and reported eCost (some outliers not shown)", subtitle=r) +
    ggplot2::geom_hline(yintercept=1, linetype="dashed",
               color = "red", size=1) +
    ggplot2::scale_fill_brewer(palette="Set3") +
    ggplot2::theme(legend.position = "bottom")
  print(p)
  ggplot2::ggsave(sprintf("../plots/cmp_ecost_est_vs_report/%s.png", r), width=8, height=5)
}

## for (r in allrules[1:1]) {
##   p <-
##     acc %>%
##     dplyr::filter(rule == r) %>%
##     dplyr::mutate(eCost = as.numeric(eCost)) %>%
##     dplyr::select(rule, seasontype, eCost, est.eCost.allday, est.eCost.consider.occhour) %>%
##     tidyr::gather(status, ecost, eCost:est.eCost.consider.occhour) %>%
##     dplyr::mutate_at(vars(status), recode, "est.eCost.allday"="estimate", "est.eCost.consider.occhour"="estimate\nconsider occupancy") %>%
##     ggplot2::ggplot(ggplot2::aes(x=status, y=ecost, fill=seasontype)) +
##     ggplot2::geom_boxplot() +
##     ggplot2::facet_grid(seasontype ~ .) +
##     ggplot2::ggtitle(r) +
##     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
##   print(p)
## }


##   readr::write_csv("compare_with_their_ecost.csv")

##   nrow()

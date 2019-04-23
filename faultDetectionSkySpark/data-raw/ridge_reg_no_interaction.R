library("dplyr")
library("ggplot2")
library("DBI")

energytype = "kWh Del Int"
## restrict to summer
time.min.str="2018-06-01"
time.max.str="2018-09-01"

## read in the data from file

for (b in gsalink_buildings) {
  df =
    readr::read_csv(sprintf("building_rule_energy_weather/%s_%s_%s_2018.csv",
                            b, r, energytype)) %>%
    dplyr::rename(value.agg=num.component.has.warning)

  head(df)

  ## compute counterfactual
  dfcmp = df %>%
    dplyr::group_by(group, is.occupied) %>%
    dplyr::do({
            dfnospark = .[.$value.agg==0,]
            dfwithspark = .[.$value.agg>0,]
            y = dfnospark[[energytype]]
            x = dfnospark[["F"]]
            model.no.spark = lm(y ~ x)
            print(summary(model.no.spark))
            new=data.frame(x=dfwithspark$F)
            ## counterfactual
            yhat = predict(model.no.spark, newdata = new)
            y = dfwithspark[[energytype]]
            x = dfwithspark[["F"]]
            model.with.spark = lm(y ~ x)
            print(summary(model.with.spark))
            yfitted = fitted.values(model.with.spark)
            asdf <- data.frame(F=x, modeled.with.spark=yfitted, modeled.no.spark=yhat)
            asdf
          }) %>%
    ## dplyr::distinct(group, F, modeled.with.spark, modeled.no.spark) %>%
    {.}

  dfcmp %>%
    tidyr::gather(status, !!rlang::sym(energytype), modeled.with.spark:modeled.no.spark) %>%
    ggplot2::ggplot(aes(x=F, y=(!!rlang::sym(energytype)), colour=status)) +
    ggplot2::geom_point(size=0.3) +
    ggplot2::ggtitle(label=sprintf("%s %s, %s", b, energytype, r),
                    subtitle = sprintf("%s -- %s", time.min, time.max)) +
    ggplot2::facet_wrap(group~is.occupied) +
    ggplot2::ylab(sprintf("%s per minute", energytype)) +
    ggplot2::xlab("outdoor temperature (F)") +
    ggplot2::theme()

  df1 = dfcmp %>%
    dplyr::mutate(with.minus.without = modeled.with.spark - modeled.no.spark) %>%
    dplyr::group_by(group, is.occupied) %>%
    dplyr::summarise(with.minus.without.dollar = sum(with.minus.without) * 0.1) %>%
    dplyr::rename(costdiff = with.minus.without.dollar) %>%
    dplyr::mutate(status="model estimate") %>%
    {.}

  df2 <- readr::read_csv(sprintf("rule_ecost_minute/%s_%s_%s_2018.csv",
                          b, r, energytype),
                  col_types = readr::cols(ecost.per.min = readr::col_double())) %>%
    dplyr::group_by(group, is.occupied) %>%
    dplyr::summarise(costdiff = sum(ecost.per.min)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(status="their estimate") %>%
    {.}

  df1 %>%
    dplyr::bind_rows(df2) %>%
    tidyr::spread(status, costdiff) %>%
    readr::write_csv(sprintf("cmp/%s_%s_%s_2018.csv",
                            b, r, energytype))
}

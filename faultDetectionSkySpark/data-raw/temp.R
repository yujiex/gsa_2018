library("dplyr")

devtools::load_all("~/Dropbox/gsa_2017/db.interface/")

ecm = db.interface::read_table_from_db(dbname="all", tablename="EUAS_ecm") %>%
  tibble::as.tibble() %>%
  {.}

ecm %>%
  head()

df = data.frame(component=c(rep("A", 3), rep("B", 1)), start=c(1, 2, 4, 1), end=c(2, 3, 6, 5))

dfleft = data.frame(time=1:7)

dfleft

df %>%
  dplyr::filter(component == "A") %>%
  tidyr::gather(timetype, time, start:end) %>%
  dplyr::mutate(value=ifelse(timetype=="start", 1, 0)) %>%
  dplyr::right_join(dfleft) %>%
  dplyr::arrange(time, timetype) %>%
  tidyr::fill(component, value) %>%
  dplyr::group_by(time) %>%
  dplyr::slice(n()) %>%
  dplyr::ungroup()
  print()

df %>%
  dplyr::group_by(component) %>%
  dplyr::do({
    df.group = .
    asdf <- df.group %>%
            tidyr::gather(timetype, time, start:end) %>%
            dplyr::mutate(value=ifelse(timetype=="start", 1, 0)) %>%
            dplyr::right_join(dfleft) %>%
            dplyr::arrange(time, timetype) %>%
            tidyr::fill(component, value) %>%
            dplyr::group_by(time) %>%
            dplyr::slice(n()) %>%
            dplyr::ungroup() %>%
           {.}
           asdf
  }) %>%
print()





component.group.interval.sums =
    dplyr::bind_rows(
               test.tbl %>>% dplyr::mutate(time=start, delta=1),
               test.tbl %>>% dplyr::mutate(time=end, delta=-1)
           ) %>>%
    dplyr::select(-start, -end) %>>%
    dplyr::group_by(component.group, time) %>>%
    dplyr::arrange(time) %>>%
    dplyr::summarize(delta = sum(delta)) %>>%
    dplyr::ungroup() %>>%
    dplyr::group_by(component.group) %>>%
    dplyr::do({
        tibble::tibble(
                    component.group=.$component.group[[1L]],
                    time=c(
                        time.min, # beginning of time
                        .$time - time.epsilon, # before delta
                        .$time + time.epsilon, # after delta
                        time.max # end of time
                    ),
                    value=c(
                        0, # assume 0 at beginning of time
                        cumsum(.$delta)-.$delta, # value before delta
                        cumsum(.$delta), # value after delta
                        0 # assume 0 at end of time
                    )
                )
    }) %>>%
    dplyr::ungroup() %>>%
    ## For visualization: jitter values based on component.group so component.group time
    ## series don't overlap, e.g., when values are simultaneously 0
    dplyr::mutate(value=value # ...
                  ## number component.groups 1..n, shift component.group i by i*component.group.jitter.scale:
                  + as.integer(as.factor(component.group))*component.group.jitter.scale
                  ## shift so that jitter amounts are centered (in some sense)
                  ## around 0:
                  - (length(unique(component.group))+1)/2
                  ) %>>%
    {.}

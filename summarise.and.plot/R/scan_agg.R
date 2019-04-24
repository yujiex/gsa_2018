#'@importFrom magrittr %>%
#'@importFrom pipeR %>>%
NULL

agg_interval <- function(df, start="start", end="end", group=NULL, value=NULL,
                         time.min=NULL, time.max=NULL,
                         time.epsilon = 0.1) {
  if (is.null(time.min)) {
    time.min=min(df[[start]])
  }
  if (is.null(time.max)) {
    time.max=max(df[[end]])
  }
  group.interval.sums =
    dplyr::bind_rows(
             df %>>% dplyr::mutate(time=!!rlang::sym(start), delta=1),
             df %>>% dplyr::mutate(time=!!rlang::sym(end), delta=-1)
           ) %>>%
    {.}
  if (!is.null(value)) {
    group.interval.sums <- group.interval.sums %>%
      dplyr::mutate(delta = delta * !!rlang::sym(value))
  }
  if (is.null(group)) {
    group = "dummy.group"
    group.interval.sums <- group.interval.sums %>%
      dplyr::mutate(dummy.group=1)
  }
  group.interval.sums <- group.interval.sums %>%
    dplyr::select(-dplyr::one_of(c(start, end))) %>>%
    dplyr::group_by_at(vars(dplyr::one_of(c(group, "time")))) %>>%
    dplyr::arrange(time) %>>%
    dplyr::summarize(delta = sum(delta)) %>>%
    dplyr::ungroup() %>>%
    dplyr::group_by_at(vars(dplyr::one_of(group))) %>>%
    dplyr::do({
      tibble::tibble(
                ## when there are multiple grouping variables, users are responsible to
                ## combine them into one column, e.g. using unite
                !!rlang::sym(group):=.[[group]][[1L]],
                time=c(
                  time.min, # beginning of time
                  .$time - time.epsilon, # before delta
                  .$time + time.epsilon, # after delta
                  time.max # end of time
                ),
                value.agg=c(
                  0, # assume 0 at beginning of time
                  cumsum(.$delta)-.$delta, # value before delta
                  cumsum(.$delta), # value after delta
                  0 # assume 0 at end of time
                ),
                row.kind=c(
                  "beginning of time",
                  rep("pre-delta", nrow(.)),
                  rep("post-delta", nrow(.)),
                  "end of time"
                )
              )
    }) %>>%
    dplyr::ungroup() %>>%
    {.}
  group.interval.sums
}

#' Scan aggregation of count or non-negative value of events
#'
#' This function plots the total number or the sum of some non-negative feature
#' (e.g. cost) where the input data has event start, end, time. If a value
#' column is provided, it computes the sum of the value column.
#' @param df required, a data frame to plot. It must contain a column of start
#'   and end time of events, by default, the name of the start time column is
#'   assumed to be "start", and the name of the end time column is assumed to be
#'   "end"
#' @param start optional, the start time of the event, default to "start"
#' @param end optional, the end time of the event, default to "end"
#' @param group optional, if group variables are selected, the aggregation will
#'   be within each group. To select one group by column, just input its column
#'   name string, to select multiple columns, do c(column_1_string,
#'   column_2_string)
#' @param value optional, the column to scan sum over. If it is not
#'   supplied, a count of the number of events in action will be computed
#' @param time.min optional, the starting point of the plot, its value should be
#'   less than or equal to all timestamps in the data frame
#' @param time.max optional, the ending point of the plot, its value should be
#'   less than or equal to all timestamps in the data frame
#' @param time.epsilon optional, the x difference of the turning point of
#'   events. It should be small so that the plotted lines look vertical
#' @param group.jitter.scale optional, the maximum jitter for y value of each group (the plotted lines)
#' @keywords scan aggregation
#' @export
#' @examples
## with one grouping variable
## summarise.and.plot::scan_agg(df=df, start="startPosix", end="endPosix",
##                              ## group=NULL, value="eCost/hour"
##                              group="rule.component.group", value="eCost/hour")
## with no grouping variable
## summarise.and.plot::scan_agg(df=df, start="startPosix", end="endPosix",
##                              group=NULL, value="eCost/hour")
scan_agg <- function(df, start="start", end="end", group=NULL, value=NULL,
                     time.min=NULL, time.max=NULL,
                     time.epsilon = 0.1,
                     group.jitter.scale = 0.01, title=NULL,
                     legend.ncol=NULL, legend.fontsize=NULL,
                     legend.wrap=40, remove.legend=FALSE
                     ) {
  if (is.null(time.min)) {
    time.min=min(df[[start]])
  }
  if (is.null(time.max)) {
    time.max=max(df[[end]])
  }
  if (is.null(group)) {
    group = "dummy.group"
  }
  group.interval.sums = agg_interval(df, start, end, group, value, time.min, time.max, time.epsilon)
  group.interval.sums <- group.interval.sums %>%
    dplyr::rowwise() %>%
    dplyr::mutate(!!rlang::sym(group):=paste0(strwrap(!!rlang::sym(group), legend.wrap), collapse = "\n")) %>%
    dplyr::ungroup() %>%
    {.}
  ## For visualization: jitter values based on group so group time
  ## series don't overlap, e.g., when values are simultaneously 0
  print(unique(group.interval.sums$value.agg))
  group.interval.sums <- group.interval.sums %>%
    dplyr::mutate(value.agg=value.agg # ...
                  ## number groups 1..n, shift group i by i*group.jitter.scale:
                  + as.integer(as.factor(!!rlang::sym(group)))*group.jitter.scale
                  ## shift so that jitter amounts are centered (in some sense)
                  ## around 0:
                  - (length(unique(!!rlang::sym(group)))+1)/2*group.jitter.scale
                  ) %>>%
    {.}
  print(unique(group.interval.sums$value.agg))
  if (length(group) == 1) {
    p <- group.interval.sums %>>%
      ggplot2::ggplot(ggplot2::aes_string(x="time",y="value.agg",colour=group))
  } else if (length(group) > 1) {
    p <- group.interval.sums %>>%
      ggplot2::ggplot(ggplot2::aes_string(x="time",y="value.agg",colour=paste(group)))
  } else {
    p <- group.interval.sums %>>%
      ggplot2::ggplot(ggplot2::aes_string("time","value.agg"))
  }
  p <- p +
    ggplot2::geom_line() +
    ## fixme: don't know why if I add linesize here, it will give error, but not reoccuring ggplot2::geom_line(size=linesize) +
    ## ggplot2::geom_point() +
    ggplot2::xlim(c(time.min, time.max))
  if (!is.null(title)) {
    p <- p +
      ggplot2::ggtitle(title)
  }
  if (!is.null(value)) {
    p <- p +
      ggplot2::ylab(value)
  }
  p <- p +
    ggplot2::guides(col = ggplot2::guide_legend(ncol = legend.ncol,
                                                label.theme = ggplot2::element_text(size=legend.fontsize)))
  if (remove.legend) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  } else {
    p <- p +
      ggplot2::theme(legend.position = "bottom")
  }
  print(p)
}


#' Plot the start and end time of events as line segments, with no grouping
#'
#' @param df required, a data frame to plot. It must contain a column of start
#'   and end time of events, by default, the name of the start time column is
#'   assumed to be "start", and the name of the end time column is assumed to be
#'   "end"
#' @param event required, the event column
#' @param start optional, the start time of the event, default to "start"
#' @param end optional, the end time of the event, default to "end"
#' @param value optional, the column to scan sum over. If it is not supplied,
#'   each event will be plotted on a separate height, where the possible heights
#'   are 1 to the number of unique events
#' @param time.min optional, x plot range left limit
#' @param time.max optional, x plot range right limit
#' @param group.jitter optional, the maximum jitter for y value of each group
#'   (the plotted lines)
#' @keywords plot start end time
#' @export
#' @examples
plot_event_start_end <- function(df, event, start="start", end="end",
                                 value=NULL, time.min=NULL, time.max=NULL,
                                 group.jitter.scale=0.01, title=NULL,
                                 legend.ncol=NULL, legend.fontsize=NULL,
                                 legend.wrap=40) {
  if (is.null(value)) {
    df <- df %>%
      dplyr::mutate(value = as.integer(as.factor(!!rlang::sym(event))))
  } else {
    df <- df %>%
      dplyr::mutate(value = !!rlang::sym(value)) %>%
      dplyr::mutate(value = value + as.integer(as.factor(!!rlang::sym(event)))*group.jitter.scale
                    ## shift so that jitter amounts are centered (in some sense)
                    ## around 0:
                    - (length(unique(!!rlang::sym(event)))+1)/2*group.jitter.scale) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(!!rlang::sym(event):=paste0(strwrap(!!rlang::sym(event), legend.wrap), collapse = "\n")) %>%
      dplyr::ungroup() %>%
      {.}
  }
  p = df %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes_string(x=start, xend=end, y="value",
                                              yend="value",
                                              color=event), size=2)
  if (!is.null(title)) {
    p <- p +
      ggplot2::ggtitle(title)
  }
  p <- p +
    ggplot2::xlim(c(time.min, time.max)) +
    ggplot2::xlab("Time") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  if (!is.null(value)) {
    p <- p + ggplot2::ylab(value)
  }
  p <- p +
    ggplot2::guides(col = ggplot2::guide_legend(ncol = legend.ncol,
                                                label.theme = ggplot2::element_text(size=legend.fontsize)))
  print(p)
}

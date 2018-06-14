#' Image to tex
#'
#' This function compiles images in a data frame to a tex file, with sorting
#' @param df required, a data frame with the image identifiers, and a column named "score"
#' @param prefix
#' @param suffix
#' @param isDesc whether the sort is descending
#' @param topn optional, if supplied, only write out the top n records
#' @keywords image to tex
#' @export
#' @examples
#' polynomial_deg_2(y, x)
img2tex <- function(df, prefix, suffix, isDesc, outfilename, topn=16, botn=4) {
  print(head(df))
  df <- df %>%
    dplyr::mutate(`lines`=paste0(prefix, `id`, suffix)) %>%
    {.}
  print(head(df))
  if (isDesc) {
    df <- df %>%
      arrange(desc(`score`)) %>%
      {.}
  } else {
    df <- df %>%
      arrange(`score`) %>%
      {.}
  }
  print(head(df))
  if (!missing(topn)) {
    df <- rbind(head(df, n=topn), tail(df, n=botn))
  }
  df %>%
    dplyr::select(`lines`) %>%
    write.table(outfilename, sep="\n", row.names=FALSE, col.names = FALSE, quote = FALSE)
}

#' Generate lean tex file
#'
#' This function generates the include image part of lean image tex file
#' @param df required, a data frame with the image identifiers, and a column named "score"
#' @param prefix
#' @param suffix
#' @param isDesc whether the sort is descending
#' @param topn top n records to include
#' @param botn bottom n records to include
#' @keywords image to tex
#' @export
#' @examples
#' polynomial_deg_2(y, x)
generate_lean_tex <- function(plotType, region, topn, botn) {
  df = readr::read_csv(sprintf("csv_FY/%s_lean_score_region_%s.csv", plotType, region)) %>%
    dplyr::rename(`id`=`Building_Number`) %>%
    {.}
  if (plotType != "base") {
    df <- df %>%
      dplyr::filter(`score` != 0) %>%
      {.}
  }
  img2tex(df, prefix=sprintf("\\includegraphics[width = 0.24\\textwidth, keepaspectratio]{lean/%s_", plotType),
          suffix=".png}",
          isDesc=TRUE, outfilename=sprintf("region_report_img/%s_region_%s.tex", plotType, region),
          topn=topn, botn=botn)
}
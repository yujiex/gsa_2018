#' Image to tex
#'
#' This function compiles images in a data frame to a tex file, with sorting
#' @param df required, a data frame with the image identifiers (column called id), and a column named "score"
#' @param prefix required, usually the \includegraphics settings, prior to the image identifiers
#' @param suffix required, usually the .png
#' @param isDesc whether the sort is descending
#' @param topn optional, if supplied, only write out the top n records
#' @keywords image to tex
#' @export
#' @examples
#' polynomial_deg_2(y, x)
img2tex <- function(df, prefix, suffix, isDesc, outfilename, topn=0, botn=0) {
  print(head(df))
  df <- df %>%
    dplyr::mutate(`lines`=paste0(prefix, `id`, suffix)) %>%
    {.}
  print(head(df))
  if (isDesc) {
    df <- df %>%
      dplyr::arrange(desc(`score`)) %>%
      {.}
  } else {
    df <- df %>%
      dplyr::arrange(`score`) %>%
      {.}
  }
  print(head(df))
  df <- df %>%
    dplyr::select(`lines`) %>%
    {.}
  topData = head(df, n=topn)
  diff_top = topn - nrow(df)
  if (0 < topn && diff_top > 0) {
    topData <- rbind(topData, data.frame(`lines`=rep(paste0(prefix, "dummy.png}"), diff_top)))
  }
  botData = tail(df, n=botn)
  diff_bot = botn - nrow(df)
  if (0 < botn && diff_bot > 0) {
    botData <- rbind(botData, data.frame(`lines`=rep(paste0(prefix, "dummy.png}"), diff_bot)))
  }
  df <- rbind(topData, botData)
  df %>%
    write.table(outfilename, sep="\n", row.names=FALSE, col.names = FALSE, quote = FALSE)
}

#' Generate lean tex file
#'
#' This function generates the include image part of lean image tex file
#' @param df required, a data frame with the image identifiers, and a column named "score"
#' @param prefix required, usually the \includegraphics settings, prior to the image identifiers
#' @param suffix required, usually the .png
#' @param isDesc whether the sort is descending
#' @param category optional, filter for specific category
#' @param topn top n records to include
#' @param botn bottom n records to include
#' @param presuffix optional, the tail identifier of data source (source_heating_cooling, source_electric_gas)
#' @keywords image to tex
#' @export
#' @examples
#' presuffix="_source_heating_cooling"
#' generate_lean_tex(plotType="base", region=region, topn=20, botn=0, presuffix=presuffix)
generate_lean_tex <- function(plotType, region, topn, botn, category, presuffix="") {
  df = readr::read_csv(sprintf("csv_FY/%s_lean_score_region_%s%s.csv", plotType, region, presuffix)) %>%
    dplyr::rename(`id`=`Building_Number`) %>%
    {.}
  if (plotType != "base") {
    df <- df %>%
      dplyr::filter(`score` != 0) %>%
      {.}
  }
  categoryTag = ""
  if (!missing(category)) {
    df <- df %>%
      dplyr::filter(`Cat`==category) %>%
      {.}
    categoryTag = sprintf("_%s", category)
  }
  if (topn != 0) {
    outfile = sprintf("region_report_img/%s_region_%s%s_top%s.tex", plotType, region, categoryTag, topn)
    img2tex(df, prefix=sprintf("\\includegraphics[width = 0.24\\textwidth, keepaspectratio]{lean/%s_", plotType),
            suffix=paste0(presuffix, ".png}"),
            isDesc=TRUE, outfilename=outfile,
            topn=topn, botn=0)
  }
  if (botn != 0) {
    outfile = sprintf("region_report_img/%s_region_%s%s_bot%s.tex", plotType, region, categoryTag, botn)
    img2tex(df, prefix=sprintf("\\includegraphics[width = 0.24\\textwidth, keepaspectratio]{lean/%s_", plotType),
            suffix=paste0(presuffix, ".png}"),
            isDesc=TRUE, outfilename=outfile,
            topn=0, botn=botn)
  }
}

#' Generate side by side lean plot comparison
#'
#' This function generates a tex file to be included to display two sets of
#' plots in two columns. One set of plots have suffixes
#' "*source_electric_gas.png", in folder "lean/", the other set of plots ends
#' with "*source_heating_cooling", the rows are aligned by base_xxxxxxxx, where
#' "xxxxxxxx" is the building ID. When a building only has one plot, fill the
#' other missing one with a dummy blank plot
#' @param df required, a data frame with the image identifiers, and a column
#'   named "score"
#' @keywords image to tex
#' @export
#' @examples
#' polynomial_deg_2(y, x)
generate_building_by_building_cmp <- function() {
  files_1 = list.files(path="~/Dropbox/gsa_2017/region_report_img/lean/", pattern="*_electric_gas.png")
  files_prefix_1 = unlist(lapply(files_1, function (x) {gsub(x=x, "source_electric_gas.png", "")}))
  files_2 = list.files(path="~/Dropbox/gsa_2017/region_report_img/lean/", pattern="*_heating_cooling.png")
  files_prefix_2 = unlist(lapply(files_2, function (x) {gsub(x=x, "source_heating_cooling.png", "")}))
  df1 = data.frame(`id`=files_prefix_1, `notes`="source_electric_gas")
  df2 = data.frame(`id`=files_prefix_2, `notes`="source_heating_cooling")
  img_prefix="\\includegraphics[width = 0.48\\textwidth, keepaspectratio]{lean/"
  lines <- df1 %>%
    dplyr::full_join(df2, by="id") %>%
    dplyr::mutate(`name1`=ifelse(is.na(`notes.x`), paste0(img_prefix, "base_dummy"),
                                 paste0(img_prefix, `id`, `notes.x`, ".png}")),
                  `name2`=ifelse(is.na(`notes.y`), paste0(img_prefix, "base_dummy"),
                                 paste0(img_prefix, `id`, `notes.y`, ".png}"))) %>%
    dplyr::select(-`notes.x`, -`notes.y`, -`id`) %>%
    t() %>%
    as.matrix() %>%
    as.vector() %>%
    {.}
  outfilename = "region_5_cmp.tex"
  df = data.frame(`lines`=lines)
  print(head(df))
  df %>%
    write.table(outfilename, sep="\n", row.names=FALSE, col.names = FALSE, quote = FALSE)
  print("write to file")
}

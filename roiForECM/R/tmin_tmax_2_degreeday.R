#' Combine TMIN and TMAX file to degree days
#'
#' This function computes HDD and CDD based on TMIN and TMAX
#' @param b required, building id
#' @param path optional, directory to save downloaded data.
#' @param overwrite optional, whether to regenerate degreeday file
#' @keywords noaa average daily temperature
#' @export
#' @examples
#' getAvgTemp(building="UT0032ZZ", start_str="2012-01-05", end_str="2015-01-30")
tmin_tmax_2_degreeday <- function(b, path=NULL, overwrite=FALSE) {
  ddfile = sprintf("%sbuilding_HDDCDD/%s.feather", path, b)
  if ((!file.exists(ddfile)) || overwrite) {
    tmin_file = sprintf("%sbuilding_TMIN/compiled/%s_TMIN.feather", path, b)
    tmax_file = sprintf("%sbuilding_TMAX/compiled/%s_TMAX.feather", path, b)
    tmin_df = feather::read_feather(tmin_file) %>%
      dplyr::mutate(`varname`="TMIN") %>%
      dplyr::rename(`weighted`=`TMIN`) %>%
      {.}
    tmax_df = feather::read_feather(tmax_file) %>%
      dplyr::mutate(`varname`="TMAX") %>%
      dplyr::rename(`weighted`=`TMAX`) %>%
      {.}
    tmin_df %>%
      dplyr::bind_rows(tmax_df) %>%
      dplyr::arrange(`Date`, `varname`) %>%
      dplyr::group_by(`Date`, `varname`) %>%
      slice(n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(varname, weighted) %>%
      dplyr::mutate(`AVGMINMAX` = (`TMIN` + `TMAX`) / 2) %>%
      dplyr::mutate(`HDD` = ifelse(`AVGMINMAX` <= 65, 65 - `AVGMINMAX`, 0)) %>%
      dplyr::mutate(`CDD` = ifelse(`AVGMINMAX` > 65, `AVGMINMAX` - 65, 0)) %>%
      dplyr::mutate(`year`=format(Date, "%Y")) %>%
      dplyr::mutate(`month`=format(Date, "%m")) %>%
      dplyr::select(-`Date`) %>%
      dplyr::group_by(`year`, `month`) %>%
      dplyr::summarise(`HDD`=sum(`HDD`), `CDD`=sum(`CDD`)) %>%
      dplyr::ungroup() %>%
      feather::write_feather(ddfile)
  } else {
    print(sprintf("degreeday file already exists for %s", b))
  }
}

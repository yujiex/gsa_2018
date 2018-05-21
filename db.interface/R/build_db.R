#' Get all tables in a sqlite database with "dbname"
#'
#' This function returns a xx of tables in the database
#' @param filename required, the filename (including extensions) of the new source file
#' @param path optional, the path to the new source file, default is "input/FY/static_info/"
#' @param id_col optional, the name of the id column, default to "Building_Number"
#' @param type_col optional, the name of the column containing building type, default to "Building_Number"
#' @param overwrite optional, whether to overwrite the old EUAS_type table, default to FALSE
#' @keywords update sqlite table
#' @export
#' @examples
#' to be added
update_euas_type <- function(filename, path, id_col, type_col, sourcelabel, overwrite) {
  con <- connect("all")
  if ((!overwrite) || missing(overwrite)) {
    df_old =
      dbGetQuery(con, "SELECT * FROM EUAS_type") %>%
      as_data_frame() %>%
      {.}
  }
  df_new = read_file(filename, columns=c(id_col, type_col))
  df_new <- df_new %>%
    ## rename type_col to the standard building type column name
    dplyr::rename(`building_type` = type_col) %>%
    dplyr::mutate(`source`=filename) %>%
    {.}
  print(head(df_new))
}

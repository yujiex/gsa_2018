#' compute ROI for all buildings with action and cost data
#'
#' This function compute ROI for all buildings with action and cost data
#' @keywords roi
#' @export
#' @examples
#' roiForAll()
roiForAll <- function() {
  buildings = actionAndCost %>%
    dplyr::filter(!is.na(`Cost`)) %>%
    distinct(`Building_Number`) %>%
    .$`Building_Number`
  print(head(buildings))
  print(sprintf("number of cases to process: %s", length(buildings)))
  i = 123
  ## note the 60th, OR0045ZZ have no good stations
  for (b in buildings[i:length(buildings)]) {
    print(sprintf("----- processing building %s: %s -----", i, b))
    roiBuilding(building=b)
    i = i + 1
  }
}

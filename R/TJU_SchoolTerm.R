
#' @title TJU School Term
#' 
#' @description ..
#' 
#' @param x a \link[base]{Date} object
#' 
#' @returns 
#' Function [TJU_SchoolTerm()] returns a \link[base]{character} \link[base]{vector}
#' 
#' @examples 
#' c('2021-03-14', '2022-01-01', '2022-05-01') |>
#'  as.Date() |> 
#'  TJU_SchoolTerm()
#' 
#' @keywords internal
#' @importFrom lubridate year month
#' @export
TJU_SchoolTerm <- function(x) {
  x |>
    month() |>
    cut.default(
      breaks = c(1, 4, 7, 9, 12), include.lowest = TRUE, right = FALSE, 
      labels = c('Winter', 'Spring', 'Summer', 'Fall')
    ) |>
    as.character.factor() |> 
    paste(year(x))
}


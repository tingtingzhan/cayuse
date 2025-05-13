
#' @title TJU Fiscal Year
#' 
#' @description ..
#' 
#' @param x \link[base]{integer} scalar
#' 
#' @returns 
#' Function [TJU_Fiscal_Year()] returns a length-two \link[base]{Date} \link[base]{vector},
#' indicating the start (July 1 of the previous calendar year) and end date (June 30) of a fiscal year.
#' 
#' @references
#' \url{www.jefferson.edu/finance/finance-for-staff/transaction-processing-deadlines.html}
#' 
#' @examples 
#' TJU_Fiscal_Year(2022L)
#' 
#' @keywords internal
#' @export
TJU_Fiscal_Year <- function(x) {
  c(
    paste0(x-1, '-07-01') |> as.Date.character(format = '%Y-%m-%d'), 
    paste0(x, '-06-30') |> as.Date.character(format = '%Y-%m-%d')
  )
}


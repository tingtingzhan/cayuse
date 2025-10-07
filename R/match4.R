
setGeneric(name = 'match')
setGeneric(name = '%in%')
setOldClass(Classes = 'yearqtr')
setOldClass(Classes = 'yearmon')

#' @title Match \link[base]{Date} to \link[zoo]{yearmon} and \link[zoo]{yearqtr}
#' 
#' @param x \link[base]{Date}
#' 
#' @param table \link[zoo]{yearmon} or \link[zoo]{yearqtr}
#' 
#' @param nomatch,incomparables see function \link[base]{match}
#' 
#' @examples
#' match(x = as.Date('2025-10-24'), table = zoo::as.yearqtr('2025 Q4'))
#' stopifnot(as.Date('2025-10-24') %in% zoo::as.yearqtr('2025 Q4'))
#' 
#' match(x = as.Date('2025-10-24'), table = zoo::as.yearmon('2025-10'))
#' stopifnot(as.Date('2025-10-24') %in% zoo::as.yearmon('2025-10'))
#' @keywords internal
#' @name match4
#' @aliases match,Date,yearqtr-method
#' @export
setMethod(
  f = match, 
  signature = c(x = 'Date', table = 'yearqtr'), 
  definition = \(x, table, nomatch = NA_integer_, incomparables = NULL) {
    match(
      x = x, 
      table = allDates.yearqtr(table), 
      nomatch = nomatch, incomparables = incomparables
    )
  }
)

#' @rdname match4
#' @aliases %in%,Date,yearqtr-method
#' @export
setMethod(
  f = '%in%',
  signature = c(x = 'Date', table = 'yearqtr'), 
  definition = \(x, table) {
    match(x = x, table = table, nomatch = 0L) > 0L
  }
) # must do this, R 4.5.1


#' @rdname match4
#' @aliases match,Date,yearmon-method
#' @export
setMethod(
  f = match, 
  signature = c(x = 'Date', table = 'yearmon'), 
  definition = \(x, table, nomatch = NA_integer_, incomparables = NULL) {
    match(
      x = x, 
      table = allDates.yearmon(table), 
      nomatch = nomatch, incomparables = incomparables
    )
  }
)

#' @rdname match4
#' @aliases %in%,Date,yearmon-method
#' @export
setMethod(
  f = '%in%',
  signature = c(x = 'Date', table = 'yearmon'), 
  definition = \(x, table) {
    match(x = x, table = table, nomatch = 0L) > 0L
  }
) # must do this, R 4.5.1
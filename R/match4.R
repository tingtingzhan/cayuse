
setGeneric(name = 'match')
setGeneric(name = '%in%')
setOldClass(Classes = 'yearqtr')
setOldClass(Classes = 'yearmon')

if (FALSE) {
  match(x = as.Date('2025-10-24'), table = zoo::as.yearqtr('2025 Q4'))
  as.Date('2025-10-24') %in% zoo::as.yearqtr('2025 Q4')
  
  match(x = as.Date('2025-10-24'), table = zoo::as.yearmon('2025-10'))
  as.Date('2025-10-24') %in% zoo::as.yearmon('2025-10')
}



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


setMethod(
  f = '%in%',
  signature = c(x = 'Date', table = 'yearqtr'), 
  definition = \(x, table) {
    match(x = x, table = table, nomatch = 0L) > 0L
  }
) # must do this, R 4.5.1


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

setMethod(
  f = '%in%',
  signature = c(x = 'Date', table = 'yearmon'), 
  definition = \(x, table) {
    match(x = x, table = table, nomatch = 0L) > 0L
  }
) # must do this, R 4.5.1
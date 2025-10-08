
#' @title Workdays
#' 
#' @description
#' To summarize the number of workdays, weekends, holidays and vacations in a given time-span 
#' (e.g., a month or a quarter of a year).
#' 
#' @param x \link[base]{character} scalar or \link[base]{vector} (e.g.,
#' `'2021-01'` for January 2021,
#' `'2021 Q1'` for 2021 Q1 (January to March)), or
#' \link[base]{integer} scalar or \link[base]{vector} (e.g., `2021L` for year 2021);
#' The time-span to be summarized.
#' Objects of classes \link[zoo]{yearqtr} and \link[zoo]{yearmon} are also accepted.
#' 
#' @param holiday \link[base]{character} \link[base]{vector}
#' 
#' @param vacation \link[base]{Date} \link[base]{vector}, vacation days
#' 
#' @details 
#' 
#' Function [workday()] summarizes the workdays, weekends,
#' Jefferson paid holidays 
#' (New Year’s Day, Martin Luther King, Jr. Day, Memorial Day, Fourth of July, Labor Day, Thanksgiving and Christmas)
#' and your vacation (e.g., sick, personal, etc.) days (if any),
#' in a given time-span.
#' 
#' Per Jefferson policy (source needed), 
#' if a holiday is on Saturday, then the preceding Friday is considered to be a weekend day.
#' If a holiday is on Sunday, then the following Monday is considered to be a weekend day.
#' 
#' @returns 
#' Function [workday()] returns a \link[base]{factor}.
#' 
#' @examples
#' '2025 Q1' |> zoo::as.yearqtr() |> workday() |> table()
#' 
#' '2025 Q2' |> 
#'  zoo::as.yearqtr() |>
#'  workday(vacation = seq.Date(
#'   from = as.Date('2025-04-10'), to = as.Date('2025-04-24'), by = 1
#'  )) |>
#'  table()
#'  
#' 2021L |> workday() |> table()
#' 
#' tryCatch(c('2019-10', '2019-12') |> zoo::as.yearmon() |> workday(), error = identity)
#' c('2019-10' |> zoo::as.yearmon() |> workday(), 
#'   '2019-12' |> zoo::as.yearmon() |> workday()) |> table() # work-around
#' 
#' @keywords internal
#' @importFrom lubridate year
#' @importFrom timeDate holiday USNewYearsDay USMLKingsBirthday USMemorialDay USIndependenceDay USLaborDay USThanksgivingDay USChristmasDay
#' @export
workday <- function(
    x, 
    holiday = c('USNewYearsDay', 'USMLKingsBirthday', 'USMemorialDay', 'USIndependenceDay', 'USLaborDay', 'USThanksgivingDay', 'USChristmasDay'), # https://hr.jefferson.edu/benefits-compensation/paid-time-off.html
    vacation
) {
  
  if (!length(nx <- length(x_dt <- allDates(x)))) return(invisible()) # use S3
  if (!all(diff.default(x_dt) == 1L)) stop('Input must be a consecutive time period')
  
  # add 1-day before and after, to deal with 'weekend & holiday' situation
  dt_ext <- c(x_dt[1L] - 1L, x_dt, x_dt[nx] + 1L)
  
  wkd <- format.Date(dt_ext, format = '%a') # ?base::weekdays.Date
  id_holiday <- dt_ext %in% holiday(year = unique.default(year(dt_ext)), Holiday = holiday) # my S4 `%in%` !!!
  id_weekend <- wkd %in% c('Sat', 'Sun')
  
  if (any(id_holiday & id_weekend)) {
    # holiday on weekend; Jefferson makes the closest weekday as weekend
    
    if (length(hSat <- setdiff(which(id_holiday & (wkd == 'Sat')), y = 1L))) {
      # holiday on Saturday
      id_weekend[hSat] <- FALSE # Saturday no longer considered as weekend; consider as holiday
      id_weekend[hSat - 1L] <- TRUE # previous (auxiliary) Friday considered as weekend
      # dont care when first day is Sunday and before-auxiliary Saturday is holiday or not
      # Takes care when last day is Friday and after-auxiliary Saturday is a holiday
    }
    
    if (length(wch_Sun <- setdiff(which(id_holiday & (wkd == 'Sun')), y = nx+2L))) {
      # holiday on Sunday
      id_weekend[wch_Sun] <- FALSE # Sunday no longer considered as weekend; consider as holiday
      id_weekend[wch_Sun + 1L] <- TRUE # next (auxiliary) Monday considered as weekend
      # dont care when last day is Saturday and after-auxiliary Sunday is holiday or not
      # Takes care when first day is Monday and before-auxiliary Sunday is a holiday
    }
    
  }
  
  id_holiday <- id_holiday[-c(1L, nx+2L)]
  id_weekend <- id_weekend[-c(1L, nx+2L)]
  if (any(id_holiday & id_weekend)) stop('should have been removed')
  
  out <- rep(1L, times = nx) # default: weekday
  out[id_weekend] <- 2L # weekend
  out[id_holiday] <- 3L # holiday
  
  if (!missing(vacation)) {
    if (!inherits(vacation, what = 'Date')) stop('`vacation` must be Date object')
    if (any(vholiday <- vacation %in% x_dt[id_holiday])) {
      vacation[vholiday] |> 
        col_magenta() |> style_bold() |>
        paste(collapse = ', ') |>
        sprintf(fmt = 'Vacation days %s are holidays.') |>
        message()
      vacation <- vacation[!vholiday]
    }
    if (any(vweekend <- vacation %in% x_dt[id_weekend])) {
      vacation[vweekend] |>
        col_cyan() |> style_bold() |>
        paste(collapse = ', ') |>
        sprintf(fmt = 'Vacation days %s are weekends.') |>
        message()
      vacation <- vacation[!vweekend]
    }
    #if (any(vout <- !(vacation %in% x_dt))) {
    #  message('Vacation day(s) ', sQuote(vacation[vout]), ' are out of the timespan.')
    #  vacation <- vacation[!vout]
    #} # this does not change the result :)
    out[x_dt %in% vacation] <- 4L # vacation
  } # else do nothing
  
  ret <- structure(
    out,
    class = 'factor',
    levels = c('Workday', 'Weekend', 'Holiday', 'Vacation')
  )
  factor(ret) # remove zero-count
}




# Objects \link[zoo]{yearqtr} and \link[zoo]{yearmon} are type-double.

# dont forget
# base::month.abb
# base::month.name

#' @title All \link[base]{Date}s in a Time Interval
#' 
#' @description
#' Find all \link[base]{Date}s in a time interval.
#' 
#' @param x R objects, such as
#' \describe{
#' \item{\link[base]{integer}}{
#' year, e.g., `x = 2020L` returns all \link[base]{Date}s from 2020-01-01 to 2020-12-31}
#' \item{\link[zoo]{yearmon}}{
#' year-month object from package \CRANpkg{zoo}}
#' \item{\link[zoo]{yearqtr}}{
#' year-quarter object from package \CRANpkg{zoo}}
#' }
#' 
#' @details
#' Function [allDates()] returns all \link[base]{Date}s in a given time interval.
#' 
#' @returns 
#' Function [allDates()] returns a \link[base]{Date} \link[base]{vector}.
#' 
#' @keywords internal
#' @name allDates
#' @export
allDates <- function(x) {
  if (!length(x)) return(invisible())
  if (anyNA(x)) stop('does not allow NA input')
  x <- unique(x) # ?base::unique.default ?zoo:::unique.yearmon ?zoo:::unique.yearqtr
  if (inherits(x, what = 'Date')) return(x)
  UseMethod(generic = 'allDates')
}



#' @rdname allDates
#' @examples
#' 2025L |>
#'   allDates()
#' @export allDates.integer
#' @export
allDates.integer <- function(x) { # `x` considered as year!
  x |>
    lapply(FUN = \(i) {
      i |> 
        paste0(c('-01-01', '-12-31')) |> 
        as.Date.character(format = '%Y-%m-%d') |> 
        as.list() |> 
        do.call(what = seq.Date, args = _)
    }) |> 
    do.call(what = c.Date, args = _)
}


#' @rdname allDates
#' @examples
#' zoo::as.yearmon('2025-1') |>
#'  allDates()
#' @importFrom zoo as.Date.yearmon
#' @export allDates.yearmon
#' @export
allDates.yearmon <- function(x) {
  x |>
    lapply(FUN = \(i) {
      i |>
        as.Date.yearmon(frac = c(from = 0, to = 1)) |> 
        as.list() |> 
        do.call(what = seq.Date, args = _)
    }) |>
    do.call(what = c.Date, args = _)
}

#' @rdname allDates
#' @examples
#' zoo::as.yearqtr('2025 Q2') |>
#'  allDates()
#' @importFrom zoo as.Date.yearqtr
#' @export allDates.yearqtr
#' @export
allDates.yearqtr <- function(x) {
  x |>
    lapply(FUN = \(i) {
      i |>
        as.Date.yearqtr(frac = c(from = 0, to = 1)) |> 
        as.list() |> 
        do.call(what = seq.Date, args = _)
    }) |> 
    do.call(what = c.Date, args = _)
}







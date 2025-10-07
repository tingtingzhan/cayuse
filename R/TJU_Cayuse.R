


#' @title Award & Effort from Cayuse 
#' 
#' @description
#' Print out grant and effort from Cayuse.
#' 
#' @param path \link[base]{character} scalar, directory of downloaded award `.csv` file.
#' Default is the download directory `'~/Downloads'`
#' 
#' @param fiscal.year \link[base]{integer} scalar
#' 
#' @returns ..
#' 
#' @details 
#' \itemize{
#' \item {go to `https://jefferson.cayuse424.com/sp/index.cfm`}
#' \item {My Proposals -> Submitted Proposals. 
#'   Lower-right corner of screen, 'Export to CSV'.
#'   Downloaded file has name pattern `'^proposals_.*\\.csv'`}
#' \item {My Awards -> Awards (*not* 'Active Projects').
#'   Lower-right corner of screen, 'Export to CSV'.
#'   Downloaded file has name pattern `'^Awards_.*\\.csv'`}
#' \item {My Awards -> Awards.  Click into each project, under 'People' tab to find my 
#'   'Sponsored Effort'}
#' }
#' 
#' Function [viewAward] aggregates grant over different period 
#' (e.g. from Axx-xx-001, Axx-xx-002, Axx-xx-003 to Axx-xx).
#' Then we need to manually added in our 'Sponsored Effort' in the returned `.csv` file.
#' 
#' @examples 
#' if (FALSE) {
#' viewAward()
#' viewProposal()
#' }
#' 
#' @name TJU_Cayuse
#' @importFrom lubridate year
#' @importFrom utils read.csv
#' @importFrom sideway sideway
#' @export
viewAward <- function(
    path = '~/Downloads', 
    fiscal.year = year(Sys.Date())
) {
  
  fls <- list.files(path = path, pattern = '^Awards_.*\\.csv$', full.names = TRUE)
  if (!length(fls)) stop('Awards file not downloaded?')
  fl <- sort.int(fls, decreasing = TRUE)[1L]
  message('\u261e ', fl |> basename() |> col_yellow())
  
  awards <- read.csv(file = fl, header = TRUE) |>
    # subset.data.frame(subset = startsWith(Status, prefix = 'Funded')) # I dont understand this yet
    subset.data.frame(subset = !nzchar(Flags) & !is.na(Award.End.Date)) |>
    within.data.frame(expr = {
      Admin.Unit <- # not relavent
        Account.Numbers <- # not relavent
        Status <- # um..
        Flags <- # used
        NULL
      Lead.PI <- gsub(' AOI$', replacement = '', x = Lead.PI)
      Award.Amount <- gsub('^\\$|,', replacement = '', x = Award.Amount) |> as.double()
      Award.No. <- Award.No. |>
        strsplit(split = '-') |> 
        vapply(FUN = \(i) paste(i[1:2], collapse = '-'), FUN.VALUE = '')
      Award.Notice.Received <- Award.Notice.Received |> as.Date.character(format = '%m/%d/%Y')
      Award.Begin.Date <- Award.Begin.Date |> as.Date.character(format = '%m/%d/%Y')
      Award.End.Date <- Award.End.Date |> as.Date.character(format = '%m/%d/%Y')
    }) |>
    subset.data.frame(subset = (Award.Amount > 0)) |>
    split.data.frame(f = ~ Award.No.) |>
    lapply(FUN = aggregate_award_) |>
    do.call(what = rbind.data.frame, args = _) |> 
    within.data.frame(expr = {
      Status2 <- Award.End.Date |> 
        as.double() |> 
        cut.default(
          breaks = c(-Inf, as.double(TJU_Fiscal_Year(fiscal.year)), Inf), 
          labels = c(sprintf('Ends before FY%d', fiscal.year), sprintf('Ends in FY%d', fiscal.year), 'Ongoing'),
          right = TRUE, include.lowest = TRUE, ordered_result = TRUE)
    }) |>
    sort_by.data.frame(y = ~ Status2 + Award.End.Date, decreasing = TRUE)
  
  awards[c(
    'Award.No.', 'Status2',
    'Project.Title', 'Lead.PI', 'Sponsor', 'Award.Amount', 'Time.Period', 'Award.Period'
  )] |>
    sideway()
  
  # message('\u21ac Fill in `Effort` by clicking into each project under \'Active Projects\'')
  return(invisible(awards))
  
}



aggregate_award_ <- function(x) {
  x |> 
    sort_by.data.frame(y = ~ Award.End.Date, decreasing = TRUE) |>
    with(expr = {
      if (!all(duplicated(Project.Title)[-1L])) stop('`Project.Title` not same')
      first_begin <- min(Award.Begin.Date, na.rm = TRUE)
      last_end <- max(Award.End.Date, na.rm = TRUE)
      Award.Period <- paste0(
        Award.Begin.Date, ' ~ ', Award.End.Date, ', ', 
        '$', formatC(Award.Amount, big.mark = ',', format = 'f', digits = 2L), collapse = '\n')
      return(data.frame(
        Award.No. = Award.No.[1L], 
        Project.Title = trimws(Project.Title[1L]), 
        Lead.PI = paste(unique(Lead.PI), collapse = ', '), 
        Sponsor = Sponsor[1L],
        Award.Amount = paste0('$', formatC(sum(Award.Amount, na.rm = TRUE), big.mark = ',', format = 'f', digits = 0L)),
        #Award.Notice.Received = min(Award.Notice.Received, na.rm = TRUE),
        Time.Period = paste(format.Date(first_begin, format = '%b %d, %Y'), '-', format.Date(last_end, format = '%b %d, %Y')),
        Award.End.Date = last_end,
        Award.Period = Award.Period
      ))
    })
}




#' @rdname TJU_Cayuse
#' @importFrom sideway sideway
#' @export
viewProposal <- function(
    path = '~/Downloads', 
    fiscal.year = year(Sys.Date())
) {
  
  proposal_csv_ <- list.files(path = path, pattern = '^proposals_.*\\.csv$', full.names = TRUE)
  if (!length(proposal_csv_)) stop('Proposal file not downloaded?')
  
  proposal_csv <- sort.int(proposal_csv_, decreasing = TRUE)[1L]
  message('\u261e ', proposal_csv |> basename() |> col_yellow())
  
  proposal <- proposal_csv |>
    read.csv(header = TRUE) |>
    within.data.frame(expr = {
      Prop.No <- trimws(Prop.No)
      Lead.PI <- Lead.PI |> 
        gsub(pattern = ' AOI$', replacement = '', x = _)
      Submitted.Date <- Submitted.Date |>
        as.Date.character(format = '%m/%d/%Y')
      Submitted_FY <- Submitted.Date |>
        .bincode(breaks = TJU_Fiscal_Year(fiscal.year)) # NA, 1, NA
      Submitted_Term <- Submitted.Date |>
        TJU_SchoolTerm()
    }) |>
    subset.data.frame(
      subset = eval(quote(!is.na(Submitted_FY) & !(Status %in% c('Abandoned', 'Withdrawn')))),
      select = c('Status', 'Submitted_Term', 'Submitted.Date', 'Project.Name', 'Sponsor', 'My.Role', 'Lead.PI')
    ) |>
    #sort_by.data.frame(y = ~ list(- Submitted.Date), ) # error: unary - is not defined for "Date" objects
    sort_by.data.frame(y = ~ Submitted.Date)
  
  if (FALSE) { #manually inspect
    dim(proposal)
    table(proposal$Submitted.Date, useNA = 'ifany')
    table(proposal$Status, useNA = 'ifany')
    length(unique(proposal$Lead.PI))
  }
  
  # copy screen output to Interfolio
  proposal |> 
    sideway()
  
  return(invisible(proposal))
  
}



if (FALSE) {
  # only inspect
  dim(proposalFunded <- subset(pr1, Status == 'Funded')) # to be compared to `awards`
  # `proposalFunded` is not reliable
  proposalFunded[c('Lead.PI', 'Project.Name')]
  subset(awards, OnGoing, select = c('Lead.PI', 'Project.Title'))
}


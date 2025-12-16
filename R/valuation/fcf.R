suppressPackageStartupMessages({
  library(dplyr)
})

pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}


compute_fcf_from_as_reported <- function(cf_df) {
  if (is.null(cf_df) || nrow(cf_df) == 0) return(NULL)

  date_col <- pick_col(cf_df, c("date", "acceptedDate", "fillingDate", "periodOfReport"))
  if (is.null(date_col)) date_col <- names(cf_df)[1]

  cfo_col <- pick_col(cf_df, c(
    "netCashProvidedByUsedInOperatingActivities",
    "netCashProvidedByUsedInOperatingActivitiesContinuingOperations",
    "netCashProvidedByUsedInOperatingActivitiesAbstract",
    "netCashProvidedByUsedInOperatingActivities_1"
  ))

  capex_col <- pick_col(cf_df, c(
    "paymentsToAcquirePropertyPlantAndEquipment",
    "paymentsToAcquirePropertyPlantAndEquipmentAbstract",
    "capitalExpenditures",
    "capitalExpendituresAbstract"
  ))

  if (is.null(cfo_col) || is.null(capex_col)) return(NULL)

  out <- cf_df %>%
    transmute(
      date = as.Date(.data[[date_col]]),
      cfo  = suppressWarnings(as.numeric(.data[[cfo_col]])),
      capex = suppressWarnings(as.numeric(.data[[capex_col]]))
    ) %>%
    filter(!is.na(date)) %>%
    arrange(date)

  out <- out %>% mutate(fcf = cfo - capex)

  out %>% select(date, fcf)
}

fmp_fcf_history <- function(symbol, limit = 12) {
  res <- fmp_get("cash-flow-statement-as-reported", list(symbol = symbol, limit = limit))
  if (is_locked(res)) return(list(ok = FALSE, reason = res$error %||% "locked/unavailable", fcf = NULL, raw = res))

  df <- tryCatch(as.data.frame(res$data), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(list(ok = FALSE, reason = "empty data", fcf = NULL, raw = res))

  fcf <- compute_fcf_from_as_reported(df)
  if (is.null(fcf) || nrow(fcf) == 0) return(list(ok = FALSE, reason = "could not compute FCF from returned fields", fcf = NULL, raw = res))

  list(ok = TRUE, reason = NULL, fcf = fcf, raw = res)
}
    

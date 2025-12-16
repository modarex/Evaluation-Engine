# R/valuation/financials.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

as_tbl_safe <- function(x) {
  tryCatch(as_tibble(x, .name_repair = "unique"), error = function(e) NULL)
}

fmp_income <- function(symbol, period = "annual", limit = 8) {
  fmp_get(paste0("income-statement/", toupper(symbol)), list(period = period, limit = limit))
}

fmp_balance <- function(symbol, period = "annual", limit = 8) {
  fmp_get(paste0("balance-sheet-statement/", toupper(symbol)), list(period = period, limit = limit))
}

fmp_cashflow <- function(symbol, period = "annual", limit = 8) {
  fmp_get(paste0("cash-flow-statement/", toupper(symbol)), list(period = period, limit = limit))
}

calc_fcf_history <- function(cf_tbl) {
  # Works for FMP-style cash flow fields
  # operatingCashFlow, capitalExpenditure are the usual keys
  stopifnot(!is.null(cf_tbl))
  cf_tbl %>%
    mutate(
      date = as.Date(date),
      operatingCashFlow = as.numeric(operatingCashFlow %||% NA),
      capitalExpenditure = as.numeric(capitalExpenditure %||% NA),
      fcf = operatingCashFlow - capitalExpenditure
    ) %>%
    select(date, operatingCashFlow, capitalExpenditure, fcf) %>%
    arrange(date)
}

get_financial_pack <- function(symbol, period = "annual", limit = 8) {
  inc_res <- fmp_income(symbol, period, limit)
  bal_res <- fmp_balance(symbol, period, limit)
  cf_res  <- fmp_cashflow(symbol, period, limit)

  inc <- if (isTRUE(inc_res$ok)) as_tbl_safe(inc_res$data) else NULL
  bal <- if (isTRUE(bal_res$ok)) as_tbl_safe(bal_res$data) else NULL
  cf  <- if (isTRUE(cf_res$ok))  as_tbl_safe(cf_res$data)  else NULL

  fcf <- NULL
  if (!is.null(cf) && nrow(cf) > 0) {
    fcf <- calc_fcf_history(cf)
  }

  list(
    ok = !is.null(inc) || !is.null(bal) || !is.null(cf),
    income = inc,
    balance = bal,
    cashflow = cf,
    fcf = fcf,
    status = list(income = inc_res$status, balance = bal_res$status, cashflow = cf_res$status),
    url = list(income = inc_res$url, balance = bal_res$url, cashflow = cf_res$url),
    error = list(income = inc_res$error, balance = bal_res$error, cashflow = cf_res$error)
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

fmp_ratios <- function(symbol, period = "annual", limit = 5) {
  fmp_get(paste0("ratios/", toupper(symbol)), list(period = period, limit = limit))
}

parse_comps_multiples <- function(ratios_tbl) {
  ratios_tbl %>%
    mutate(date = as.Date(date)) %>%
    select(any_of(c(
      "date",
      "priceEarningsRatio",
      "priceToBookRatio",
      "enterpriseValueMultiple",
      "priceToSalesRatio",
      "pegRatio"
    ))) %>%
    arrange(desc(date))
}

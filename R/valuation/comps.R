# R/valuation/comps.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Free-tier reality: comps endpoints may be locked.
# So we provide:
# 1) Attempt to pull valuation multiples if available
# 2) Always allow manual comps table in the UI (done in app.R)

fmp_ratios <- function(symbol, period = "annual", limit = 5) {
  fmp_get(paste0("ratios/", toupper(symbol)), list(period = period, limit = limit))
}

parse_comps_multiples <- function(ratios_tbl) {
  # Typical fields (if available): priceEarningsRatio, priceToBookRatio, enterpriseValueMultiple, etc.
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

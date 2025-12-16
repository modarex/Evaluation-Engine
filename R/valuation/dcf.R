# R/valuation/dcf.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

project_fcf <- function(last_fcf, growth, years = 5) {
  # growth as decimal (e.g., 0.08)
  tibble(
    year = 1:years,
    fcf = last_fcf * (1 + growth)^(1:years)
  )
}

dcf_value <- function(fcf_proj, wacc, terminal_g, net_debt = 0, shares_out = 1) {
  # wacc, terminal_g in decimals
  years <- nrow(fcf_proj)
  disc <- (1 + wacc)^(fcf_proj$year)
  pv_fcf <- sum(fcf_proj$fcf / disc, na.rm = TRUE)

  # Gordon terminal value at end of projection horizon
  fcf_n <- fcf_proj$fcf[years]
  tv <- (fcf_n * (1 + terminal_g)) / (wacc - terminal_g)
  pv_tv <- tv / (1 + wacc)^years

  enterprise_value <- pv_fcf + pv_tv
  equity_value <- enterprise_value - net_debt

  per_share <- equity_value / shares_out

  list(
    pv_fcf = pv_fcf,
    pv_tv = pv_tv,
    enterprise_value = enterprise_value,
    equity_value = equity_value,
    per_share = per_share
  )
}

dcf_sensitivity <- function(last_fcf, growth, years,
                            wacc_seq = seq(0.07, 0.13, by = 0.01),
                            g_seq    = seq(0.01, 0.04, by = 0.005),
                            net_debt = 0, shares_out = 1) {
  grid <- expand.grid(wacc = wacc_seq, g = g_seq)
  out <- lapply(seq_len(nrow(grid)), function(i) {
    w <- grid$wacc[i]; g <- grid$g[i]
    proj <- project_fcf(last_fcf, growth, years)
    v <- dcf_value(proj, w, g, net_debt, shares_out)
    tibble(wacc = w, terminal_g = g, per_share = v$per_share)
  })
  bind_rows(out)
}

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(here)
  library(quantmod)
})

source(here("R", "data_sources", "fmp.R"))
source(here("R", "data_sources", "cache.R"))

`%||%` <- function(a, b) if (!is.null(a)) a else b

fmt_money <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  out[ok] <- paste0("$", formatC(x[ok], format = "f", digits = digits, big.mark = ","))
  out
}

fmt_num <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  out[ok] <- formatC(x[ok], format = "f", digits = digits, big.mark = ",")
  out
}

fmt_pct <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  out[ok] <- paste0(formatC(100 * x[ok], format = "f", digits = digits), "%")
  out
}

fmt_human <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  v <- x[ok]
  out_ok <- ifelse(abs(v) >= 1e12, paste0(formatC(v / 1e12, format = "f", digits = digits), "T"),
            ifelse(abs(v) >= 1e9,  paste0(formatC(v / 1e9,  format = "f", digits = digits), "B"),
            ifelse(abs(v) >= 1e6,  paste0(formatC(v / 1e6,  format = "f", digits = digits), "M"),
                                   formatC(v, format = "f", digits = digits, big.mark = ","))))
  out[ok] <- out_ok
  out
}

safe_tbl <- function(x) {
  if (is.null(x)) return(tibble())
  if (is.data.frame(x)) return(as_tibble(x, .name_repair = "unique"))
  tibble()
}

get_yahoo_price <- function(symbol) {
  cache_key <- paste0("yahoo_px_", symbol)
  cached <- cache_read(cache_key, max_age_hours = 0.5)
  if (is.numeric(cached) && length(cached) == 1 && is.finite(cached)) return(as.numeric(cached))

  px <- tryCatch({
    q <- suppressWarnings(quantmod::getQuote(symbol))
    if (is.data.frame(q) && nrow(q) == 1) {
      if ("Last" %in% names(q)) return(as.numeric(q$Last))
      if ("Trade" %in% names(q)) return(as.numeric(q$Trade))
      if ("Last Trade" %in% names(q)) return(as.numeric(q[["Last Trade"]]))
    }
    NA_real_
  }, error = function(e) NA_real_)

  if (!is.finite(px)) {
    px <- tryCatch({
      x <- suppressWarnings(quantmod::getSymbols(symbol, src = "yahoo", auto.assign = FALSE, warnings = FALSE))
      as.numeric(tail(quantmod::Cl(x), 1))
    }, error = function(e) NA_real_)
  }

  if (is.finite(px)) cache_write(cache_key, px)
  px
}

calc_beta_vs_spy <- function(symbol, years = 2) {
  tryCatch({
    cache_key <- paste0("beta_", symbol, "_", years)
    cached <- cache_read(cache_key, max_age_hours = 24)
    if (is.numeric(cached) && length(cached) == 1 && is.finite(cached)) return(as.numeric(cached))

    from <- Sys.Date() - round(365.25 * years)
    px_sym <- suppressWarnings(getSymbols(Symbols = symbol, src = "yahoo", from = from, auto.assign = FALSE))
    px_mkt <- suppressWarnings(getSymbols(Symbols = "SPY", src = "yahoo", from = from, auto.assign = FALSE))
    px_sym <- suppressWarnings(Ad(px_sym))
    px_mkt <- suppressWarnings(Ad(px_mkt))
    r_sym <- weeklyReturn(px_sym, type = "log")
    r_mkt <- weeklyReturn(px_mkt, type = "log")
    df <- na.omit(merge(r_sym, r_mkt))
    if (nrow(df) < 20) return(NA_real_)
    y <- as.numeric(df[, 1])
    x <- as.numeric(df[, 2])
    b <- as.numeric(coef(lm(y ~ x))[2])
    if (is.finite(b)) cache_write(cache_key, b)
    b
  }, error = function(e) NA_real_)
}

fmp_search <- function(query) fmp_get("search-name", list(query = query))
fmp_income <- function(symbol, period = "annual", limit = 5) fmp_get("income-statement", list(symbol = symbol, period = period, limit = limit))
fmp_balance <- function(symbol, period = "annual", limit = 5) fmp_get("balance-sheet-statement", list(symbol = symbol, period = period, limit = limit))
fmp_cashflow <- function(symbol, period = "annual", limit = 5) fmp_get("cash-flow-statement", list(symbol = symbol, period = period, limit = limit))
fmp_shares_float <- function(symbol) fmp_get("shares-float", list(symbol = symbol))

pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

last_row <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df %>% arrange(desc(as.Date(.data$date))) %>% slice_head(n = 1)
}

calc_net_debt <- function(bs_df) {
  if (is.null(bs_df) || nrow(bs_df) == 0) return(NA_real_)
  r <- last_row(bs_df)
  if (is.null(r)) return(NA_real_)
  cash_col <- pick_col(r, c("cashAndCashEquivalents", "cashAndCashEquivalentsUSD"))
  std_col  <- pick_col(r, c("shortTermDebt", "shortTermDebtUSD"))
  ltd_col  <- pick_col(r, c("longTermDebt", "longTermDebtUSD"))
  cash <- if (!is.null(cash_col)) suppressWarnings(as.numeric(r[[cash_col]])) else NA_real_
  std  <- if (!is.null(std_col))  suppressWarnings(as.numeric(r[[std_col]]))  else 0
  ltd  <- if (!is.null(ltd_col))  suppressWarnings(as.numeric(r[[ltd_col]]))  else 0
  if (!is.finite(cash)) return(NA_real_)
  (std + ltd) - cash
}

calc_total_debt <- function(bs_df) {
  if (is.null(bs_df) || nrow(bs_df) == 0) return(NA_real_)
  r <- last_row(bs_df)
  if (is.null(r)) return(NA_real_)
  std_col <- pick_col(r, c("shortTermDebt", "shortTermDebtUSD"))
  ltd_col <- pick_col(r, c("longTermDebt", "longTermDebtUSD"))
  std <- if (!is.null(std_col)) suppressWarnings(as.numeric(r[[std_col]])) else 0
  ltd <- if (!is.null(ltd_col)) suppressWarnings(as.numeric(r[[ltd_col]])) else 0
  td <- std + ltd
  ifelse(is.finite(td) && td > 0, td, NA_real_)
}

shares_from_income <- function(is_df) {
  if (is.null(is_df) || nrow(is_df) == 0) return(NA_real_)
  r <- last_row(is_df)
  if (is.null(r)) return(NA_real_)
  sh_col <- pick_col(r, c("weightedAverageShsOutDil", "weightedAverageShsOut", "weightedAverageShsOutDiluted"))
  if (is.null(sh_col)) return(NA_real_)
  suppressWarnings(as.numeric(r[[sh_col]]))
}

shares_from_float <- function(sf_df) {
  if (is.null(sf_df) || nrow(sf_df) == 0) return(NA_real_)
  r <- sf_df %>% slice_head(n = 1)
  col <- pick_col(r, c("outstandingShares", "floatShares"))
  if (is.null(col)) return(NA_real_)
  suppressWarnings(as.numeric(r[[col]]))
}

tax_rate_from_income <- function(is_df) {
  if (is.null(is_df) || nrow(is_df) == 0) return(NA_real_)
  r <- last_row(is_df)
  if (is.null(r)) return(NA_real_)
  tax_col <- pick_col(r, c("incomeTaxExpense", "incomeTaxExpenseUSD"))
  pbt_col <- pick_col(r, c("incomeBeforeTax", "incomeBeforeTaxUSD"))
  if (is.null(tax_col) || is.null(pbt_col)) return(NA_real_)
  tax <- suppressWarnings(as.numeric(r[[tax_col]]))
  pbt <- suppressWarnings(as.numeric(r[[pbt_col]]))
  if (!is.finite(tax) || !is.finite(pbt) || pbt == 0) return(NA_real_)
  max(0, min(0.50, tax / pbt))
}

dcf_value <- function(last_fcf, g, wacc, tg, horizon, net_debt, shares) {
  if (!is.finite(last_fcf) || !is.finite(wacc) || !is.finite(tg) || !is.finite(shares) || shares <= 0) {
    return(list(error = "Missing/invalid inputs for DCF."))
  }
  if (wacc <= tg) return(list(error = "WACC must be > terminal growth."))
  years <- seq_len(horizon)
  fcfs <- last_fcf * (1 + g) ^ years
  pv_fcf <- sum(fcfs / (1 + wacc) ^ years)
  terminal_fcf <- last_fcf * (1 + g) ^ horizon
  tv <- terminal_fcf * (1 + tg) / (wacc - tg)
  pv_t <- tv / (1 + wacc) ^ horizon
  ev <- pv_fcf + pv_t
  eq <- ev - net_debt
  ps <- eq / shares
  list(pv_fcf = pv_fcf, pv_terminal = pv_t, enterprise_value = ev, equity_value = eq, per_share = ps)
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
  body{background:#0b0f14;color:#e5e7eb}
  .card,.bigbox{background:#111827;border:1px solid #1f2937;border-radius:14px;box-shadow:0 0 0 1px rgba(255,255,255,0.02)}
  .muted,.note{color:#9ca3af}
  .green{color:#22c55e}
  .red{color:#ef4444}
  .kpi{font-weight:800;color:#e5e7eb}
  .form-control,.selectize-input{background:#020617!important;color:#e5e7eb!important;border:1px solid #1f2937!important}
  .selectize-dropdown{background:#020617;color:#e5e7eb;border:1px solid #1f2937}
  .btn{background:#020617;color:#e5e7eb;border:1px solid #1f2937}
  .btn:hover{background:#111827}
  .nav-tabs>li>a{background:transparent;color:#9ca3af;border:none}
  .nav-tabs>li.active>a{color:#f9fafb;border-bottom:2px solid #3b82f6;background:transparent}
  .table{color:#e5e7eb!important;background-color:transparent!important}
  .table thead th{color:#e5e7eb!important;background-color:#020617!important;border-color:#1f2937!important}
  .table tbody td,.table tbody th{color:#e5e7eb!important;background-color:#0b0f14!important;border-color:#1f2937!important}
  .table-striped>tbody>tr:nth-of-type(odd)>*{background-color:#0f172a!important;color:#e5e7eb!important}
  .table-striped>tbody>tr:nth-of-type(even)>*{background-color:#0b0f14!important;color:#e5e7eb!important}

  .card{
    background: linear-gradient(180deg, #0b1220 0%, #0b0f14 100%);
    border: 1px solid rgba(148,163,184,.18);
    border-radius: 18px;
    box-shadow:
      0 10px 30px rgba(0,0,0,.45),
      inset 0 1px 0 rgba(255,255,255,.04);
    padding: 18px 18px 16px 18px;
  }

  .title{
    font-size:26px;
    font-weight:900;
    color:#f9fafb;
    margin: 0 0 6px 0;
    letter-spacing: .3px;
  }

  .sub{
    margin: 0 0 10px 0;
    font-size: 13px;
    opacity: .95;
  }

  .price{
    display: inline-block;
    margin-top: 6px;
    padding: 10px 14px;
    border-radius: 14px;
    background: rgba(2,6,23,.65);
    border: 1px solid rgba(148,163,184,.22);
    box-shadow: inset 0 1px 0 rgba(255,255,255,.04);
    font-size: 30px;
    font-weight: 950;
    color:#f9fafb;
  }

  .bigvalue{font-size:40px;font-weight:950;color:#f9fafb}
  "))),
  titlePanel("Valuation Engine"),
  uiOutput("api_key_notice"),
  fluidRow(
    column(
      4,
      selectInput("exchange", "Exchange", choices = c("All" = ""), selected = ""),
      checkboxInput("hide_etfs", "Hide ETFs/ETNs/leveraged products", value = TRUE),
      textInput("query", "Search company/ticker", placeholder = "Type e.g. apple, amazon, alphabet..."),
      selectizeInput("ticker", "Select ticker", choices = NULL, multiple = FALSE,
                     options = list(placeholder = "Pick from results...", maxOptions = 50)),
      actionButton("clear", "Clear selection")
    ),
    column(
      7,
      div(class = "card", uiOutput("company_header")),
      tabsetPanel(
        tabPanel(
          "Financials",
          div(class = "note", "Use the year range slider to choose which years appear. FCF chart and tables update instantly."),
          uiOutput("hist_controls"),
          uiOutput("changes_table"),
          plotOutput("fcf_plot", height = 240),
          tableOutput("fcf_table")
        ),
        tabPanel(
          "DCF",
          div(class = "note", "Forecast assumes the same growth rate (g) every forecast year. Terminal growth applies only to the terminal value."),
          uiOutput("wacc_box"),
          fluidRow(
            column(3, sliderInput("forecast_years", "Forecast years", min = 1, max = 30, value = 10, step = 1)),
            column(3, sliderInput("g", "FCF growth (g)", min = -0.20, max = 0.30, value = 0.065, step = 0.005)),
            column(3, sliderInput("wacc_adj", "WACC adjustment", min = -0.05, max = 0.05, value = 0.00, step = 0.0025)),
            column(3, sliderInput("tg", "Terminal growth (tg)", min = -0.01, max = 0.08, value = 0.025, step = 0.005))
          ),
          fluidRow(
            column(4, numericInput("last_fcf_manual", "Last FCF (manual fallback)", value = 1e10)),
            column(4, numericInput("net_debt_manual", "Net debt (manual fallback)", value = 0)),
            column(4, numericInput("shares_manual", "Shares (manual fallback)", value = 1e10))
          ),
          uiOutput("dcf_big"),
          plotOutput("ff_plot", height = 260)
        ),
        tabPanel(
          "Comps",
          div(class = "note", "Add peers, pick a multiple, and we compute a comps range (min/mean/max). That range is also plotted on Football Field."),
          selectInput("multiple", "Multiple", choices = c("EV/FCF", "P/E", "EV/EBITDA"), selected = "EV/FCF"),
          fluidRow(
            column(
              7,
              textInput("peer_query", "Search peer (type name/ticker)", placeholder = "e.g. alphabet, microsoft..."),
              selectizeInput("peer_pick", "Pick peer from search", choices = NULL, multiple = FALSE,
                             options = list(placeholder = "Pick a peer...", maxOptions = 50))
            ),
            column(5, br(), actionButton("add_peer", "Add peer"), actionButton("clear_peers", "Clear peers"))
          ),
          uiOutput("peers_line"),
          uiOutput("comps_summary"),
          tableOutput("comps_table")
        ),
        tabPanel("Football Field", plotOutput("ff_plot_only", height = 300))
      )
    )
  )
)

server <- function(input, output, session) {

  output$api_key_notice <- renderUI({
    if (nzchar(fmp_api_key())) return(NULL)
    div(
      class = "card",
      span(class = "red", "FMP_API_KEY is not set."),
      div(class = "muted", "Set the FMP_API_KEY environment variable to enable search and financial data.")
    )
  })

  observeEvent(input$clear, {
    updateTextInput(session, "query", value = "")
    updateSelectizeInput(session, "ticker", choices = c(), selected = "")
    updateSelectInput(session, "exchange", choices = c("All" = ""), selected = "")
  })

  search_results <- reactive({
    q <- input$query
    if (is.null(q) || nchar(q) < 2) return(tibble())
    key <- paste0("search_", gsub("[^A-Za-z0-9]+", "_", tolower(q)))
    cached <- cache_read(key, max_age_hours = 24)
    if (!is.null(cached)) return(safe_tbl(cached))
    res <- fmp_search(q)
    if (!isTRUE(res$ok) || !is.data.frame(res$data)) return(tibble())
    cache_write(key, res$data)
    safe_tbl(res$data)
  }) %>% shiny::debounce(300)

  observeEvent(search_results(), {
    df <- search_results()
    if (nrow(df) == 0 || !("exchange" %in% names(df))) return()
    ex <- sort(unique(df$exchange))
    ex <- ex[!is.na(ex) & nzchar(ex)]
    updateSelectInput(session, "exchange", choices = c("All" = "", stats::setNames(ex, ex)), selected = input$exchange %||% "")
  }, ignoreInit = TRUE)

  observeEvent(list(search_results(), input$exchange, input$hide_etfs), {
    df <- search_results()
    if (nrow(df) == 0) {
      updateSelectizeInput(session, "ticker", choices = c(), selected = "")
      return()
    }
    if (nzchar(input$exchange) && "exchange" %in% names(df)) df <- df %>% filter(exchange == input$exchange)
    if (isTRUE(input$hide_etfs) && "name" %in% names(df)) {
      bad <- str_detect(tolower(df$name),
                        "etf|etn|fund|trust|leveraged|2x|3x|bear|bull|direxion|proshares|invesco|ishares|vanguard")
      df <- df %>% filter(!bad)
    }
    if (nrow(df) == 0) {
      updateSelectizeInput(session, "ticker", choices = c(), selected = "")
      return()
    }
    sym <- df$symbol %||% rep("", nrow(df))
    ex  <- df$exchange %||% rep("", nrow(df))
    nm  <- df$name %||% rep("", nrow(df))
    lbl <- paste0(sym, " (", ex, ") - ", nm)
    choices <- stats::setNames(sym, lbl)
    updateSelectizeInput(session, "ticker", choices = choices, server = TRUE)
  }, ignoreInit = TRUE)

  selected_row <- reactive({
    df <- search_results()
    tkr <- input$ticker
    if (nrow(df) == 0 || is.null(tkr) || !nzchar(tkr)) return(NULL)
    df %>% filter(symbol == tkr) %>% slice_head(n = 1)
  })

  stmt_bundle <- reactive({
    tkr <- input$ticker
    if (is.null(tkr) || !nzchar(tkr)) return(NULL)
    key <- paste0("stmt_", tkr)
    cached <- cache_read(key, max_age_hours = 24)
    if (!is.null(cached)) return(cached)
    is_res <- fmp_income(tkr, limit = 5)
    bs_res <- fmp_balance(tkr, limit = 5)
    cf_res <- fmp_cashflow(tkr, limit = 5)
    out <- list(
      is_status = is_res$status,
      bs_status = bs_res$status,
      cf_status = cf_res$status,
      is_df = if (isTRUE(is_res$ok) && is.data.frame(is_res$data)) safe_tbl(is_res$data) else tibble(),
      bs_df = if (isTRUE(bs_res$ok) && is.data.frame(bs_res$data)) safe_tbl(bs_res$data) else tibble(),
      cf_df = if (isTRUE(cf_res$ok) && is.data.frame(cf_res$data)) safe_tbl(cf_res$data) else tibble()
    )
    cache_write(key, out)
    out
  })

  output$hist_controls <- renderUI({
    b <- stmt_bundle()
    if (is.null(b) || nrow(b$cf_df) == 0) return(div(class = "card", "No cash flow data returned for this ticker."))
    yrs <- b$cf_df %>%
      mutate(Year = as.integer(substr(as.character(date), 1, 4))) %>%
      filter(is.finite(Year)) %>%
      pull(Year)
    yrs <- sort(unique(yrs))
    if (length(yrs) < 2) return(div(class = "card", "Not enough years returned to build a range slider."))
    sliderInput("hist_years", "Historical range (years)", min = min(yrs), max = max(yrs),
                value = c(min(yrs), max(yrs)), step = 1, sep = "")
  })

  hist_window <- reactive({
    b <- stmt_bundle()
    if (is.null(b) || nrow(b$cf_df) == 0 || is.null(input$hist_years)) return(tibble())
    yr_min <- input$hist_years[1]
    yr_max <- input$hist_years[2]
    b$cf_df %>%
      mutate(
        Year = as.integer(substr(as.character(date), 1, 4)),
        FCF = suppressWarnings(as.numeric(freeCashFlow))
      ) %>%
      filter(is.finite(Year), Year >= yr_min, Year <= yr_max) %>%
      arrange(desc(Year)) %>%
      select(Year, FCF)
  })

  output$changes_table <- renderUI({
    b <- stmt_bundle()
    if (is.null(b) || nrow(b$is_df) == 0) return(NULL)

    is_df <- b$is_df %>%
      mutate(
        Year = as.integer(substr(as.character(date), 1, 4)),
        EPS = suppressWarnings(as.numeric(eps)),
        NetIncome = suppressWarnings(as.numeric(netIncome))
      ) %>%
      filter(is.finite(Year)) %>%
      arrange(Year) %>%
      mutate(
        EPS_YoY = EPS - lag(EPS),
        NetIncome_YoY = NetIncome - lag(NetIncome)
      ) %>%
      arrange(desc(Year)) %>%
      select(Year, EPS, EPS_YoY, NetIncome, NetIncome_YoY) %>%
      filter(!is.na(EPS_YoY) | !is.na(NetIncome_YoY))

    if (nrow(is_df) == 0) return(NULL)

    yoy_cell <- function(x, fmt_fun) {
      if (!is.finite(x)) return(tags$span(""))
      cls <- if (x >= 0) "green" else "red"
      sign <- if (x >= 0) "+" else ""
      tags$span(class = cls, paste0(sign, fmt_fun(x)))
    }

    tags$table(
      class = "table table-striped",
      tags$thead(tags$tr(
        tags$th("Year"), tags$th("EPS"), tags$th("EPS YoY"), tags$th("Net Income"), tags$th("Net Income YoY")
      )),
      tags$tbody(lapply(seq_len(nrow(is_df)), function(i) {
        tags$tr(
          tags$td(is_df$Year[i]),
          tags$td(fmt_num(is_df$EPS[i], 2)),
          tags$td(yoy_cell(is_df$EPS_YoY[i], function(z) fmt_num(z, 2))),
          tags$td(fmt_human(is_df$NetIncome[i], 2)),
          tags$td(yoy_cell(is_df$NetIncome_YoY[i], function(z) fmt_human(z, 2)))
        )
      }))
    )
  })

  output$fcf_plot <- renderPlot({
    h <- hist_window()
    if (nrow(h) == 0) return(NULL)

    df <- h %>% arrange(Year)
    y <- df$FCF / 1e9

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)

    par(bg = "#0b0f14", fg = "#e5e7eb", col.axis = "#9ca3af", col.lab = "#e5e7eb", col.main = "#f9fafb",
        mar = c(4, 4, 3, 1) + 0.1)

    plot(df$Year, y, type = "o", pch = 19, xlab = "Year", ylab = "FCF (Billions)", main = "Free Cash Flow",
         col = "#60a5fa", axes = FALSE)

    axis(1, col = "#1f2937", col.axis = "#9ca3af")
    axis(2, col = "#1f2937", col.axis = "#9ca3af")
    box(col = "#1f2937")
    grid(col = "#111827", lty = 1)
  })

  output$fcf_table <- renderTable({
    h <- hist_window()
    if (nrow(h) == 0) return(NULL)
    h %>% transmute(Year = Year, FCF = fmt_human(FCF, 2))
  }, striped = TRUE, digits = 2)

  output$company_header <- renderUI({
    tkr <- input$ticker
    if (is.null(tkr) || !nzchar(tkr)) return(div(class = "muted", "Select a ticker to begin."))
    row <- selected_row()
    nm <- if (!is.null(row) && "name" %in% names(row)) row$name[[1]] else tkr
    ex <- if (!is.null(row) && "exchange" %in% names(row)) row$exchange[[1]] else "Exchange unknown"
    cc <- if (!is.null(row) && "currency" %in% names(row)) row$currency[[1]] else ""
    px <- get_yahoo_price(tkr)
    tagList(
      h2(class = "title", tkr),
      div(class = "sub", paste0(nm, if (nzchar(cc)) paste0(" | ", cc) else "", " | ", ex)),
      div(class = "price", if (is.finite(px)) fmt_money(px, 2) else "Price unavailable")
    )
  })

  shares_pack <- reactive({
    tkr <- input$ticker
    if (is.null(tkr) || !nzchar(tkr)) return(list(shares = NA_real_, source = "none"))
    b <- stmt_bundle()
    sh1 <- shares_from_income(b$is_df)
    if (is.finite(sh1) && sh1 > 0) return(list(shares = sh1, source = "income_statement"))
    sf_res <- fmp_shares_float(tkr)
    sf_df <- if (isTRUE(sf_res$ok) && is.data.frame(sf_res$data)) safe_tbl(sf_res$data) else tibble()
    sh2 <- shares_from_float(sf_df)
    if (is.finite(sh2) && sh2 > 0) return(list(shares = sh2, source = "shares_float"))
    list(shares = input$shares_manual, source = "manual")
  })

  wacc_pack <- reactive({
    tkr <- input$ticker
    if (is.null(tkr) || !nzchar(tkr)) return(NULL)

    px <- get_yahoo_price(tkr)
    beta <- calc_beta_vs_spy(tkr, years = 2)

    b <- stmt_bundle()
    is_df <- b$is_df
    bs_df <- b$bs_df

    sp <- shares_pack()
    shares_used <- sp$shares

    net_debt_auto <- calc_net_debt(bs_df)
    net_debt_used <- if (is.finite(net_debt_auto)) net_debt_auto else input$net_debt_manual
    total_debt <- calc_total_debt(bs_df)

    rf  <- input$rf %||% 0.045
    erp <- input$erp %||% 0.055
    cost_equity <- if (is.finite(beta)) rf + beta * erp else rf + 1.0 * erp

    tax_auto <- tax_rate_from_income(is_df)
    tax_rate <- if (is.finite(tax_auto)) tax_auto else 0.21

    cost_debt <- input$cost_debt %||% 0.055

    mkt_cap <- if (is.finite(px) && is.finite(shares_used)) px * shares_used else NA_real_
    debt_val <- if (is.finite(total_debt)) total_debt else NA_real_

    if (!is.finite(mkt_cap) || mkt_cap <= 0 || !is.finite(debt_val) || debt_val < 0) {
      we <- 1; wd <- 0
    } else {
      we <- mkt_cap / (mkt_cap + debt_val)
      wd <- debt_val / (mkt_cap + debt_val)
    }

    wacc_raw <- we * cost_equity + wd * cost_debt * (1 - tax_rate)
    wacc_final <- wacc_raw + input$wacc_adj

    list(
      price = px, beta = beta, rf = rf, erp = erp,
      cost_equity = cost_equity, cost_debt = cost_debt, tax_rate = tax_rate,
      we = we, wd = wd, wacc_raw = wacc_raw, wacc = wacc_final,
      shares_used = shares_used, shares_source = sp$source, net_debt_used = net_debt_used
    )
  })

  output$wacc_box <- renderUI({
    tkr <- input$ticker
    if (is.null(tkr) || !nzchar(tkr)) return(NULL)
    wp <- wacc_pack()
    if (is.null(wp)) return(NULL)

    tagList(
      fluidRow(
        column(3, numericInput("rf", "Risk-free rate (Rf)", value = 0.045, step = 0.0025)),
        column(3, numericInput("erp", "Equity risk premium (ERP)", value = 0.055, step = 0.0025)),
        column(3, numericInput("cost_debt", "Cost of debt", value = 0.055, step = 0.0025)),
        column(3, numericInput("tax_display", "Tax rate (auto)", value = wp$tax_rate, step = 0.01))
      ),
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function(){
          setTimeout(function(){
            var el = document.getElementById('tax_display');
            if(el) el.setAttribute('disabled','disabled');
          }, 200);
        });
      ")),
      div(class = "card",
          div(span(class = "kpi", "Beta (Yahoo vs SPY): "), ifelse(is.finite(wp$beta), fmt_num(wp$beta, 3), "NA")),
          div(span(class = "kpi", "Cost of equity (CAPM): "), fmt_pct(wp$cost_equity, 2)),
          div(span(class = "kpi", "Weights (E/D): "), paste0(fmt_num(wp$we, 3), " / ", fmt_num(wp$wd, 3))),
          div(span(class = "kpi", "WACC (computed): "), fmt_pct(wp$wacc, 2)),
          div(class = "muted",
              paste0("Shares used: ", fmt_human(wp$shares_used, 2), " (", wp$shares_source, ")",
                     " | Net debt used: ", fmt_human(wp$net_debt_used, 2))
          )
      )
    )
  })

  output$dcf_big <- renderUI({
    tkr <- input$ticker
    if (is.null(tkr) || !nzchar(tkr)) return(NULL)
    wp <- wacc_pack()
    if (is.null(wp)) return(NULL)

    h <- hist_window()
    last_fcf_auto <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$FCF[which.max(h$Year)])) else NA_real_
    last_fcf_used <- if (is.finite(last_fcf_auto)) last_fcf_auto else input$last_fcf_manual

    shares_used <- wp$shares_used %||% input$shares_manual
    net_debt_used <- wp$net_debt_used %||% input$net_debt_manual
    wacc_used <- wp$wacc %||% 0.10

    res <- dcf_value(
      last_fcf = last_fcf_used, g = input$g, wacc = wacc_used, tg = input$tg,
      horizon = input$forecast_years, net_debt = net_debt_used, shares = shares_used
    )

    if (!is.null(res$error)) return(div(class = "card", span(class = "red", res$error)))

    px <- wp$price
    dcf_ps <- res$per_share

    cls <- "red"
    if (is.finite(px) && is.finite(dcf_ps)) cls <- if (dcf_ps >= px) "green" else "red"
    gap <- if (is.finite(px) && is.finite(dcf_ps) && px != 0) (dcf_ps / px - 1) else NA_real_

    div(class = "bigbox",
        p(class = paste("bigvalue", cls), paste0("DCF value: ", fmt_money(dcf_ps, 2), " per share")),
        div(class = "muted",
            paste0("Current price: ", if (is.finite(px)) fmt_money(px, 2) else "NA",
                   if (is.finite(gap)) paste0(" | Gap: ", fmt_pct(gap, 2)) else "")
        ),
        div(class = "note", "Tip: start with Forecast years → Growth (g) → Terminal growth (tg). Use WACC adjustment only for sensitivity.")
    )
  })

  peers <- reactiveVal(character(0))

  observeEvent(input$clear_peers, { peers(character(0)) })

  peer_search <- reactive({
    q <- input$peer_query
    if (is.null(q) || nchar(q) < 2) return(tibble())
    key <- paste0("peer_search_", gsub("[^A-Za-z0-9]+", "_", tolower(q)))
    cached <- cache_read(key, max_age_hours = 24)
    if (!is.null(cached)) return(safe_tbl(cached))
    res <- fmp_search(q)
    if (!isTRUE(res$ok) || !is.data.frame(res$data)) return(tibble())
    cache_write(key, res$data)
    safe_tbl(res$data)
  }) %>% shiny::debounce(250)

  observeEvent(peer_search(), {
    df <- peer_search()
    if (nrow(df) == 0) {
      updateSelectizeInput(session, "peer_pick", choices = c(), selected = "")
      return()
    }
    sym <- df$symbol %||% rep("", nrow(df))
    ex  <- df$exchange %||% rep("", nrow(df))
    nm  <- df$name %||% rep("", nrow(df))
    lbl <- paste0(sym, " (", ex, ") - ", nm)
    choices <- stats::setNames(sym, lbl)
    updateSelectizeInput(session, "peer_pick", choices = choices, server = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$add_peer, {
    p <- input$peer_pick
    if (is.null(p) || !nzchar(p)) return()
    cur <- peers()
    if (!(p %in% cur)) peers(c(cur, p))
  })

  output$peers_line <- renderUI({
    ps <- peers()
    if (length(ps) == 0) return(div(class = "muted", "Peers: (none yet)"))
    div(class = "muted", paste0("Peers: ", paste(ps, collapse = ", ")))
  })

  peer_metrics_one <- function(sym) {
    cache_key <- paste0("peer_metrics_", sym)
    cached <- cache_read(cache_key, max_age_hours = 0.5)
    if (!is.null(cached)) return(cached)

    px <- get_yahoo_price(sym)

    is_res <- fmp_income(sym, limit = 1)
    bs_res <- fmp_balance(sym, limit = 1)
    cf_res <- fmp_cashflow(sym, limit = 1)

    is_df <- if (isTRUE(is_res$ok) && is.data.frame(is_res$data)) safe_tbl(is_res$data) else tibble()
    bs_df <- if (isTRUE(bs_res$ok) && is.data.frame(bs_res$data)) safe_tbl(bs_res$data) else tibble()
    cf_df <- if (isTRUE(cf_res$ok) && is.data.frame(cf_res$data)) safe_tbl(cf_res$data) else tibble()

    sh <- shares_from_income(is_df)
    if (!is.finite(sh) || sh <= 0) {
      sf_res <- fmp_shares_float(sym)
      sf_df <- if (isTRUE(sf_res$ok) && is.data.frame(sf_res$data)) safe_tbl(sf_res$data) else tibble()
      sh <- shares_from_float(sf_df)
    }

    net_debt <- calc_net_debt(bs_df)
    if (!is.finite(net_debt)) net_debt <- 0

    fcf <- if (nrow(cf_df) > 0) suppressWarnings(as.numeric(cf_df$freeCashFlow[1])) else NA_real_
    ebitda <- if (nrow(is_df) > 0 && "ebitda" %in% names(is_df)) suppressWarnings(as.numeric(is_df$ebitda[1])) else NA_real_
    eps <- if (nrow(is_df) > 0 && "eps" %in% names(is_df)) suppressWarnings(as.numeric(is_df$eps[1])) else NA_real_

    mktcap <- if (is.finite(px) && is.finite(sh)) px * sh else NA_real_
    ev <- if (is.finite(mktcap) && is.finite(net_debt)) mktcap + net_debt else NA_real_

    ev_fcf <- if (is.finite(ev) && is.finite(fcf) && fcf != 0) ev / fcf else NA_real_
    pe <- if (is.finite(px) && is.finite(eps) && eps != 0) px / eps else NA_real_
    ev_ebitda <- if (is.finite(ev) && is.finite(ebitda) && ebitda != 0) ev / ebitda else NA_real_

    out <- tibble(peer = sym, price = px, shares = sh, net_debt = net_debt, mktcap = mktcap, ev = ev, fcf = fcf,
                  ebitda = ebitda, eps = eps, `EV/FCF` = ev_fcf, `P/E` = pe, `EV/EBITDA` = ev_ebitda)

    if (is.finite(px)) cache_write(cache_key, out)

    out
  }

  comps_table_raw <- reactive({
    ps <- peers()
    if (length(ps) == 0) return(tibble())
    map_dfr(ps, peer_metrics_one)
  })

  output$comps_table <- renderTable({
    df <- comps_table_raw()
    if (nrow(df) == 0) return(NULL)
    df %>%
      transmute(
        peer,
        price = fmt_money(price, 2),
        shares = fmt_human(shares, 2),
        fcf = fmt_human(fcf, 2),
        eps = fmt_num(eps, 2),
        `EV/FCF` = fmt_num(`EV/FCF`, 2),
        `P/E` = fmt_num(`P/E`, 2),
        `EV/EBITDA` = fmt_num(`EV/EBITDA`, 2)
      )
  }, striped = TRUE)

  comps_range <- reactive({
    df <- comps_table_raw()
    if (nrow(df) == 0) return(NULL)
    mult <- input$multiple
    v <- df[[mult]]
    v <- v[is.finite(v)]
    if (length(v) == 0) return(NULL)
    list(multiple = mult, min = min(v), mean = mean(v), max = max(v))
  })

  output$comps_summary <- renderUI({
    cr <- comps_range()
    if (is.null(cr)) return(div(class = "muted", "Add peer tickers to compute multiples."))
    div(class = "card",
        strong(paste0(cr$multiple, " range (peers)")),
        div(paste0("Min: ", fmt_num(cr$min, 2), " | Mean: ", fmt_num(cr$mean, 2), " | Max: ", fmt_num(cr$max, 2)))
    )
  })

  ff_values <- reactive({
    tkr <- input$ticker
    if (is.null(tkr) || !nzchar(tkr)) return(NULL)
    wp <- wacc_pack()
    if (is.null(wp)) return(NULL)

    px <- wp$price

    h <- hist_window()
    last_fcf_auto <- if (nrow(h) > 0) suppressWarnings(as.numeric(h$FCF[which.max(h$Year)])) else NA_real_
    last_fcf_used <- if (is.finite(last_fcf_auto)) last_fcf_auto else input$last_fcf_manual

    res <- dcf_value(last_fcf = last_fcf_used, g = input$g, wacc = wp$wacc, tg = input$tg,
                     horizon = input$forecast_years, net_debt = wp$net_debt_used, shares = wp$shares_used)
    dcf_ps <- if (!is.null(res$per_share) && is.finite(res$per_share)) res$per_share else NA_real_

    cr <- comps_range()
    implied <- NULL
    if (!is.null(cr)) {
      mult <- cr$multiple
      b <- stmt_bundle()
      cf <- b$cf_df
      is_df <- b$is_df

      tgt_fcf <- if (nrow(cf) > 0 && "freeCashFlow" %in% names(cf)) suppressWarnings(as.numeric(cf$freeCashFlow[1])) else NA_real_
      tgt_eps <- if (nrow(is_df) > 0 && "eps" %in% names(is_df)) suppressWarnings(as.numeric(is_df$eps[1])) else NA_real_
      tgt_ebitda <- if (nrow(is_df) > 0 && "ebitda" %in% names(is_df)) suppressWarnings(as.numeric(is_df$ebitda[1])) else NA_real_

      net_debt <- wp$net_debt_used
      sh <- wp$shares_used

      if (mult == "EV/FCF" && is.finite(tgt_fcf) && tgt_fcf != 0 && is.finite(sh) && sh > 0) {
        ev_min <- cr$min * tgt_fcf
        ev_mean <- cr$mean * tgt_fcf
        ev_max <- cr$max * tgt_fcf
        implied <- list(label = "Comps", min = (ev_min - net_debt) / sh, mean = (ev_mean - net_debt) / sh, max = (ev_max - net_debt) / sh)
      } else if (mult == "P/E" && is.finite(tgt_eps) && tgt_eps != 0) {
        implied <- list(label = "Comps", min = cr$min * tgt_eps, mean = cr$mean * tgt_eps, max = cr$max * tgt_eps)
      } else if (mult == "EV/EBITDA" && is.finite(tgt_ebitda) && tgt_ebitda != 0 && is.finite(sh) && sh > 0) {
        ev_min <- cr$min * tgt_ebitda
        ev_mean <- cr$mean * tgt_ebitda
        ev_max <- cr$max * tgt_ebitda
        implied <- list(label = "Comps", min = (ev_min - net_debt) / sh, mean = (ev_mean - net_debt) / sh, max = (ev_max - net_debt) / sh)
      }
    }

    list(price = px, dcf = dcf_ps, comps = implied)
  })

  plot_ff <- function() {
    v <- ff_values()
    if (is.null(v)) return(NULL)

    px <- v$price
    dcf <- v$dcf
    comps <- v$comps

    pts <- c()
    if (is.finite(px)) pts <- c(pts, px)
    if (is.finite(dcf)) pts <- c(pts, dcf)
    if (!is.null(comps)) {
      rng <- c(comps$min, comps$mean, comps$max)
      pts <- c(pts, rng[is.finite(rng)])
    }
    pts <- pts[is.finite(pts)]
    if (length(pts) == 0) return(NULL)

    lo <- min(pts, na.rm = TRUE)
    hi <- max(pts, na.rm = TRUE)
    pad <- 0.10 * (hi - lo)
    if (!is.finite(pad) || pad == 0) pad <- max(1, abs(lo) * 0.05)

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)

    par(bg = "#0b0f14", fg = "#e5e7eb", col.axis = "#9ca3af", col.lab = "#e5e7eb",
        col.main = "#f9fafb", mar = c(4, 5, 3, 2) + 0.1)

    plot(c(0, 1), c(lo - pad, hi + pad), type = "n", xaxt = "n", xlab = "", ylab = "Value per share",
         main = "Football Field (Price vs DCF vs Comps)", axes = FALSE)

    axis(2, col = "#1f2937", col.axis = "#9ca3af")
    box(col = "#1f2937")
    grid(col = "#111827", lty = 1)

    if (is.finite(px)) {
      points(0.5, px, pch = 19, cex = 1.1, col = "#93c5fd")
      text(0.53, px, labels = paste0("Price: ", fmt_money(px, 2)), pos = 4, col = "#e5e7eb")
    }

    if (is.finite(dcf)) {
      dcf_col <- "#ef4444"
      if (is.finite(px) && dcf >= px) dcf_col <- "#22c55e"
      points(0.5, dcf, pch = 19, cex = 1.2, col = dcf_col)
      text(0.53, dcf, labels = paste0("DCF: ", fmt_money(dcf, 2)), pos = 4, col = "#e5e7eb")
    }

    if (!is.null(comps) && is.finite(comps$min) && is.finite(comps$max)) {
      segments(0.5, comps$min, 0.5, comps$max, lwd = 10, col = "#64748b")
      if (is.finite(comps$mean)) points(0.5, comps$mean, pch = 19, cex = 1.1, col = "#fbbf24")
      text(0.53, comps$max, labels = paste0("Comps Max: ", fmt_money(comps$max, 2)), pos = 4, col = "#e5e7eb")
      text(0.53, comps$min, labels = paste0("Comps Min: ", fmt_money(comps$min, 2)), pos = 4, col = "#e5e7eb")
    }
  }

  output$ff_plot <- renderPlot({ plot_ff() })
  output$ff_plot_only <- renderPlot({ plot_ff() })
}

shinyApp(ui, server)

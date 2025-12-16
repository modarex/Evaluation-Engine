library(shiny)
library(dplyr)
library(quantmod)

`%||%` <- function(a, b) if (!is.null(a)) a else b

companyHeaderUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("hdr"))
}

companyHeaderServer <- function(id, selected_symbol, latest_search_df) {
  moduleServer(id, function(input, output, session) {

    get_price_yahoo <- function(symbol) {
      tryCatch({
        q <- quantmod::getQuote(symbol, src = "yahoo")
        if (!("Last" %in% colnames(q))) return(NA_real_)
        as.numeric(q$Last[1])
      }, error = function(e) NA_real_)
    }

    output$hdr <- renderUI({
      req(selected_symbol())
      sym <- selected_symbol()

      df <- latest_search_df()
      row <- NULL
      if (!is.null(df) && nrow(df) > 0 && "symbol" %in% names(df)) {
        row <- df %>% filter(symbol == sym) %>% slice(1)
        if (nrow(row) == 0) row <- NULL
      }

      company_name <- if (!is.null(row) && "name" %in% names(row)) row$name else sym
      exg <- if (!is.null(row) && "exchange" %in% names(row)) row$exchange else ""

      px <- get_price_yahoo(sym)
      price_txt <- if (is.na(px)) "Price unavailable (Yahoo)" else sprintf("$%.2f", px)

      div(
        style = "padding:12px; border:1px solid #ddd; background:#fff; border-radius:10px; margin-bottom:10px;",
        h3(style = "margin:0;", company_name),
        div(style = "color:#666; margin-top:4px;", paste0(sym, if (nzchar(exg)) paste0(" · ", exg) else "")),
        div(style = "margin-top:8px; font-size:18px;", strong(price_txt))
      )
    })
  })
}

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

fmp_base_url <- function() {
  base <- Sys.getenv("FMP_BASE_URL")
  if (!nzchar(base)) base <- "https://financialmodelingprep.com/stable"
  sub("/+$", "", base)
}

fmp_api_key <- function() Sys.getenv("FMP_API_KEY")

safe_json_parse <- function(txt) {
  tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE), error = function(e) NULL)
}

fmp_get <- function(endpoint, query = list(), timeout_sec = 15) {
  base <- fmp_base_url()
  url <- paste0(base, "/", gsub("^/+", "", endpoint))
  key <- fmp_api_key()

  if (!nzchar(key)) {
    return(list(ok = FALSE, status = NA, url = url, error = "FMP_API_KEY env var is not set.", data = NULL))
  }

  full_url <- httr::modify_url(url, query = c(query, apikey = key))
  resp <- tryCatch(
    httr::GET(full_url, httr::timeout(timeout_sec)),
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    return(list(ok = FALSE, status = NA, url = full_url, error = conditionMessage(resp), data = NULL))
  }

  status <- httr::status_code(resp)
  body_txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
  parsed <- safe_json_parse(body_txt)
  ok <- status >= 200 && status < 300

  err <- NULL
  if (!ok) {
    err <- parsed$error %||% if (nzchar(body_txt)) substr(body_txt, 1, 500) else paste0("HTTP ", status)
  }

  list(
    ok = ok,
    status = status,
    url = full_url,
    data = parsed,
    error = err
  )
}

is_locked <- function(res) {
  if (is.null(res)) return(FALSE)
  status <- res$status %||% NA_integer_
  status %in% c(401, 402, 403)
}

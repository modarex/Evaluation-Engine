library(httr)
library(jsonlite)

fmp_get <- function(path, query = list()) {
  base_url <- "https://financialmodelingprep.com/stable"
  api_key  <- Sys.getenv("FMP_API_KEY")

  if (!nzchar(api_key)) {
    stop("FMP_API_KEY is not set. Add it to your environment.")
  }

  query$apikey <- api_key

  url <- modify_url(base_url, path = path, query = query)

  resp <- GET(url)
  text <- content(resp, as = "text", encoding = "UTF-8")

  parsed <- tryCatch(
    fromJSON(text, flatten = TRUE),
    error = function(e) text
  )

  list(
    ok = status_code(resp) >= 200 && status_code(resp) < 300,
    status = status_code(resp),
    data = parsed,
    url = url
  )
}

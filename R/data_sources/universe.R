library(dplyr)
library(here)

source(here("R", "data_sources", "fmp.R"))
source(here("R", "data_sources", "cache.R"))

get_exchanges <- function() {
  key <- "fmp_available_exchanges"
  cached <- cache_read(key, max_age_hours = 72)
  if (!is.null(cached)) return(as_tibble(cached, .name_repair = "unique"))

  res <- fmp_get("available-exchanges", list())

  if (!isTRUE(res$ok)) return(NULL)

  cache_write(key, res$data)
  as_tibble(res$data, .name_repair = "unique")
}

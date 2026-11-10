# ---------------------------------------------------------------------------
# Small helpers shared by SixtyEight95 posts and infographics.
#   get_fred("AHETPI")  -> tibble(date, value) straight from FRED, no API key
# ---------------------------------------------------------------------------
library(tidyverse)
library(lubridate)

get_fred <- function(series_id) {
  url  <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  data <- read.csv(url, na.strings = ".")
  names(data) <- c("date", "value")
  data %>%
    as_tibble() %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(value))
}

dollars <- function(x) paste0("$", formatC(x, format = "f", digits = 2))

# SixtyEight95 brand colors
brand <- list(
  navy  = "#0F1B2D",
  teal  = "#2A9D8F",
  mint  = "#7FE0CF",
  amber = "#F4B942",
  red   = "#F06A6A",
  white = "#FFFFFF",
  muted = "#B8C4D6",
  grid  = "#2A3A52",
  band  = "#1F4E5A",
  font  = "Bahnschrift"
)

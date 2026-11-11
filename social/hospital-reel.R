# ---------------------------------------------------------------------------
# Reel for the hospital bills post:  hospital-reel.mp4  (1080 x 1920, ~31 s)
# Run from this folder:  source("hospital-reel.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/hospital_data.R")
source("../R/reel_helpers.R")

k <- function(x) paste0("$", formatC(x / 1000, format = "f", digits = 1), "k")
times <- function(x) paste0(formatC(x, format = "f", digits = 1), "×")

hip <- procedures %>%
  filter(procedure == "Hip replacement", country %in% c("Spain", "Germany", "UK", "USA")) %>%
  arrange(price_usd) %>%
  mutate(label = ifelse(country == "USA", "U.S.", country),
         color = ifelse(country == "USA", brand$red, "#8FA9C2"))

reasons <- list(
  list(n = "1", big = paste0(facts$private_vs_medicare_pct, "%"), text = "Private insurers pay\nthis much of Medicare's rate"),
  list(n = "2", big = paste0(facts$metros_one_or_two_pct, "%"),  text = "of metro areas have just\n1 or 2 hospital systems"),
  list(n = "3", big = paste0("~", facts$admin_share_pct, "%"),   text = "of hospital budgets\ngo to administration"),
  list(n = "4", big = paste0("$", facts$list_per_dollar_cost),   text = "list price for every\n$1 of actual cost")
)

draw_frame <- function(t) {
  background()

  # Scene 1: hook (0 - 3.5s)
  a <- fade(t, 0, 3.5)
  txt("Why are hospital bills\nso insanely expensive?", 0.5, 0.6, 44, brand$white, "bold", a)
  txt("Let's follow the money.", 0.5, 0.45, 38, brand$amber, "bold", a * appear(t, 1.7))

  # Scene 2: same hip replacement (3.5 - 10.5s)
  a <- fade(t, 3.5, 10.5)
  if (a > 0) {
    txt("THE SAME HIP REPLACEMENT", 0.5, 0.78, 32, brand$white, "bold", a)
    txt("(what private insurers paid, 2022)", 0.5, 0.745, 22, brand$muted, "plain", a)
    x0 <- 0.3; max_w <- 0.44
    for (i in seq_len(nrow(hip))) {
      r <- hip[i, ]
      y <- 0.64 - (i - 1) * 0.09
      p <- appear(t, 4.3 + (i - 1) * 0.7, if (r$country == "USA") 1.3 else 0.6)
      w <- max(r$price_usd / max(hip$price_usd) * max_w * p, 0.004)
      txt(r$label, x0 - 0.02, y, 30, brand$white, "plain", a, just = "right")
      grid.rect(x0, y, width = w, height = 0.055, just = "left", gp = gpar(fill = r$color, col = NA, alpha = a))
      txt(k(r$price_usd), x0 + w + 0.015, y, 28, r$color, "bold", a * p, just = "left")
    }
    txt("Same surgery. Same hip.", 0.5, 0.25, 32, brand$amber, "bold", a * appear(t, 8))
  }

  # Scene 3: four reasons (10.5 - 22.5s), one every 3 seconds
  for (i in seq_along(reasons)) {
    s <- 10.5 + (i - 1) * 3
    a <- fade(t, s, s + 3)
    r <- reasons[[i]]
    txt(paste0("REASON ", r$n, " OF 4"), 0.5, 0.68, 30, brand$muted, "bold", a)
    txt(r$big, 0.5, 0.56, 110, brand$amber, "bold", a)
    txt(r$text, 0.5, 0.42, 36, brand$white, "bold", a)
  }

  # Scene 4: since 1980 (22.5 - 26s)
  a <- fade(t, 22.5, 26)
  txt("SINCE 1980", 0.5, 0.68, 32, brand$muted, "bold", a)
  txt(paste0("Hospital prices: ", times(growth_1980$hospital)), 0.5, 0.58, 42, brand$red, "bold", a)
  txt(paste0("Paychecks: ", times(growth_1980$wages)), 0.5, 0.5, 42, brand$white, "bold", a * appear(t, 23.3))
  txt(paste0(facts$debt_people_millions, " million adults owe\nmedical debt."), 0.5, 0.38, 34, brand$amber, "bold",
      a * appear(t, 24.2))

  # Scene 5: end card
  end_card(t, 26)

  txt("Data: iFHP/HCCI, RAND, KFF, Health Affairs, BLS", 0.5, 0.05, 18, brand$muted)
}

render_reel(draw_frame, "hospital-reel.mp4", seconds = 29)

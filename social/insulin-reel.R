# ---------------------------------------------------------------------------
# Reel for the insulin post:  insulin-reel.mp4  (1080 x 1920, about 29 seconds)
# Run from this folder:  source("insulin-reel.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/insulin_data.R")
source("../R/reel_helpers.R")

vial <- function(x) paste0("$", round(x))

bars <- bind_rows(
  tibble(label = "Cost to make", value = cost_high, shown = paste0("$", round(cost_low), "–", round(cost_high)),
         color = brand$mint),
  rand %>% filter(country %in% c("UK", "France", "Germany", "Japan", "Canada")) %>% arrange(price_per_vial) %>%
    transmute(label = country, value = price_per_vial, shown = vial(price_per_vial), color = "#8FA9C2"),
  tibble(label = "U.S.", value = us_list_vial, shown = vial(us_list_vial), color = brand$red)
)

draw_frame <- function(t) {
  background()

  # Scene 1: hook (0 - 4s)
  a <- fade(t, 0, 4)
  txt("Insulin costs about\n$5 a vial to make.", 0.5, 0.6, 46, brand$white, "bold", a)
  txt(paste0("So why was the U.S.\nprice ", vial(us_list_vial), "?"), 0.5, 0.43, 42, brand$amber, "bold", a * appear(t, 1.8))

  # Scene 2: the bars (4 - 13s)
  a <- fade(t, 4, 13)
  if (a > 0) {
    txt("ONE VIAL OF INSULIN", 0.5, 0.8, 32, brand$white, "bold", a)
    txt("(1,000 units, 2022 prices before discounts)", 0.5, 0.765, 22, brand$muted, "plain", a)
    x0 <- 0.34; max_w <- 0.45
    for (i in seq_len(nrow(bars))) {
      r <- bars[i, ]
      y <- 0.68 - (i - 1) * 0.065
      p <- appear(t, 4.8 + (i - 1) * 0.6, if (r$label == "U.S.") 1.4 else 0.6)
      w <- max(r$value / max(bars$value) * max_w * p, 0.004)
      txt(r$label, x0 - 0.02, y, 26, brand$white, "plain", a, just = "right")
      grid.rect(x0, y, width = w, height = 0.04, just = "left",
                gp = gpar(fill = r$color, col = NA, alpha = a))
      txt(r$shown, x0 + w + 0.015, y, 24, r$color, "bold", a * p, just = "left")
    }
    txt("Every country pays a markup.\nThe U.S. was in a different league.", 0.5, 0.22, 28, brand$amber, "bold",
        a * appear(t, 10))
  }

  # Scene 3: the twist (13 - 19s)
  a <- fade(t, 13, 19)
  txt("BUT...", 0.5, 0.7, 34, brand$muted, "bold", a)
  txt(paste0(vial(us_list_vial), " was the sticker price."), 0.5, 0.62, 38, brand$white, "bold", a)
  txt(paste0("After secret drugmaker\ndiscounts: about ", vial(us_net_vial), " a vial."), 0.5, 0.51, 36, brand$amber,
      "bold", a * appear(t, 14.2))
  txt(paste0("Still ", round(us_net_index_pct / 100, 1), "× other wealthy countries.\n",
             "And the uninsured often\npaid the full sticker price."), 0.5, 0.37, 30, brand$white, "plain",
      a * appear(t, 15.8))

  # Scene 4: what's changed (19 - 25.5s)
  a <- fade(t, 19, 25.5)
  txt("SINCE 2023", 0.5, 0.72, 34, brand$mint, "bold", a)
  txt("List prices cut 65–78%", 0.5, 0.63, 34, brand$white, "bold", a * appear(t, 19.6))
  txt("$35/month cap on Medicare", 0.5, 0.55, 34, brand$white, "bold", a * appear(t, 20.6))
  txt("California insulin: ~$11 a pen", 0.5, 0.47, 32, brand$white, "bold", a * appear(t, 21.6))
  txt("The gap is closing.\nIt hasn't closed.", 0.5, 0.36, 34, brand$amber, "bold", a * appear(t, 22.8))

  # Scene 5: end card
  end_card(t, 25.5)

  txt("Data: RAND (2022), BMJ Global Health (2018)", 0.5, 0.05, 18, brand$muted)
}

render_reel(draw_frame, "insulin-reel.mp4", seconds = 29)

# ---------------------------------------------------------------------------
# Reel for the EV cost post:  ev-cost-reel.mp4  (1080 x 1920, ~29 s)
# Run from this folder:  source("ev-cost-reel.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/ev_data.R")
source("../R/reel_helpers.R")

usd <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
s_ev <- cost_row("Medium sedan", "Electric"); s_gas <- cost_row("Medium sedan", "Gas")
pt_colors <- c(Electric = brand$mint, Gas = brand$red, Hybrid = brand$amber)

draw_frame <- function(t) {
  background()

  # Scene 1: hook (0 - 3.5s)
  a <- fade(t, 0, 3.5)
  txt("Does an electric car\nactually save you money?", 0.5, 0.6, 42, brand$white, "bold", a)
  txt("We did the math, per year.", 0.5, 0.45, 36, brand$amber, "bold", a * appear(t, 1.7))

  # Scene 2: running costs (3.5 - 9s)
  a <- fade(t, 3.5, 9)
  txt("FUEL + MAINTENANCE", 0.5, 0.72, 32, brand$muted, "bold", a)
  txt("(medium sedan, per year)", 0.5, 0.685, 22, brand$muted, "plain", a)
  txt(paste0("Gas: ", usd(s_gas$running)), 0.5, 0.6, 46, brand$red, "bold", a * appear(t, 4.2))
  txt(paste0("EV: ", usd(s_ev$running)), 0.5, 0.51, 46, brand$mint, "bold", a * appear(t, 5))
  txt(paste0("EV saves ", usd(s_gas$running - s_ev$running), " a year!"), 0.5, 0.39, 36, brand$white, "bold",
      a * appear(t, 6.2))

  # Scene 3: but... (9 - 15s)
  a <- fade(t, 9, 15)
  txt("BUT...", 0.5, 0.74, 34, brand$muted, "bold", a)
  txt("EVs lose value fast.", 0.5, 0.66, 42, brand$white, "bold", a)
  txt("Depreciation per year:", 0.5, 0.57, 30, brand$muted, "plain", a * appear(t, 9.8))
  txt(paste0("Gas: ", usd(s_gas$depreciation)), 0.5, 0.5, 42, brand$red, "bold", a * appear(t, 10.4))
  txt(paste0("EV: ", usd(s_ev$depreciation)), 0.5, 0.43, 42, brand$mint, "bold", a * appear(t, 11.1))
  txt("Plus higher insurance, EV fees,\nand no more $7,500 tax credit.", 0.5, 0.31, 30, brand$amber, "bold",
      a * appear(t, 12.3))

  # Scene 4: total cost bars (15 - 23.5s)
  a <- fade(t, 15, 23.5)
  if (a > 0) {
    txt("TOTAL COST PER YEAR", 0.5, 0.8, 32, brand$white, "bold", a)
    txt("(15,000 miles, new 2026 models)", 0.5, 0.765, 22, brand$muted, "plain", a)
    classes <- levels(aaa$class)
    for (ci in seq_along(classes)) {
      rows <- aaa %>% filter(class == classes[ci]) %>% arrange(powertrain)
      y0 <- 0.69 - (ci - 1) * 0.12
      txt(classes[ci], 0.08, y0 + 0.03, 24, brand$white, "bold", a, just = "left")
      for (pi in seq_len(nrow(rows))) {
        r <- rows[pi, ]; y <- y0 - (pi - 1) * 0.027
        p <- appear(t, 15.6 + (ci - 1) * 0.6, 0.8)
        w <- r$total_per_year / 18000 * 0.6 * p
        grid.rect(0.08, y, width = max(w, 0.002), height = 0.022, just = "left",
                  gp = gpar(fill = pt_colors[[as.character(r$powertrain)]], col = NA, alpha = a))
        txt(paste0("$", formatC(r$total_per_year / 1000, format = "f", digits = 1), "k"), 0.09 + w, y, 18,
            pt_colors[[as.character(r$powertrain)]], "bold", a * p, just = "left")
      }
    }
    txt("Electric", 0.25, 0.2, 24, brand$mint, "bold", a)
    txt("Gas", 0.5, 0.2, 24, brand$red, "bold", a)
    txt("Hybrid", 0.75, 0.2, 24, brand$amber, "bold", a)
  }

  # Scene 5: verdict (23.5 - 27s)
  a <- fade(t, 23.5, 27)
  txt("THE VERDICT", 0.5, 0.7, 32, brand$muted, "bold", a)
  txt("Hybrids win every class.", 0.5, 0.61, 40, brand$amber, "bold", a)
  txt("EVs win if you charge at home,\ndrive a lot, keep it long,\nor buy used.", 0.5, 0.47, 32, brand$white, "bold",
      a * appear(t, 24.4))

  end_card(t, 27)
  txt("Data: AAA Your Driving Costs 2026", 0.5, 0.05, 18, brand$muted)
}

render_reel(draw_frame, "ev-cost-reel.mp4", seconds = 30)

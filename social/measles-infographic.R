# Infographic: measles is back (measles-infographic.png, 1080 x 1350)
# Run from this folder:  source("measles-infographic.R")
source("../R/fred.R")
source("../R/measles_data.R")
library(ragg)
library(grid)

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}

agg_png("measles-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("MEASLES IS BACK", 0.5, 0.93, 28, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.895, 0.895), gp = gpar(col = brand$amber, lwd = 4))
txt(paste0("As of Nov 17, the U.S. is no longer \"measles-free,\" a status it held since ", elimination_since, "."),
    0.5, 0.865, 10.5, brand$mint)

# bar chart of yearly cases
txt("MEASLES CASES PER YEAR", 0.5, 0.82, 11.5, brand$amber, "bold")
box <- list(x0 = 0.1, x1 = 0.92, y0 = 0.5, y1 = 0.78)
n <- nrow(measles_cases); bw <- (box$x1 - box$x0) / n
ymax <- 5600
for (i in seq_len(n)) {
  v <- measles_cases$cases[i]; yr <- measles_cases$year[i]
  col <- if (yr == 2026) brand$red else if (yr == 2025) brand$amber else "#3A5A73"
  grid.rect(box$x0 + (i - 0.5) * bw, box$y0, width = bw * 0.75, height = v / ymax * (box$y1 - box$y0),
            just = c("center", "bottom"), gp = gpar(fill = col, col = NA))
  if (yr %in% c(2000, 2010, 2019, 2026)) txt(yr, box$x0 + (i - 0.5) * bw, box$y0 - 0.015, 8.5, brand$muted)
}
txt(format(cases_2026, big.mark = ","), box$x1 - bw * 0.5 - 0.005, box$y0 + cases_2026 / ymax * (box$y1 - box$y0) + 0.014, 13, brand$red, "bold")
txt("so far in 2026", box$x1 - bw * 0.5 - 0.045, box$y0 + cases_2026 / ymax * (box$y1 - box$y0) - 0.012, 8.5, brand$white, just = "right")
txt("1,274 in 2019", box$x0 + 19.5 * bw, box$y0 + 1274 / ymax * (box$y1 - box$y0) + 0.012, 8, brand$muted)
txt("2,242 in 2025", box$x0 + 25.5 * bw - 0.025, box$y0 + 2242 / ymax * (box$y1 - box$y0) + 0.012, 8, brand$amber, just = "right")
txt(paste0("2000–2018 average: about ", round(avg_2000_2018), " a year"), 0.3, 0.7, 9.5, brand$muted)

# tiles
tiles <- tribble(~x, ~value, ~label, ~col,
  0.2, paste0(hosp_2026, "+"), "hospitalized\nin 2026", brand$white,
  0.5, paste0(unvax_share_2026, "%"), "of cases were unvaccinated\nor status unknown", brand$amber,
  0.8, paste0(states_2026, " + DC"), "states with cases\nthis year", brand$white)
for (i in 1:3) {
  txt(tiles$value[i], tiles$x[i], 0.42, 24, tiles$col[i], "bold")
  txt(tiles$label[i], tiles$x[i], 0.37, 9.3, brand$muted)
}

# kindergarten coverage
grid.rect(x = 0.5, y = 0.225, width = 0.88, height = 0.2, gp = gpar(fill = brand$band, col = NA))
txt("WHY: FEWER KIDS ARE VACCINATED", 0.5, 0.305, 11.5, brand$amber, "bold")
k1 <- kinder[1, ]; k2 <- kinder[nrow(kinder), ]
txt(paste0(k1$mmr, "%"), 0.3, 0.255, 24, brand$white, "bold")
txt(paste0("of kindergartners had\nboth MMR shots (", k1$school_year, ")"), 0.3, 0.205, 9, brand$muted)
txt(paste0(k2$mmr, "%"), 0.7, 0.255, 24, brand$red, "bold")
txt(paste0("in ", k2$school_year, ". Exemptions hit\na record ", k2$exempt, "%."), 0.7, 0.205, 9, brand$muted)
txt("→", 0.5, 0.255, 22, brand$muted)
txt(paste0("Experts say about ", herd_target, "% is needed to stop outbreaks. Only ", states_at_target, " states are there."),
    0.5, 0.15, 9.5, brand$white, "bold")
txt(paste0("Two MMR doses are about ", mmr_effective, "% effective. Not sure you're covered? Ask your doctor."),
    0.5, 0.115, 9.5, brand$mint, "bold")

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: CDC (cases, SchoolVaxView); PAHO. 2026 = through mid-November.", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved measles-infographic.png")

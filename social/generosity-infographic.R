# Infographic: Americans are more generous than you think (generosity-infographic.png, 1080 x 1350)
# Run from this folder:  source("generosity-infographic.R")
source("../R/fred.R")
source("../R/generosity_data.R")
library(ragg)
library(grid)

warm <- "#F4B942"; rose <- "#F28C6F"
txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}
tile <- function(x, y, value, label, color) {
  grid.roundrect(x, y, width = 0.42, height = 0.135, r = unit(0.015, "npc"),
                 gp = gpar(fill = "#16263D", col = NA))
  txt(value, x, y + 0.025, 26, color, "bold")
  txt(label, x, y - 0.035, 9.8, brand$white)
}

agg_png("generosity-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("AMERICANS ARE MORE\nGENEROUS THAN YOU THINK", 0.5, 0.92, 24, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.862, 0.862), gp = gpar(col = warm, lwd = 4))
txt("A little Thanksgiving good news, straight from the data.", 0.5, 0.835, 11, brand$mint)

tile(0.27, 0.735, paste0("$", round(giving_total_2025_b), "B"),
     "given to charity in 2025,\na new record", warm)
tile(0.73, 0.735, paste0("$", formatC(giving_per_day_b, format = "f", digits = 1), "B"),
     "given away every\nsingle day", warm)
tile(0.27, 0.585, paste0(round(volunteers_m, 1), "M"),
     paste0("people volunteered\n(", volunteer_rate, "% of Americans 16+)"), brand$mint)
tile(0.73, 0.585, paste0(volunteer_hours_b, "B hrs"),
     paste0("of volunteer time, like\n", formatC(fulltime_jobs_equiv_m, format = "f", digits = 1), "M full-time jobs"), brand$mint)
tile(0.27, 0.435, paste0(round(neighbors_helped_m, 1), "M"),
     "helped a neighbor\n(more than half of us)", rose)
tile(0.73, 0.435, paste0(round(giving_tuesday_2025_m), "M"),
     paste0("gave on Giving Tuesday,\n$", giving_tuesday_2025_b, "B in one day"), rose)

# volunteering bounce-back mini chart
txt("VOLUNTEERING IS BOUNCING BACK", 0.5, 0.33, 11, warm, "bold")
bx <- c(0.3, 0.5, 0.7)
for (i in 1:3) {
  h <- volunteer_trend$rate[i] / 32 * 0.095
  grid.rect(bx[i], 0.18, width = 0.12, height = h, just = c("center", "bottom"),
            gp = gpar(fill = ifelse(i == 3, brand$mint, "#3A5A73"), col = NA))
  txt(paste0(formatC(volunteer_trend$rate[i], format = "f", digits = 1), "%"), bx[i], 0.18 + h + 0.015, 12, brand$white, "bold")
  txt(volunteer_trend$year[i], bx[i], 0.165, 10, brand$muted)
}
txt("Share of Americans who volunteered:\nthe biggest jump since tracking began in 2002.", 0.5, 0.125, 8.5, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt(paste("Giving Tuesday:", giving_tuesday_2026), 0.95, 0.048, 12, warm, "bold", just = "right")
txt("Data: Giving USA 2026; U.S. Census Bureau & AmeriCorps; GivingTuesday", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved generosity-infographic.png")

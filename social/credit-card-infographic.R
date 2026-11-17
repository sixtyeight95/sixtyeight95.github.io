# Infographic: putting the holidays on a credit card (credit-card-infographic.png, 1080 x 1350)
# Run from this folder:  source("credit-card-infographic.R")
source("../R/fred.R")
source("../R/credit_card_data.R")
library(ragg)
library(grid)

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}
usd <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
yrs <- function(m) if (m >= 24) paste0(formatC(m / 12, format = "f", digits = 1), " years") else paste(m, "months")

agg_png("credit-card-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("PUTTING THE HOLIDAYS\nON A CREDIT CARD?", 0.5, 0.92, 24, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.862, 0.862), gp = gpar(col = brand$amber, lwd = 4))
txt(paste0("Here's what ", usd(holiday_bill), " in gifts really costs at today's ",
           formatC(cc_now$value, format = "f", digits = 1), "% average card rate."), 0.5, 0.835, 10.5, brand$mint)

txt("IF YOU ONLY PAY THE MINIMUM", 0.5, 0.78, 11.5, brand$amber, "bold")
txt(usd(min_case$total), 0.5, 0.715, 52, brand$red, "bold")
txt(paste0("total paid, including ", usd(min_case$interest), " in interest"), 0.5, 0.655, 11, brand$white, "bold")
txt(paste0("and it takes about ", yrs(min_case$months), " to pay off"), 0.5, 0.63, 11, brand$white)

# plans table
txt("PAY MORE, PAY WAY LESS", 0.5, 0.57, 11.5, brand$amber, "bold")
hdr_y <- 0.54
txt("Monthly payment", 0.12, hdr_y, 9.5, brand$muted, "bold", just = "left")
txt("Paid off in", 0.62, hdr_y, 9.5, brand$muted, "bold")
txt("Interest", 0.86, hdr_y, 9.5, brand$muted, "bold")
for (i in seq_len(nrow(scenarios))) {
  y <- hdr_y - 0.04 * i
  col <- if (i == 1) brand$red else brand$white
  txt(scenarios$plan[i], 0.12, y, 11, col, "bold", just = "left")
  txt(yrs(scenarios$months[i]), 0.62, y, 11, col)
  txt(usd(scenarios$interest[i]), 0.86, y, 11, if (i == 1) brand$red else brand$mint, "bold")
}

# rate history
grid.rect(x = 0.5, y = 0.245, width = 0.88, height = 0.19, gp = gpar(fill = brand$band, col = NA))
txt("CARD RATES ARE NEAR RECORD HIGHS", 0.5, 0.315, 11.5, brand$amber, "bold")
txt(paste0(formatC(cc_2019, format = "f", digits = 1), "%"), 0.3, 0.265, 26, brand$white, "bold")
txt("average in 2019", 0.3, 0.225, 9.5, brand$muted)
txt("→", 0.5, 0.265, 22, brand$muted)
txt(paste0(formatC(cc_now$value, format = "f", digits = 1), "%"), 0.7, 0.265, 26, brand$red, "bold")
txt(paste0("now (", format(cc_now$date, "%B %Y"), ")"), 0.7, 0.225, 9.5, brand$muted)
txt("And the Fed has started raising rates again.", 0.5, 0.18, 9.5, brand$white, "bold")

txt(paste0("Every day a ", usd(holiday_bill), " balance sits there, it costs about ", round(interest_per_day * 100), " cents in interest."),
    0.5, 0.12, 9.5, brand$mint, "bold")

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: Federal Reserve (rate on accounts charged interest). Minimum = interest + 1% of balance, $25 floor.", 0.5, 0.012, 7, brand$muted)
dev.off(); message("Saved credit-card-infographic.png")

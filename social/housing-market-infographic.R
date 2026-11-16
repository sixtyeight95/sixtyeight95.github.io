# Infographic: home prices dipping - good time to buy? (housing-market-infographic.png, 1080 x 1350)
# Run from this folder:  source("housing-market-infographic.R")
source("../R/fred.R")
source("../R/housing_market_data.R")
library(ragg)
library(grid)

usd <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
pct <- function(x) paste0(ifelse(x > 0, "+", ""), formatC(x, format = "f", digits = 1), "%")
h <- listing_hist %>% filter(date >= as.Date("2017-01-01"))
pk <- h %>% slice_max(payment, n = 1); lo <- h %>% filter(date >= as.Date("2020-06-01")) %>% slice_min(payment, n = 1)
nw <- tail(h, 1)

chart <- ggplot(h, aes(date, payment)) +
  geom_area(fill = brand$band, alpha = 0.6) +
  geom_line(color = brand$amber, linewidth = 1.3) +
  geom_point(data = bind_rows(lo, pk, nw), color = brand$white, size = 3) +
  geom_text(data = bind_rows(lo, pk, nw) %>% mutate(lab = paste0(format(date, "%b %Y"), "\n", usd(payment)),
                                                    hj = c(0.5, 0.5, 0.85)),
            aes(label = lab, hjust = hj), vjust = -0.45, color = brand$white, family = brand$font,
            fontface = "bold", size = 3.9, lineheight = 0.9) +
  scale_y_continuous(limits = c(0, 3300), breaks = seq(0, 3000, 1000), labels = function(x) usd(x), expand = c(0, 0)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.04))) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = brand$grid), axis.text = element_text(color = brand$muted))

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}

agg_png("housing-market-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("HOME PRICES ARE DIPPING.\nIS NOW A GOOD TIME TO BUY?", 0.5, 0.915, 23, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt("Monthly mortgage payment on the median listed home (10% down, 30-year fixed).", 0.5, 0.825, 10.5, brand$mint)

print(chart, vp = viewport(x = 0.5, y = 0.635, width = 0.92, height = 0.33))

tiles <- tribble(~x, ~value, ~label, ~col,
  0.14, pct(m("MEDLISPRIUS")$change), "median list\nprice", brand$mint,
  0.38, paste0(formatC(pay_latest$rate, format = "f", digits = 2), "%"), paste0("mortgage rate\n(was ", formatC(pay_yearago$rate, format = "f", digits = 2), "%)"), brand$red,
  0.62, paste0("+", usd(pay_now - pay_then)), "per month vs.\na year ago", brand$red,
  0.86, pct(m("ACTLISCOUUS")$change), "homes for\nsale", brand$mint)
txt("CHANGE FROM A YEAR AGO", 0.5, 0.44, 11, brand$amber, "bold")
for (i in 1:4) {
  txt(tiles$value[i], tiles$x[i], 0.395, 19, tiles$col[i], "bold")
  txt(tiles$label[i], tiles$x[i], 0.345, 9.5, brand$muted)
}

grid.rect(x = 0.5, y = 0.215, width = 0.88, height = 0.135, gp = gpar(fill = brand$band, col = NA))
txt("WHAT THE NUMBERS SAY", 0.5, 0.265, 11, brand$amber, "bold")
txt(paste0("Prices dipped a little, but mortgage rates rose more, so the typical monthly payment is\n",
           "higher than a year ago. The upside: buyers have more choices and leverage than in years\n",
           "(more listings, sales down ", round(abs(m("EXHOSLUSM495S")$change)), "%, homes sitting ", round(m("MEDDAYONMARUS")$now),
           " days). Rents are up ", formatC(m("CUSR0000SEHA")$change, format = "f", digits = 1), "% too."),
    0.5, 0.205, 9.2, brand$white)

txt(paste0("Median listing price and inventory: Realtor.com. Mortgage rate: Freddie Mac, ", format(pay_latest$date, "%B"),
           " average (latest week: ", formatC(mortgage$now, format = "f", digits = 2), "%).\nPrincipal and interest only; taxes and insurance add more. ",
           "Broadest price index (Case-Shiller) is still up ", formatC(m("CSUSHPINSA")$change, format = "f", digits = 1),
           "% over the year. Not financial advice."), 0.5, 0.115, 7, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: Realtor.com, Freddie Mac, S&P Case-Shiller, NAR, BLS via FRED", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved housing-market-infographic.png")

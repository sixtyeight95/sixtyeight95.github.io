# ---------------------------------------------------------------------------
# Infographic: the same hospital procedure, U.S. vs. other wealthy countries
#   hospital-infographic.png  1080 x 1350
#
# Run from this folder:  source("hospital-infographic.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/hospital_data.R")
library(ragg)
library(grid)

show_procedures <- c("Hip replacement", "Knee replacement", "Appendectomy", "C-section", "Vaginal birth")
show_countries  <- c("USA", "UK", "Germany", "Spain")

chart_data <- procedures %>%
  filter(procedure %in% show_procedures, country %in% show_countries) %>%
  mutate(procedure = factor(procedure, levels = rev(show_procedures)),
         country = factor(country, levels = show_countries))

ranges <- chart_data %>% group_by(procedure) %>%
  summarise(lo = min(price_usd), hi = max(price_usd), us = price_usd[country == "USA"], .groups = "drop")

k <- function(x) paste0("$", formatC(x / 1000, format = "f", digits = 1), "k")

country_colors <- c(USA = brand$red, UK = brand$amber, Germany = brand$mint, Spain = "#8FA9C2")

chart <- ggplot() +
  geom_segment(data = ranges, aes(x = lo, xend = hi, y = procedure, yend = procedure),
               color = brand$grid, linewidth = 3) +
  geom_point(data = chart_data, aes(price_usd, procedure, color = country), size = 5.5) +
  geom_text(data = ranges, aes(us, procedure, label = k(us)), nudge_y = 0.33,
            color = brand$red, family = brand$font, fontface = "bold", size = 4.6) +
  scale_color_manual(values = country_colors, guide = "none") +
  scale_x_continuous(limits = c(0, 32000), breaks = seq(0, 30000, 10000),
                     labels = function(x) paste0("$", x / 1000, "k")) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background  = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = brand$grid),
        axis.text.y = element_text(color = brand$white, size = 14),
        axis.text.x = element_text(color = brand$muted))

# --- words ------------------------------------------------------------------
headline <- "THE SAME SURGERY,\nVERY DIFFERENT BILLS"
subhead  <- "What private insurers paid for the same hospital procedure in 2022."

tiles <- tribble(
  ~x,     ~value,                                               ~label,
  0.14,   paste0(facts$private_vs_medicare_pct, "%"),           "of Medicare's rate\nis what employer\nplans pay",
  0.38,   paste0(facts$metros_one_or_two_pct, "%"),             "of metro areas have\njust 1 or 2 hospital\nsystems",
  0.62,   paste0(facts$admin_share_pct, "%"),                   "of hospital budgets\ngo to admin\n(2x Canada)",
  0.86,   paste0("$", facts$list_per_dollar_cost),              "list price for\nevery $1 of\nactual cost"
)

how_note <- paste0("Procedure prices: median paid by private insurers, iFHP/HCCI 2024 report (2022 data). Medicare ratio: RAND (2022). ",
                   "Hospital markets: KFF.\nAdmin share: Health Affairs (2014, 2011 data; authors favor single-payer). ",
                   "List price markup: Health Affairs (2016, 2013 data).")
link_text <- "Full story: sixtyeight95.github.io"
source_tx <- "Data: iFHP/HCCI, RAND, KFF, Health Affairs"

# --- draw the page ------------------------------------------------------------
txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color,
                      fontface = face, lineheight = 1))
}

agg_png("hospital-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = brand$navy, col = NA))

txt(headline, 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt(subhead, 0.5, 0.825, 10.5, brand$mint)

legend_x <- c(0.27, 0.42, 0.58, 0.73)
for (i in seq_along(show_countries)) {
  grid.circle(legend_x[i] - 0.035, 0.79, r = unit(0.006, "npc"),
              gp = gpar(fill = country_colors[[show_countries[i]]], col = NA))
  txt(ifelse(show_countries[i] == "USA", "U.S.", show_countries[i]), legend_x[i] - 0.02, 0.79, 11,
      country_colors[[show_countries[i]]], "bold", just = "left")
}

print(chart, vp = viewport(x = 0.5, y = 0.58, width = 0.92, height = 0.37))

txt("WHERE THE MONEY GOES", 0.5, 0.36, 11.5, brand$amber, "bold")
for (i in seq_len(nrow(tiles))) {
  txt(tiles$value[i], tiles$x[i], 0.31, 20, brand$white, "bold")
  txt(tiles$label[i], tiles$x[i], 0.245, 8.5, brand$muted)
}

grid.rect(x = 0.5, y = 0.165, width = 0.88, height = 0.05, gp = gpar(fill = brand$band, col = NA))
txt(paste0("Biggest gap: heart bypass. $", formatC(procedures$price_usd[procedures$procedure == "Heart bypass" & procedures$country == "USA"], big.mark = ",", format = "d"),
           " in the U.S. vs. $", formatC(procedures$price_usd[procedures$procedure == "Heart bypass" & procedures$country == "Germany"], big.mark = ",", format = "d"),
           " in Germany."), 0.5, 0.165, 10, brand$white, "bold")

txt(how_note, 0.5, 0.11, 6.6, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt(link_text, 0.95, 0.048, 12, brand$mint, just = "right")
txt(source_tx, 0.5, 0.012, 7.5, brand$muted)
dev.off()

message("Saved hospital-infographic.png")

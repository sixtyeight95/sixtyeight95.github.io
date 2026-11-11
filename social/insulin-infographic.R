# ---------------------------------------------------------------------------
# Infographic: what one vial of insulin costs to make vs. what countries pay
#   insulin-infographic.png  1080 x 1350
#
# Run from this folder:  source("insulin-infographic.R")
# ---------------------------------------------------------------------------
source("../R/fred.R")
source("../R/insulin_data.R")
library(ragg)
library(grid)

bar_colors <- c(cost = brand$mint, country = "#6C8BA8", average = brand$muted,
                us_net = brand$amber, us_list = brand$red)

chart_data <- country_chart %>%
  mutate(label = factor(label, levels = rev(label)),
         value_label = ifelse(group == "cost",
                              paste0("$", round(low), "–", round(high)),
                              paste0("$", round(value))))

chart <- ggplot(chart_data, aes(x = value, y = label, fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(x = pmax(value, ifelse(is.na(high), 0, high)) + 4, label = value_label, color = group),
            hjust = 0, family = brand$font, fontface = "bold", size = 4.6) +
  scale_fill_manual(values = bar_colors, guide = "none") +
  scale_color_manual(values = c(bar_colors[c("cost", "average", "us_net", "us_list")], country = brand$white),
                     guide = "none") +
  scale_x_continuous(limits = c(0, 265), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = brand$font, base_size = 14) +
  theme(plot.background  = element_rect(fill = brand$navy, color = NA),
        panel.background = element_rect(fill = brand$navy, color = NA),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = brand$white, size = 13.5))

# --- words ------------------------------------------------------------------
headline <- "WHAT A VIAL OF\nINSULIN REALLY COSTS"
subhead  <- paste0("One standard vial: 1,000 units, about 25 days at a typical 40 units a day.\n",
                   "Cost to make it vs. what countries paid in 2022.")

callout_t <- "THE CATCH"
callout_b <- paste0("The U.S. list price was never what insurers paid. After secret drugmaker discounts,\n",
                    "the U.S. still paid about ", round(us_net_index_pct / 100, 1),
                    "× other wealthy countries. Since 2023, the big three makers cut list\n",
                    "prices 65–78%, and Medicare caps insulin at $35 a month.")

how_note <- paste0("Country prices: average manufacturer price across all insulin types, RAND (2022 data). ",
                   "*U.S. discount of ", us_gross_to_net_discount * 100, "% estimated by RAND.\n",
                   "Cost to make: estimate including a 20% profit margin, Gotham et al., BMJ Global Health (2018). ",
                   "Excludes original R&D and pharmacy markups.")
link_text <- "Full story: sixtyeight95.github.io"
source_tx <- "Data: RAND Corporation (IQVIA data); BMJ Global Health"

# --- draw the page ------------------------------------------------------------
txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color,
                      fontface = face, lineheight = 1))
}

agg_png("insulin-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = brand$navy, col = NA))

txt(headline, 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt(subhead, 0.5, 0.815, 10.5, brand$mint)

print(chart, vp = viewport(x = 0.5, y = 0.55, width = 0.92, height = 0.45))

grid.rect(x = 0.5, y = 0.235, width = 0.9, height = 0.11, gp = gpar(fill = brand$band, col = NA))
txt(callout_t, 0.5, 0.27, 11, brand$amber, "bold")
txt(callout_b, 0.5, 0.225, 9.3, brand$white)

txt(how_note, 0.5, 0.135, 6.8, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt(link_text, 0.95, 0.048, 12, brand$mint, just = "right")
txt(source_tx, 0.5, 0.012, 7.5, brand$muted)
dev.off()

message("Saved insulin-infographic.png")

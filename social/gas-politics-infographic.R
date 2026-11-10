# ---------------------------------------------------------------------------
# Infographic for the post "Does the President Set Gas Prices?"
#   gas-politics-infographic.png  1080 x 1350  (Facebook / Instagram portrait)
#
# Run from this folder:  source("gas-politics-infographic.R")
# Text lives in the "words" block; numbers are calculated from the data.
# ---------------------------------------------------------------------------

gas_dir <- "../../gas_forecasting"
source(file.path(gas_dir, "R", "gas_data_prep.R"))
library(ragg)
library(grid)

# --- brand ------------------------------------------------------------------
background <- "#0F1B2D"
teal       <- "#7FE0CF"
amber      <- "#F4B942"
white      <- "#FFFFFF"
dem_color  <- "#5AA2F0"   # lighter blue/red so they read on navy
rep_color  <- "#F06A6A"
font       <- "Bahnschrift"

# --- numbers ----------------------------------------------------------------
cpi <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL")
names(cpi) <- c("month", "cpi")
cpi$month <- as.Date(cpi$month)
latest_cpi <- cpi$cpi[cpi$month == max(cpi$month)]

gas_weekly <- gas_weekly %>%
  mutate(month = floor_date(date, "month")) %>%
  left_join(cpi, by = "month") %>%
  fill(cpi) %>%
  mutate(real_price = price * latest_cpi / cpi)

dem_avg <- mean(gas_weekly$real_price[gas_weekly$pres_party == "D"])
rep_avg <- mean(gas_weekly$real_price[gas_weekly$pres_party == "R"])

con <- dbConnect(RSQLite::SQLite(), file.path(gas_dir, "database", "gas_data.db"))
crude <- dbGetQuery(con, "select period, price from crude_prices")
dbDisconnect(con)

crude_monthly <- crude %>%
  mutate(month = floor_date(as.Date(period), "month")) %>%
  group_by(month) %>%
  summarise(brent = mean(price), .groups = "drop")

gas_vs_oil <- gas_monthly %>% inner_join(crude_monthly, by = "month")
oil_r2 <- summary(lm(price ~ brent, data = gas_vs_oil))$r.squared

dollars <- function(x) paste0("$", formatC(x, format = "f", digits = 2))

# --- one row per presidency (average in today's dollars) ---------------------
by_president <- gas_weekly %>%
  mutate(term_id = consecutive_id(president)) %>%
  group_by(term_id, president, pres_party) %>%
  summarise(avg = mean(real_price), from = year(min(date)), to = year(max(date)), .groups = "drop") %>%
  mutate(name  = word(president, -1),
         years = paste0(from, "-", ifelse(term_id == max(term_id), "now", to)),
         label = paste0(name, "\n", years))

first_week <- gas_weekly %>% slice_min(date, n = 1)   # used to explain inflation

# --- words ------------------------------------------------------------------
headline  <- "DOES THE PRESIDENT\nSET GAS PRICES?"
answer    <- "Not directly. Gas follows crude oil."
oil_line  <- paste0("Oil prices alone explain ", round(oil_r2 * 100),
                    "% of gas price swings, no matter who's in office.")

caveat_title <- "But can't a president move oil prices?"
caveat_body  <- paste0("Sometimes. A war or sanctions can push oil up (the 2026 Iran strike did), and releasing\n",
                       "the emergency oil reserve can pull it down. But the biggest swings, like the 2008 crash\n",
                       "and COVID, were out of any president's hands.")

bars_title <- "WHAT A GALLON COST UNDER EACH PRESIDENT"
tie_note   <- paste0("Add it all up and both parties average ", dollars(dem_avg), " a gallon. That tie is a coincidence:\n",
                     "each party has had cheap years and expensive years. There's no pattern by party.")
infl_note  <- paste0("All prices are in today's money. Prices rise over time, so we convert old prices to what they\n",
                     "would cost now: a ", dollars(first_week$price), " gallon in ", year(first_week$date),
                     " counts as ", dollars(first_week$real_price), ", because ", dollars(first_week$price),
                     " back then bought what ", dollars(first_week$real_price), " buys today.")

link_text <- "Full story: sixtyeight95.github.io"
source_tx <- "Data: U.S. EIA (gas and Brent crude prices); inflation from FRED (CPI)"

# --- chart 1: actual gas price vs what oil alone predicts --------------------
oil_fit    <- lm(price ~ brent, data = gas_vs_oil)
gas_vs_oil <- gas_vs_oil %>% mutate(oil_says = predict(oil_fit))

bands <- presidencies %>%
  mutate(start = pmax(start, min(gas_vs_oil$month)), end = pmin(end, max(gas_vs_oil$month))) %>%
  filter(end > start)

dark_theme <- theme_minimal(base_family = font, base_size = 13) +
  theme(plot.background  = element_rect(fill = background, color = NA),
        panel.background = element_rect(fill = background, color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#2A3A52"),
        axis.text = element_text(color = "#B8C4D6"))

chart <- ggplot() +
  geom_rect(data = bands, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = pres_party),
            alpha = 0.3) +
  geom_line(data = gas_vs_oil, aes(month, oil_says), color = amber, linewidth = 1.1) +
  geom_line(data = gas_vs_oil, aes(month, price), color = white, linewidth = 0.7) +
  scale_fill_manual(values = c(D = dem_color, R = rep_color), guide = "none") +
  scale_y_continuous(labels = function(x) paste0("$", x)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  dark_theme

# --- chart 2: one bar per presidency -------------------------------------------
bars <- ggplot(by_president, aes(x = reorder(label, term_id), y = avg, fill = pres_party)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = dollars(avg)), vjust = -0.4, color = white, family = font,
            fontface = "bold", size = 4.2) +
  scale_fill_manual(values = c(D = dem_color, R = rep_color), guide = "none") +
  scale_y_continuous(limits = c(0, max(by_president$avg) * 1.22), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  dark_theme +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.x = element_text(color = white, size = 10.5, lineheight = 0.9))

# --- draw the page ------------------------------------------------------------
txt <- function(label, x, y, size, color = white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = font, fontsize = size, col = color, fontface = face, lineheight = 0.95))
}
muted <- "#B8C4D6"

agg_png("gas-politics-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage()
grid.rect(gp = gpar(fill = background, col = NA))

txt(headline, 0.5, 0.925, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.868, 0.868), gp = gpar(col = amber, lwd = 4))
txt(answer, 0.5, 0.84, 16, teal, "bold")

# chart 1
txt("Actual gas price", 0.47, 0.8, 11.5, white, "bold", just = "right")
txt("What oil prices alone predict", 0.53, 0.8, 11.5, amber, "bold", just = "left")
print(chart, vp = viewport(x = 0.5, y = 0.66, width = 0.92, height = 0.245))
txt("Monthly price per gallon, as paid at the time.  Shading = president's party (blue D, red R).", 0.5, 0.528, 9, muted)
txt(oil_line, 0.5, 0.503, 10.5, white)

# can a president move oil?
txt(caveat_title, 0.5, 0.472, 11.5, amber, "bold")
txt(caveat_body, 0.5, 0.435, 9, white)

# chart 2
grid.lines(x = c(0.08, 0.92), y = c(0.398, 0.398), gp = gpar(col = "#2A3A52", lwd = 1.5))
txt(bars_title, 0.5, 0.377, 11, teal, "bold")
txt("average price per gallon, in today's money", 0.5, 0.354, 9, muted)
print(bars, vp = viewport(x = 0.5, y = 0.255, width = 0.92, height = 0.17))
txt(tie_note, 0.5, 0.142, 9, white)
txt(infl_note, 0.5, 0.103, 8, muted)

# footer
grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = "#1F4E5A", col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, white, "bold", just = "left")
txt(link_text, 0.95, 0.048, 12, teal, just = "right")
txt(source_tx, 0.5, 0.012, 7.5, muted)
dev.off()

message("Saved gas-politics-infographic.png")

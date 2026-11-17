# Reel for the credit card post: credit-card-reel.mp4 (1080 x 1920, ~25 s)
# Run from this folder:  source("credit-card-reel.R")
source("../R/fred.R")
source("../R/credit_card_data.R")
source("../R/reel_helpers.R")
usd <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
count_to <- function(final, t, start, len = 1.6) final * ease((t - start) / len)

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("Putting $1,000 of\nholiday gifts on a\ncredit card?", 0.5, 0.6, 46, brand$white, "bold", a)
  txt("Here's what it really costs.", 0.5, 0.43, 34, brand$amber, "bold", a * appear(t, 1.6))

  a <- fade(t, 3.5, 10.5)
  txt(paste0("At ", formatC(cc_now$value, format = "f", digits = 1), "% interest,\npaying only the minimum:"), 0.5, 0.7, 38, brand$white, "bold", a)
  txt(usd(holiday_bill + count_to(min_case$interest, t, 4.6)), 0.5, 0.55, 120, brand$red, "bold", a * appear(t, 4.3))
  txt("total paid", 0.5, 0.47, 30, brand$muted, "plain", a * appear(t, 4.3))
  txt(paste0("About ", formatC(min_case$months / 12, format = "f", digits = 0), " years\nto pay it off."), 0.5, 0.36, 40, brand$white, "bold", a * appear(t, 7))

  a <- fade(t, 10.5, 17)
  txt("Pay more, pay way less", 0.5, 0.74, 40, brand$amber, "bold", a)
  s <- scenarios %>% filter(plan != "$50 a month")
  for (i in 1:3) {
    y <- 0.63 - (i - 1) * 0.12
    aa <- a * appear(t, 11 + i * 0.8)
    txt(s$plan[i], 0.5, y, 34, brand$white, "bold", aa)
    txt(paste0(usd(s$interest[i]), " in interest"), 0.5, y - 0.045, 40, if (i == 1) brand$red else brand$mint, "bold", aa)
  }

  a <- fade(t, 17, 22)
  txt("Average card rate", 0.5, 0.7, 36, brand$white, "bold", a)
  txt(paste0("2019: ", formatC(cc_2019, format = "f", digits = 1), "%"), 0.5, 0.61, 52, brand$white, "bold", a * appear(t, 17.4))
  txt(paste0("Now: ", formatC(cc_now$value, format = "f", digits = 1), "%"), 0.5, 0.52, 52, brand$red, "bold", a * appear(t, 18.2))
  txt("And the Fed is raising\nrates again.", 0.5, 0.39, 34, brand$amber, "bold", a * appear(t, 19.2))

  end_card(t, 22)
  txt("Data: Federal Reserve. Minimum = interest + 1%, $25 floor.", 0.5, 0.05, 17, brand$muted)
}
render_reel(draw_frame, "credit-card-reel.mp4", seconds = 25)

# Reel for the SNAP post: snap-reel.mp4 (1080 x 1920, ~26 s)
# Run from this folder:  source("snap-reel.R")
source("../R/fred.R")
source("../R/snap_data.R")
source("../R/reel_helpers.R")

usd2 <- function(x) paste0("$", formatC(x, format = "f", digits = 2))

draw_frame <- function(t) {
  background()
  a <- fade(t, 0, 3.5)
  txt("What does SNAP\nfood aid actually pay?", 0.5, 0.58, 44, brand$white, "bold", a)

  a <- fade(t, 3.5, 9)
  txt(usd2(benefit_meal), 0.5, 0.6, 120, brand$amber, "bold", a)
  txt("per person, per meal", 0.5, 0.49, 36, brand$white, "bold", a * appear(t, 4.3))
  txt(paste0("(about $", round(benefit_now), " a month)"), 0.5, 0.43, 28, brand$muted, "plain", a * appear(t, 4.8))

  a <- fade(t, 9, 15)
  txt("WHO GETS IT", 0.5, 0.72, 32, brand$muted, "bold", a)
  txt(paste0(share_hh_child_senior_disabled, "% of households have a\nchild, senior, or someone\nwith a disability"), 0.5, 0.6, 32,
      brand$white, "bold", a * appear(t, 9.6))
  txt(paste0(share_hh_below_poverty, "% are at or below\nthe poverty line"), 0.5, 0.44, 32, brand$white, "bold", a * appear(t, 11))

  a <- fade(t, 15, 22)
  txt("THE PROPOSED RULE", 0.5, 0.72, 32, brand$amber, "bold", a)
  txt(paste0(rule_people_millions, " million people"), 0.5, 0.62, 52, brand$red, "bold", a * appear(t, 15.6))
  txt("would lose SNAP by 2029,\nper USDA's estimate", 0.5, 0.53, 30, brand$white, "bold", a * appear(t, 16.2))
  txt(paste0("Not final yet. Public comments\nopen until ", rule_comment_deadline, "."), 0.5, 0.4, 28, brand$mint, "bold",
      a * appear(t, 17.8))

  end_card(t, 22)
  txt("Data: USDA Food and Nutrition Service", 0.5, 0.05, 18, brand$muted)
}
render_reel(draw_frame, "snap-reel.mp4", seconds = 25)

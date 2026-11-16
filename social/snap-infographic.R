# Infographic: what SNAP food aid really is (snap-infographic.png, 1080 x 1350)
# Run from this folder:  source("snap-infographic.R")
source("../R/fred.R")
source("../R/snap_data.R")
library(ragg)
library(grid)

txt <- function(label, x, y, size, color = brand$white, face = "plain", just = "center") {
  grid.text(label, x = x, y = y, just = just,
            gp = gpar(fontfamily = brand$font, fontsize = size, col = color, fontface = face, lineheight = 1))
}

agg_png("snap-infographic.png", width = 1080, height = 1350, res = 144)
grid.newpage(); grid.rect(gp = gpar(fill = brand$navy, col = NA))
txt("WHAT SNAP FOOD AID\nREALLY IS", 0.5, 0.915, 25, face = "bold")
grid.lines(x = c(0.4, 0.6), y = c(0.855, 0.855), gp = gpar(col = brand$amber, lwd = 4))
txt("The facts behind the headlines about the proposed SNAP rule.", 0.5, 0.825, 10.5, brand$mint)

# big number
txt(paste0("$", formatC(benefit_meal, format = "f", digits = 2)), 0.5, 0.735, 46, brand$amber, "bold")
txt(paste0("the average SNAP benefit per person, per meal\n($", round(benefit_now), " a month, about $",
           formatC(benefit_day, format = "f", digits = 2), " a day)"), 0.5, 0.665, 11, brand$white)

txt("WHO GETS IT", 0.5, 0.6, 11.5, brand$amber, "bold")
tiles <- tribble(~x, ~value, ~label,
  0.2, paste0(round(snap_july_2026_millions), "M"), paste0("people (July 2026),\nabout 1 in ", round(us_population_millions / snap_july_2026_millions), " Americans"),
  0.5, paste0(share_hh_child_senior_disabled, "%"), "of households include a\nchild, senior, or person\nwith a disability",
  0.8, paste0(share_hh_below_poverty, "%"), "of households are at\nor below the\npoverty line")
for (i in 1:3) {
  txt(tiles$value[i], tiles$x[i], 0.55, 24, brand$white, "bold")
  txt(tiles$label[i], tiles$x[i], 0.48, 9.5, brand$muted)
}

# the proposed rule
grid.rect(x = 0.5, y = 0.29, width = 0.88, height = 0.2, gp = gpar(fill = brand$band, col = NA))
txt("THE PROPOSED RULE", 0.5, 0.37, 11.5, brand$amber, "bold")
txt(paste0("USDA estimates ", rule_people_millions, " million people (about ",
           round(rule_people_millions / snap_july_2026_millions * 100), "% of today's recipients) would lose SNAP by 2029."), 0.5, 0.335, 10.5, brand$white, "bold")
txt(paste0("WHAT CHANGES: Many states let families just above the normal income or savings limits\n",
           "qualify if they get other aid, like help with childcare. The rule would end that.\n",
           "USDA SAYS: it keeps aid within the program's normal limits, saving ~$", rule_federal_savings_billions, "B over 5 years.\n",
           "CRITICS SAY: it cuts off working families and ", rule_kids_school_meals_millions,
           "M kids from automatic free school meals."), 0.5, 0.268, 8.9, brand$white)

txt(paste0("It's a proposal, not final. Public comments are open until ", rule_comment_deadline, " at regulations.gov."), 0.5, 0.165, 9.5, brand$mint, "bold")
txt(paste0("Benefit: USDA FNS, fiscal year 2026 so far (Oct 2025–Jun 2026, preliminary). Household facts: USDA ",
           "Characteristics of SNAP Households, FY2023.\nRule figures: USDA's own estimates in the proposed rule (Nov 10, 2026)."),
    0.5, 0.115, 7, brand$muted)

grid.rect(x = 0.5, y = 0.04, width = 1, height = 0.08, gp = gpar(fill = brand$band, col = NA))
txt("SixtyEight95", 0.05, 0.048, 15, brand$white, "bold", just = "left")
txt("Full story: sixtyeight95.github.io", 0.95, 0.048, 12, brand$mint, just = "right")
txt("Data: USDA Food and Nutrition Service", 0.5, 0.012, 7.5, brand$muted)
dev.off(); message("Saved snap-infographic.png")

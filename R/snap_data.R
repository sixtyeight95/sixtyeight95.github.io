# ---------------------------------------------------------------------------
# SNAP (food stamps): who gets it, how much, and the proposed 2026 rule.
# Numbers typed in from USDA Food and Nutrition Service (FNS) publications:
#   - FNS SNAP Data Tables (annual summary; FY2026 = Oct 2025-Jun 2026, preliminary)
#   - FNS "Characteristics of SNAP Households, FY2023"
#   - USDA proposed rule on categorical eligibility, Federal Register, Nov 10, 2026
#     (USDA's own estimate of people affected; comments due Dec 10, 2026)
# Update these when USDA publishes new figures.
# ---------------------------------------------------------------------------
snap_years <- tribble(
  ~fiscal_year, ~people_millions, ~benefit_per_person, ~note,
  "FY2022", 41.2, 236.40, "includes pandemic emergency allotments",
  "FY2023", 42.1, 208.75, "emergency allotments ended early 2023",
  "FY2024", 41.7, 187.17, "",
  "FY2025", 42.1, 187.91, "",
  "FY2026*", 38.2, 189.15, "Oct 2025-Jun 2026, preliminary"
)

snap_july_2026_millions <- 36.1          # latest month (July 2026)
benefit_now   <- 189.15                    # per person per month, FY2026 so far
benefit_day   <- benefit_now / 30.4
benefit_meal  <- benefit_day / 3

share_hh_child_senior_disabled <- 79       # % of households (FY2023)
share_people_children <- 40                # "almost 4 in 10" recipients (FY2023)
share_hh_below_poverty <- 73               # % of households at/below poverty line (FY2023)

rule_people_millions <- 6.3                # USDA estimate, full effect FY2029
rule_households_millions <- 3.2
rule_federal_savings_billions <- 45        # over five years (USDA)
rule_kids_school_meals_millions <- 1.78    # lose automatic free school meal eligibility
rule_comment_deadline <- "December 10, 2026"
us_population_millions <- 337

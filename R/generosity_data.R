# ---------------------------------------------------------------------------
# How generous are Americans? Typed in from published reports (update yearly):
#   Giving USA 2026 (Giving USA Foundation / IU Lilly Family School of Philanthropy):
#     total U.S. charitable giving in 2025 and individual giving
#   U.S. Census Bureau & AmeriCorps, Current Population Survey Civic Engagement and
#     Volunteering Supplement (Sept 2023 data, released Nov 2024)
#   GivingTuesday Data Commons, GivingTuesday 2025 results
# ---------------------------------------------------------------------------
giving_total_2025_b       <- 617.2   # $ billions, record; first time over $600B
giving_individuals_2025_b <- 394.2   # $ billions (64% of total)
giving_growth_2025        <- 5.7     # % increase vs 2024 (3.0% after inflation)
giving_per_day_b          <- giving_total_2025_b / 365

volunteers_m          <- 75.7        # millions formally volunteered (Sep 2022-Sep 2023)
volunteer_rate        <- 28.3        # % of people 16+
volunteer_hours_b     <- 4.99        # billions of hours
volunteer_value_b     <- 167.2       # $ billions of economic value
fulltime_jobs_equiv_m <- volunteer_hours_b * 1e9 / 2080 / 1e6   # 40 hrs x 52 weeks
neighbors_helped_m    <- 137.5       # millions who informally helped neighbors
neighbors_rate        <- 54.2        # %

volunteer_trend <- tribble(~year, ~rate,
  2019, 30.0,    # 28.3 + 1.7 points ("1.7 points below pre-pandemic levels")
  2021, 23.2,
  2023, 28.3)

giving_tuesday_2025_b   <- 4.0       # $ billions raised in one day (U.S.)
giving_tuesday_2025_m   <- 38.1      # millions of participants
giving_tuesday_2026     <- "Tuesday, December 1"

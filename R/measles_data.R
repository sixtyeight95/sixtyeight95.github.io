# ---------------------------------------------------------------------------
# Measles in the U.S.: cases, vaccination coverage, and lost elimination status.
# Typed in from published sources (update as CDC posts new numbers):
#   - CDC "Measles Cases and Outbreaks" (yearly confirmed cases, 2000-2025)
#   - 2026 cases: CDC count as reported by CBS News, Nov 17, 2026 (5,412 cases)
#   - CDC SchoolVaxView, kindergarten MMR coverage and exemptions (2019-20 to 2025-26)
#   - PAHO news release, Nov 17, 2026 (loss of elimination status)
# ---------------------------------------------------------------------------
measles_cases <- tribble(~year, ~cases,
  2000, 86, 2001, 116, 2002, 44, 2003, 56, 2004, 37, 2005, 66, 2006, 55, 2007, 43,
  2008, 140, 2009, 71, 2010, 63, 2011, 220, 2012, 55, 2013, 187, 2014, 667, 2015, 188,
  2016, 86, 2017, 120, 2018, 375, 2019, 1274, 2020, 13, 2021, 49, 2022, 121, 2023, 59,
  2024, 285, 2025, 2242, 2026, 5412)            # 2026 = so far (through mid-November)

cases_2026          <- 5412
hosp_2026           <- 590     # "more than 590" hospitalizations
deaths_2026         <- 6       # "at least six" per CDC; some states report more
states_2026         <- 45      # plus Washington, D.C.
unvax_share_2026    <- 96      # % unvaccinated or unknown vaccination status
fully_vax_share     <- 2       # % of cases in fully vaccinated people
mmr_effective       <- 97      # % effectiveness of two MMR doses
avg_2000_2018       <- measles_cases %>% filter(year <= 2018) %>% summarise(mean(cases)) %>% pull()

kinder <- tribble(~school_year, ~mmr, ~exempt,
  "2019-20", 95.2, 2.5,
  "2020-21", 93.9, 2.2,
  "2021-22", 93.0, 2.6,
  "2022-23", 93.1, 3.0,
  "2023-24", 92.7, 3.3,
  "2024-25", 92.5, 3.6,
  "2025-26", 92.4, 4.2)
herd_target       <- 95
states_at_target  <- 10        # states at or above 95% MMR, 2025-26
kids_no_mmr_docs  <- 280000    # kindergartners without documented 2 doses, 2025-26
mmr_high_state    <- "West Virginia (98.9%)"
mmr_low_state     <- "Idaho (75.2%)"
elimination_since <- 2000

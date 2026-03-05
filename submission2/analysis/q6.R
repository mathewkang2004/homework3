# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table)

data_2010 <- read_csv("data/output/data-2010.csv") %>%
  mutate(market_share = avg_enrollment / avg_enrolled)

final_2010 <- data_2010 %>%
  mutate(
    raw_rating = rowMeans(
      cbind(
        breastcancer_screen, 
        rectalcancer_screen, 
        cv_diab_cholscreen,
        glaucoma_test,
        monitoring,
        flu_vaccine,
        pn_vaccine,
        physical_health,
        mental_health,
        osteo_test,
        physical_monitor,
        primaryaccess,
        osteo_manage,
        diab_healthy,
        bloodpressure,
        ra_manage,
        copd_test,
        bladder,
        falling,
        nodelays,
        doctor_communicate,                   
        carequickly,
        customer_service,                   
        overallrating_care,
        overallrating_plan,
        complaints_plan,
        appeals_timely,
        appeals_review,
        leave_plan,
        audit_problems,
        hold_times,
        info_accuracy,
        ttyt_available
      ),
      na.rm = TRUE
    )
  ) %>%
  select(contractid, planid, fips, plan_type, partd, avg_enrollment, avg_eligibles, avg_enrolled, premium, premium_partc, ma_rate, partc_score, partcd_score, Star_Rating, raw_rating, market_share)


ma.rd275 <- final_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating == 2.5 | Star_Rating == 3) %>%
  mutate(score = raw_rating - 2.75,
         treat = (score>=0),
         bandwidth = (abs(score) <= 0.125),
         score_treat = score * treat)

est225 <- rdrobust(y = ma.rd275$market_share, x = ma.rd275$score, c = 0, h = 0.125, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")


ma.rd325 <- final_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating == 3 | Star_Rating == 3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score >= 0),
         bandwidth = (abs(score) <= 0.125),
         score_treat = score * treat)

est325 <- rdrobust(y = ma.rd325$market_share, x = ma.rd325$score, c = 0, h = 0.125, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")

q6_data <- data.frame(
  Comparison = c("2.5 vs 3.0 Stars", "3.0 vs. 3.5 Stars"),
  Cutoff = c(2.75, 3.25),
  Estimate = c(est225$Estimate[1], est325$Estimate[1]),
  P_Value = c(est225$pv[1], est325$pv[1]),
  Robust_CI = c(est225$ci[1], est325$ci[1]),
  N = c(est225$N[1], est325$N[1])
)

knitr::kable(q6_data, digits = 5, col.names = c("Comparison", "Star Cutoff", "RD Estimate (ATE)", "P-Value", "95% CI", "N"))
  
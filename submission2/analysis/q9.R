# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table, rddensity)

data_2010 <- read_csv("data/output/data-2010.csv") %>%
  mutate(market_share = avg_enrollment / avg_enrolled)

final_2010 <- data_2010 %>%
  mutate(
    is_hmo = if_else(plan_type == "HMO/HMOPOS", 1, 0),
    is_partd = if_else(partd == "Yes", 1, 0),
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
  select(contractid, planid, fips, plan_type, partd, avg_enrollment, avg_eligibles, avg_enrolled, premium, premium_partc, ma_rate, partc_score, partcd_score, Star_Rating, raw_rating, market_share, is_hmo, is_partd)

ma.rd275 <- final_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating %in% c(2.5, 3.0)) %>%
  mutate(score = raw_rating - 2.75)

ma.rd325 <- final_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating %in% c(3.0, 3.5)) %>%
  mutate(score = raw_rating - 3.25)

rdplot(y = ma.rd275$is_hmo, x = ma.rd275$score, c = 0, p = 1,
       title = "Balance Test: HMO Status at 2.75 Threshold",
       y.label = "Probability of being HMO",
       x.label = "Running Variable (Raw Score - 2.75)")

# Graphing Part D Balance at the 3.25 Threshold
rdplot(y = ma.rd325$is_partd, x = ma.rd325$score, c = 0, p = 1,
       title = "Balance Test: Part D Status at 3.25 Threshold",
       y.label = "Probability of having Part D",
       x.label = "Running Variable (Raw Score - 3.25)")
# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table, rdrobust, rddensity)

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
  select(contractid, planid, fips, plan_type, partd, avg_enrollment, avg_eligibles, avg_enrolled, ma_rate, partc_score, partcd_score, Star_Rating, raw_rating, market_share)

ma.rd275 <- final_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating == 2.5 | Star_Rating == 3) %>%
  mutate(score = raw_rating - 2.75,
         treat = (score>=0),
         score_treat = score * treat)

ma.rd325 <- final_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating == 3 | Star_Rating == 3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score >= 0),
         score_treat = score * treat)

dens1 <- rddensity(ma.rd275$score, c = 0)

stats_275 <- data.frame(
  `T-Statistic` = round(dens1$test$t_jk, 4),
  `P-Value` = format.pval(dens1$test$p_jk, eps = 0.001),
  `N-left` = dens1$N$left,
  `N-right` = dens1$N$right
)

knitr::kable(stats_275, col.names = c("T-Statistic", "P-Value", "N (Left)", "N (Right)"),
             caption = "McCrary Test results for 2.75-Star Threshold")

densplot1 <- rdplotdensity(dens1, ma.rd275$score,
                           title = "Density Continuity: 2.75-Star Threshold",
                           xlabel = "Centered Raw Score (Raw - 2.75)",
                           ylabel = "Density Estimate")

dens2 <- rddensity(ma.rd325$score, c = 0)

stats_325 <- data.frame(
  `T-Statistic` = round(dens2$test$t_jk, 4),
  `P-Value` = format.pval(dens2$test$p_jk, eps = 0.001),
  `N-left` = dens2$N$left,
  `N-right` = dens2$N$right
)

knitr::kable(stats_325, col.names = c("T-Statistic", "P-Value", "N (Left)", "N (Right)"),
             caption = "McCrary Test results for 3.25-Star Threshold")

densplot2 <- rdplotdensity(dens2, ma.rd325$score,
                           title = "Density Continuity: 3.25-Star Threshold",
                           xlabel = "Centered Raw Score (Raw - 3.25)",
                           ylabel = "Density Estimate")

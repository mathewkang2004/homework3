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
         score_treat = score * treat)

ma.rd325 <- final_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating == 3 | Star_Rating == 3.5) %>%
  mutate(score = raw_rating - 3.25,
         treat = (score >= 0),
         score_treat = score * treat)

# Define bandwidths
bandwidths <- c(0.1, 0.12, 0.13, 0.14, 0.15)

# Create empty data frame
sens_results <- data.frame()

# Run for loop for both cutoffs

for(h in bandwidths) {
  fit_275 <- rdrobust(y = ma.rd275$market_share, x = ma.rd275$score, c = 0, h = h, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")
  fit_325 <- rdrobust(y = ma.rd325$market_share, x = ma.rd325$score, c = 0, h = h, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")
  temp_results <- data.frame(
    Bandwidth = h,
    Estimate = c(fit_275$Estimate[1], fit_325$Estimate[1]),
    Comparison = c("2.5 vs 3.0 Stars", "3.0 vs 3.5 Stars")
  )
  
  sens_results <- rbind(sens_results, temp_results)
}

# Graph results
ggplot(sens_results, aes(x = Bandwidth, y = Estimate, color = Comparison, group = Comparison)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Sensitivity of RD Estimates to Bandwidth Selection",
    x = "Bandwidth (h)",
    y = "Estimated Market Share Jump (ATE)",
    color = "Bandwidth"
  ) +
  scale_color_manual(values = c("darkblue", "darkorange")) +
  expand_limits(y = 0)
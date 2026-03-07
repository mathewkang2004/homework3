# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table, rdrobust, rddensity)

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
  select(contractid, planid, fips, plan_type, partd, avg_enrollment, avg_eligibles, avg_enrolled, ma_rate, partc_score, partcd_score, Star_Rating, raw_rating, market_share, is_hmo, is_partd)

q9_2010 <- final_2010 %>%
  mutate(
    is_hmo = if_else(plan_type == "HMO/HMOPOS", 1, 0),
    is_partd = if_else(partd == "Yes", 1, 0)
  )

ma.rd275 <- q9_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating %in% c(2.5, 3.0)) %>%
  mutate(score = raw_rating - 2.75)

bal_hmo_275 <- rdrobust(y = ma.rd275$is_hmo, x = ma.rd275$score, c = 0, h = 0.125, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")
bal_partd_275 <- rdrobust(y = ma.rd275$is_partd, x = ma.rd275$score, c = 0, h = 0.125, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")

ma.rd325 <- q9_2010 %>%
  filter(!is.na(raw_rating), !is.na(partc_score), Star_Rating %in% c(3.0, 3.5)) %>%
  mutate(score = raw_rating - 3.25)

bal_hmo_325 <- rdrobust(y = ma.rd325$is_hmo, x = ma.rd325$score, c = 0, h = 0.125, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")
bal_partd_325 <- rdrobust(y = ma.rd325$is_partd, x = ma.rd325$score, c = 0, h = 0.125, p = 1, kernel = "uniform", vce = "hc0", masspoints = "off")

balance_results <- data.frame(
  Variable  = rep(c("HMO Status", "Part D Status"), 2),
  Threshold = rep(c("2.75 Threshold", "3.25 Threshold"), each = 2),
  Estimate  = c(bal_hmo_275$Estimate[1], bal_partd_275$Estimate[1], 
                bal_hmo_325$Estimate[1], bal_partd_325$Estimate[1]),
  Lower = c(bal_hmo_275$ci[1,1], bal_partd_275$ci[1,1], 
            bal_hmo_325$ci[1,1], bal_partd_325$ci[1,1]),
  Upper = c(bal_hmo_275$ci[1,2], bal_partd_275$ci[1,2], 
            bal_hmo_325$ci[1,2], bal_partd_325$ci[1,2]),
  N_left = c(bal_hmo_275$N[1], bal_partd_275$N[1], 
                 bal_hmo_325$N[1], bal_partd_325$N[1]),
  N_right = c(bal_hmo_275$N[2], bal_partd_275$N[2], 
                  bal_hmo_325$N[2], bal_partd_325$N[2])
)


balance_results %>%
  knitr::kable(digits = 3, col.names = c("Variable", "Threshold", "Estimate", "Lower", "Upper", "N (Left)", "N (Right)"),
               caption = "Balance Test Estimates and Sample Sizes") %>%
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

ggplot(balance_results, aes(x = Estimate, y = Variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_pointrange(aes(xmin = Lower, xmax = Upper), color = "darkblue") +
  facet_wrap(~Threshold) +
  theme_minimal() +
  labs(title = "Covariate Balance Tests",
       x = "Mean Difference",
       y = "")
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

# Define cutoffs
thresholds <- tibble(
  lower = c(2.5, 3.0, 3.5, 4.0, 4.5),
  upper = c(3.0, 3.5, 4.0, 4.5, 5.0),
  cutoff     = c(2.75, 3.25, 3.75, 4.25, 4.75)
)

q5_round <- thresholds %>%
  pmap_dfr(function(lower, upper, cutoff) {
    final_2010 %>%
      filter(
        !is.na(raw_rating),
        Star_Rating == upper,
        raw_rating >= cutoff
      ) %>%
      summarize(
        `Star Rating` = as.character(upper),
        `Threshold` = as.character(cutoff),
        `Plans Rounded Up` = n()
      )
  })

knitr::kable(q5_round)
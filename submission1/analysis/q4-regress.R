# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table)

# Load data ---------------------------------------------------------------
final_ma <- read.csv("data/output/final_data.csv")

library(broom)
library(tidyverse)

# 1. Prepare the data: Calculate market share and dummies first
reg_ready_data <- final_ma %>%
  mutate(
    # Market share is enrollment divided by total county eligibles (or total enrolled)
    # Using avg_enrollment / avg_enrolled based on your previous logic
    mkt_share = as.numeric(avg_enrollment) / as.numeric(avg_enrolled),
    
    # Create the dummies (assuming Star_Rating is numeric or can be converted)
    star_rating_num = as.numeric(Star_Rating),
    star_3   = if_else(star_rating_num == 3, 1, 0),
    star_3.5 = if_else(star_rating_num == 3.5, 1, 0),
    star_4   = if_else(star_rating_num == 4, 1, 0),
    star_4.5_plus = if_else(star_rating_num >= 4.5, 1, 0)
  ) %>%
  filter(!is.na(mkt_share), !is.na(star_rating_num))

# 2. Run the regressions year by year
final_results <- map_dfr(2010:2015, function(y) {
  
  # Run OLS for the specific year
  # Baseline is implicitly < 3 stars (including the 2.5 category)
  model <- lm(mkt_share ~ star_3 + star_3.5 + star_4 + star_4.5_plus, 
              data = filter(reg_ready_data, year == y))
  
  # Return only the coefficients
  tidy(model) %>%
    filter(term != "(Intercept)") %>%
    mutate(year = y)
})

# 3. Pivot to a wide table showing only estimates
final_results %>%
  select(term, estimate, year) %>%
  pivot_wider(names_from = year, values_from = estimate) %>%
  knitr::kable(digits = 4, caption = "OLS Coefficients: Impact of Star Ratings on Market Share")
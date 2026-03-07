# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table)


# Load data ---------------------------------------------------------------
final_ma <- read.csv("data/output/final_data.csv") %>%
  mutate(market_share = avg_enrollment / avg_enrolled)

# Filter star ratings and create summary table ----
q2_nostar <- final_ma %>%
  filter(is.na(Star_Rating)) %>%
  group_by(year) %>%
  summarize(
    `Avg. Enrollment` = mean(as.numeric(avg_enrollment), na.rm = TRUE),
    `Avg. Market Share` = mean(market_share, na.rm = TRUE),
    `Total Plans` = n()
  ) %>%
  ungroup()


# Visualize table ---------------------------------------------------------
knitr::kable(q2_nostar, 
             digits = 2,
             col.names = c("Year", "Mean Enrollment", "Mean Market Share", "Plan Count")) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
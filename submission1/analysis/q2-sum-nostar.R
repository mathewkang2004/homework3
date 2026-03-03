# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table)


# Load data ---------------------------------------------------------------
final_ma <- read.csv("data/output/final_data.csv")


# Filter star ratings and create summary table ----------------------------------------------------
q2_nostar <- final_ma %>%
  filter(is.na(Star_Rating)) %>%
  group_by(year) %>%
  summarize(
    `Avg. Enrollment` = mean(as.numeric(avg_enrollment), na.rm = TRUE),
    `Avg. Market Share` = mean(as.numeric(avg_enrollment) / as.numeric(avg_enrolled), na.rm = TRUE),
    `Total Plans` = n_distinct(contractid, planid)
  ) %>%
  ungroup()


# Visualize table ---------------------------------------------------------
knitr::kable(q2_nostar, 
             digits = 2, 
             caption = "Medicare Advantage Market Summary Without Star Ratings (2010-2015)",
             col.names = c("Year", "Mean Enrollment", "Mean Market Share", "Plan Count")) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


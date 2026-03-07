# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table)


# Load data ---------------------------------------------------------------
final_ma <- read.csv("data/output/final_data.csv") %>%
  mutate(market_share = avg_enrollment / avg_enrolled)

run_ols_year <- function(yr, data) {
  
  year_data <- data %>%
    filter(year == yr) %>%
    mutate(
      star_cat = case_when(
        Star_Rating <= 2.5 ~ "2.5 or below",
        Star_Rating == 3.0 ~ "3.0",
        Star_Rating == 3.5 ~ "3.5",
        Star_Rating == 4.0 ~ "4.0",
        Star_Rating >= 4.5 ~ "4.5 or above"
      ),
      star_cat = relevel(as.factor(star_cat), ref = "2.5 or below")
    ) %>%
    filter(!is.na(market_share), !is.na(star_cat))
  
  
  model <- lm(market_share ~ star_cat, data = year_data)
  
  
  tidy(model) %>%
    select(term, estimate) %>%
    rename(!!as.character(yr) := estimate)
}


years <- 2010:2015
ols_results <- map(years, ~run_ols_year(.x, final_ma)) %>%
  reduce(full_join, by = "term")

q4_table <- ols_results %>%
  rename(`Star Rating Category` = term) %>%
  mutate(`Star Rating Category` = case_when(
    `Star Rating Category` == "(Intercept)" ~ "2.5 Stars or Below (Excluded)",
    `Star Rating Category` == "star_cat3.0" ~ "3.0 Stars",
    `Star Rating Category` == "star_cat3.5" ~ "3.5 Stars",
    `Star Rating Category` == "star_cat4.0" ~ "4.0 Stars",
    `Star Rating Category` == "star_cat4.5 or above" ~ "4.5 Stars or Above",
    TRUE ~ `Star Rating Category`
  ))

knitr::kable(q4_table, digits = 4) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
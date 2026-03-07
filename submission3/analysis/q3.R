# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, gdata, knitr, kableExtra, scales, data.table)


# Load data ---------------------------------------------------------------
final_ma <- read.csv("data/output/final_data.csv") %>%
  mutate(market_share = avg_enrollment / avg_enrolled)

# Filter for years 2010, 2012, and 2015
star_dist <- final_ma %>%
  filter(year %in% c(2010, 2012, 2015)) %>%
  filter(!is.na(Star_Rating)) %>%
  mutate(Star_Rating = as.factor(Star_Rating))


# Create bar graph
ggplot(star_dist, aes(x = Star_Rating)) +
  geom_bar(fill = "darkblue", color = "white") +
  facet_wrap(~year, ncol = 3) +
  labs(title = "Distribution of Medicare Advantage Star Ratings (2010-2015)",
       x = "Star Rating",
       y = "Count of Plans") +
  theme_minimal()
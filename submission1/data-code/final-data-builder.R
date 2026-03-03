# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table)


# Set year ----------------------------------------------------------------
year <- 2010:2015


# Stack data --------------------------------------------------------------
final_ma <- year %>%
  map_dfr(~ {
    file_path <- paste0("data/output/data-", .x, ".csv")
    read_csv(file_path, col_types = cols(.default = "c")) %>%
      mutate(year_new = as.integer(.x)) %>%
      select(-contains("year"), year_new) %>%
      rename(year = year_new)
  })


# Save data ---------------------------------------------------------------
write_csv(final_ma,"data/output/final_data.csv")
# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table)


# Load functions and set year ---------------------------------------------
source("submission1/data-code/rating-variables.R")
source("submission1/data-code/functions-loader.R")
monthlist <- sprintf("%02d", 1:12)
y <- 2014


# Enrollment & Contract data ---------------------------------------------------------
plan.data <- map_dfr(monthlist, ~ load_month(.x, y)) %>%
  arrange(contractid, planid, state, county, month) %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%                
  ungroup() %>%
  group_by(contractid, planid) %>%
  fill(plan_type, partd, snp, eghp, plan_name, .direction = "downup") %>%
  ungroup() %>%
  group_by(contractid) %>%
  fill(org_type, org_name, org_marketing_name, parent_org, .direction = "downup") %>%
  ungroup()

plan.data.dt <- as.data.table(plan.data)
setorder(plan.data.dt, contractid, planid, fips, year, month)

plan.year <- plan.data.dt[
  , {
    nonmiss <- !is.na(enrollment)
    n <- sum(nonmiss)
    list(
      n_nonmiss = n,
      avg_enrollment = if (n>0) mean(enrollment[nonmiss]) else NA_real_,
      sd_enrollment  = if (n>1) sd(enrollment[nonmiss]) else NA_real_,
      min_enrollment = if (n>0) min(enrollment[nonmiss]) else NA_real_,
      max_enrollment = if (n>0) max(enrollment[nonmiss]) else NA_real_,
      first_enrollment = if (n>0) enrollment[which(nonmiss)[1]] else NA_real_,
      last_enrollment  = if (n>0) enrollment[tail(which(nonmiss), 1)] else NA_real_,
      state  = tail(state, 1),
      county = tail(county, 1),
      org_type = tail(org_type, 1),
      plan_type = tail(plan_type, 1),
      partd = tail(partd, 1),
      snp   = tail(snp, 1),
      eghp  = tail(eghp, 1),
      org_name = tail(org_name, 1),
      org_marketing_name = tail(org_marketing_name, 1),
      plan_name = tail(plan_name, 1),
      parent_org = tail(parent_org, 1),
      contract_date = tail(contract_date, 1)
    )
  },
  by = .(contractid, planid, fips, year)
]

plan.data.2014 <- as_tibble(plan.year)


# Service area data -------------------------------------------------------
service.year <- map_dfr(monthlist, ~ load_month_sa(.x, y))

# Ensure stable order before fills
service.year <- service.year %>%
  arrange(contractid, fips, state, county, month)

# Fill missing identifiers/labels
service.year <- service.year %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%
  ungroup() %>%
  group_by(contractid) %>%
  fill(plan_type, partial, eghp, org_type, org_name, .direction = "downup") %>%
  ungroup()

# Collapse to yearly: one row per contract × county (fips) × year --------
service.data.2014 <- service.year %>%
  group_by(contractid, fips, year) %>%
  arrange(month, .by_group = TRUE) %>%
  summarize(
    state     = last(state),
    county    = last(county),
    org_name  = last(org_name),
    org_type  = last(org_type),
    plan_type = last(plan_type),
    partial   = last(partial),
    eghp      = last(eghp),
    ssa       = last(ssa),
    notes     = last(notes),
    .groups = "drop"
  )


# Landscape data ----------------------------------------------------------
ma.path.a <- paste0("data/input/ma/landscape/Extracted Data/2014LandscapeSource file MA_AtoM 05292014.csv")
ma.data.a <- read_csv(ma.path.a,
                      skip=6,
                      col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                  "drug_type","gap_coverage","drug_type_detail","contractid",
                                  "planid","segmentid","moop","star_rating"),
                      col_types = cols(
                        state = col_character(),
                        county = col_character(),
                        org_name = col_character(),
                        plan_name = col_character(),
                        plan_type = col_character(),
                        premium = col_character(),
                        partd_deductible = col_character(),
                        drug_type = col_character(),
                        gap_coverage = col_character(),
                        drug_type_detail = col_character(),
                        contractid = col_character(),
                        planid = col_double(),
                        segmentid = col_double(),
                        moop = col_character(),
                        star_rating = col_character()
                      )) %>%
  mutate_at(c('premium','partd_deductible'), ~str_replace(.,"-","0")) %>%
  mutate_at(c('premium','partd_deductible'), ~parse_number(.))



ma.path.b <- paste0("data/input/ma/landscape/Extracted Data/2014LandscapeSource file MA_NtoW 05292014.csv")
ma.data.b <- read_csv(ma.path.b,
                      skip=6,
                      col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                  "drug_type","gap_coverage","drug_type_detail","contractid",
                                  "planid","segmentid","moop","star_rating"),
                      col_types = cols(
                        state = col_character(),
                        county = col_character(),
                        org_name = col_character(),
                        plan_name = col_character(),
                        plan_type = col_character(),
                        premium = col_character(),
                        partd_deductible = col_character(),
                        drug_type = col_character(),
                        gap_coverage = col_character(),
                        drug_type_detail = col_character(),
                        contractid = col_character(),
                        planid = col_double(),
                        segmentid = col_double(),
                        moop = col_character(),
                        star_rating = col_character()
                      )) %>%
  mutate_at(c('premium','partd_deductible'), ~str_replace(.,"-","0")) %>%
  mutate_at(c('premium','partd_deductible'), ~parse_number(.))


ma.data <- rbind(ma.data.a,ma.data.b)


mapd.path.a <- paste0("data/input/ma/landscape/Extracted Data/PartCD/2014/Medicare Part D 2014 Plan Report 05292014.xls")
mapd.data.a <- read_xls(mapd.path.a,
                        range="A5:AA15859",
                        sheet="Alabama to Montana",
                        col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                    "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                    "national_pdp","premium_partc",
                                    "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                    "partd_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                    "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                    "gap_coverage","gap_coverage_type"))



mapd.path.b <- paste0("data/input/ma/landscape/Extracted Data/PartCD/2014/Medicare Part D 2014 Plan Report 05292014.xls")
mapd.data.b <- read_xls(mapd.path.b,
                        range="A5:AA20305",
                        sheet="Nebraska to Wyoming",
                        col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                    "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                    "national_pdp","premium_partc",
                                    "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                    "partd_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                    "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                    "gap_coverage","gap_coverage_type"))
mapd.data <- rbind(mapd.data.a,mapd.data.b)

landscape.2014 <- mapd.clean.merge(ma.data=ma.data, mapd.data=mapd.data, y)


# Penetration data --------------------------------------------------------
ma.penetration <- map_dfr(monthlist, ~ load_month_pen(.x, y)) %>%
  arrange(state, county, month) %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%
  ungroup()

# Collapse to yearly (safe summaries; avoid NaN/Inf) ----------------------
pen.2014 <- ma.penetration %>%
  group_by(fips, state, county, year) %>%
  arrange(month, .by_group = TRUE) %>%
  summarize(
    n_elig  = sum(!is.na(eligibles)),
    n_enrol = sum(!is.na(enrolled)),
    
    avg_eligibles   = ifelse(n_elig  > 0, mean(eligibles, na.rm = TRUE), NA_real_),
    sd_eligibles    = ifelse(n_elig  > 1,  sd(eligibles,  na.rm = TRUE), NA_real_),
    min_eligibles   = ifelse(n_elig  > 0, min(eligibles,  na.rm = TRUE), NA_real_),
    max_eligibles   = ifelse(n_elig  > 0, max(eligibles,  na.rm = TRUE), NA_real_),
    first_eligibles = ifelse(n_elig  > 0, first(na.omit(eligibles)),     NA_real_),
    last_eligibles  = ifelse(n_elig  > 0,  last(na.omit(eligibles)),     NA_real_),
    
    avg_enrolled    = ifelse(n_enrol > 0, mean(enrolled,   na.rm = TRUE), NA_real_),
    sd_enrolled     = ifelse(n_enrol > 1,  sd(enrolled,    na.rm = TRUE), NA_real_),
    min_enrolled    = ifelse(n_enrol > 0, min(enrolled,    na.rm = TRUE), NA_real_),
    max_enrolled    = ifelse(n_enrol > 0, max(enrolled,    na.rm = TRUE), NA_real_),
    first_enrolled  = ifelse(n_enrol > 0, first(na.omit(enrolled)),       NA_real_),
    last_enrolled   = ifelse(n_enrol > 0,  last(na.omit(enrolled)),       NA_real_),
    
    ssa = last(ssa),
    .groups = "drop"
  )

# Star Ratings Data -------------------------------------------------------------
ma.path.a <- "data/input/ma/star-ratings/Extracted Star Ratings/Part C 2014 Fall/2014_Part_C_Report_Card_Master_Table_2013_10_17_stars.csv"
star.data.a <- read_csv(
  ma.path.a,
  skip = 3,
  col_names = rating.vars.2014,
  na = c("", "NA", "*")
) %>%
  mutate(across(
    -any_of(c("contractid","org_type","contract_name","org_marketing","org_parent")),
    ~ parse_number(as.character(.))
  ))

ma.path.b <- "data/input/ma/star-ratings/Extracted Star Ratings/Part C 2014 Fall/2014_Part_C_Report_Card_Master_Table_2013_10_17_summary.csv"
star.data.b <- fread(
  ma.path.b,
  skip = 2,
  stringsAsFactors=FALSE, 
  select=c(1:9),
  col.names=c("contractid","org_type","org_marketing","contract_name", "org_parent","snp","sanction", "partc_score","partcd_score"),
  na = c("", "NA", "*")
) %>%
  mutate(
    new_contract=ifelse(partc_score=="Plan too new to be measured",1, ifelse(partcd_score=="Plan too new to be measured",1,0)),
    partc_score  = ifelse(new_contract == 1, NA_real_, parse_number(as.character(partc_score))),
    partcd_score = ifelse(new_contract == 1, NA_real_, parse_number(as.character(partcd_score)))
  ) %>%
  select(contractid, new_contract, partc_score, partcd_score)

star.ratings.2014 <- star.data.a %>%
  select(-contract_name, -org_type, -org_marketing) %>%  
  left_join(star.data.b, by=c("contractid")) %>%
  mutate(year=2014)


# Benchmarks data ---------------------------------------------------------
bench.data <- read_csv("data/input/ma/benchmarks/ratebook2014/CountyRate2014.csv",
                       skip=2,
                       col_names=c("ssa","state","county_name","risk_star5",
                                   "risk_star45","risk_star4","risk_star35",
                                   "risk_star3","risk_star25","esrd_ab"))

benchmark.2014 <- bench.data %>%
  select(ssa,risk_star5,risk_star45,risk_star4,risk_star35,risk_star3,risk_star25) %>%
  mutate(aged_parta=NA_real_, aged_partb=NA_real_, risk_ab=NA_real_,
         risk_bonus5=NA_real_, risk_bonus35=NA_real_, risk_bonus0=NA_real_,
         year=2014)

# Merge data --------------------------------------------------------------
ma.2014 <- plan.data.2014 %>%
  inner_join(service.data.2014 %>% select(contractid, fips),
             by = c("contractid","fips")) %>%
  filter(!state %in% c("VI","PR","MP","GU","AS",""),
         snp == "No",
         (planid < 800 | planid >= 900),
         !is.na(planid), !is.na(fips)) %>%
  left_join(pen.2014 %>% ungroup() %>% rename(state_long = state, county_long = county) %>%
              mutate(state_long=str_to_lower(state_long)) %>% 
              group_by(fips) %>% mutate(ncount=n()) %>% filter(ncount==1),
            by = c("fips"))

state.2014 <- ma.2014 %>%
  group_by(state) %>%
  summarize(state_name = last(state_long[!is.na(state_long)]), .groups = "drop")

ma.2014 <- ma.2014 %>%
  mutate(year = coalesce(as.numeric(year.x), as.numeric(year.y), 2014)) %>%
  select(-starts_with("year."))

full.2014 <- ma.2014 %>%
  left_join(state.2014, by = "state") %>%
  mutate(ssa = as.character(ssa)) %>%
  left_join(landscape.2014 %>% mutate(state=str_to_lower(state)), by = c("contractid","planid","state_name" = "state","county")) %>%
  left_join(star.ratings.2014 %>% select(-any_of(c("contract_name", "org_type", "org_marketing"))),
            by = c("contractid")) %>%
  mutate(
    Star_Rating = case_when(
      partd == "No" ~ partc_score,
      partd == "Yes" & is.na(partcd_score) ~ partc_score,
      partd == "Yes" & !is.na(partcd_score) ~ partcd_score
    )
  ) %>%
  left_join(benchmark.2014 %>% filter(!is.na(ssa)), by = c("ssa")) %>%
  select(-starts_with("year.")) %>%
  mutate(year = y) %>%
  mutate(
    ma_rate = case_when(
      year < 2012 ~ risk_ab,
      year >= 2012 & year < 2015 & Star_Rating == 5    ~ risk_star5,
      year >= 2012 & year < 2015 & Star_Rating == 4.5  ~ risk_star45,
      year >= 2012 & year < 2015 & Star_Rating == 4    ~ risk_star4,
      year >= 2012 & year < 2015 & Star_Rating == 3.5  ~ risk_star35,
      year >= 2012 & year < 2015 & Star_Rating == 3    ~ risk_star3,
      year >= 2012 & year < 2015 & Star_Rating < 3     ~ risk_star25,
      year >= 2012 & year < 2015 & is.na(Star_Rating)  ~ risk_star35,
      year >= 2015 & Star_Rating >= 4                   ~ risk_bonus5,
      year >= 2015 & Star_Rating < 4                    ~ risk_bonus0,
      year >= 2015 & is.na(Star_Rating)                 ~ risk_bonus35
    )
  )

# Save data ---------------------------------------------------------------
write_csv(full.2014,"data/output/data-2014.csv")


# Clear all objects to prepare for the next year -------------------------------------------------------
gc()


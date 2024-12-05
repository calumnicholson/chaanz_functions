library(tidyverse)

sample.groupb <- function(data, recruits, seed) {
  
  set.seed(seed)
  
  # Calculate the numbers needed for each group
  n.mild.f <- 75 - recruits %>% filter(sex == 0) %>% filter(chd_severity == 1) %>% nrow()
  n.mild.m <- 75 - recruits %>% filter(sex == 1) %>% filter(chd_severity == 1) %>% nrow()
  
  n.moderate.f <- 150 - recruits %>% filter(sex == 0) %>% filter(chd_severity == 2) %>% nrow()
  n.moderate.m <- 150 - recruits %>% filter(sex == 1) %>% filter(chd_severity == 2) %>% nrow()
  
  n.severe.f <- 75 - recruits %>% filter(sex == 0) %>% filter(chd_severity == 3) %>% nrow()
  n.severe.m <- 75 - recruits %>% filter(sex == 1) %>% filter(chd_severity == 3) %>% nrow()
  
  # Split the groups by severity
  mild <- data %>% filter(chd_complexity == 'mild')
  moderate <- data %>% filter(chd_complexity == 'moderate')
  severe <- data %>% filter(chd_complexity == 'severe')
  
  # Split each severity group by gender
  mild.m <- mild %>% filter(sex == 'Male')
  mild.f <- mild %>% filter(sex == 'Female')
  
  moderate.m <- moderate %>% filter(sex == 'Male')
  moderate.f <- moderate %>% filter(sex == 'Female')
  
  severe.m <- severe %>% filter(sex == 'Male')
  severe.f <- severe %>% filter(sex == 'Female')
  
  # Mild Samples
  sample.mild.m <- mild.m %>%
    # Weights for location measure
    mutate(loc_weights = case_when(
      location_measure %in% c("City", NA) ~ 
        ( 0.8 * nrow(mild.m) ) / ( nrow(mild.m %>% filter(location_measure %in% c("City", NA))) ),
      location_measure == 'Regional' ~ 
        ( 0.2 * nrow(mild.m) ) / ( nrow(mild.m %>% filter(location_measure == "Regional")) )
    )) %>%
    sample_n(n.mild.m, weight = loc_weights)
  
  sample.mild.f <- mild.f %>%
    # Weights for location measure
    mutate(loc_weights = case_when(
      location_measure %in% c("City", NA) ~ 
        ( 0.8 * nrow(mild.f) ) / ( nrow(mild.f %>% filter(location_measure %in% c("City", NA))) ),
      location_measure == 'Regional' ~ 
        ( 0.2 * nrow(mild.f) ) / ( nrow(mild.f %>% filter(location_measure == "Regional")) )
    )) %>%
    sample_n(n.mild.f, weight = loc_weights)
  
  # Moderate Samples
  sample.moderate.m <- moderate.m %>%
    # Weights for location measure
    mutate(loc_weights = case_when(
      location_measure %in% c("City", NA) ~ 
        ( 0.8 * nrow(moderate.m) ) / ( nrow(moderate.m %>% filter(location_measure %in% c("City", NA))) ),
      location_measure == 'Regional' ~ 
        ( 0.2 * nrow(moderate.m) ) / ( nrow(moderate.m %>% filter(location_measure == "Regional")) )
    )) %>%
    sample_n(n.moderate.m, weight = loc_weights)
  
  sample.moderate.f <- moderate.f %>%
    # Weights for location measure
    mutate(loc_weights = case_when(
      location_measure %in% c("City", NA) ~ 
        ( 0.8 * nrow(moderate.f) ) / ( nrow(moderate.f %>% filter(location_measure %in% c("City", NA))) ),
      location_measure == 'Regional' ~ 
        ( 0.2 * nrow(moderate.f) ) / ( nrow(moderate.f %>% filter(location_measure == "Regional")) )
    )) %>%
    sample_n(n.moderate.f, weight = loc_weights)
  
  # Severe Samples
  sample.severe.m <- severe.m %>%
    # Weights for location measure
    mutate(loc_weights = case_when(
      location_measure %in% c("City", NA) ~ 
        ( 0.8 * nrow(severe.m) ) / ( nrow(severe.m %>% filter(location_measure %in% c("City", NA))) ),
      location_measure == 'Regional' ~ 
        ( 0.2 * nrow(severe.m) ) / ( nrow(severe.m %>% filter(location_measure == "Regional")) )
    )) %>%
    sample_n(n.severe.m, weight = loc_weights)
  
  sample.severe.f <- severe.f %>%
    # Weights for location measure
    mutate(loc_weights = case_when(
      location_measure %in% c("City", NA) ~ 
        ( 0.8 * nrow(severe.f) ) / ( nrow(severe.f %>% filter(location_measure %in% c("City", NA))) ),
      location_measure == 'Regional' ~ 
        ( 0.2 * nrow(severe.f) ) / ( nrow(severe.f %>% filter(location_measure == "Regional")) )
    )) %>%
    sample_n(n.severe.f, weight = loc_weights)
  
  #Join together into final sample
  groupb.sample <- rbind(sample.mild.m,
                         sample.mild.f,
                         sample.moderate.m,
                         sample.moderate.f,
                         sample.severe.m,
                         sample.severe.f)
  
  
  
  message(sprintf(
    "Numbers sampled from each group:
      Mild - Female: 75 - %i = %i
      Mild - Male: 75 - %i = %i
      Moderate - Female: 150 - %i = %i
      Moderate - Male: 150 - %i = %i
      Severe - Female: 75 - %i = %i
      Severe - Male: 75 - %i = %i
Number of patients in replacement sample: %i",
    recruits %>% filter(sex == 0) %>% filter(chd_severity == 1) %>% nrow(), n.mild.f,
    recruits %>% filter(sex == 1) %>% filter(chd_severity == 1) %>% nrow(), n.mild.m,
    recruits %>% filter(sex == 0) %>% filter(chd_severity == 2) %>% nrow(), n.moderate.f,
    recruits %>% filter(sex == 1) %>% filter(chd_severity == 2) %>% nrow(), n.moderate.m,
    recruits %>% filter(sex == 0) %>% filter(chd_severity == 3) %>% nrow(), n.severe.f,
    recruits %>% filter(sex == 1) %>% filter(chd_severity == 3) %>% nrow(), n.severe.m,
    dim(groupb.sample)[1]
  )
  )
  
  groupb.sample
  
  
}
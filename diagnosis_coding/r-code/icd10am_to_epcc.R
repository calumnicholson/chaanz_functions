# Preamble ----------------------------------------------------------------

# Title: ICD10AM to EPCC Coding
# Author: Calum Nicholson
# Purpose: To generate a comprehensive translations from the ICD10AM to the
#          the EPCC short list used in the CHAANZ Registry, using previous work
#          from Keysoft, and EPCC resources


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)

# load the function to load the diagnosis reference table
source(here("coding_tables","drt_select.R"))

# Load data ---------------------------------------------------------------

# Diagnosis Reference Table
drt <- drt.select_V3(drt_path = here("data", "drt/"))
rch.drt <- readxl::read_xlsx(here("diagnosis_coding", "data", 
                                  "RCH", 
                                  "CHAANZ_Registry_Inclusion_Dx.xlsx")) %>%
  clean_names() %>%
  rename("rch_dx_name" = chd_diagnosis_primary_diagnosis_name,
         "icd10_code" = icd10_list_for_primary_diagnosis)

# IPCCC coding
ipccc <- read_rds(here("diagnosis_coding", "data", "ipccc", "ipccc_2021.rds"))

# EPCC coding, from EPCC publications
epcc.final <- read_csv(here("diagnosis_coding", "r-output", 
                            "epcc_coding_final.csv"))
# First version of keysoft coding, from pre 2020
keysoft.coding.v1 <- readxl::read_xlsx(here("diagnosis_coding", "data", 
                                            "from_keysoft", 
                                            "ICD10AM_Mapping_v1.xlsx")) %>%
                        clean_names()
#updated version of keysoft coding, from 2024
keysoft.coding.v2 <- readxl::read_xlsx(here("diagnosis_coding", "data", 
                                            "from_keysoft", 
                                            "RCH_EPIC_Dx_Matchup.xlsx")) %>%
                        drop_na(code_id) %>%
                        clean_names()
# ICD10AM list, v12
icd10am.dx <- read_csv(here("diagnosis_coding", "data",
                            paste0("ICD-10-AM ACHI Electronic Code List - ",
                                   "Twelfth Edition (Final 6 April 2022)"),
                            paste0("01 Disease Codes Current Edition ",
                                   "(12th Edition).csv")
                            )
                       )
icd10am.dx.2 <- read_csv(here("diagnosis_coding", "data", 
                              paste0("ICD-10-AM ACHI Electronic Code List - ",
                                     "Twelfth Edition (Final 6 April 2022)"),
                              "02 Disease Codes (12th Edition).csv"))
icd10am.proc <- read_csv(here("diagnosis_coding", "data", 
                              paste0("ICD-10-AM ACHI Electronic Code List - ",
                                     "Twelfth Edition (Final 6 April 2022)"),
                              paste0("06 Intervention Codes Current Edition ",
                                     "(12th Edition).csv")
                              )
                         )

# Join tables -------------------------------------------------------------

icd10am.join <- icd10am.dx %>%
  
  # Select columns
  select(code_id, ascii_desc, block, 
         block_desc, chapter_range, chapter_desc) %>%
  
  # Join the EPCC codes from Keysoft Coding V1
  left_join(
    keysoft.coding.v1 %>%
      select("code_id" = icd_10_am_code,
             "epcc_code_ks1" = epcc_code) %>%
      drop_na(code_id) %>% 
      mutate(icd_seq = row_number(), .by = code_id) %>%
      filter(icd_seq == 1) %>% 
      select(-icd_seq),
    by = "code_id"
  ) %>%
  
  # Join the EPCC codes from Keysoft Coding V2
  left_join(
    keysoft.coding.v2 %>%
      select(code_id,
             "epcc_code_ks2" = icd10am_diagnosis_ref_icd10_code_ipccc_code
            ),
    by = "code_id"
  ) %>%
  
  # Join the EPCC codes from the EPCC Resource
  left_join(
    epcc.final %>%
           arrange(icd10am_code) %>%
           mutate(icd_seq = row_number(),
                  .by = icd10am_code) %>% 
           select("code_id" = icd10am_code, icd10am_name,
                  epcc_code, epcc_name, icd_seq) %>%
           filter(icd10am_name != "Multiple codes") %>%
           reframe(
                 epcc_code_first = unique(epcc_code)[1],
                 epcc_code_all = toString(unique(epcc_code)),
                 epcc_code_multiple = length(unique(epcc_code)) > 1,
                 .by = c(code_id)
             ),
    by = "code_id"
  ) %>%
  
  # Get relevant chapter groups
  mutate(
    chapter_groups = case_when(
      block_desc == "Congenital malformations of the circulatory system" ~ 
        "CHD",
      block_desc %in% c(paste0("Congenital malformations and deformations ",
                               "of the musculoskeletal system"),
                        "Congenital malformations of eye, ear, face and neck",
                        "Congenital malformations of genital organs",
                        "Congenital malformations of the nervous system",
                        "Congenital malformations of the respiratory system",
                        "Congenital malformations of the urinary system") ~ 
        "other congential",
      chapter_desc == "Diseases of the circulatory system (I00-I99)" ~ 
        "cariovascular",
      !chapter_desc %in% c("Diseases of the circulatory system (I00-I99)",
                           paste0("Congenital malformations, deformations ",
                                  "and chromosomal abnormalities")) ~ 
        "other"
    ),
    
    # Combine into one EPCC translation, preferring Keysoft verions 2, then
    # keysoft version 1, then epcc resource
    epcc_code = case_when(
      !is.na(epcc_code_ks2) ~ epcc_code_ks2,
      !is.na(epcc_code_ks1) ~ epcc_code_ks1,
      !is.na(epcc_code_all) ~ epcc_code_first,
    ),
    # Source of final epcc translation
    epcc_code_source = case_when(
      !is.na(epcc_code_ks2) ~ "keysoft_v2",
      !is.na(epcc_code_ks1) ~ "keysoft_v1",
      !is.na(epcc_code_all) ~ "epcc",
    )
  ) %>%
  
  # Join the EPCC names from the DRT
  left_join(
    drt %>% select(epcc_code, epcc_name),
    by = "epcc_code"
  ) %>%
  
  # tidy columns
  select(
    "icd10am_code" = code_id,
    "icd10am_name" = ascii_desc,
    epcc_code, epcc_name, epcc_code_source, chapter_groups,
    epcc_code_multiple, epcc_code_all, epcc_code_first,
    epcc_code_ks1, epcc_code_ks2,
    block, block_desc, chapter_range, chapter_desc
  )


# Assessing the gaps in translation ----------------------------------------

# Keysoft Translation v1
icd10am.join %>%
  mutate(epcc_tranlsation = case_when(
    is.na(epcc_code_ks1) ~ "not translated",
    epcc_code == "nc" ~ "no code available",
    TRUE ~ "translated")
  ) %>%
  tabyl(chapter_groups, epcc_tranlsation) %>%
  adorn_totals() %>%
  adorn_title()

# Keysoft Translation v2
icd10am.join %>%
  mutate(epcc_tranlsation = case_when(
    is.na(epcc_code_ks2) ~ "not translated",
    epcc_code == "nc" ~ "no code available",
    TRUE ~ "translated")
  ) %>%
  tabyl(chapter_groups, epcc_tranlsation) %>%
  adorn_totals() %>%
  adorn_title()

# EPCC resource
icd10am.join %>%
  mutate(epcc_tranlsation = case_when(
    is.na(epcc_code_first) ~ "not translated",
    epcc_code == "nc" ~ "no code available",
    TRUE ~ "translated")
  ) %>%
  tabyl(chapter_groups, epcc_tranlsation) %>%
  adorn_totals() %>%
  adorn_title()

# Combined
icd10am.join %>%
  mutate(epcc_tranlsation = case_when(
    is.na(epcc_code) ~ "not translated",
    epcc_code == "nc" ~ "no code available",
    TRUE ~ "translated")
  ) %>%
  tabyl(chapter_groups, epcc_tranlsation) %>%
  adorn_totals() %>%
  adorn_title()

# Number of EPCC codes that are not found in the diagnosis reference table
icd10am.join %>%
  filter(!is.na(epcc_code)) %>%
  mutate(in_drt = !is.na(epcc_name)) %>%
  tabyl(in_drt)


#  Save for manual matching -----------------------------------------------

# write.csv(icd10am.join, here("diagnosis_coding",
#                              "r-output", 
#                              paste0("icd10am_to_epcc_for_matching_", 
#                                     format(Sys.Date(), "%Y%m%d"),
#                                     ".csv")),
#           row.names = FALSE,
#           na = "")


# Prepare final version ---------------------------------------------------

# Load manual matching file

icd10am.epcc <- read_csv(
                      here("diagnosis_coding",
                           "r-output",
                           "icd10am_to_epcc_for_matching_20241205_CNedit.csv")
                        )

                  

# Clean final file
icd10am.epcc.clean <- icd10am.epcc %>%
  
  # Add flag for whether EPCC code is found in DRT
  left_join(drt %>% 
              select(epcc_code) %>%
              mutate(in_drt = TRUE),
            by = "epcc_code"
            ) %>%
  mutate(in_drt = if_else(is.na(in_drt), FALSE, TRUE)) %>%
  
  # Select relevant columns to keep
  select(icd10am_code, icd10am_name, epcc_code, epcc_name, chapter_groups, 
         block, block_desc, chapter_range, chapter_desc,
         epcc_code_source, in_drt)

# Checking completeness

# How many ICD codes have an EPCC translation by chapter group
icd10am.epcc.clean %>%
  mutate(epcc_tranlsation = case_when(
                                is.na(epcc_code) ~ "not translated",
                                epcc_code == "nc" ~ "no code available",
                                TRUE ~ "translated")
         ) %>%
  tabyl(chapter_groups, epcc_tranlsation) %>%
  adorn_totals() %>%
  adorn_title()

# How many translated EPCC codes are present in the DRT
icd10am.epcc.clean %>%
  drop_na(epcc_code) %>%
  filter(epcc_code != "nc") %>%
  tabyl(chapter_groups, in_drt) %>%
  adorn_totals() %>%
  adorn_title()

# Save final coding table
write.csv(icd10am.epcc.clean,
          here("diagnosis_coding", "r-output", "icd10amv12_to_epcc.csv"),
          na = "", row.names = TRUE)

library(tidyverse)
library(here)

drt.select_V1 <- function(drt_path = "G:/r-projects/r-projects/central/chaanz_functions/data/drt/archive/",
                       filename = "DRT_CHAANZ_20230105.csv",
                       remove_misc = TRUE,
                       remove_esc = FALSE,
                       remove_sts = TRUE,
                       remove_icd = TRUE ) {
  
  read_csv(paste0(drt_path, filename), col_types = cols(.default = "c")) %>% 
          select("epcc_code" = IPCCC_CODE_R,
                 "epcc_name" = diagnosis_r,
                 "chd" = CHAANZELIGABILITY,
                 "sts_code" = STS_CODE_R,
                 "sts_name" = sts_name_6_22,
                 "icd10_code" = icd10_code_r,
                 mild, moderate, severe, 
                 CHAANZ_source, WARNINGS) %>% 
          mutate(
            mild = as.logical(mild),
            moderate = as.logical(moderate),
            severe = as.logical(severe)) %>% {
                   if(remove_misc) {select(., -CHAANZ_source, -WARNINGS)} else {.}
                 } %>% {
                   if(remove_esc) {select(., -mild, -moderate, -severe)} else {.}
                 } %>% {
                   if(remove_sts) {select(., -sts_code, -sts_name)} else {.}
                 } %>% {
                   if(remove_icd) {select(., -icd10_code)} else {.} 
                 }
}


drt.select_V2 <- function (drt_path = "G:/r-projects/r-projects/central/chaanz_functions/data/drt/",
                           filename = "DRT2_20231026.csv",
                           remove_misc = TRUE,
                           remove_genetic = FALSE,
                           remove_rule = FALSE,
                           remove_chd = FALSE,
                           remove_esc = FALSE,
                           remove_sts = TRUE,
                           remove_icd = TRUE) {
  
  read_csv(paste0(drt_path, filename), col_types = cols(.default = "c")) %>%
    select("epcc_code" = IPCCC_CODE_R, 
           "epcc_name" = DIAGNOSIS_R,
           "chd" = CHAANZELIGIBILITY,
           "genetic" = GENE_FLAG,
           "rule" = RULE,
           "rule_type" = RULE_TYPE,
           "associated_genes" = ASSOCIATED_GENES,
           sts_code, sts_name, icd10_code,
           mild, moderate, severe,
           CHAANZ_source) %>%
    mutate(
      chd = as.numeric(chd),
      genetic = as.numeric(genetic),
      rule = as.numeric(rule),
      mild = as.logical(mild),
      moderate = as.logical(moderate),
      severe = as.logical((severe))) %>% {
       if(remove_misc) {select(., -CHAANZ_source)} else {.}
     } %>% {
       if(remove_genetic) {select(., -genetic, -associated_genes)} else {.}
     } %>% {
       if(remove_rule) {select(., -rule, -rule_type)} else {.}
     } %>% {
       if(remove_chd) {select(., -chd)} else {.}
     }  
}

drt.select_V3 <- function (drt_path = "G:/r-projects/r-projects/central/chaanz_functions/data/drt/",
                           filename = "DRT3_20240305.csv",
                           remove_misc = TRUE,
                           remove_genetic = FALSE,
                           remove_rule = FALSE,
                           remove_chd = FALSE,
                           remove_esc = FALSE,
                           remove_sts = TRUE,
                           remove_icd = TRUE) {
  
  read_csv(paste0(drt_path, filename), col_types = cols(.default = "c")) %>%
    select("epcc_code" = IPCCC_CODE_R, 
           "epcc_name" = DIAGNOSIS_R,
           "chd" = CHAANZELIGIBILITY,
           "genetic" = GENE_FLAG,
           "rule" = RULE,
           "rule_type" = RULE_TYPE,
           "associated_genes" = ASSOCIATED_GENES,
           sts_code, sts_name, icd10_code,
           mild, moderate, severe,
           CHAANZ_source) %>%
    mutate(
      chd = as.numeric(chd),
      genetic = as.numeric(genetic),
      rule = as.numeric(rule),
      mild = as.logical(mild),
      moderate = as.logical(moderate),
      severe = as.logical((severe))) %>% {
        if(remove_misc) {select(., -CHAANZ_source)} else {.}
      } %>% {
        if(remove_genetic) {select(., -genetic, -associated_genes)} else {.}
      } %>% {
        if(remove_rule) {select(., -rule, -rule_type)} else {.}
      } %>% {
        if(remove_chd) {select(., -chd)} else {.}
      }  
}


prt.select <- function (prt_path = "/Volumes/PRJ-CHAANZ/data/",
                        filename = "PRT_CHAANZ_20240312.csv",
                        remove_misc = TRUE,
                        remove_sts = FALSE,
                        remove_chd = FALSE,
                        remove_implied = FALSE) {
  
  read_csv(paste0(prt_path, filename), col_types = cols(.default = "c")) %>%
    select("epcc_code" = IPCCC_CODE_R, 
           "epcc_name" = PROCEDURE_IPCCC_R,
           "sts_code" = STS_CODE_R,
           "sts_name" = PROCEDURE_STS_R,
           "chd" = PROC_ELIGIBILITY,
           implied_diagnosis, implied_code, implied_code_drt,
           "CHAANZ_source" = CHAANZ_SOURCE) %>%
    mutate(chd = as.numeric(chd)) %>% {
             if(remove_misc) {select(., -CHAANZ_source)} else {.}
           } %>% {
             if(remove_sts) {select(., -sts_code, -sts_name)} else {.}
           } %>% {
             if(remove_chd) {select(., -chd)} else {.}
           } %>% {
             if(remove_implied) {select(., -implied_diagnosis, 
                                           -implied_code, 
                                           -implied_code_drt)} else {.}
           }  
}


sts.to.prt.select <- function(prt_path = "/Volumes/PRJ-CHAANZ/data/",
                              filename = "sts_to_epcc_matches_20230413.csv",
                              remove_sts_proc = FALSE,
                              remove_sts_dx = FALSE,
                              remove_comment = TRUE) {
                              
                    read_csv(paste0(prt_path, filename), col_types = cols(.default = "c")) %>% {
                              if(remove_sts_proc) {select(., -sts_code_proc, -sts_name_proc)} else {.}
                            } %>% {
                              if(remove_sts_dx) {select(., -sts_code_dx, -sts_name_dx)} else {.}
                            } %>% {
                              if(remove_comment) {select(., -comment)} else {.}
                            }  

    }


sts.to.drt.select <- function(drt_path = "/Volumes/PRJ-CHAANZ/data/",
                              filename = "sts_diag_for_matching_20230125.csv",
                              remove_comment = TRUE,
                              filter_prt = TRUE) {
  
  read_csv(paste0(drt_path, filename), col_types = cols(.default = "c")) %>% {
    if(filter_prt) {filter(., is.na(comment)|comment!="PRT")} else {.}
  } %>% {
    if(remove_comment) {select(., -comment)} else {.}
  }  
  
}





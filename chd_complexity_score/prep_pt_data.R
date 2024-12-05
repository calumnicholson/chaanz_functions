library(tidyverse)

pt.dx <- read_csv("patient_diagnosis.csv") %>%
           mutate(
             patient_id_scramble = match(patient_id, sample(unique(patient_id)))
           ) %>%
           select(
             -patient_id,
             "patient_id" = patient_id_scramble
           ) %>%
          filter( !is.na(EPCC_code) )

write.csv(pt.dx, "patient_diagnosis.csv", row.names=FALSE)
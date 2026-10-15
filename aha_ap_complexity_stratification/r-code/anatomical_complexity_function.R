library(tidyverse)

calculate_aha_complexity <- function(patient_diagnoses, 
                                     patient_procedures, 
                                     diagnosis_reference,
                                     procedure_reference) {
  
  
  
  # 1. Prepare Baseline Diagnoses
  dx_baseline <- patient_diagnoses %>%
    filter(chaanzeligibility == 1) %>%
    left_join(diagnosis_reference, by = "ipccc_code_r") %>%
    select(zkp_id, ipccc_code_r, aha_tier, is_cyanotic)
  
  # 2. Prepare Procedures with Flags
  proc_flags <- patient_procedures %>%
    left_join(procedure_reference, by = "ipccc_code_r")
  
  # 3. Aggregate Procedures per Patient (Corrective trumps Palliative)
  proc_summary <- proc_flags %>%
    group_by(zkp_id) %>%
    summarise(
      has_corrective = any(is_corrective == 1, na.rm = TRUE),
      has_palliative = any(is_palliative == 1, na.rm = TRUE) & !has_corrective, # Corrective wins
      has_arterial_switch = any(is_arterial_switch == 1, na.rm = TRUE),
      has_atrial_switch   = any(is_atrial_switch == 1, na.rm = TRUE)
    )
  
  # 4. Apply Modulating Rules
  dx_modulated <- dx_baseline %>%
    left_join(proc_summary, by = "zkp_id") %>%
    mutate(
      modulated_tier = case_when(
        # Rule A: TGA + Arterial Switch = Moderate
        ipccc_code_r == "01.01.02" & has_arterial_switch ~ "II: Moderate Complexity",
        
        # Rule B: Cyanotic + Palliative (and no Corrective) = Great Complexity
        is_cyanotic == 1 & has_palliative ~ "III: Great Complexity",
        
        # Default: Keep baseline
        TRUE ~ aha_tier
      )
    )
  
  # 5. Aggregate to Person-Level
  person_complexity <- dx_modulated %>%
    group_by(zkp_id) %>%
    summarise(
      max_tier = max(modulated_tier, na.rm = TRUE),
      moderate_count = sum(modulated_tier == "II: Moderate Complexity", na.rm = TRUE)
    ) %>%
    mutate(
      final_aha_tier = case_when(
        max_tier == "III: Great Complexity" ~ "III: Great Complexity",
        moderate_count >= 3 ~ "III: Great Complexity",
        max_tier == "II: Moderate Complexity" ~ "II: Moderate Complexity",
        TRUE ~ "I: Simple"
      )
    )
  
  return(person_complexity)
}
patient.select <- function(patient_table, stage = c("clean", "final"), other_cols = NA) {
  
        if( !(stage %in% c("clean", "final")) ) {
          
          stop("invalid value for 'stage', must equal 'clean' or 'final'")
          
        } else {
  
        patient_table %>%  {
          if(stage == "clean" & is.na(other_cols)) {select(., zkp_id, zx_owner_id, zx_ownercode_c, zka_alternate, 
                                                           patient_mrn_nhi_no, name_title, name_first, name_middle,
                                                           name_last, name_formernames, dateofbirth, gender, medicare_dva_no, 
                                                           address_l1, address_l2, address_suburb, address_state, address_dhb, 
                                                           address_postcode, address_country, comms_email, comms_mobile_ph, 
                                                           comms_home_ph, comms_work_ph, ethnicity, countryofbirth, 
                                                           indigenous_status, languagespokenathome, maritalstatus, 
                                                           mortality_status, mortality_date, mortality_asofdate, mortality_autopsy, 
                                                           mortality_location, mortality_notes, mortality_primcauseofdeath,
                                                           consent_date_post18, consent_date_pre18)} else {.}
        } %>% {
          if(stage == "clean" & !is.na(other_cols)) {select(., zkp_id, zx_owner_id, zx_ownercode_c, zka_alternate, 
                                                           patient_mrn_nhi_no, name_title, name_first, name_middle,
                                                           name_last, name_formernames, dateofbirth, gender, medicare_dva_no, 
                                                           address_l1, address_l2, address_suburb, address_state, address_dhb, 
                                                           address_postcode, address_country, comms_email, comms_mobile_ph, 
                                                           comms_home_ph, comms_work_ph, ethnicity, countryofbirth, 
                                                           indigenous_status, languagespokenathome, maritalstatus, 
                                                           mortality_status, mortality_date, mortality_asofdate, mortality_autopsy, 
                                                           mortality_location, mortality_notes, mortality_primcauseofdeath,
                                                           consent_date_post18, consent_date_pre18, all_of(other_cols))} else {.}
        } %>% {
          if(stage == "final") {select(., zkp_id, zx_owner_id, zx_ownercode_c, zka_alternate, patient_mrn_nhi_no, name_title, name_first, name_middle,
                                       name_last, name_formernames, dateofbirth, gender, medicare_dva_no, address_l1, address_l2, address_suburb,
                                       address_state, address_dhb, address_postcode, address_country, comms_email, comms_mobile_ph, comms_home_ph,
                                       comms_work_ph, ethnicity, countryofbirth, indigenous_status, languagespokenathome, maritalstatus, mortality_status,
                                       mortality_date, mortality_asofdate, mortality_autopsy, mortality_location, mortality_notes, mortality_primcauseofdeath,
                                       consent_date_post18, consent_date_pre18, last_fup, gene_patient, gene_isolated, structural_only)} else {.}
          
        } # Close select if statement
          
      } # close "stage" else
  
    } # Close function

diagnosis.select <- function(diagnosis_table, stage = "final", other_cols = NA) {
  
  if( !(stage %in% c("clean", "final")) ) {
    
    stop("invalid value for 'stage', must equal 'clean' or 'final'")
    
  } else {
  
  diagnosis_table %>%  {
    if(stage == "clean" & is.na(other_cols)) {select(., zkp_id, zka_alternate, diagnosis_r, ipccc_code_r, diagnosis_date,
                                                     icd10_code_r, sts_code_r, source_diagnosis, source_code, source_codetype,
                                                     source_codedescription, drt_chaanz_source_diagnosis, chaanzeligibility)} else {.}
    } %>% {
    if(stage == "clean" & !is.na(other_cols)) {select(., zkp_id, zka_alternate, diagnosis_r, ipccc_code_r, diagnosis_date,
                                                      icd10_code_r, sts_code_r, source_diagnosis, source_code, source_codetype,
                                                      source_codedescription, drt_chaanz_source_diagnosis, chaanzeligibility, all_of(other_cols))} else {.}
    
    } %>% {
      if(stage == "final") {select(., zkp_id, zka_alternate, diagnosis_r, ipccc_code_r, diagnosis_date,
                                   icd10_code_r, sts_code_r, source_diagnosis, source_code, source_codetype,
                                   source_codedescription, drt_chaanz_source_diagnosis, chaanzeligibility)} else {.}
    
    } # Close select if statement
    
  } # close "stage" else
  
} # Close function

procedure.select <- function(procedure_table, stage = "final", other_cols = NA) {
  
  if( !(stage %in% c("clean", "final")) ) {
    
    stop("invalid value for 'stage', must equal 'clean' or 'final'")
    
  } else {
    
  procedure_table  %>%  {
    if(stage == "clean" & is.na(other_cols)) {select(., zkp_id, zka_alternate, zka_alternate_proc, procedure_ipccc_r, ipccc_code_r, procedure_date,
                                                     treatingsurgeon_r, procedure_sts_r, sts_code_r, source_procedure, source_code,
                                                     source_codetype, source_codedescription, drt_chaanz_source_procedure, chaanzeligibility)} else {.}
    } %>% {
    if(stage == "clean" & !is.na(other_cols)) {select(., zkp_id, zka_alternate, zka_alternate_proc, procedure_ipccc_r, ipccc_code_r, procedure_date,
                                                      treatingsurgeon_r, procedure_sts_r, sts_code_r, source_procedure, source_code,
                                                      source_codetype, source_codedescription, drt_chaanz_source_procedure, chaanzeligibility, all_of(other_cols))} else {.}
    } %>% {
    if(stage == "final")  {select(., zkp_id, zka_alternate, zka_alternate_proc, procedure_ipccc_r, ipccc_code_r, procedure_date,
                                  treatingsurgeon_r, procedure_sts_r, sts_code_r, source_procedure, source_code,
                                  source_codetype, source_codedescription, drt_chaanz_source_procedure, chaanzeligibility)} else {.}
      
      } # Close select if statement
  
    } # close "stage" else
    
  } # Close function








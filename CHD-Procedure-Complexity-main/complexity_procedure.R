library(tidyverse)
library(rlang)
library(magrittr)

esc.complexity.procedure <- function(patient.diagnosis, 
                                     patient.procedures, 
                                     complexity.reference,
                                     implied.reference,
                                     include = c("ASD", "PDA", "VSD", "TGA", "TOF", "Implied")) {
  
  ## This section joins the patient's list of procedures to the implied diagnosis table
  joinedprocedures <- left_join(
    patient.procedures,
    implied.reference %>% drop_na(implied_code),
    by = "procedure_code") %>%
    select(diagnosis_code = implied_code, patient_id) %>%
    drop_na(diagnosis_code) %>%
    separate_longer_delim(cols = diagnosis_code, delim = ",")
  
  ## This generates a list of CHD diagnoses to be used later in the algorithm
  congenital <- complexity.reference %>%
    filter(mild | moderate | severe) %>% # Select only CHD diagnoses
    pull(diagnosis_code) 
  
  ## This is a list of potential residua and sequelae of ASD, VSD and PDA repair 
  ## These would cause the repaired lesion to be moderate rather than mild
  
  sequelae <- list( 
    # Pericarditis etc.
    "10.08.29", "10.08.00", "10.08.09", "10.08.31", 
    "10.08.15", "15.83.00", "10.08.13",
    
    # PDA residuae and sequelae
    "10.14.80", "15.39.01", "15.39.02",
    
    # VSD residuae and sequelae
    "10.16.60", "10.16.62", "15.22.02",
    
    #ASD residuae and sequelae
    "10.17.40", "15.10.61", "15.10.63", "15.10.79",
    
    # Acquired mitral abnormality
    "10.33.01", "10.33.02", "10.33.03", "10.33.04",
    "10.33.06", "15.12.00", "15.12.01", "15.12.03",
    
    # Mitral regurgitation/prolapse
    "06.02.91", "06.02.35",
    
    # Acquired tricuspid abnormality
    "10.32.01", "15.11.00", "15.11.03",
    
    # Tricuspid regurgitation
    "06.01.91") 
  
  # These codes represent pulmonary vascular disease
  ## Comorbidity with these codes upgrades other CHD codes to severe
  
  pulmonary <- list("10.13.50", "10.13.68", "10.13.69",
                    "15.32.01", "15.32.21", "15.32.41",
                    "15.32.23", "15.32.43", "10.13.01",
                    "10.13.02", "10.13.20", "10.13.21",
                    "10.13.06", "10.13.08")  
  patient.complexity <- patient.diagnosis %>%
    
    # Remove "normal heart" diagnoses
    filter(!diagnosis_code %in% c("01.01.00", "01.03.00", "01.03.00")) %>%
    
    ##--ADD IMPLIED CODES--##
    {if ("ASD" %in% include)
      bind_rows(., filter(joinedprocedures, str_detect(diagnosis_code, "05.04.02r")))
      else .} %>%
    {if ("PDA" %in% include)
      bind_rows(., filter(joinedprocedures, str_detect(diagnosis_code, "09.27.21r")))
      else .} %>%
    {if ("VSD" %in% include)
      bind_rows(., filter(joinedprocedures, str_detect(diagnosis_code, "07.10.00r")))
      else .} %>%
    {if ("TGA" %in% include)
      bind_rows(., filter(joinedprocedures, str_detect(diagnosis_code, "01.05.01r")))
      else .} %>%
    {if ("TOF" %in% include)
      bind_rows(., filter(joinedprocedures, str_detect(diagnosis_code, "01.01.01r")))
      else .} %>%
    {if ("Implied" %in% include)
      bind_rows(., filter(joinedprocedures, !str_detect(diagnosis_code, "r")))
      else .} %>%
    distinct() %>%
    
    ##--CONVERT TO WIDE FORMAT--##
    ## First reformat the diagnoses
    mutate(n = 1) %>%
    pivot_wider(id_cols = patient_id, 
                names_from = diagnosis_code, 
                values_from = n) %>%
    ## Next reformat the procedures and join
    bind_rows(patient.procedures %>%
                mutate(n = 1) %>%
                pivot_wider(id_cols = patient_id, 
                            names_from = procedure_code, 
                            values_from = n)) %>%
    group_by(patient_id) %>%
    arrange(patient_id) %>%
    ## Then merge duplicate codes
    summarise(across(where(is.numeric), ~any(!is.na(.x)))) %T>%
    
    ##--INITIALISE NEW COLUMNS--##
    ## Codes appended with an "r" indicate that the defect is repaired
    {newcodes <<- c(
      `05.04.02r` = FALSE, # Repaired ASD
      `05.03.00` = FALSE, # Other ASD Codes
      `05.04.02` = FALSE,
      `05.04.01` = FALSE,
      `05.05.00` = FALSE, # Sinus Venosus Defect
      
      `12.45.13` = FALSE, # Transluminal device closure
      
      `09.27.21r` = FALSE, # Repaired PDA
      `09.27.21` = FALSE, # PDA
      
      `07.10.00r` = FALSE, # Repaired VSD
      `07.10.00` = FALSE, # VSD
      `07.15.05` = FALSE, # Other VSD Codes
      `07.15.01` = FALSE, 
      `07.15.04` = FALSE, 
      `07.10.01` = FALSE, 
      `07.10.12` = FALSE, 
      `07.14.05` = FALSE,
      `07.11.01` = FALSE, 
      `07.12.00` = FALSE, 
      `07.12.01` = FALSE, 
      `07.14.02` = FALSE,
      
      `01.05.01r` = FALSE, # Repaired TGA
      `01.05.01` = FALSE, # Other TGA Codes
      `01.01.02` = FALSE,
      
      `01.01.01r` = FALSE, # Repaired TOF,
      `01.01.01` = FALSE) # TOF
    } %>% 
    add_column(!!!newcodes[!names(newcodes)  %in% names(.)]) %>%  # Adds missing columns
    
    ##--CALCULATE OPERATIVE STATUS--##
    # If operated, set repaired code to TRUE and nonspecific codes to FALSE. #
    
    {if ("ASD" %in% include)
      mutate(.,
             `05.04.02r` = `05.04.02r` | (`05.04.02` & `12.45.13` & !(`09.27.21` | `09.27.21r`)), # Special rule for ASD + Transluminal Device Closure if no PDA
             across(c(`05.03.00`, `05.04.02`,`05.04.01`, `05.05.00`), 
                    ~and(.x, !`05.04.02r`)))
      else .} %>%
    {if ("PDA" %in% include)
      mutate(.,
             `09.27.21r` = `09.27.21r` | (`09.27.21` & `12.45.13` & !(`05.04.02` | `05.04.02r`)), # Special rule for PDA + Transluminal Device Closure if no ASD
             `09.27.21` = `09.27.21` & !`09.27.21r`)
      else .} %>%
    {if ("VSD" %in% include)
      mutate(., across(c(`07.10.00`, `07.15.05`, `07.15.01`, `07.15.04`, `07.10.01`, `07.10.12`, `07.14.05`,`07.11.01`, `07.12.00`, `07.12.01`, `07.14.02`), 
                       ~and(.x, !`07.10.00r`)))
      else .} %>%
    {if ("TGA" %in% include)
      mutate(., across(c(`01.05.01`, `01.01.02`, `01.01.03`), 
                       ~and(.x, !`01.05.01r`)))
      else .} %>%
    {if ("TOF" %in% include)
      mutate(., `01.01.01` = `01.01.01` & !`01.01.01r`)
      else .} %>%
    
    ##--CONVERT BACK TO LONG FORMAT--##
    ## This format is better-suited for the next step of the algorithm
    pivot_longer(-patient_id, names_to = "diagnosis_code") %>%
    filter(value) %>%
    select(-value) %>%
    ## Add in the complexity information for each diagnosis
    left_join(complexity.reference, by = 'diagnosis_code') %>%
    
    ##--THE ESC ALGORITHM--##
    ## This is a two-pass algorithm.
    ## The first pass gives a complexity score to each diagnosis
    ## The second pass adjusts these based on comorbidities
    
    ## FIRST PASS ##
    # sort by Unique ID - ascending
    arrange(patient_id) %>%
    # Create new columns - Diagnosis level
    mutate(# complexity score for each dx                        
      diagnosiscomplexity = ( mild + ( 2 * moderate ) + ( 3 * severe ) ) / 
        ( mild + moderate + severe ),
      # flag for dx with multiple complexity options
      multiplecomplexity = if_else( (mild + moderate + severe == 1), 0, 1)
    ) %>%
    # Create person level dataset, nesting the diagnosis level variables
    nest(diagnosis = c(type, diagnosis_code, diagnosis_name, diagnosis_category, 
                       mild, moderate, severe, 
                       diagnosiscomplexity, multiplecomplexity)) %>%
    ## SECOND PASS ##
    # Reevaluate the inconclusive diagnoses based on what other diagnoses a person has
    mutate(
      diagnosis = 
        map(diagnosis, 
            ~.x %>% 
              filter(type == "Diagnosis") %>%
              mutate(
                diagnosiscomplexity2 = 
                  if_else(
                    multiplecomplexity == 1,
                    case_when(
                      # Atrial Septum Abnormality; Interatrial Communication
                      diagnosis_code %in% c("05.03.00", "05.04.01")  ~ 
                        if (any(diagnosis_code %in% pulmonary)) {3} 
                      else if (any(diagnosis_code %in% "06.06.01")) {2}
                      else if (any(diagnosis_code %in% c("05.03.01", "05.04.03", "05.05.00"))) {1}
                      else {1.5},
                      # ASD in oval fossa
                      diagnosis_code == "05.04.02" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else {1.5},
                      # Mitral Stenosis
                      diagnosis_code == "06.02.92" ~ 
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (any(diagnosis_code %in% "06.02.56")) {2}
                      else {1},
                      # Mitral Valvar Abnormality
                      diagnosis_code == "06.02.00" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (any(diagnosis_code %in% c("06.02.56", "06.02.36"))) {2}
                      else {1},
                      # VSD and varieties
                      diagnosis_code %in% c("07.10.00", "07.10.01", "07.11.01", 
                                            "07.12.00", "07.12.01", "07.14.02") ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (length(unique(diagnosis_code[diagnosis_code %in% congenital])) > 1) {2}
                      else {1},
                      # Pulmonary Stenosis
                      diagnosis_code == "09.05.92" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (any(diagnosis_code %in% c("09.05.01", "09.05.04"))) {1}
                      else {1.5},
                      # Subpulmonary Stenosis
                      diagnosis_code == "07.05.30" ~ 
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else {1.5},
                      # Aortic Stenosis
                      diagnosis_code == "09.15.92" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (length(unique(diagnosis_code[diagnosis_code %in% congenital])) > 1) {2}
                      else {1},
                      # Patent Arterial Duct (PDA)
                      diagnosis_code == "09.27.21" ~ 
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (length(unique(diagnosis_code[diagnosis_code %in% congenital])) > 1) {2}
                      else {1},
                      # AVSD with ventricular imbalance 
                      diagnosis_code == "06.07.26" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else {2},
                      # Aortopulmonary Window
                      diagnosis_code == "09.04.01" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (length(unique(diagnosis_code[diagnosis_code %in% congenital])) > 1) {2}
                      else {1},
                      # Aortic Abnormality 
                      diagnosis_code == "07.09.31" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (any(diagnosis_code %in% "09.29.01")) {2}
                      else {1.5},
                      # Tricuspid Valvar Abnormality
                      diagnosis_code == "06.01.00" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (any(diagnosis_code %in% "06.01.01")) {3}
                      else if (any(diagnosis_code %in% "06.01.34")) {2}
                      else {1.5},
                      # Pulmonary Valvar Abnormality
                      diagnosis_code == "09.05.00" ~
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else if (any(diagnosis_code %in% "09.05.11")) {3}
                      else {1.5},
                      # Tetralogy of Fallot (unknown repair status)
                      diagnosis_code == "01.01.01" ~ 
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else {2.5}
                    ),
                    case_when(
                      # Repaired ASD, VSD or PDA
                      diagnosis_code %in% c("05.04.02r", "09.27.21r", "07.10.00r") ~
                        if (any(diagnosis_code %in% sequelae)) {2}
                      else {1},
                      # Anything with pulmonary vascular disease
                      diagnosis_code %in% congenital ~ 
                        if (any(diagnosis_code %in% pulmonary)) {3}
                      else {diagnosiscomplexity},
                      TRUE ~ diagnosiscomplexity
                    ))
              )) # Closes Map
    ) %>% # Closes Mutate
    
    mutate(# Number of diagnoses per person
      no_dx = map_dbl(diagnosis, ~.x %>% pull(diagnosis_code) %>% unique() %>% length()),
      # if all diagnosis complexity scores are missing, make it 0.5 for unknown
      diagnosis = map(diagnosis, 
                      ~.x %>% mutate(diagnosiscomplexity2 = if_else(is.na(.$diagnosiscomplexity2),
                                                                    0.5, diagnosiscomplexity2))),
      
      # Complexity score for each person based on maximum of diagnosis complexity
      patientcomplexity = map_dbl(diagnosis, ~ max(c(.$diagnosiscomplexity2,0), na.rm = TRUE)), # Patients with no diagnoses, only procedures, have complexity 0.
      # Flag for patients who complexity is uncertain
      uncertaincomplexity = if_else(patientcomplexity %in% c(1,2,3), 0, 1),
      # Create the patient classification
      patientclassification = as_factor(case_when(
        patientcomplexity == 1 ~ "mild",
        patientcomplexity == 2 ~ "moderate",
        patientcomplexity == 3 ~ "severe",
        !is.integer(patientcomplexity) ~ "unknown")
      )
    )
  patient.complexity
}
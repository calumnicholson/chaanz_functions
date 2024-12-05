# A function to calculate eligibility at child sites
calc.el.child <- function(
    
              # Gene Associated Codes, associated codes that make genetic conditions eligible
              marfans.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
              williams.codes = c("09.16.00", "10.14.77", "09.10.01", "09.10.06", "09.10.07", "09.10.25", "09.10.26"),
              loeysdietz.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
              # Codes for child site rules
              pfo.codes = c("05.03.01"),
              pda.codes = c("09.27.00", "09.27.21"),
              valve.codes = c("06.01.00", "06.01.25", "06.01.91", "06.01.92", "06.02.00", "06.02.25", "06.02.91", "09.05.22", "09.15.07", "09.15.91",
                              "10.33.04", "10.33.06", "10.35.04", "10.36.04", "10.36.06", "15.11.03", "15.12.03", "15.30.03", "15.35.03"),
              # The chaanz data tables
              patient, # Patient Table
              diagnosis, # Diagnosis Table
              procedure, # Procedure Table
              followup # Follow Up Table
) {
              # Patients with no diagnosis or procedure
              el.has.dx <- patient %>%
                              select(zka_alternate) %>%
                              # summarise all diagnosis records, count the number of times a patient occurs, join to IDs in patient table
                              left_join(diagnosis %>% count(zka_alternate, sort = TRUE), by = "zka_alternate") %>%
                              rename("n_dx" = n) %>%
                              mutate(
                                # if a patient id was not present in the diagnosis table, make the number of dx = 0
                                n_dx = if_else(is.na(n_dx), 0, n_dx),
                                # If number of DX is not 0, then patient has a diagnosis
                                has_dx = n_dx != 0
                              ) %>%
                              # summaries all procedure records, count the number of times a patient occurs, join to IDs in patient table
                              left_join(procedure %>% count(zka_alternate, sort = TRUE), by = "zka_alternate") %>%
                              rename("n_proc" = n) %>%
                              mutate(
                                # if a patient id was not present in the procedure table, make the number of proc = 0                  
                                n_proc = if_else(is.na(n_proc), 0, n_proc),
                                # If number of proc is not 0, then patient has a procedure
                                has_proc = n_proc != 0
                              )
              
              # Baseline eligibility based on diagnoses
              el.base.chd.dx <- diagnosis %>%
                                  group_by(zka_alternate) %>%
                                  summarise(
                                    # Count number of CHAANZ eligible diagnoses for each patient
                                    n_chd_dx = sum(chaanzeligibility),
                                    # TRUE if patient has at least one CHAANZ eligible diagnosis
                                    is_chd_dx = n_chd_dx > 0
                                  )
              
              # Baseline eligibility based on procedures
              el.base.chd.proc <- procedure %>%
                                      group_by(zka_alternate) %>%
                                      summarise(
                                        # Count number of CHAANZ eligible procedures for each patient
                                        n_chd_proc = sum(chaanzeligibility),
                                        # TRUE if patient has at least one CHAANZ eligible procedure
                                        is_chd_proc = n_chd_proc > 0
                                      )
              
              # Eligibility of genetic conditions, based on their associated codes
              el.gene <- diagnosis %>%
                              group_by(zka_alternate) %>%
                              summarise(
                                # Find all Marfan's Syndrome patients
                                marfans = "14.02.17" %in% ipccc_code_r,
                                # Find all Marfan's Syndrome patients with an associated code
                                marfans_assoc = (marfans == TRUE) & ( any(ipccc_code_r %in% marfans.codes) ),
                                # Find all William's Syndrome patients
                                williams = "14.02.30" %in% ipccc_code_r,
                                # Find all William's Syndrome patients with an associated code
                                williams_assoc = (williams == TRUE) & ( any(ipccc_code_r %in% williams.codes) ),
                                # Find all Loeys-Dietz Syndrome patients
                                loeysdietz = "14.04.85" %in% ipccc_code_r,
                                # Find all Loeys-Dietz Syndrome patients
                                loeysdietz_assoc = (loeysdietz == TRUE) & ( any(ipccc_code_r %in% loeysdietz.codes))
                              )
              
              # Flag to show patient has follow up after 12 months
              el.fup <- followup %>%
                            group_by(zka_alternate) %>%
                            summarise(
                              # Number of follow ups in data for each patient
                              n_fup = n(),
                              # TRUE if there is a follow update date after 12 month of age
                              fup_12m = any(age_at_encounter >= 1)
                            )
              
              # Flags to identify transient circulatory features
              el.child <- diagnosis %>%
                              group_by(zka_alternate) %>%
                              summarise(
                                # Count number of CHAANZ eligible diagnoses for each patient
                                n_chd_dx = sum(chaanzeligibility),
                                # Count the number of pfo codes for each patient
                                n_pfo = sum(ipccc_code_r %in% pfo.codes),
                                # Count the number of pda codes for each patient
                                n_pda = sum(ipccc_code_r %in% pda.codes),
                                # Count the number of valve codes for each patient
                                n_valve = sum(ipccc_code_r %in% valve.codes),
                                # TRUE if pfo present in dx list
                                is_pfo = n_pfo > 0,
                                # TRUE if pda present in dx list
                                is_pda = n_pda > 0,
                                # TRUE if valve present in dx list
                                is_valve = n_valve > 0,
                                # TRUE if transient codes are the only CHD codes in diagnosis list
                                transient_query = (n_chd_dx == sum(n_pfo, n_pda, n_valve)) & (n_chd_dx > 0)
                              ) %>%
                              # Add flags for follow up after 12m
                              left_join(el.fup, by = "zka_alternate") %>%
                              # transient patients with follow up after 12 months
                              mutate(
                                transient_nofup = transient_query & !fup_12m
                )
              
              # Add all of the eligibility flags together, with the IDs in the patient file as the base patient list
              el.final <- el.has.dx %>%
                              left_join(el.base.chd.dx, by = "zka_alternate") %>%
                              left_join(el.base.chd.proc, by = "zka_alternate") %>%
                              left_join(el.gene, by = "zka_alternate") %>%
                              left_join(el.child, by = "zka_alternate") %>%
                              # Select the relevant eligibility flag columns  
                              select(
                                zka_alternate, # patient ID
                                has_dx, has_proc, # Is there a diagnosis or proc present 
                                is_chd_dx, is_chd_proc, # is there are CHAANZ Eligible diagnosis or proc present
                                marfans_assoc, williams_assoc, loeysdietz_assoc, # The genetic eligibility variables
                                is_pfo, is_pda, is_valve, transient_query, fup_12m, transient_nofup, # The chlid eligibility variables
                              ) %>%
                              # Clean the Eligibility flag columns (fills NAs with FALSE)
                              mutate(
                                is_chd_dx = if_else(is.na(is_chd_dx), FALSE, is_chd_dx),
                                is_chd_proc = if_else(is.na(is_chd_proc), FALSE, is_chd_proc),
                                marfans_assoc = if_else(is.na(marfans_assoc), FALSE, marfans_assoc),
                                williams_assoc = if_else(is.na(williams_assoc), FALSE, williams_assoc),
                                loeysdietz_assoc = if_else(is.na(loeysdietz_assoc), FALSE, loeysdietz_assoc),
                                is_pfo = if_else(is.na(is_pfo), FALSE, is_pfo),
                                is_pda = if_else(is.na(is_pda), FALSE, is_pda),
                                is_valve = if_else(is.na(is_valve), FALSE, is_valve),
                                transient_query = if_else(is.na(transient_query), FALSE, transient_query),
                                fup_12m = if_else(is.na(fup_12m), FALSE, fup_12m),
                                transient_nofup = if_else(is.na(transient_nofup), FALSE, transient_nofup),
                              ) %>%
                              # Calculate Final Eligibility
                              mutate(
                                # Baseline Eligibility, must have CHD diagnosis that is not a transient without follow up, or a CHD procedure
                                el_base = (is_chd_dx & !transient_nofup) | is_chd_proc,
                                # Genetic Eligibility, must have either marfans, williams or loeys dietz, plus assoicated codes
                                el_gene = marfans_assoc | williams_assoc | loeysdietz_assoc,
                                # Final Eligibility, must have Baseline Eligibility = TRUE OR Genetic Eligibility = TRUE
                                el_final = ( (el_base | el_gene) )
                              )    
}


# A function to calculate eligibility at adult sites
calc.el.adult <- function(# Gene Associated Codes, associated codes that make genetic conditions eligible
                          marfans.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                          williams.codes = c("09.16.00", "10.14.77", "09.10.01", "09.10.06", "09.10.07", "09.10.25", "09.10.26"),
                          loeysdietz.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                          # Codes for child site rules
                          pfo.codes = c("05.03.01"),
                          pda.codes = c("09.27.00", "09.27.21"),
                          valve.codes = c("06.01.00", "06.01.25", "06.01.91", "06.01.92", "06.02.00", "06.02.25", "06.02.91", "09.05.22", "09.15.07", "09.15.91",
                                          "10.33.04", "10.33.06", "10.35.04", "10.36.04", "10.36.06", "15.11.03", "15.12.03", "15.30.03", "15.35.03"),
                          patient,
                          diagnosis,
                          procedure
                        ) {
  
                          # Patients with no diagnosis or procedure
                          el.has.dx <- patient %>%
                                            select(zka_alternate) %>%
                                            # summarise all diagnosis records, count the number of times a patient occurs, join to IDs in patient table
                                            left_join(diagnosis %>% count(zka_alternate, sort = TRUE), by = "zka_alternate") %>%
                                            rename("n_dx" = n) %>%
                                            mutate(
                                              # if a patient id was not present in the diagnosis table, make the number of dx = 0
                                              n_dx = if_else(is.na(n_dx), 0, n_dx),
                                              # If number of DX is not 0, then patient has a diagnosis
                                              has_dx = n_dx != 0
                                            ) %>%
                                            # summaries all procedure records, count the number of times a patient occurs, join to IDs in patient table
                                            left_join(procedure %>% count(zka_alternate, sort = TRUE), by = "zka_alternate") %>%
                                            rename("n_proc" = n) %>%
                                            mutate(
                                              # if a patient id was not present in the procedure table, make the number of proc = 0                  
                                              n_proc = if_else(is.na(n_proc), 0, n_proc),
                                              # If number of proc is not 0, then patient has a procedure
                                              has_proc = n_proc != 0
                                            )
                          
                          # Baseline eligibility based on diagnoses
                          el.base.chd.dx <- diagnosis %>%
                                                  group_by(zka_alternate) %>%
                                                  summarise(
                                                    # Count number of CHAANZ eligible diagnoses for each patient
                                                    n_chd_dx = sum(chaanzeligibility),
                                                    # TRUE if patient has at least one CHAANZ eligible diagnosis
                                                    is_chd_dx = n_chd_dx > 0,   
                                                    # Count the number of pfo codes for each patient
                                                    n_pfo = sum(ipccc_code_r %in% pfo.codes),
                                                    # Count the number of pda codes for each patient
                                                    n_pda = sum(ipccc_code_r %in% pda.codes),
                                                    # Count the number of valve codes for each patient
                                                    n_valve = sum(ipccc_code_r %in% valve.codes),
                                                    # TRUE if pfo present in dx list
                                                    is_pfo = n_pfo > 0,
                                                    # TRUE if pda present in dx list
                                                    is_pda = n_pda > 0,
                                                    # TRUE if valve present in dx list
                                                    is_valve = n_valve > 0,
                                                    # TRUE if transient codes are the only CHD codes in diagnosis list
                                                    transient_query = (n_chd_dx == sum(n_pfo, n_pda, n_valve)) & (n_chd_dx > 0)
                                                  )
                          
                          # Baseline eligibility based on procedures
                          el.base.chd.proc <- procedure %>%
                                                  group_by(zka_alternate) %>%
                                                  summarise(
                                                    # Count number of CHAANZ eligible procedures for each patient
                                                    n_chd_proc = sum(chaanzeligibility),
                                                    # TRUE if patient has at least one CHAANZ eligible procedure
                                                    is_chd_proc = n_chd_proc > 0
                                                  )
                          
                          # Eligibility of genetic conditions, based on their associated codes
                          el.gene <- diagnosis %>%
                                          group_by(zka_alternate) %>%
                                          summarise(
                                            # Find all Marfan's Syndrome patients
                                            marfans = "14.02.17" %in% ipccc_code_r,
                                            # Find all Marfan's Syndrome patients with an associated code
                                            marfans_assoc = (marfans == TRUE) & ( any(ipccc_code_r %in% marfans.codes) ),
                                            # Find all William's Syndrome patients
                                            williams = "14.02.30" %in% ipccc_code_r,
                                            # Find all William's Syndrome patients with an associated code
                                            williams_assoc = (williams == TRUE) & ( any(ipccc_code_r %in% williams.codes) ),
                                            # Find all Loeys-Dietz Syndrome patients
                                            loeysdietz = "14.04.85" %in% ipccc_code_r,
                                            # Find all Loeys-Dietz Syndrome patients
                                            loeysdietz_assoc = (loeysdietz == TRUE) & ( any(ipccc_code_r %in% loeysdietz.codes))
                                          )
                          
                          # Add all of the eligibility flags together, with the IDs in the patient file as the base patient list
                          el.final <- el.has.dx %>%
                                            left_join(el.base.chd.dx, by = "zka_alternate") %>%
                                            left_join(el.base.chd.proc, by = "zka_alternate") %>%
                                            left_join(el.gene, by = "zka_alternate") %>%
                                            # Select the relevant eligibility flag columns  
                                            select(
                                              zka_alternate, # patient ID
                                              has_dx, has_proc, # Is there a diagnosis or proc present 
                                              is_chd_dx, is_chd_proc, # is there are CHAANZ Eligible diagnosis or proc present
                                              marfans_assoc, williams_assoc, loeysdietz_assoc, # The genetic eligibility variables
                                              is_pfo, is_pda, is_valve, transient_query, # The child eligibility variables - no child eligibilty applied, but good to check patients with these diags
                                            ) %>%
                                            # Clean the Eligibility flag columns (fills NAs with FALSE)
                                            mutate(
                                              is_chd_dx = if_else(is.na(is_chd_dx), FALSE, is_chd_dx),
                                              is_chd_proc = if_else(is.na(is_chd_proc), FALSE, is_chd_proc),
                                              marfans_assoc = if_else(is.na(marfans_assoc), FALSE, marfans_assoc),
                                              williams_assoc = if_else(is.na(williams_assoc), FALSE, williams_assoc),
                                              loeysdietz_assoc = if_else(is.na(loeysdietz_assoc), FALSE, loeysdietz_assoc),
                                              is_pfo = if_else(is.na(is_pfo), FALSE, is_pfo),
                                              is_pda = if_else(is.na(is_pda), FALSE, is_pda),
                                              is_valve = if_else(is.na(is_valve), FALSE, is_valve),
                                              transient_query = if_else(is.na(transient_query), FALSE, transient_query),
                                            ) %>%
                                            # Calculate Final Eligibility
                                            mutate(
                                              # Baseline Eligibility, must have CHD diagnosis that is not a transient without follow up, or a CHD procedure
                                              el_base = is_chd_dx | is_chd_proc,
                                              # Genetic Eligibility, must have either marfans, williams or loeys dietz, plus assoicated codes
                                              el_gene = marfans_assoc | williams_assoc | loeysdietz_assoc,
                                              # Final Eligibility, must have Baseline Eligibility = TRUE OR Genetic Eligibility = TRUE
                                              el_final = ( (el_base | el_gene) )
                                            )    
                                        }


# A function to calculate eligibility at adult sites
calc.el.adult.noproc <- function(# Gene Associated Codes, associated codes that make genetic conditions eligible
  
                        marfans.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                        williams.codes = c("09.16.00", "10.14.77", "09.10.01", "09.10.06", "09.10.07", "09.10.25", "09.10.26"),
                        loeysdietz.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                        # Codes for child site rules
                        pfo.codes = c("05.03.01"),
                        pda.codes = c("09.27.00", "09.27.21"),
                        valve.codes = c("06.01.00", "06.01.25", "06.01.91", "06.01.92", "06.02.00", "06.02.25", "06.02.91", "09.05.22", "09.15.07", "09.15.91",
                                        "10.33.04", "10.33.06", "10.35.04", "10.36.04", "10.36.06", "15.11.03", "15.12.03", "15.30.03", "15.35.03"),
                        patient,
                        diagnosis
                      ) {
                        
                        # Patients with no diagnosis or procedure
                        el.has.dx <- patient %>%
                          select(zka_alternate) %>%
                          # summarise all diagnosis records, count the number of times a patient occurs, join to IDs in patient table
                          left_join(diagnosis %>% count(zka_alternate, sort = TRUE), by = "zka_alternate") %>%
                          rename("n_dx" = n) %>%
                          mutate(
                            # if a patient id was not present in the diagnosis table, make the number of dx = 0
                            n_dx = if_else(is.na(n_dx), 0, n_dx),
                            # If number of DX is not 0, then patient has a diagnosis
                            has_dx = n_dx != 0
                          )
                        
                        # Baseline eligibility based on diagnoses
                        el.base.chd.dx <- diagnosis %>%
                          group_by(zka_alternate) %>%
                          summarise(
                            # Count number of CHAANZ eligible diagnoses for each patient
                            n_chd_dx = sum(chaanzeligibility),
                            # TRUE if patient has at least one CHAANZ eligible diagnosis
                            is_chd_dx = n_chd_dx > 0,   
                            # Count the number of pfo codes for each patient
                            n_pfo = sum(ipccc_code_r %in% pfo.codes),
                            # Count the number of pda codes for each patient
                            n_pda = sum(ipccc_code_r %in% pda.codes),
                            # Count the number of valve codes for each patient
                            n_valve = sum(ipccc_code_r %in% valve.codes),
                            # TRUE if pfo present in dx list
                            is_pfo = n_pfo > 0,
                            # TRUE if pda present in dx list
                            is_pda = n_pda > 0,
                            # TRUE if valve present in dx list
                            is_valve = n_valve > 0,
                            # TRUE if transient codes are the only CHD codes in diagnosis list
                            transient_query = (n_chd_dx == sum(n_pfo, n_pda, n_valve)) & (n_chd_dx > 0)
                          )
                        
                        # Eligibility of genetic conditions, based on their associated codes
                        el.gene <- diagnosis %>%
                          group_by(zka_alternate) %>%
                          summarise(
                            # Find all Marfan's Syndrome patients
                            marfans = "14.02.17" %in% ipccc_code_r,
                            # Find all Marfan's Syndrome patients with an associated code
                            marfans_assoc = (marfans == TRUE) & ( any(ipccc_code_r %in% marfans.codes) ),
                            # Find all William's Syndrome patients
                            williams = "14.02.30" %in% ipccc_code_r,
                            # Find all William's Syndrome patients with an associated code
                            williams_assoc = (williams == TRUE) & ( any(ipccc_code_r %in% williams.codes) ),
                            # Find all Loeys-Dietz Syndrome patients
                            loeysdietz = "14.04.85" %in% ipccc_code_r,
                            # Find all Loeys-Dietz Syndrome patients
                            loeysdietz_assoc = (loeysdietz == TRUE) & ( any(ipccc_code_r %in% loeysdietz.codes))
                          )
                        
                        # Add all of the eligibility flags together, with the IDs in the patient file as the base patient list
                        el.final <- el.has.dx %>%
                          left_join(el.base.chd.dx, by = "zka_alternate") %>%
                          left_join(el.gene, by = "zka_alternate") %>%
                          # Select the relevant eligibility flag columns  
                          select(
                            zka_alternate, # patient ID
                            has_dx, # Is there a diagnosis or proc present 
                            is_chd_dx, # is there are CHAANZ Eligible diagnosis or proc present
                            marfans_assoc, williams_assoc, loeysdietz_assoc, # The genetic eligibility variables
                            is_pfo, is_pda, is_valve, transient_query, # The child eligibility variables - no child eligibilty applied, but good to check patients with these diags
                          ) %>%
                          # Clean the Eligibility flag columns (fills NAs with FALSE)
                          mutate(
                            is_chd_dx = if_else(is.na(is_chd_dx), FALSE, is_chd_dx),
                            marfans_assoc = if_else(is.na(marfans_assoc), FALSE, marfans_assoc),
                            williams_assoc = if_else(is.na(williams_assoc), FALSE, williams_assoc),
                            loeysdietz_assoc = if_else(is.na(loeysdietz_assoc), FALSE, loeysdietz_assoc),
                            is_pfo = if_else(is.na(is_pfo), FALSE, is_pfo),
                            is_pda = if_else(is.na(is_pda), FALSE, is_pda),
                            is_valve = if_else(is.na(is_valve), FALSE, is_valve),
                            transient_query = if_else(is.na(transient_query), FALSE, transient_query),
                          ) %>%
                          # Calculate Final Eligibility
                          mutate(
                            # Baseline Eligibility, must have CHD diagnosis that is not a transient without follow up, or a CHD procedure
                            el_base = is_chd_dx,
                            # Genetic Eligibility, must have either marfans, williams or loeys dietz, plus assoicated codes
                            el_gene = marfans_assoc | williams_assoc | loeysdietz_assoc,
                            # Final Eligibility, must have Baseline Eligibility = TRUE OR Genetic Eligibility = TRUE
                            el_final = ( (el_base | el_gene) )
                          )    
                      }












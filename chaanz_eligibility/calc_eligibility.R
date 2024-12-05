# A function to calculate eligibility at child sites
calc.el.child <- function(
              # Genetic conditions with potential cardiac involvement
              gene = c("07.01.10", "09.16.05", "10.10.01", "10.10.20", "10.10.25", "10.19.02", "14.01.01", "14.01.02", "14.01.03", 
                       "14.01.04", "14.01.05", "14.01.21", "14.02.00", "14.02.06", "14.02.10", "14.02.17", "14.02.19", "14.02.21", 
                       "14.02.28", "14.02.30", "14.02.34", "14.02.58", "14.02.62", "14.02.66", "14.03.21", "14.04.85"),
              # Gene Associated Codes, associated codes that make genetic conditions eligible
              marfans.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
              williams.codes = c("09.16.00", "10.14.77", "09.10.01", "09.10.06", "09.10.07", "09.10.25", "09.10.26"),
              loeysdietz.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
              # Dilated Cardiomyopathy associated codes
              dcm.codes = c("10.09.01", "10.09.02", "10.09.08", "10.05.01", "10.05.21", "10.05.30", "10.05.31", "10.05.33"),
              # Codes for child site rules
              pfo.codes = c("05.03.01"),
              pda.codes = c("09.27.00", "09.27.21"),
              stenosis.codes = c("04.08.91", "06.01.92", "06.02.13", "06.02.92", "06.02.93", "07.05.30", "07.09.00", "09.05.01", "09.10.01",
                                 "09.10.06", "09.10.07", "09.10.25", "09.10.26", "09.15.13", "09.15.92"),
              rhd.codes = c("10.05.01", "10.05.21", "10.05.30", "10.05.31", "10.05.33"),
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
                                # Find all Loeys-Dietz Syndrome patients with an associated code
                                loeysdietz_assoc = (loeysdietz == TRUE) & ( any(ipccc_code_r %in% loeysdietz.codes)),
                                # Count number of CHAANZ eligible diagnoses for each patient
                                n_chd_dx = sum(chaanzeligibility),
                                # number of genetic, CHAANZ Eligible diagnoses for each patient
                                n_gene_chd = sum( (ipccc_code_r %in% gene) == (chaanzeligibility == 1) & (ipccc_code_r %in% gene) ),
                                # TRUE if the number of CHD diagnoses equals 
                                # the number of genetic diagnoses 
                                # (for patients with at least one CHD)
                                chd_gene_only = (n_chd_dx == n_gene_chd & (n_chd_dx != 0) ) 
                              )
              
              # Eligibility of Dilated Cardiomyopathy, based on their associated codes
              el.dcm <- diagnosis %>%
                group_by(zka_alternate) %>%
                summarise(
                  # Count number of CHAANZ eligible diagnoses for each patient
                  n_chd_dx = sum(chaanzeligibility),
                  # Find all Dilated Cardiomyopathy patients
                  n_dcm = sum("10.10.25" %in% ipccc_code_r),
                  # TRUE if transient codes are the only CHD codes in diagnosis list
                  dcm_query = (n_chd_dx == n_dcm) & (n_chd_dx > 0),  
                  # Find all patients with Dilated Cardiomyopathy and an associated code
                  dcm_assoc = (dcm_query == TRUE) & ( any(ipccc_code_r %in% dcm.codes) )
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
                                # Count the number of stenosis codes for each patient
                                n_stenosis = sum(ipccc_code_r %in% stenosis.codes),
                                # count the number of RHD codes for each patient
                                n_rhd = sum(ipccc_code_r %in% rhd.codes),
                                # TRUE if pfo present in dx list
                                is_pfo = n_pfo > 0,
                                # TRUE if pda present in dx list
                                is_pda = n_pda > 0,
                                # TRUE if stenosis present in dx list
                                is_stenosis = n_stenosis > 0,
                                # TRUE if RHD code is present in dx list
                                is_rhd = n_rhd > 0,
                                # TRUE if transient codes are the only CHD codes in diagnosis list
                                transient_query = (n_chd_dx == sum(n_pfo, n_pda)) & (n_chd_dx > 0),
                                # TRUE if stenosis codes are the only CHD codes in diagnosis list
                                stenosis_query = (n_chd_dx == sum(n_pfo, n_stenosis)) & (n_chd_dx > 0),
                                # TRUE if stenosis codes is the only CHD code and no RHD present
                                stenosis_chd = stenosis_query & !is_rhd
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
                              left_join(el.dcm, by = "zka_alternate") %>%
                              left_join(el.child, by = "zka_alternate") %>%
                              # Select the relevant eligibility flag columns  
                              select(
                                zka_alternate, # patient ID
                                has_dx, has_proc, # Is there a diagnosis or proc present 
                                is_chd_dx, is_chd_proc, # is there are CHAANZ Eligible diagnosis or proc present
                                marfans_assoc, williams_assoc, loeysdietz_assoc, chd_gene_only, # The genetic eligibility variables
                                dcm_query, dcm_assoc, # Dilated Cardiomyopathy variable
                                is_pfo, is_pda, is_stenosis, is_rhd, # flag for presence of diagnoses
                                transient_query, stenosis_query, stenosis_chd, fup_12m, transient_nofup, # The chlid eligibility variables
                              ) %>%
                              # Clean the Eligibility flag columns (fills NAs with FALSE)
                              mutate(
                                is_chd_dx = if_else(is.na(is_chd_dx), FALSE, is_chd_dx),
                                is_chd_proc = if_else(is.na(is_chd_proc), FALSE, is_chd_proc),
                                marfans_assoc = if_else(is.na(marfans_assoc), FALSE, marfans_assoc),
                                williams_assoc = if_else(is.na(williams_assoc), FALSE, williams_assoc),
                                loeysdietz_assoc = if_else(is.na(loeysdietz_assoc), FALSE, loeysdietz_assoc),
                                chd_gene_only = if_else(is.na(chd_gene_only), FALSE, chd_gene_only),
                                dcm_query = if_else(is.na(dcm_query), FALSE, dcm_query),
                                dcm_assoc = if_else(is.na(dcm_assoc), FALSE, dcm_assoc),
                                is_pfo = if_else(is.na(is_pfo), FALSE, is_pfo),
                                is_pda = if_else(is.na(is_pda), FALSE, is_pda),
                                is_stenosis = if_else(is.na(is_stenosis), FALSE, is_stenosis),
                                is_rhd = if_else(is.na(is_rhd), FALSE, is_rhd),
                                transient_query = if_else(is.na(transient_query), FALSE, transient_query),
                                stenosis_query = if_else(is.na(stenosis_query), FALSE, stenosis_query),
                                stenosis_chd = if_else(is.na(stenosis_chd), FALSE, stenosis_chd),
                                fup_12m = if_else(is.na(fup_12m), FALSE, fup_12m),
                                transient_nofup = if_else(is.na(transient_nofup), FALSE, transient_nofup),
                              ) %>%
                              # Update the is_chd_dx flag to be true if patient has a stenosis (child sites only)
                              mutate(is_chd_dx = if_else(stenosis_chd, TRUE, is_chd_dx)) %>%
                              # Calculate Final Eligibility
                              mutate(
                                # Baseline Eligibility, 
                                # must have CHD diagnosis that is not a transient without follow up or 
                                # not a DCM with associated code, or a CHD procedure
                                el_base = (is_chd_dx & (!transient_nofup & !dcm_assoc) ) | is_chd_proc,
                                # Genetic Eligibility, must have either marfans, williams or loeys dietz, plus assoicated codes
                                el_gene = marfans_assoc | williams_assoc | loeysdietz_assoc,
                                # Final Eligibility, must have Baseline Eligibility = TRUE OR Genetic Eligibility = TRUE
                                el_final = ( (el_base | el_gene) )
                              ) %>%
                              # Identify isolated cardiac genetic conditions (no structural CHD)
                              mutate(
                                # TRUE if a patients has an isolated genetic cardiac diagnosis
                                gene_isolated = 
                                  chd_gene_only # The only eligible diagnoses are genetic codes 
                                  |  # OR
                                  (el_gene & !is_chd_dx) # TRUE if a patient has either Marfan's, William's OR Loeys-Dietz, 
                                                         # plus associated codes and no other CHD diagnoses
                              )  %>%
                              # Identify structural CHD patients
                              mutate(
                                structural_only = el_final & 
                                                  !(transient_query&is_pfo&!is_pda) & 
                                                  !gene_isolated
                              ) 
}

# A function to calculate eligibility at adult sites
calc.el.adult <- function(# Genetic conditions with potential cardiac involvement
                          gene = c("07.01.10", "09.16.05", "10.10.01", "10.10.20", "10.10.25", "10.19.02", "14.01.01", "14.01.02", "14.01.03", 
                                   "14.01.04", "14.01.05", "14.01.21", "14.02.00", "14.02.06", "14.02.10", "14.02.17", "14.02.19", "14.02.21", 
                                   "14.02.28", "14.02.30", "14.02.34", "14.02.58", "14.02.62", "14.02.66", "14.03.21", "14.04.85"),
                          # Gene Associated Codes, associated codes that make genetic conditions eligible
                          marfans.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                          williams.codes = c("09.16.00", "10.14.77", "09.10.01", "09.10.06", "09.10.07", "09.10.25", "09.10.26"),
                          loeysdietz.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                          # Dilated Cardiomyopathy associated codes
                          dcm.codes = c("10.09.01", "10.09.02", "10.09.08", "10.05.01", "10.05.21", "10.05.30", "10.05.31", "10.05.33"),
                          # Codes for child site rules
                          pfo.codes = c("05.03.01"),
                          pda.codes = c("09.27.00", "09.27.21"),
                          stenosis.codes = c("04.08.91", "06.01.92", "06.02.13", "06.02.92", "06.02.93", "07.05.30", "07.09.00", "09.05.01", "09.10.01",
                                             "09.10.06", "09.10.07", "09.10.25", "09.10.26", "09.15.00", "09.15.13", "09.15.92"),
                          rhd.codes = c("10.05.01", "10.05.21", "10.05.30", "10.05.31", "10.05.33"),
                          # Tables
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
                                                    # Count the number of stenosis codes for each patient
                                                    n_stenosis = sum(ipccc_code_r %in% stenosis.codes),
                                                    # count the number of RHD codes for each patient
                                                    n_rhd = sum(ipccc_code_r %in% rhd.codes),
                                                    # TRUE if pfo present in dx list
                                                    is_pfo = n_pfo > 0,
                                                    # TRUE if pda present in dx list
                                                    is_pda = n_pda > 0,
                                                    # TRUE if stenosis present in dx list
                                                    is_stenosis = n_stenosis > 0,
                                                    # TRUE if RHD code is present in dx list
                                                    is_rhd = n_rhd > 0,
                                                    # TRUE if transient codes are the only CHD codes in diagnosis list
                                                    transient_query = (n_chd_dx == sum(n_pfo, n_pda)) & (n_chd_dx > 0),
                                                    # TRUE if stenosis codes are the only CHD codes in diagnosis list
                                                    stenosis_query = (n_chd_dx == sum(n_pfo, n_stenosis)) & (n_chd_dx > 0),
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
                                            loeysdietz_assoc = (loeysdietz == TRUE) & ( any(ipccc_code_r %in% loeysdietz.codes)),
                                            # Count number of CHAANZ eligible diagnoses for each patient
                                            n_chd_dx = sum(chaanzeligibility),
                                            # number of genetic, CHAANZ Eligible diagnoses for each patient
                                            n_gene_chd = sum( (ipccc_code_r %in% gene) == (chaanzeligibility == 1) & (ipccc_code_r %in% gene) ),
                                            # TRUE if the number of CHD diagnoses equals 
                                            # the number of genetic diagnoses 
                                            # (for patients with at least one CHD)
                                            chd_gene_only = (n_chd_dx == n_gene_chd & (n_chd_dx != 0) ) 
                                          )
                          
                          # Eligibility of Dilated Cardiomyopathy, based on their associated codes
                          el.dcm <- diagnosis %>%
                            group_by(zka_alternate) %>%
                            summarise(
                              # Count number of CHAANZ eligible diagnoses for each patient
                              n_chd_dx = sum(chaanzeligibility),
                              # Find all Dilated Cardiomyopathy patients
                              n_dcm = sum("10.10.25" %in% ipccc_code_r),
                              # TRUE if transient codes are the only CHD codes in diagnosis list
                              dcm_query = (n_chd_dx == n_dcm) & (n_chd_dx > 0),  
                              # Find all patients with Dilated Cardiomyopathy and an associated code
                              dcm_assoc = (dcm_query == TRUE) & ( any(ipccc_code_r %in% dcm.codes) )
                            )
                          
                          # Add all of the eligibility flags together, with the IDs in the patient file as the base patient list
                          el.final <- el.has.dx %>%
                                            left_join(el.base.chd.dx, by = "zka_alternate") %>%
                                            left_join(el.base.chd.proc, by = "zka_alternate") %>%
                                            left_join(el.gene, by = "zka_alternate") %>%
                                            left_join(el.dcm, by = "zka_alternate") %>%
                                            # Select the relevant eligibility flag columns  
                                            select(
                                              zka_alternate, # patient ID
                                              has_dx, has_proc, # Is there a diagnosis or proc present 
                                              is_chd_dx, is_chd_proc, # is there are CHAANZ Eligible diagnosis or proc present
                                              marfans_assoc, williams_assoc, loeysdietz_assoc, chd_gene_only, # The genetic eligibility variables
                                              dcm_query, dcm_assoc, # Dilated Cardiomyopathy variable
                                              is_pfo, is_pda, is_stenosis, is_rhd, # flag for presence of diagnoses
                                              transient_query, stenosis_query # The child eligibility variables 
                                              ### no child eligibility applied, but good to check patients with these diags
                                            ) %>%
                                            # Clean the Eligibility flag columns (fills NAs with FALSE)
                                            mutate(
                                              is_chd_dx = if_else(is.na(is_chd_dx), FALSE, is_chd_dx),
                                              is_chd_proc = if_else(is.na(is_chd_proc), FALSE, is_chd_proc),
                                              marfans_assoc = if_else(is.na(marfans_assoc), FALSE, marfans_assoc),
                                              williams_assoc = if_else(is.na(williams_assoc), FALSE, williams_assoc),
                                              loeysdietz_assoc = if_else(is.na(loeysdietz_assoc), FALSE, loeysdietz_assoc),
                                              chd_gene_only = if_else(is.na(chd_gene_only), FALSE, chd_gene_only),
                                              dcm_query = if_else(is.na(dcm_query), FALSE, dcm_query),
                                              dcm_assoc = if_else(is.na(dcm_assoc), FALSE, dcm_assoc),
                                              is_pfo = if_else(is.na(is_pfo), FALSE, is_pfo),
                                              is_pda = if_else(is.na(is_pda), FALSE, is_pda),
                                              is_stenosis = if_else(is.na(is_stenosis), FALSE, is_stenosis),
                                              is_rhd = if_else(is.na(is_rhd), FALSE, is_rhd),
                                              transient_query = if_else(is.na(transient_query), FALSE, transient_query),
                                              stenosis_query = if_else(is.na(stenosis_query), FALSE, stenosis_query),
                                            ) %>%
                                            # Calculate Final Eligibility
                                            mutate(
                                              # Baseline Eligibility, 
                                              # must have CHD diagnosis that is not a transient without follow up 
                                              # or not a DCM with associated code, or a CHD procedure
                                              el_base = (is_chd_dx & !dcm_assoc) | is_chd_proc,
                                              # Genetic Eligibility, must have either marfans, williams or loeys dietz, plus assoicated codes
                                              el_gene = marfans_assoc | williams_assoc | loeysdietz_assoc,
                                              # Final Eligibility, must have Baseline Eligibility = TRUE OR Genetic Eligibility = TRUE
                                              el_final = ( (el_base | el_gene) )
                                            ) %>%
                                            # Identify isolated cardiac genetic conditions (no structural CHD)
                                            mutate(
                                              # TRUE if a patients has an isolated genetic cardiac diagnosis
                                              gene_isolated = 
                                                chd_gene_only # The only eligible diagnoses are genetic codes 
                                                |  # OR
                                                (el_gene & !is_chd_dx) # TRUE if a patient has either Marfan's, William's OR Loeys-Dietz, 
                                                                       # plus associated codes and no other CHD diagnoses
                                            ) %>%
                                            # Identify structural CHD patients
                                            mutate(
                                              structural_only = el_final & 
                                                !(transient_query&is_pfo&!is_pda) & 
                                                !gene_isolated
                                            ) 
                                                        }


# A function to calculate eligibility at adult sites, where the procedure code is missing
calc.el.adult.noproc <- function(
                        # Genetic conditions with potential cardiac involvement
                        gene = c("07.01.10", "09.16.05", "10.10.01", "10.10.20", "10.10.25", "10.19.02", "14.01.01", "14.01.02", "14.01.03", 
                                 "14.01.04", "14.01.05", "14.01.21", "14.02.00", "14.02.06", "14.02.10", "14.02.17", "14.02.19", "14.02.21", 
                                 "14.02.28", "14.02.30", "14.02.34", "14.02.58", "14.02.62", "14.02.66", "14.03.21", "14.04.85"),
                        # Gene Associated Codes, associated codes that make genetic conditions eligible
                        marfans.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                        williams.codes = c("09.16.00", "10.14.77", "09.10.01", "09.10.06", "09.10.07", "09.10.25", "09.10.26"),
                        loeysdietz.codes = c("09.16.09", "10.14.40", "10.14.42", "10.14.43", "10.14.44", "10.14.50", "10.14.51", "09.28.16", "06.02.35"),
                        # Dilated Cardiomyopathy assoicated codes
                        dcm.codes = c("10.09.01", "10.09.02", "10.09.08", "10.05.01", "10.05.21", "10.05.30", "10.05.31", "10.05.33"),
                        # Codes for child site rules
                        pfo.codes = c("05.03.01"),
                        pda.codes = c("09.27.00", "09.27.21"),
                        stenosis.codes = c("04.08.91", "06.01.92", "06.02.13", "06.02.92", "06.02.93", "07.05.30", "07.09.00", "09.05.01", "09.10.01",
                                           "09.10.06", "09.10.07", "09.10.25", "09.10.26", "09.15.00", "09.15.13", "09.15.92"),
                        rhd.codes = c("10.05.01", "10.05.21", "10.05.30", "10.05.31", "10.05.33"),
                        # Tables
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
                            # Count the number of stenosis codes for each patient
                            n_stenosis = sum(ipccc_code_r %in% stenosis.codes),
                            # count the number of RHD codes for each patient
                            n_rhd = sum(ipccc_code_r %in% rhd.codes),
                            # TRUE if pfo present in dx list
                            is_pfo = n_pfo > 0,
                            # TRUE if pda present in dx list
                            is_pda = n_pda > 0,
                            # TRUE if stenosis present in dx list
                            is_stenosis = n_stenosis > 0,
                            # TRUE if RHD code is present in dx list
                            is_rhd = n_rhd > 0,
                            # TRUE if transient codes are the only CHD codes in diagnosis list
                            transient_query = (n_chd_dx == sum(n_pfo, n_pda)) & (n_chd_dx > 0),
                            # TRUE if stenosis codes are the only CHD codes in diagnosis list
                            stenosis_query = (n_chd_dx == sum(n_pfo, n_stenosis)) & (n_chd_dx > 0)
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
                            loeysdietz_assoc = (loeysdietz == TRUE) & ( any(ipccc_code_r %in% loeysdietz.codes)),
                            # Count number of CHAANZ eligible diagnoses for each patient
                            n_chd_dx = sum(chaanzeligibility),
                            # number of genetic, CHAANZ Eligible diagnoses for each patient
                            n_gene_chd = sum( (ipccc_code_r %in% gene) == (chaanzeligibility == 1) & (ipccc_code_r %in% gene) ),
                            # TRUE if the number of CHD diagnoses equals 
                            # the number of genetic diagnoses 
                            # (for patients with at least one CHD)
                            chd_gene_only = (n_chd_dx == n_gene_chd & (n_chd_dx != 0) ) 
                          )
                        
                        # Eligibility of Dilated Cardiomyopathy, based on their associated codes
                        el.dcm <- diagnosis %>%
                          group_by(zka_alternate) %>%
                          summarise(# Count number of CHAANZ eligible diagnoses for each patient
                                    n_chd_dx = sum(chaanzeligibility),
                                    # Find all Dilated Cardiomyopathy patients
                                    n_dcm = sum("10.10.25" %in% ipccc_code_r),
                                    # TRUE if transient codes are the only CHD codes in diagnosis list
                                    dcm_query = (n_chd_dx == n_dcm) & (n_chd_dx > 0),  
                                    # Find all patients with Dilated Cardiomyopathy and an associated code
                                    dcm_assoc = (dcm_query == TRUE) & ( any(ipccc_code_r %in% dcm.codes) )
                                    )
                        
                        # Add all of the eligibility flags together, with the IDs in the patient file as the base patient list
                        el.final <- el.has.dx %>%
                          left_join(el.base.chd.dx, by = "zka_alternate") %>%
                          left_join(el.gene, by = "zka_alternate") %>%
                          left_join(el.dcm, by = "zka_alternate") %>%
                          # Select the relevant eligibility flag columns  
                          select(
                            zka_alternate, # patient ID
                            has_dx, # Is there a diagnosis or proc present 
                            is_chd_dx, # is there are CHAANZ Eligible diagnosis or proc present
                            marfans_assoc, williams_assoc, loeysdietz_assoc, chd_gene_only, # The genetic eligibility variables
                            dcm_query, dcm_assoc, # Dilated Cardiomyopathy variable
                            is_pfo, is_pda, is_stenosis, is_rhd, # flag for presence of diagnoses
                            transient_query, stenosis_query # The child eligibility variables 
                            ### no child eligibility applied, but good to check patients with these diags
                          ) %>%
                          # Clean the Eligibility flag columns (fills NAs with FALSE)
                          mutate(
                            is_chd_dx = if_else(is.na(is_chd_dx), FALSE, is_chd_dx),
                            marfans_assoc = if_else(is.na(marfans_assoc), FALSE, marfans_assoc),
                            williams_assoc = if_else(is.na(williams_assoc), FALSE, williams_assoc),
                            loeysdietz_assoc = if_else(is.na(loeysdietz_assoc), FALSE, loeysdietz_assoc),
                            chd_gene_only = if_else(is.na(chd_gene_only), FALSE, chd_gene_only),
                            dcm_query = if_else(is.na(dcm_query), FALSE, dcm_query),
                            dcm_assoc = if_else(is.na(dcm_assoc), FALSE, dcm_assoc),
                            is_pfo = if_else(is.na(is_pfo), FALSE, is_pfo),
                            is_pda = if_else(is.na(is_pda), FALSE, is_pda),
                            is_stenosis = if_else(is.na(is_stenosis), FALSE, is_stenosis),
                            is_rhd = if_else(is.na(is_rhd), FALSE, is_rhd),
                            transient_query = if_else(is.na(transient_query), FALSE, transient_query),
                            stenosis_query = if_else(is.na(stenosis_query), FALSE, stenosis_query)
                          ) %>%
                          # Calculate Final Eligibility
                          mutate(
                            # Baseline Eligibility, must have CHD diagnosis that is not a transient without follow up or not a DCM with associated code, or a CHD procedure
                            el_base = is_chd_dx & !dcm_assoc,
                            # Genetic Eligibility, must have either marfans, williams or loeys dietz, plus assoicated codes
                            el_gene = marfans_assoc | williams_assoc | loeysdietz_assoc,
                            # Final Eligibility, must have Baseline Eligibility = TRUE OR Genetic Eligibility = TRUE
                            el_final = ( (el_base | el_gene) )
                          ) %>%
                          # Identify isolated cardiac genetic conditions (no structural CHD)
                          mutate(
                            # TRUE if a patients has an isolated genetic cardiac diagnosis
                            gene_isolated = 
                              chd_gene_only # The only eligible diagnoses are genetic codes 
                            |  # OR
                              (el_gene & !is_chd_dx) # TRUE if a patient has either Marfan's, William's OR Loeys-Dietz, 
                            # plus associated codes and no other CHD diagnoses
                          )  %>%
                          # Identify structural CHD patients
                          mutate(
                            structural_only = el_final & 
                              !(transient_query&is_pfo&!is_pda) & 
                              !gene_isolated
                          )     
                      }












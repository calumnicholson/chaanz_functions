library(tidyverse)
library(here)

get.dx.categories <- function(# Data tables
                              diagnosis.table, procedure.table, eligibility.table,
                              # If the column names for the patient ID and EPCC code
                              # are different, use these argument, but leave as default otherwise
                              id_col = "zka_alternate", dx_col = "ipccc_code_r",
                              proc_col = "ipccc_code_r",
                              # Categories for Diagnosis Categories
                              
                              # Fontan Patients
                              fontan = c("12.30.01", "12.30.05", "12.30.06", "12.30.13", "12.30.27", "12.30.28",
                                          "12.30.32", "12.30.34", "12.30.37", "12.30.50", "12.30.51", "12.30.54"),
                              fontan.failed = c("15.90.60"),
                              fontan.takedown = c("12.30.56", "12.30.31"),
                              
                              # Eisenmenger’s   
                              eisenmenger = c("10.13.08"),
                              eisenmenger.unrepair = c("01.01.40"),
                              
                              # Univentricular Heart but not Fontan
                              uvh = c("01.01.22", "15.20.75", "01.01.09", "12.31.15", "12.10.00", "12.31.03",
                                       "12.31.11", "12.31.06", "12.06.43"),
                              uvh.unrepair = c("01.01.06", "09.05.11", "01.01.07", "01.01.25"),
                              
                              # Transposition of the Great Arteries
                              tga.dx = c("01.01.02", "01.01.18", "01.03.09", "01.05.01"),
                              tga.other = c("01.01.40"),
                              cctga.dx = c("01.01.03"),
                              tga.switch = c("12.29.21"),
                              tga.srv = c("12.29.01", "12.29.02", "12.29.79"),
                              
                              # Truncus Arteriosus
                              truncus = c("09.01.01", "12.11.00"),
                              
                              # Pulmonary atresia – with Pulmonary Valve Replacement
                              pa.dx = c("09.05.11", "01.01.07", "09.05.12"),
                              pa.proc = c("12.13.21", "12.13.22", "12.13.55", "12.25.00"),
                              
                              # Tetralogy of Fallot
                              tof = c("01.01.01", "01.01.20", "01.01.17", "09.05.25"),
                              tof.pa.dx = c("01.01.06"),
                              tof.pa.proc =  c("12.28.01"),
                              
                              # Ebstien's Anomaly
                              ebstein = c("06.01.34", "12.02.09", "12.02.77"),
                              
                              # AVSD
                              avsd.dx = c("06.05.01", "06.05.06", "06.05.98", "06.06.00", "06.06.01", "06.06.08", "06.06.09",
                                          "06.06.10", "06.07.26", "10.34.44", "10.34.60", "15.13.02", "15.14.00", "15.15.00",
                                           "07.14.02"),
                              avsd.proc = c("12.04.01", "12.04.09", "12.04.20", "12.04.40", "12.04.45", "12.05.01", "12.05.10",
                                             "12.05.11", "12.48.02"),
                              
                              # Coarctation of the Aorta
                              coarc = c("09.29.01", "09.29.11", "12.18.30", "09.29.31"),
                              
                              # Congential Complete Heart Block
                              chb = c("11.06.16"),
                              
                              # Aortic valve disorders
                              av.dx = c("09.15.00", "09.15.13", "09.15.01", "09.15.03", "09.15.07",
                                         "09.15.22", "09.15.30", "09.16.00", "07.09.00", "07.09.03",
                                         "07.09.01", "09.15.92", "09.16.00"),
                              av.replace = c("12.13.81", "12.13.84",
                                             "12.16.22", "12.16.28", "12.16.29", "12.16.35",
                                              "12.16.21"),
                              av.ross = c("12.16.30", "12.17.99"),
                              av.a.repair = c("12.16.33", "12.16.50", "12.16.63", "12.16.64",
                                              "12.16.97", "12.17.90", "12.17.91"),
                              av.v.repair = c("12.16.02", "12.16.04", "12.16.11", "12.16.14",
                                              "12.16.25", "12.16.61", "12.16.00", "12.16.05",
                                               "12.16.40", "12.16.61"),
                              
                              # Mitral valve disorders
                              mv.dx = c("06.02.00", "06.02.93", "06.02.07", "06.02.09", "06.02.25",
                                        "06.02.35", "06.02.36", "06.02.12", "06.02.13", "06.02.56",
                                        "05.02.02", "06.02.01", "06.02.92"),
                              mv.replace = c("12.03.11", "12.03.84", "12.04.89"),
                              mv.repair = c("12.01.32", "12.03.00", "12.03.01", "12.03.03",
                                            "12.03.04", "12.03.10", "12.03.19", "12.03.97"),
                              
                              # Pulmonary valve disorders
                              pv.dx = c("09.05.00", "09.05.01", "09.05.04", "09.05.22", "09.07.13", 
                                        "09.05.92", "07.05.01"),
                              pv.replace = c("12.13.21", "12.13.22", "12.13.55", "12.13.86"),
                              pv.repair = c("12.13.00", "12.13.02", "12.13.05", "12.13.09",
                                            "12.13.12", "12.13.15", "12.13.85"),
                              
                              # Tricuspid valve disorders
                              tv.dx = c("06.01.00", "06.01.01", "06.01.03", "06.01.09", "06.01.25", 
                                        "06.01.92"),
                              tv.replace = c("12.02.11", "12.02.83"),
                              tv.repair = c("12.02.00", "12.02.02", "12.02.04", "12.02.22",
                                            "12.02.70", "12.02.83"),
                              
                              # Pulmonary Arterial Hypoplasia
                              pah = c("09.10.11", "09.10.06", "09.10.07"),
                              
                              # Vascular Ring
                              vascular.ring = c("09.31.00", "09.28.09", "12.17.11", "12.17.32"),
                              
                              # Anomalous aortic origin or course of coronary artery 
                              aocca = c("09.42.00", "09.41.01"),
                              
                              # Anomalous Origin of Pulmonary Vein 
                              pul.vein = c("04.06.00", "04.07.01", "04.08.05", "04.08.06", "04.08.10", "04.08.20",
                                           "04.08.30", "01.01.16"),
                              
                              # Ventricular Septal Defect
                              vsd = c("07.10.00", "07.10.01", "07.10.12", "07.11.01", "07.12.00", "07.12.01",
                                      "07.14.05", "07.15.01", "07.15.04", "07.15.05", "07.16.01",
                                      "10.16.62"),
                              
                              # Atrial Septal Defect
                              asd = c("05.04.01", "05.04.02", "05.04.03", "05.05.00", "05.05.03", "15.10.63"),
                              
                              # Congenital Vascular abnormality – without shunt 
                              vasc.abn = c("04.01.01", "09.30.00", "04.03.10", "09.28.00", "09.28.15",
                                           "09.30.02", "09.30.04", "09.16.09"),
                              
                              # Persistent Arterial Duct
                              pda = c("09.27.00", "09.27.21"),
                              
                              # Bicuspid Aortic Valve
                              bav = c("09.15.22")
                              ) {
  
  # Standardise the column names for the next steps
  if (id_col != "zka_alternate") {
    diagnosis.table["zka_alternate"] <- diagnosis.table[id_col]
    procedure.table["zka_alternate"] <- procedure.table[id_col]
    eligibility.table["zka_alternate"] <- eligibility.table[id_col]
  }
  
  if (dx_col != "ipccc_code_r") {
    diagnosis.table["ipccc_code_r"] <- diagnosis.table[dx_col]
  }
  
  if (dx_col != "ipccc_code_r") {
    procedure.table["ipccc_code_r"] <- procedure.table[proc_col]
  }
  
  
  dx.categories <-
    # join all the procedure and diagnosis codes together
    rbind(diagnosis.table %>% select(zka_alternate, ipccc_code_r),
          procedure.table %>% select(zka_alternate, ipccc_code_r)) %>%
    # Add flag for "has procedure"
    left_join(eligibility.table %>% select(zka_alternate, 
                                           has_proc, is_chd_proc),
              by = "zka_alternate") %>%
    # Group by patient
    group_by(zka_alternate) %>%
    # Add a the flags to make if a patient has a code from each group
    reframe(fontan_failed = any(ipccc_code_r %in% fontan.failed), 
            fontan_takedown = any(ipccc_code_r %in% fontan.takedown),
            fontan = any(ipccc_code_r %in% fontan),
            eisenmenger_dx = any(ipccc_code_r %in% eisenmenger),
            eisenmenger_unrepair = any(ipccc_code_r %in% eisenmenger.unrepair) & !all(is_chd_proc),
            eisenmenger = eisenmenger_dx | eisenmenger_unrepair,
            uvh_dx = any(ipccc_code_r %in% uvh),
            uvh_unrepair = any(ipccc_code_r %in% uvh.unrepair) & !all(is_chd_proc),
            uvh = uvh_dx | uvh_unrepair,
            tga_dx = any(ipccc_code_r %in% tga.dx),
            tga_dx_other = any(ipccc_code_r %in% tga.other),
            switch = any(ipccc_code_r %in% tga.switch),
            srv = any(ipccc_code_r %in% tga.srv),
            tga_switch = tga_dx & switch,
            tga_srv_repaired = tga_dx & srv,
            tga_srv_cctga = any(ipccc_code_r %in% cctga.dx),
            tga_other = (tga_dx & all(is_chd_proc)) | tga_dx_other,
            tga_unrepair = tga_dx & !all(is_chd_proc),
            truncus = any(ipccc_code_r %in% truncus),
            pa_dx = any(ipccc_code_r %in% pa.dx),
            pa_proc = any(ipccc_code_r %in% pa.proc),
            pa_replacement = pa_dx & pa_proc,
            tof =  any(ipccc_code_r %in% tof) | any( (ipccc_code_r %in% tof.pa.dx) & 
                                                       (ipccc_code_r %in% tof.pa.proc) ),
            ebstein = any(ipccc_code_r %in% ebstein),
            avsd_dx = any(ipccc_code_r %in% avsd.dx),
            avsd_proc = any(ipccc_code_r %in% avsd.proc),
            avsd_repair = avsd_dx & all(is_chd_proc),
            avsd_unrepair = avsd_dx & !all(is_chd_proc),
            chb = any(ipccc_code_r %in% chb),
            coarc_vsd = any(ipccc_code_r %in% coarc) & any(ipccc_code_r %in% vsd),
            coarc_dx = any(ipccc_code_r %in% coarc),
            coarc_bav = coarc_dx & any(ipccc_code_r %in% bav),
            coarc_bav_repair = coarc_bav & any(ipccc_code_r %in% c(av.replace,
                                                                   mv.replace,
                                                                   pv.replace,
                                                                   tv.replace)),
            coarc = coarc_dx & !coarc_bav_repair,
            av_dx = any(ipccc_code_r %in% av.dx),
            mv_dx = any(ipccc_code_r %in% mv.dx),
            pv_dx = any(ipccc_code_r %in% pv.dx),
            tv_dx = any(ipccc_code_r %in% tv.dx),
            av_replace = any(ipccc_code_r %in% av.replace),
            mv_replace = any(ipccc_code_r %in% mv.replace),
            pv_replace = any(ipccc_code_r %in% pv.replace),
            tv_replace = any(ipccc_code_r %in% tv.replace),
            av_ross = any(ipccc_code_r %in% av.ross),
            av_a_repair = any(ipccc_code_r %in% av.a.repair),
            av_v_repair = any(ipccc_code_r %in% av.v.repair),
            mv_repair = any(ipccc_code_r %in% mv.repair),
            pv_repair = any(ipccc_code_r %in% pv.repair),
            tv_repair = any(ipccc_code_r %in% tv.repair),
            av_unrepair = av_dx & !(av_replace | av_ross | av_a_repair | av_v_repair),
            mv_unrepair = mv_dx & !(mv_replace | mv_repair),
            pv_unrepair = pv_dx & !(pv_replace | pv_repair),
            tv_unrepair = tv_dx & !(tv_replace | tv_repair),
            pah = any(ipccc_code_r %in% pah),
            vascular_ring = any(ipccc_code_r %in% vascular.ring),
            aocca = any(ipccc_code_r %in% aocca),
            pul_vein = any(ipccc_code_r %in% pul.vein),
            vsd = any(ipccc_code_r %in% vsd),
            asd = any(ipccc_code_r %in% asd),
            vasc_abn = any(ipccc_code_r %in% vasc.abn),
            pda = any(ipccc_code_r %in% pda),
            other = !any(fontan_failed, fontan_takedown, fontan, eisenmenger,
                         uvh, tga_switch, tga_srv_repaired, tga_srv_cctga,
                         tga_other, tga_unrepair, truncus, pa_replacement,
                         tof, ebstein, avsd_repair, avsd_unrepair, coarc_vsd,
                         coarc, chb, av_replace, mv_replace, pv_replace,
                         tv_replace, av_ross, av_a_repair, av_v_repair,
                         mv_repair, pv_repair, tv_repair, av_unrepair,
                         mv_unrepair, pv_unrepair, tv_unrepair, pah,
                         vascular_ring, aocca, pul_vein, vsd, asd, vasc_abn,
                         pda)
    ) %>% # Close Reframe
    # Assign patients into one category
    # The order the case_when defines the heirarchy
    mutate(
      chaanz_category = case_when(
        fontan_failed ~ "Failed Fontan circulation",
        fontan_takedown ~ "Takedown of Fontan",
        fontan ~ "Fontan",
        eisenmenger ~ "Eisenmenger's Syndrome",
        uvh ~ "Univentricular Heart (not Fontan)",
        tga_switch ~ "TGA - Arterial Switch",
        tga_srv_repaired ~ "Systemic Right Ventricle - Repaired",
        tga_srv_cctga ~ "Systemic Right Ventricle - cc-TGA",
        tga_other ~ "TGA - Other Repaired",
        tga_unrepair ~ "TGA - Unrepaired",
        truncus ~ "Truncus Arteriosus",
        pa_replacement ~ "Pulmonary atresia w\ PV Replacement",
        tof ~ "Tetralogy of Fallot",
        ebstein ~ "Ebstein's Anomaly",
        avsd_repair ~ "AVSD - Repaired",
        avsd_unrepair ~ "AVSD - Unrepaired",
        chb ~ "Congenital Complete Heart Block",
        coarc_vsd ~ "Coarctation of the Aorta and VSD",
        coarc ~ "Coarctation of the Aorta",
        av_replace ~ "Aortic Valve Disorder - Replacement",
        av_ross ~ "Aortic Valve Disorder - Ross Repair",
        av_a_repair ~ "Aortic Valve Disorder - Aorta Repaired and Valve Replaced",
        mv_replace ~ "Mitral Valve Disorder - Replacement",
        pv_replace ~ "Pulmonary Valve Disorder - Replacement",
        tv_replace ~ "Tricuspid Valve Disorder - Replacement",
        av_v_repair ~ "Aortic Valve Disorder - Valve Repaired",
        mv_repair ~ "Mitral Valve Disorder - Repaired",
        pv_repair ~ "Pulmonary Valve Disorder - Repaired",
        tv_repair ~ "Tricuspid Valve Disorder - Repaired",
        av_unrepair ~ "Aortic Valve Disorder - Unrepaired",
        mv_unrepair ~ "Mitral Valve Disorder - Unrepaired",
        pv_unrepair ~ "Pulmonary Valve Disorder - Unrepaired",
        tv_unrepair ~ "Tricuspid Valve Disorder - Unrepaired",
        pah ~ "Pulmonary Arterial Hypoplasia",
        vascular_ring ~ "Vascular Ring",
        aocca ~ "Anomalous Origin/Course of the Coronary Artery",
        pul_vein ~ "Anomalous Origin of Pulmonary Vein",
        vsd ~ "Ventricular Septal Defect",
        asd ~ "Atrial Septal Defect",
        vasc_abn ~ "Congenital Vascular abnormality without shunt",
        pda ~ "Persistent Arterial Duct",
        other ~ "Other"
      ), # close case_when
      # Make the categories ordered categorical data type
      chaanz_category = factor(
        chaanz_category,
        levels = c("Failed Fontan circulation",
                   "Takedown of Fontan",
                   "Fontan",
                   "Eisenmenger's Syndrome",
                   "Univentricular Heart (not Fontan)",
                   "TGA - Arterial Switch",
                   "Systemic Right Ventricle - Repaired",
                   "Systemic Right Ventricle - cc-TGA",
                   "TGA - Other Repaired",
                   "TGA - Unrepaired",
                   "Truncus Arteriosus",
                   "Pulmonary atresia w\ PV Replacement",
                   "Tetralogy of Fallot",
                   "Ebstein's Anomaly",
                   "AVSD - Repaired",
                   "AVSD - Unrepaired",
                   "Congenital Complete Heart Block",
                   "Coarctation of the Aorta and VSD",
                   "Coarctation of the Aorta",
                   "Aortic Valve Disorder - Replacement",
                   "Aortic Valve Disorder - Ross Repair",
                   "Aortic Valve Disorder - Aorta Repaired and Valve Replaced",
                   "Mitral Valve Disorder - Replacement",
                   "Pulmonary Valve Disorder - Replacement",
                   "Tricuspid Valve Disorder - Replacement",
                   "Aortic Valve Disorder - Valve Repaired",
                   "Mitral Valve Disorder - Repaired",
                   "Pulmonary Valve Disorder - Repaired",
                   "Tricuspid Valve Disorder - Repaired",
                   "Aortic Valve Disorder - Unrepaired",
                   "Mitral Valve Disorder - Unrepaired",
                   "Pulmonary Valve Disorder - Unrepaired",
                   "Tricuspid Valve Disorder - Unrepaired",
                   "Pulmonary Arterial Hypoplasia",
                   "Vascular Ring",
                   "Anomalous Origin/Course of the Coronary Artery",
                   "Anomalous Origin of Pulmonary Vein",
                   "Ventricular Septal Defect",
                   "Atrial Septal Defect",
                   "Congenital Vascular abnormality without shunt",
                   "Persistent Arterial Duct",
                   "Other")
      ) # Close Factor
    ) %>% # Close Mutate 
    # Make another column for some more grouped categories
    mutate(grouped_category = chaanz_category,
           grouped_category = case_when(
             chaanz_category %in% c("Failed Fontan circulation",
                                    "Takedown of Fontan",
                                    "Fontan") ~ "Fontan",
             # split into 
             # Systemic RV and Arterial Switch
             chaanz_category %in% c("TGA - Arterial Switch",
                                    "Systemic Right Ventricle - Repaired",
                                    "Systemic Right Ventricle - cc-TGA",
                                    "TGA - Other Repaired",
                                    "TGA - Unrepaired") ~ "Transposition of the Great Arteries",
             chaanz_category %in% c("AVSD - Repaired",
                                    "AVSD - Unrepaired") ~ "Atrioventricular Septal Defect",
             chaanz_category %in% c("Coarctation of the Aorta and VSD",
                                    "Coarctation of the Aorta") ~ "Coarctation of the Aorta",
             # Could divide into replacement/ross vs other
             chaanz_category %in% c("Aortic Valve Disorder - Replacement",
                                    "Aortic Valve Disorder - Ross Repair",
                                    "Aortic Valve Disorder - Aorta Repaired and Valve Replaced",
                                    "Aortic Valve Disorder - Valve Repaired",
                                    "Aortic Valve Disorder - Unrepaired") ~ "Aortic Valve Disorders",
             chaanz_category %in% c("Mitral Valve Disorder - Replacement",
                                    "Mitral Valve Disorder - Repaired",
                                    "Mitral Valve Disorder - Unrepaired") ~ "Mitral Valve Disorders",
             chaanz_category %in% c("Pulmonary Valve Disorder - Replacement",
                                    "Pulmonary Valve Disorder - Repaired",
                                    "Pulmonary Valve Disorder - Unrepaired") ~ "Pulmonary Valve Disorders",
             chaanz_category %in% c("Tricuspid Valve Disorder - Replacement",
                                    "Tricuspid Valve Disorder - Repaired",
                                    "Tricuspid Valve Disorder - Unrepaired") ~ "Tricuspid Valve Disorders",
             TRUE ~ chaanz_category,
           ) # Close Case When
    ) # Close Mutate
  
  if (id_col != "zka_alternate") {
    dx.categories <- rename(dx.categories, id_col = zka_alternate)
  }
  
  dx.categories
  
  
}

get.dx.combinations <- function(dx.data, 
                                category,
                                asd = c("05.04.01", "05.04.02", "05.04.03", 
                                        "05.05.00", "05.05.03", "06.06.01", 
                                        "15.10.63"),
                                vsd = c("07.10.00", "07.15.05", "07.15.01", 
                                        "07.15.04", "07.10.01", "07.10.12", 
                                        "07.14.05", "07.11.01", "07.12.00", 
                                        "07.12.01", "07.14.02", "07.16.01"),
                                exclude = c("pfo","pda","non-chd"),
                                filter_ndx = "all", #options: c("all", "single only", "combination only")
                                slice_n = 40,
                                caption = "Diagnosis Combination frequency",
                                sep = "<br>"
) {
  
  dx.asd.vsd <- dx.data %>% {
    if(category != 'none') 
    {filter(., chaanz_category == category)} else {.}
  } %>%
    filter(
      ( ipccc_code_r %in% asd ) | ( ipccc_code_r %in% vsd )
    ) %>%
    mutate(diagnosis_short = if_else(ipccc_code_r %in% asd, 
                                     "Atrial septal defect (ASD); various", 
                                     "Ventricular septal defect (VSD); various")
    ) %>%
    distinct(zka_alternate, diagnosis_short)
  
  dx.combinations <- dx.data %>% {
    if(category != 'none') 
    {filter(., chaanz_category == category)} else {.}
  } %>%
    filter(
      !( ( ipccc_code_r %in% asd ) | ( ipccc_code_r %in% vsd ) )
    )  %>%
    # get the short epcc names
    mutate(dx_short = gsub(",$", "", diagnosis_r),
           split_name = str_split(dx_short, ","),
           short_name = map(split_name, ~last(.)),
           short_name = str_trim(short_name, side = "both")) %>%
    select(-dx_short, -split_name) %>%
    rename("diagnosis_short" = short_name) %>% {
      if("non-chd" %in% exclude) 
      { filter(., chaanzeligibility == 1)} else {.}
    } %>%
    select(zka_alternate, diagnosis_short) %>%
    rbind(dx.asd.vsd) %>%
    mutate(diagnosis_short = if_else(diagnosis_short == "Arterial duct (ductus arteriosus) abnormality",
                                     "Patent arterial duct (PDA)", diagnosis_short)
    ) %>% {
      if("pfo" %in% exclude) 
      { filter(., diagnosis_short != "Patent foramen ovale (PFO)")} else {.}
    } %>% {
      if("pda" %in% exclude) 
      { filter(., diagnosis_short != "Patent arterial duct (PDA)")} else {.}
    } %>%
    group_by(zka_alternate) %>%
    reframe(diagnosis_combinations = paste0(sort(diagnosis_short), collapse = sep),
            n_dx = n()) %>%
    count(diagnosis_combinations, n_dx, sort = TRUE)  %>%
    mutate(p = ( n / sum(n) ) * 100)
  
  dx.combinations %>% {
    if(filter_ndx == "single only") { 
      filter(., n_dx == 1)
    } else if (filter_ndx == "combination only") {
      filter(., n_dx > 1)
    } else if (filter_ndx == "all") {.}
  }
}

get.proc.combinations <- function(proc.data, 
                                  category,
                                  slice_n = 40,
                                  caption = "Procedure Combination frequency",
                                  sep = "<br>") {
  
  proc.combinations <- chaanz.proc.cat %>% {
    if(category != 'none') 
    {filter(., chaanz_category == category)} else {.}
  } %>%
    # get the short epcc names
    mutate(proc_short = gsub(",$", "", procedure_ipccc_r),
           split_name = str_split(proc_short, ","),
           short_name = map(split_name, ~last(.)),
           short_name = str_trim(short_name, side = "both")) %>%
    select(-proc_short, -split_name) %>%
    rename("procedure_short" = short_name) %>%
    filter(chd == 1) %>%
    distinct(zka_alternate, procedure_short) %>%
    group_by(zka_alternate) %>%
    reframe(procedure_combinations = paste0(sort(procedure_short), collapse = sep),
            n_proc = n()) %>%
    count(procedure_combinations, n_proc, sort = TRUE)  %>%
    mutate(p = ( n / sum(n) ) * 100)
  
  proc.combinations %>%
    slice_max(n, n = slice_n) %>%
    adorn_totals() %>%
    kbl(format = "html",
        escape=FALSE,
        align=rep('r', 4), # right align the data
        # Add nice Column Names
        col.names = c("Procedures",
                      "n_proc", "n", "%"),
        # Table caption
        caption = caption) %>%
    kable_classic_2(full_width = F, position = "left",
                    html_font = "Cambria",
                    font_size = 16) %>%
    kable_styling("striped")
}



#' Categorize CHD Patients based on Diagnosis and Procedure codes
#'
#' @param diag_df Data frame containing patient diagnosis codes (requires zka_alternate, ipccc_code_r, diagnosis_r)
#' @param proc_df Data frame containing patient procedure codes (requires zka_alternate, ipccc_code_r, procedure_ipccc_r)
#' @param mapping_df The master category mapping (requires epcc_code, category, category_type)
#' @param eligibility_df Data frame for patient eligibility (requires zka_alternate, has_proc, is_chd_proc)
#'
#' @return A data frame grouped by patient (zka_alternate) with assigned CHAANZ categories
categorise_chd_patients <- function(diag_df, proc_df, mapping_df, eligibility_df) {
  
  require(dplyr)
  
  # Internal helper for code selection logic
  select.codes <- function(car_var, cat_name, cat_type_var, cat_type) {
    any(cat_name %in% car_var) & any(cat_type %in% cat_type_var)
  }
  
  # 1. Combine Diagnosis and Procedures
  combined_codes <- rbind(
    diag_df %>% select(zka_alternate, ipccc_code_r, "ipccc_name" = diagnosis_r),
    proc_df %>% select(zka_alternate, ipccc_code_r, "ipccc_name" = procedure_ipccc_r)
  ) %>%
    # Join with the mapping reference
    left_join(mapping_df %>% select(-any_of("diagnosis_r")), by = c("ipccc_code_r" = "epcc_code")) %>%
    # Fill missing categories
    mutate(
      category = if_else(is.na(category), "Other CHD Diagnoses", category),
      category_type = if_else(is.na(category_type), "Other CHD Diagnoses", category_type)
    ) %>%
    # Add eligibility flags
    left_join(eligibility_df %>% select(zka_alternate, has_proc, is_chd_proc), by = "zka_alternate")
  
  # 2. Patient-level Logic and Flagging
  patient_categories <- combined_codes %>%
    group_by(zka_alternate) %>%
    reframe(
      fontan_failed = any("Failed Fontan" %in% category),
      fontan_takedown = any("Takedown of Fontan" %in% category),
      fontan = any("Fontan" %in% category),
      eisenmenger = any("Eisenmenger's Syndrome" %in% category),
      dorv_vsd = any("01.01.40" %in% ipccc_code_r),
      uvh_dx = select.codes(category, "Functionally Univentricular Heart (Not Fontan)", 
                            category_type, c("Diagnosis Codes", "Procedure Codes")),
      uvh_pa = (any("01.01.25" %in% ipccc_code_r) | any("Pulmonary atresia" %in% category)) & 
        !select.codes(category, "Disorders of the Pulmonary Valve", category_type, "Procedure Codes - Replacement"),
      uvh = uvh_dx | uvh_pa,
      tga_dx = select.codes(category, "TGA", category_type, "Diagnosis Codes"),
      tga_dx_other = select.codes(category, "TGA", category_type, "Diagnosis Codes - Other"),
      art_switch = select.codes(category, "TGA", category_type, "Procedure Codes - Arterial Switch"),
      at_switch = select.codes(category, "TGA", category_type, "Procedure Codes - Atrial Switch"),
      other_tga_proc = select.codes(category, "TGA", category_type, "Procedure Codes - Other"),
      tga_switch = tga_dx & art_switch,
      tga_srv_repaired = tga_dx & at_switch,
      tga_srv_cctga = select.codes(category, "TGA", category_type, "Diagnosis Codes - ccTGA"),
      tga_other = (tga_dx | tga_dx_other) & other_tga_proc,
      tga_unrepair = (tga_dx | tga_dx_other) & !all(art_switch, at_switch, other_tga_proc),
      bivent = any("Other Complex Biventricular Circulation" %in% category),
      truncus = any("Truncus Arteriosus" %in% category),
      pa_dx = any(c("09.05.11", "01.01.07", "09.05.12") %in% ipccc_code_r),
      pa_proc_1 = select.codes(category, "Disorders of the Pulmonary Valve", category_type, "Procedure Codes - Replacement"),
      pa_proc_2 = select.codes(category, "Pulmonary atresia – with Pulmonary Valve Replacement", category_type, "Procedure Codes"),
      pa_replacement = pa_dx & (pa_proc_1 | pa_proc_2),
      tof_dx = select.codes(category, "Tetralogy of Fallot (Repaired), and Pulmonary Atresia with VSD (Repaired)", category_type, "Diagnosis Codes - TOF"),
      tof_proc = select.codes(category, "Tetralogy of Fallot (Repaired), and Pulmonary Atresia with VSD (Repaired)", category_type, "Procedure Codes - TOF"),
      tof_pa_dx = select.codes(category, "Pulmonary atresia", category_type, "Diagnosis Codes - PA-VSD"),
      tof_pa_proc = select.codes(category, "Tetralogy of Fallot (Repaired), and Pulmonary Atresia with VSD (Repaired)", category_type, "Procedure Codes - PA-VSD"),
      tof_repair = (tof_dx & tof_proc) | (tof_pa_dx & tof_pa_proc),
      tof_unrepair = (tof_dx & !tof_proc) | (tof_pa_dx & !tof_pa_proc),
      ebstein = any("Ebstein’s Anomaly" %in% category),
      avsd_dx = select.codes(category, "AVSD", category_type, "Diagnosis Codes"),
      avsd_proc = select.codes(category, "AVSD", category_type, "Procedure Codes"),
      avsd_repair = avsd_dx & avsd_proc,
      avsd_unrepair = avsd_dx & !avsd_proc,
      coa_codes = any("Coartation of the Aorta" %in% category),
      coa_bav = coa_codes & any("09.15.22" %in% ipccc_code_r),
      coa = coa_codes & !(coa_bav & select.codes(category, "Disorders of the Aortic Valve", category_type, "Procedure Codes - Replacement")),
      chb = any("Congenital Complete Heart Block" %in% category),
      av_dx = select.codes(category, "Disorders of the Aortic Valve", category_type, "Diagnosis Codes"),
      mv_dx = select.codes(category, "Disorders of the Mitral Valve", category_type, "Diagnosis Codes"),
      pv_dx = select.codes(category, "Disorders of the Pulmonary Valve", category_type, "Diagnosis Codes"),
      tv_dx = select.codes(category, "Disorders of the Tricuspid Valve", category_type, "Diagnosis Codes"),
      av_replace = select.codes(category, "Disorders of the Aortic Valve", category_type, "Procedure Codes - Replacement"),
      mv_replace = select.codes(category, "Disorders of the Mitral Valve", category_type, "Procedure Codes - Replacement"),
      pv_replace = select.codes(category, "Disorders of the Pulmonary Valve", category_type, "Procedure Codes - Replacement"),
      tv_replace = select.codes(category, "Disorders of the Tricuspid Valve", category_type, "Procedure Codes - Replacement"),
      av_ross = select.codes(category, "Disorders of the Aortic Valve", category_type, "Procedure Codes - Ross Repair"),
      av_a_repair = select.codes(category, "Disorders of the Aortic Valve", category_type, "Procedure Codes - Aorta Repair"),
      av_v_repair = select.codes(category, "Disorders of the Aortic Valve", category_type, "Procedure Codes - Valve Repair"),
      mv_repair = select.codes(category, "Disorders of the Mitral Valve", category_type, "Procedure Codes - Repair"),
      pv_repair = select.codes(category, "Disorders of the Pulmonary Valve", category_type, "Procedure Codes - Repair"),
      tv_repair = select.codes(category, "Disorders of the Tricuspid Valve", category_type, "Procedure Codes - Repair"),
      av_unrepair = av_dx & !(av_replace | av_ross | av_a_repair | av_v_repair),
      mv_unrepair = mv_dx & !(mv_replace | mv_repair),
      pv_unrepair = pv_dx & !(pv_replace | pv_repair),
      tv_unrepair = tv_dx & !(tv_replace | tv_repair),
      pah = any("Pulmonary arterial hypoplasia" %in% category),
      vascular_ring = any("Vascular Ring" %in% category),
      aocca = any("Anomalous origin or course of coronary artery" %in% category),
      pul_vein = any("Anomalous Origin of Pulmonary Vein" %in% category),
      vsd = any("Ventricular Septal Defect" %in% category),
      coa_vsd = coa & vsd,
      asd = any("Atrial Septal Defect" %in% category),
      vasc_abn = any("Congenital Vascular abnormality – without shunt" %in% category),
      pda = any("Patent Ductus Arteriosus" %in% category),
      other = !any(fontan_failed, fontan_takedown, fontan, eisenmenger, uvh, tga_switch, 
                   tga_srv_repaired, tga_srv_cctga, tga_other, tga_unrepair, bivent, 
                   truncus, pa_replacement, tof_repair, tof_unrepair, ebstein, 
                   avsd_repair, avsd_unrepair, chb, coa_vsd, coa, av_replace, mv_replace, 
                   pv_replace, tv_replace, av_ross, av_a_repair, av_v_repair, mv_repair, 
                   pv_repair, tv_repair, av_unrepair, mv_unrepair, pv_unrepair, 
                   tv_unrepair, pah, vascular_ring, aocca, pul_vein, vsd, asd, 
                   vasc_abn, pda)
    ) %>%
    # 3. Hierarchy assignment
    mutate(
      chaanz_category = case_when(
        fontan_failed ~ "Failed Fontan circulation",
        fontan_takedown ~ "Takedown of Fontan",
        fontan ~ "Fontan",
        eisenmenger ~ "Eisenmenger's Syndrome",
        uvh ~ "Univentricular Heart (not Fontan)",
        tga_switch ~ "TGA - Arterial Switch",
        tga_srv_repaired ~ "Systemic Right Ventricle – TGA After Atrial Switch",
        tga_srv_cctga ~ "Systemic Right Ventricle - cc-TGA",
        tga_other ~ "TGA - Other Repaired",
        tga_unrepair ~ "TGA - Unrepaired",
        bivent ~ "Other complex bi-ventricular circulation",
        truncus ~ "Truncus Arteriosus",
        pa_replacement ~ "Pulmonary atresia w\\ PV Replacement",
        tof_repair ~ "Tetralogy of Fallot (Repaired), and Pulmonary Atresia with VSD (Repaired)",
        tof_unrepair ~ "Tetralogy of Fallot (Unrepaired, no operation or palliative shunt)",
        ebstein ~ "Ebstein's Anomaly",
        avsd_repair ~ "AVSD - Repaired",
        avsd_unrepair ~ "AVSD - Unrepaired",
        chb ~ "Congenital Complete Heart Block",
        coa_vsd ~ "Coarctation of the Aorta and VSD",
        coa ~ "Coarctation of the Aorta",
        av_replace ~ "Aortic Valve Disorder - Replacement",
        mv_replace ~ "Mitral Valve Disorder - Replacement",
        pv_replace ~ "Pulmonary Valve Disorder - Replacement",
        tv_replace ~ "Tricuspid Valve Disorder - Replacement",
        av_ross ~ "Aortic Valve Disorder - Ross Repair",
        av_a_repair ~ "Aortic Valve Disorder - Aorta Repaired",
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
        vasc_abn ~ "Congenital Vascular abnormality – without shunt",
        pda ~ "Persistent Arterial Duct",
        TRUE ~ "Other"
      )
    ) %>%
    # 4. Final Formatting (Factors and Grouped categories)
    mutate(
      chaanz_category = factor(chaanz_category, levels = c(
        "Failed Fontan circulation", "Takedown of Fontan", "Fontan", "Eisenmenger's Syndrome",
        "Univentricular Heart (not Fontan)", "TGA - Arterial Switch", 
        "Systemic Right Ventricle – TGA After Atrial Switch", "Systemic Right Ventricle - cc-TGA",
        "TGA - Other Repaired", "TGA - Unrepaired", "Other complex bi-ventricular circulation",
        "Truncus Arteriosus", "Pulmonary atresia w\\ PV Replacement", 
        "Tetralogy of Fallot (Repaired), and Pulmonary Atresia with VSD (Repaired)",
        "Tetralogy of Fallot (Unrepaired, no operation or palliative shunt)", "Ebstein's Anomaly",
        "AVSD - Repaired", "AVSD - Unrepaired", "Congenital Complete Heart Block",
        "Coarctation of the Aorta and VSD", "Coarctation of the Aorta", "Aortic Valve Disorder - Replacement",
        "Mitral Valve Disorder - Replacement", "Pulmonary Valve Disorder - Replacement",
        "Tricuspid Valve Disorder - Replacement", "Aortic Valve Disorder - Ross Repair",
        "Aortic Valve Disorder - Aorta Repaired", "Aortic Valve Disorder - Valve Repaired",
        "Mitral Valve Disorder - Repaired", "Pulmonary Valve Disorder - Repaired",
        "Tricuspid Valve Disorder - Repaired", "Aortic Valve Disorder - Unrepaired",
        "Mitral Valve Disorder - Unrepaired", "Pulmonary Valve Disorder - Unrepaired",
        "Tricuspid Valve Disorder - Unrepaired", "Pulmonary Arterial Hypoplasia", "Vascular Ring",
        "Anomalous Origin/Course of the Coronary Artery", "Anomalous Origin of Pulmonary Vein",
        "Ventricular Septal Defect", "Atrial Septal Defect", 
        "Congenital Vascular abnormality – without shunt", "Persistent Arterial Duct", "Other"
      )),
      grouped_category = case_when(
        chaanz_category %in% c("Failed Fontan circulation", "Takedown of Fontan", "Fontan") ~ "Fontan",
        chaanz_category %in% c("TGA - Arterial Switch", "Systemic Right Ventricle – TGA After Atrial Switch", "Systemic Right Ventricle - cc-TGA", "TGA - Other Repaired", "TGA - Unrepaired") ~ "Transposition of the Great Arteries",
        chaanz_category %in% c("Tetralogy of Fallot (Repaired), and Pulmonary Atresia with VSD (Repaired)", "Tetralogy of Fallot (Unrepaired, no operation or palliative shunt)") ~ "Tetralogy of Fallot",
        chaanz_category %in% c("AVSD - Repaired", "AVSD - Unrepaired") ~ "Atrioventricular Septal Defect",
        chaanz_category %in% c("Coarctation of the Aorta and VSD", "Coarctation of the Aorta") ~ "Coarctation of the Aorta",
        chaanz_category %in% c("Aortic Valve Disorder - Replacement", "Aortic Valve Disorder - Ross Repair", "Aortic Valve Disorder - Aorta Repaired", "Aortic Valve Disorder - Valve Repaired", "Aortic Valve Disorder - Unrepaired") ~ "Aortic Valve Disorders",
        chaanz_category %in% c("Mitral Valve Disorder - Replacement", "Mitral Valve Disorder - Repaired", "Mitral Valve Disorder - Unrepaired") ~ "Mitral Valve Disorders",
        chaanz_category %in% c("Pulmonary Valve Disorder - Replacement", "Pulmonary Valve Disorder - Repaired", "Pulmonary Valve Disorder - Unrepaired") ~ "Pulmonary Valve Disorders",
        chaanz_category %in% c("Tricuspid Valve Disorder - Replacement", "Tricuspid Valve Disorder - Repaired", "Tricuspid Valve Disorder - Unrepaired") ~ "Tricuspid Valve Disorders",
        TRUE ~ as.character(chaanz_category)
      )
    )
  
  return(patient_categories)
}
library(tidyverse)
library(readxl)
library(clipr) # Useful for copying directly to your clipboard

# 1. Load your diagnosis and procedure table
drt <- read_excel("data/DRT3_20240305.xlsx", sheet = "DRT3_20240305")
prt <- read_csv("data/PRT_CHAANZ_20240312.csv")

# 2. Extract the NEXT 50 codes (Rows 51 to 100)
next_dx_batch <- drt %>%
  select(IPCCC_CODE_R, DIAGNOSIS_R, CHAANZELIGIBILITY) %>%
  slice(601:689) 

# Copy directly to clipboard to paste into the chat
write_clip(next_dx_batch)

# Extract 100 codes at a time
next_proc_batch <- prt %>%
  select(IPCCC_CODE_R, PROCEDURE_IPCCC_R) %>%
  slice(401:483) # Adjust to 101:200 for the next batch

# Copy to clipboard
write_clip(next_proc_batch)

# 3. AFTER THE CHAT: Read in the categorized data you copied from the chat
# Save the chat's markdown table as a CSV first, or read it from your clipboard
chat_output_complex <- read_excel("data/drt_aha_categories.xlsx", sheet = "Sheet1") 
chat_output_cyanosis <- read_excel("data/drt_aha_categories.xlsx", sheet = "Sheet2") 
chat_output_proc <- read_excel("data/drt_aha_categories.xlsx", sheet = "Sheet3") 

# 4. Join the new categories back to your original dataset
updated_drt <- drt %>%
  left_join(
    chat_output_complex %>% select(IPCCC_CODE_R, CATEGORY, AHA_COMPLEXITY_TIER),
    by = "IPCCC_CODE_R"
  ) %>%
  left_join(
    chat_output_cyanosis %>% select(IPCCC_CODE_R, CYANOTIC_FLAG),
    by = "IPCCC_CODE_R"
  )

updated_prt <- prt %>%
  left_join(
    chat_output_proc %>% select(IPCCC_CODE_R, IS_CORRECTIVE, IS_PALLIATIVE, IS_ARTERIAL_SWITCH, IS_ATRIAL_SWITCH),
    by = "IPCCC_CODE_R"
  )

# 5. Save your progress
write_csv(updated_drt, "r-output/DRT4_draft1_20261015.csv")
write_csv(updated_prt, "r-output/PRT2_draft1_20261015.csv")


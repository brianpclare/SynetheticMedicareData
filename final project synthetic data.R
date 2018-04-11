
# Synthetic Medicare Claims Data - not included in Github repo, over file size limit
# Outpatient Claims - 154 MB file
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DESample01.html

# Variable Guide
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/Downloads/SynPUF_Codebook.pdf
# page 7, page 
library(tidyverse)
library(data.table)

suppressWarnings(
  outpatient <- fread("outpatient claims.csv")
)



AD <- outpatient %>% filter(ICD9_DGNS_CD_1 == "3310") %>% select(DESYNPUF_ID) %>% unique()

AD_patient_claims <- outpatient %>% filter(DESYNPUF_ID %in% AD$DESYNPUF_ID)

AD_patient_codes <- AD_patient_claims %>% filter(ICD9_DGNS_CD_1 != "") %>% filter(ICD9_DGNS_CD_1 != "OTHER") %>% 
  group_by(ICD9_DGNS_CD_1) %>% summarize(count = n()) %>% arrange(desc(count))

AD_num_claims_by_patient <- AD_patient_claims %>% group_by(DESYNPUF_ID) %>% summarize(count = n())
summary(AD_num_claims_by_patient$count)
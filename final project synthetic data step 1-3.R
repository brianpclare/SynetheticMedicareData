
# Synthetic Medicare Claims Data - not included in Github repo, over file size limit
# Outpatient Claims - 154 MB file
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DESample01.html

# Variable Guide
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/Downloads/SynPUF_Codebook.pdf
# page 7, page 
library(tidyverse)
library(dplyr)
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

AD_claim_codes <- AD_patient_claims %>% select(Patient_ID = DESYNPUF_ID, Diagnosis = ICD9_DGNS_CD_1) %>%
  filter(Diagnosis != "") %>% filter(Diagnosis != "OTHER")


non_AD <- outpatient %>% select(DESYNPUF_ID) %>% filter(!(DESYNPUF_ID %in% AD$DESYNPUF_ID)) %>% unique()
non_AD_claims <- outpatient %>% filter(!(DESYNPUF_ID %in% AD$DESYNPUF_ID))


## Let's take a sample of 50 AD patients, 50 other patients

set.seed(455)
model_AD_IDs <- sample_n(AD, 50)
model_non_IDs <- sample_n(non_AD, 50)

model_AD <- AD_patient_claims %>% filter(DESYNPUF_ID %in% model_AD_IDs$DESYNPUF_ID)
model_non <- non_AD_claims %>% filter(DESYNPUF_ID %in% model_non_IDs$DESYNPUF_ID)

#Data Processing
#Step 1 Getting "Patient ID", "Diagnosis", and "Date"
model_AD_codes <- model_AD %>% select(Patient_ID = DESYNPUF_ID, Diagnosis = ICD9_DGNS_CD_1, Date = CLM_FROM_DT) %>% 
  filter(Diagnosis != "") %>% filter(Diagnosis != "OTHER")

model_non_codes <- model_non %>% select(Patient_ID = DESYNPUF_ID, Diagnosis = ICD9_DGNS_CD_1, Date = CLM_FROM_DT) %>% 
  filter(Diagnosis != "") %>% filter(Diagnosis != "OTHER")

#Step 2

#Split the AD sample data by patient
by_patient<-group_by(model_AD_codes,Patient_ID)
split<-split(by_patient, by_patient$Patient_ID)

#A function filtering out all the claims happened before the AD claim
myfunction <- function(x){
  result <- subset(PatientX, Date<=Date[PatientX$Diagnosis == "3310"])
  return(result)
}

#Repeat the process on each patient using "lapply"
AllPatient<-lapply(split, myfunction)

#Step 3, for all patients
total<-rbind(model_AD_codes,model_non_codes)
ADMS<-total%>%filter(Diagnosis == "3310" |Diagnosis == "340")






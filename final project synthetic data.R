
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


## Let's take a sample of 133 AD patients (all of them), 133 other patients

set.seed(740)
model_AD_IDs <- sample_n(AD, 133)
model_non_IDs <- sample_n(non_AD, 133)

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
myfunction <- function(PatientX){
  result <- subset(PatientX, Date<Date[PatientX$Diagnosis == "3310"])
  return(result)
}

#Repeat the process on each patient using "lapply"
AllPatient<-lapply(split, myfunction)
model_AD_patient_claims <- bind_rows(AllPatient)
model_AD_patient_claims$AD <- 1

model_non_codes$AD <- 0


# Step 3, for all patients
total_model <- bind_rows(model_AD_patient_claims, model_non_codes) %>% filter(Diagnosis != "340")

# just stopping here to clean up the environment, all we really need now is total_model and I'll keep the 
# tables that identify the patients by ID

rm(list = c("outpatient", "non_AD_claims", "non_AD", "model_non_codes", "model_non", 
            "model_AD_patient_claims", "model_AD", "by_patient", "split", "AllPatient", 
            "AD", "AD_claim_codes", "AD_num_claims_by_patient", "AD_patient_codes", "model_AD_codes",
            "AD_patient_claims"))

# Step 4

distinct_diagnoses <- total_model %>% ungroup %>% select(Diagnosis) %>% unique()

num_diagnoses <- length(distinct_diagnoses$Diagnosis)

distinct_diagnoses$index <- 1:num_diagnoses

# So there are num_diagnoses unique codes, which will be indexed from 1 to num_diagnoses

model_AD_IDs <- total_model %>% filter(AD == 1) %>% select(Patient_ID) %>% unique()
model_non_IDs <- total_model %>% filter(AD == 0) %>% select(Patient_ID) %>% unique()


for(i in 1:length(total_model$Diagnosis)){
  x <- total_model$Diagnosis[i]
  y <- distinct_diagnoses$index[distinct_diagnoses$Diagnosis == x]
  total_model$Diagnosis[i] <- y
}

total_model <- total_model %>% select(-Date)

y_train <- total_model %>% summarize(n = max(AD)) %>% ungroup() %>% select(-Patient_ID) %>%
  unlist() %>% unname()

claim_counts <- total_model %>% select(Patient_ID) %>% summarize(count = n())
max_claims <- max(claim_counts$count)

split_model <- total_model %>% split(.$Patient_ID) %>% unname()

for(i in 1:length(split_model)){
  split_model[[i]] <- split_model[[i]] %>% ungroup() %>% select(Diagnosis) %>% t()
  
  while(length(split_model[[i]]) < max_claims){
    split_model[[i]] <- append(split_model[[i]], 0, after = 0)
  }
  
}

input_train <- do.call(rbind, split_model)






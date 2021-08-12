# import useful libraries
library("tidyverse")

# set working directory
setwd("/Users/michael/Clinical_Priority/data/OneDrive_1_2-15-2021")

# read in csv
demog <- read_csv("demographics_deid.csv")
diag <- read_csv("diagnoses_deid.csv") # look at
manag <- read_csv("evaluation_management_deid.csv")
labs <- read_csv("labs_deid.csv") # look at
meds <- read_csv("medications_deid.csv")
mpews <- read_csv("mpews_deid.csv")
proced <- read_csv("procedures_deid.csv")
vit <- read_csv("vitals_deid.csv")

# ------------------------------------------------------------------------------
#                   Data Cleaning of (labs) data frame
# ------------------------------------------------------------------------------
# format date column
# labs$EVENT_TS <- as.Date(labs$EVENT_TS)
# reference range low and high need to be converted to numeric
labs$NORMAL_HIGH <- as.numeric(labs$NORMAL_HIGH)
labs$NORMAL_LOW <- as.numeric(labs$NORMAL_LOW)
labs$RESULT_VALUE <- as.numeric(labs$RESULT_VALUE)
# add normal lab values mean and reference range
labs <- labs %>% 
  mutate(NORMAL_MEAN = (NORMAL_HIGH + NORMAL_LOW) / 2) %>% 
  mutate(REFERENCE_RANGE = NORMAL_HIGH - NORMAL_LOW)

# filter only labs of interest
labs_filtered <- labs %>% 
  filter(EVENT_NAME %in% c("Sodium Level",
                         "Potassium Level",
                         "Chloride Lvl",
                         "CO2 Lvl",
                         "Magnesium Serum",
                         "Phosphorus Serum",
                         "Albumin Level",
                         "Glucose Level",
                         "BUN",
                         "Creatinine",
                         "Calcium Level, Total",
                         "Calcium Ionized",
                         "Lactic Acid Blood, Venous",
                         "Lactic Acid Blood, Arterial",
                         "White Blood Cell Count",
                         "Hemoglobin",
                         "Platelet Count",
                         #"C Reactive Protein",
                         "Erythrocyte SedRate"))

# add demographic data to the filtered labs data
labs_filtered <- right_join(labs_filtered, demog, by = c("ENCRYPTED_PAT_MRN_ID", "ENCRYPTED_PAT_ENC_CSN_ID"))

# select out unused columns
labs_filtered <- labs_filtered %>% 
  select(-c(CRITICAL_LOW, CRITICAL_HIGH, SEX, RACE_NUM, RACE, ETHNICITY, CONTACT_DATE))

# write csv files
write_csv(labs_filtered, "labs_clean.csv")

# ------------------------------------------------------------------------------
#                   Cleaned Data by Encounter Type
# ------------------------------------------------------------------------------
# Function that filters by encounter type
# param: encounter = encounter type
filter_encounter <- function(encounter) {
  labs_encounter <- labs_filtered %>% 
    filter(ENCOUNTER_TYPE == encounter)
}

# filter by NEPH_VISIT_HOSPITAL
labs_neph_hospital <- filter_encounter("NEPH_VISIT_HOSPITAL")
write_csv(labs_neph_hospital, "labs_neph_hosp.csv")
# filter by ICU_VISIT_HOSPITAL
labs_neph_hospital <- filter_encounter("ICU_VISIT_HOSPITAL")
write_csv(labs_neph_hospital, "labs_icu_hosp.csv")
# filter by NON_NEPH_NON_ICU_HOSPITAL
labs_neph_hospital <- filter_encounter("NON_NEPH_NON_ICU_HOSPITAL")
write_csv(labs_neph_hospital, "labs_non_neph_non_icu_hosp.csv")
# filter by NON_NEPH_NON_ICU_OUTPATIENT
labs_neph_hospital <- filter_encounter("NON_NEPH_NON_ICU_OUTPATIENT")
write_csv(labs_neph_hospital, "labs_non_neph_non_icu_out.csv")
# filter by NEPH_OUTPATIENT
labs_neph_hospital <- filter_encounter("NEPH_OUTPATIENT")
write_csv(labs_neph_hospital, "labs_neph_out.csv")


# ------------------------------------------------------------------------------
#                 Randomized Patient Subset Function
# ------------------------------------------------------------------------------
# function that creates randomized dataset of n patients
# paramater: df = dataframe
# parameter: no_pt = number of patients in new subset data frame
create_pt_df <- function(df, no_pt) {
  # set seed for reproducible random sampling
  set.seed(4)
  # create vector of unique pt id's
  pt_ids <- unique(df$ENCRYPTED_PAT_MRN_ID)
  # randomly sample n pts from vector 
  pt_ids <- sample(pt_ids, size = no_pt)
  # get subset of df using vector created above as filter
  new_df <- subset(df, ENCRYPTED_PAT_MRN_ID %in% pt_ids)
  
  # combine multiple df to create inclusive df
}

# example of sample dataset
labs_sample <- create_pt_df(labs, 2)
# arrange by patient then lab name to view different labs by patient
labs_sample <- labs_sample %>%
  arrange(ENCRYPTED_PAT_MRN_ID, EVENT_NAME)
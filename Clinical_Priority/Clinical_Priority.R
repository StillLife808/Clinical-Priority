# import useful libraries
library("tidyverse")

# set working directory
setwd("/Users/michael/Clinical_Priority/data/OneDrive_1_2-15-2021")

# read in csv
labs <- read_csv("labs_clean.csv") # look at
labs_neph_hosp <- read_csv("labs_neph_hosp.csv")
labs_icu_hosp <- read_csv("labs_icu_hosp.csv")
labs_non_neph_non_icu_hosp <- read_csv("labs_non_neph_non_icu_hosp.csv")
labs_non_neph_non_icu_out <- read_csv("labs_non_neph_non_icu_out.csv")
labs_neph_out <- read_csv("labs_neph_out.csv")
# ------------------------------------------------------------------------------
#                       Acuity Score Calculations
# ------------------------------------------------------------------------------
# Random sampling of timestamp
set.seed(2)
labs_random <- labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>% 
  sample_n(1) %>% 
  select(ENCRYPTED_PAT_MRN_ID, EVENT_TS)

# Rename the timestamp to a new name for joining
labs_random <- labs_random %>% 
  rename(TS = EVENT_TS)

# join the random timestamp to original labs dataframe
labs_z <- left_join(labs, labs_random)

# create new column with difference between random date per patient
# and date per lab (in seconds)
labs_z <- labs_z %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  mutate(date_minimum = abs(EVENT_TS - TS),
         date_range = EVENT_TS - TS)

# find the lab date per lab closest to random date per patient
labs_z_min <- labs_z %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(date_minimum == min(date_minimum))

# filter out for only 12 hours plus or minus random date per patient
# to create 24 window
labs_z_min <- labs_z_min %>% 
  filter(date_minimum <= 43200) # 43,200 seconds is a 12 hr window

# reassign dataframe to labs_z to calculate acuity
labs_z <- labs_z_min %>% 
  mutate(NORMAL_MEAN = (NORMAL_HIGH + NORMAL_LOW) / 2)

# add column of z-scores per latest lab results
labs_z <-  labs_z %>% 
  mutate(z_score = abs((RESULT_VALUE - NORMAL_MEAN) / ((NORMAL_HIGH - NORMAL_MEAN) / 2)))

# Acuity Score Dataframe
# create dataframe of acuity score (z-score sums per patient)
labs_z_score <- labs_z %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>% 
  summarise(acuity_score = sum(z_score, na.rm = T))

# normalize acuity score by dividing by all values by max acuity score
# find max acuity score
acuity_max <- max(labs_z_score$acuity_score)
# create column with values scaled to 100 max
labs_z_score <- labs_z_score %>% 
  mutate(acuity_score_norm = (acuity_score / acuity_max) * 100)

# create dataframe with patient id and last encounter type
encounter_type <- labs_z %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>%
  filter(EVENT_TS == unique(max(EVENT_TS))) %>% 
  select(ENCRYPTED_PAT_MRN_ID, ENCOUNTER_TYPE, EVENT_TS)
# eliminate duplicate entries
encounter_type <- unique(encounter_type)

labs_acuity <- left_join(labs_z_score, encounter_type)

# write csv of z scores
write_csv(labs_acuity, "z_score_labs.csv")
# ------------------------------------------------------------------------------
#                           Stats Summary Table
# ------------------------------------------------------------------------------
# 1) Load lines 1-17 and lines 61-73 from Clinical_Priority_Visualizaiton file
#     to get labs_score and acuity_median and CoT_median
labs_score <- labs_score %>% 
  mutate(category = ifelse(log_acuity < acuity_median & log_CoT < CoT_median, 1, 
                            ifelse(log_acuity >= acuity_median & log_CoT < CoT_median, 2,
                                   ifelse(log_acuity >= acuity_median & log_CoT >= CoT_median, 3, 4))))
# make the category column into factors
labs_score$category <- as.factor(labs_score$category)
# label the factors
levels(labs_score$category) <- c("Well", "Chronically Sick", "Acutely Sick", "Improved")

# create a summary; finds sum of each factor
labs_summary_count <- labs_score %>% 
  group_by(ENCOUNTER_TYPE, category) %>% 
  summarise(count = n())

# write csv file
write_csv(labs_summary_count, "labs_count_encounter.csv")

# create a column categorizing encounter type as inpatient or outpatient
labs_score <- labs_score %>% 
  mutate(in_vs_outpatient = ifelse(ENCOUNTER_TYPE == 	"ICU_VISIT_HOSPITAL" | 
                                     ENCOUNTER_TYPE == "NEPH_VISIT_HOSPITAL" | 
                                     ENCOUNTER_TYPE == "NON_NEPH_NON_ICU_HOSPITAL", 1, 2))

# factor in_vs_oupatient column and label
labs_score$in_vs_outpatient <- as.factor(labs_score$in_vs_outpatient)
levels(labs_score$in_vs_outpatient) <- c("Inpatient", "Outpatient")

# summarize column; find sum of categories
labs_summary_count_in_vs_out <- labs_score %>% 
  group_by(in_vs_outpatient, category) %>% 
  summarise(count = n())

# write csv
write_csv(labs_summary_count_in_vs_out, "labs_count_in_vs_out.csv")

# statistical summary of timeframe of lab draws
time_summary_acuity <- labs_z %>% 
  group_by(ENCOUNTER_TYPE) %>% 
  summarise(median_time = median(date_range),
            min_time = min(date_range),
            max_time = max(date_range))

# write csv
write_csv(time_summary_acuity, "time_summary_acuity.csv")



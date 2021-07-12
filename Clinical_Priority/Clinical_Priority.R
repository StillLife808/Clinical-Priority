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
# create a data frame with acuity score of all patients
# takes most recent labs from any date

# Z-score per lab data frame
# filter out any NA's in lab name and time
labs <- labs %>% 
  filter(!(is.na(EVENT_NAME)),
         !(is.na(EVENT_TS)))
# filter out latest measurements for each lab
labs_z <- labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(EVENT_TS == max(EVENT_TS)) %>% 
  mutate(NORMAL_MEAN = (NORMAL_HIGH + NORMAL_LOW) / 2) # new column for mean of ref. range
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
#                         Acuity Score Function
# ------------------------------------------------------------------------------
# function that creates a data frame with acuity score of all patients 
# takes most recent labs from from any date

# Z-score per lab data frame
# filter out latest measurements for each lab
calc_acuity <- function(labs) {
  labs_z <- labs %>% 
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
    filter(EVENT_TS == max(EVENT_TS)) %>% 
    mutate(NORMAL_MEAN = (NORMAL_HIGH + NORMAL_LOW) / 2) # new column for mean of ref. range
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
}

# ------------------------------------------------------------------------------
#                   Acuity Score by Encounter Type
# ------------------------------------------------------------------------------
# calculate acuity by NEPH_HOSPITAL_VISIT encounter
acuity_neph_hosp <- calc_acuity(labs_neph_hospital)
# write csv of z scores
write_csv(acuity_neph_hosp, "z_score_labs_neph_hosp.csv")

# calculate acuity by ICU_VISIT_HOSPITAL encounter
acuity_icu_hosp <- calc_acuity(labs_icu_hosp)
# write csv of z scores
write_csv(acuity_icu_hosp, "z_score_labs_icu_hosp.csv")

# calculate acuity by NON_NEPH_NON_ICU_HOSPITAL encounter
acuity_non_neph_non_icu_hosp <- calc_acuity(labs_non_neph_non_icu_hosp)
# write csv of z scores
write_csv(acuity_non_neph_non_icu_hosp, "z_score_labs_non_neph_icu_hosp.csv")

# calculate acuity by NON_NEPH_NON_ICU_OUTPATIENT encounter
acuity_non_neph_non_icu_out <- calc_acuity(labs_non_neph_non_icu_out)
# write csv of z scores
write_csv(acuity_non_neph_non_icu_out, "z_score_labs_non_neph_non_icu_out.csv")

# calculate acuity by NEPH_OUTPATIENT encounter
acuity_neph_out <- calc_acuity(labs_neph_out)
# write csv of z scores
write_csv(acuity_neph_out, "z_score_labs_neph_out.csv")
# ------------------------------------------------------------------------------
#                 Acuity Function (Select timeframe from most recent)
# ------------------------------------------------------------------------------
calc_acuity_time <- function(time) { #time is in hours
  labs_z <- labs %>% 
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
    filter(EVENT_TS >= max(EVENT_TS) - 60*60*time) %>% 
    mutate(NORMAL_MEAN = (NORMAL_HIGH + NORMAL_LOW) / 2) # new column for mean of ref. range
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
}
acuity_12_hours <- calc_acuity_time(12)
# ------------------------------------------------------------------------------
#                 Using MPEWS timeframe to calculate acuity
# ------------------------------------------------------------------------------
mpews <- read_csv("mpews_deid.csv")
  
# add MPEWS dataframe to labs dataframe
mpews_labs <- right_join(labs, mpews) %>% 
  select(-c(ENCRYPTED_PAT_ENC_CSN_ID, EVNT_DESCR))
# filter out lab time within four hours of mpews time
mpews_labs <- mpews_labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS) %>% 
  filter(EVENT_TS >= EVNT_END_TS - 14400 && EVENT_TS <= EVNT_END_TS + 14400) # 14400 is number of seconds in four hours

mpews_labs <- mpews_labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS) %>% 
  mutate(date_minimum = abs(EVENT_TS - EVNT_END_TS)) %>% 
  filter(date_minimum <= 14400)

# find lab date per lab closest to mpews date
mpews_labs <- mpews_labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS, EVENT_NAME) %>% 
  filter(date_minimum == min(date_minimum))

mpews_labs_z <-  mpews_labs %>% 
  mutate(z_score = abs((RESULT_VALUE - NORMAL_MEAN) / ((NORMAL_HIGH - NORMAL_MEAN) / 2)))

mpews_labs_z_score <- mpews_labs_z %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS) %>% 
  mutate(z_score_total = sum(z_score))

mpews_labs_z_score <- mpews_labs_z_score %>% 
  select(ENCRYPTED_PAT_MRN_ID, ENCOUNTER_TYPE, EVNT_END_TS, EVNT_TAG, z_score_total)

mpews_labs_z_score <- unique(mpews_labs_z_score)

mpews_labs_z_score <- mpews_labs_z_score %>% 
  mutate(log_z_score = log(z_score_total)) %>% 
  mutate(mpews_cat = ifelse(EVNT_TAG <= 2, 1, 
                            ifelse(EVNT_TAG >= 3 & EVNT_TAG <= 5, 2,
                                   ifelse(EVNT_TAG >= 6 & EVNT_TAG <= 7, 3, 4))))

mpews_labs_z_score$mpews_cat <- as.factor(mpews_labs_z_score$mpews_cat)
mpews_labs_z_score$EVNT_TAG <- as.factor(mpews_labs_z_score$EVNT_TAG)

ggplot(na.omit(mpews_labs_z_score), aes(x=EVNT_TAG, y=log_z_score)) +
  geom_violin() +
  geom_smooth(method = "lm") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  xlab("MPEWS") +
  ylab("Acuity Score") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
# ------------------------------------------------------------------------------
#                           Random Time Sample
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
labs_z_min <- labs_z_min %>% 
  filter(date_minimum <= 43200) # 43,200 seconds is a 12 hr window

# reassign dataframe to labs_z to calculate acuity
labs_z <- labs_z_min %>% 
  mutate(NORMAL_MEAN = (NORMAL_HIGH + NORMAL_LOW) / 2)
# continue to line 31 to finish acuity scores

# ------------------------------------------------------------------------------
#                           Stats Summary Table
# ------------------------------------------------------------------------------
# 1) Load lines 1-17 and lines 61-73 from Clinical_Priority_Visualizaiton file
#     to get labs_score and acuity_median and CoT_median
labs_score <- labs_score %>% 
  mutate(category = ifelse(log_acuity < acuity_median & log_CoT < CoT_median, 1, 
                            ifelse(log_acuity >= acuity_median & log_CoT < CoT_median, 2,
                                   ifelse(log_acuity >= acuity_median & log_CoT >= CoT_median, 3, 4))))

labs_score$category <- as.factor(labs_score$category)

levels(labs_score$category) <- c("Well", "Chronically Sick", "Acutely Sick", "Improved")

labs_summary_count <- labs_score %>% 
  group_by(ENCOUNTER_TYPE, category) %>% 
  summarise(count = n())

write_csv(labs_summary_count, "labs_count_encounter.csv")

labs_score <- labs_score %>% 
  mutate(in_vs_outpatient = ifelse(ENCOUNTER_TYPE == 	"ICU_VISIT_HOSPITAL" | ENCOUNTER_TYPE == "NEPH_VISIT_HOSPITAL" | ENCOUNTER_TYPE == "NON_NEPH_NON_ICU_HOSPITAL", 1, 2))

labs_score$in_vs_outpatient <- as.factor(labs_score$in_vs_outpatient)
levels(labs_score$in_vs_outpatient) <- c("Inpatient", "Outpatient")

labs_summary_count_in_vs_out <- labs_score %>% 
  group_by(in_vs_outpatient, category) %>% 
  summarise(count = n())

write_csv(labs_summary_count_in_vs_out, "labs_count_in_vs_out.csv")

time_summary_acuity <- labs_z %>% 
  group_by(ENCOUNTER_TYPE) %>% 
  summarise(median_time = median(date_range),
            min_time = min(date_range),
            max_time = max(date_range))

write_csv(time_summary_acuity, "time_summary_acuity.csv")



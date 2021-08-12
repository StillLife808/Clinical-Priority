# import useful libraries
library("tidyverse")

# set working directory
setwd("/Users/michael/data/OneDrive_1_2-15-2021")

# read in labs csv
labs <- read_csv("labs_clean.csv")

# ------------------------------------------------------------------------------
#                   Change-over-time Score Calculations
# ------------------------------------------------------------------------------
# Finding the most recent lab draw at most recent dates
# create df with values from the latest date
df1 <- labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(EVENT_TS == max(EVENT_TS))

# Finding the second most recent lab draw values
# create df filtering out most recent lab draw date
df2 <- labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(EVENT_TS != max(EVENT_TS))
# filter out only the most recent lab draws from df2
df2 <- df2 %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(EVENT_TS == max(EVENT_TS))

# merge df1 and df2 to create df w/ labs draws from most recent date and next
# most recent date
labs_cot <- rbind(df1, df2)
# filter out any lab value that does not have two results
labs_cot <- labs_cot %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(n() > 1)
# just looking at the data
labs_cot <- labs_cot %>% 
  arrange(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS)
# combine same lab type data on same day by finding mean
labs_cot <- labs_cot %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS) %>% 
  filter(RESULT_VALUE == mean(RESULT_VALUE))

# add normalized lab values
labs_cot <- labs_cot %>% 
  mutate(RESULT_VALUE_NORM = (RESULT_VALUE - NORMAL_MEAN) / (NORMAL_HIGH - NORMAL_LOW))

# write csv file for patients two most recent labs
write_csv(labs_cot, "labs_most_recent.csv")

# calculate difference in most recent labs and most recent dates
labs_cot_test <- labs_cot %>%
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>%
  arrange(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS) %>%
  summarise(CHANGE_LAB_NORM = diff(RESULT_VALUE_NORM),
            TIME_DIFF = diff(EVENT_TS)) # convert from days to hours
# calculate change over time per lab value
cot_labs <- na.omit(labs_cot_test) %>% 
  mutate(CHANGE_OVER_TIME = abs(CHANGE_LAB_NORM / as.numeric(TIME_DIFF)))
# calculate change over time per patient via sum of CoT per lab per patient
cot_labs_total <- cot_labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>% 
  summarise(CoT = sum(CHANGE_OVER_TIME, na.rm = T))

# normalize cot score by dividing all values by maximum cot score
# max cot score
max_cot <- max(cot_labs_total$CoT)
# add column with values scaled to 100 max
cot_labs_total <- cot_labs_total %>% 
  mutate(CoT_norm =(CoT / max_cot) * 100)

# write csv file
write_csv(cot_labs_total, "cot_labs.csv")

# ------------------------------------------------------------------------------
#                 Change-over-time Score Calculations Function
# ------------------------------------------------------------------------------
# Function that finds the most recent lab draw at most recent dates
calc_cot <- function(labs) {
  # create df with values from the latest date
  df1 <- labs %>% 
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
    filter(EVENT_TS == max(EVENT_TS))
  
  # Finding the second most recent lab draw values
  # create df filtering out most recent lab draw date
  df2 <- labs %>% 
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
    filter(EVENT_TS != max(EVENT_TS))
  # filter out only the most recent lab draws from df2
  df2 <- df2 %>% 
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
    filter(EVENT_TS == max(EVENT_TS))
  
  # merge df1 and df2 to create df w/ labs draws from most recent date and next
  # most recent date
  labs_cot <- rbind(df1, df2)
  # filter out any lab value that does not have two results
  labs_cot <- labs_cot %>% 
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
    filter(n() > 1)
  # just looking at the data
  labs_cot <- labs_cot %>% 
    arrange(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS)
  # combine same lab type data on same day by finding mean
  labs_cot <- labs_cot %>% 
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS) %>% 
    filter(RESULT_VALUE == mean(RESULT_VALUE))
  
  # add normal value means
  labs_cot <- labs_cot %>% 
    mutate(NORMAL_MEAN = (NORMAL_HIGH + NORMAL_LOW) / 2)
  
  # add normalized lab values
  labs_cot <- labs_cot %>% 
    mutate(RESULT_VALUE_NORM = (RESULT_VALUE - NORMAL_MEAN) / (NORMAL_HIGH - NORMAL_LOW))
  
  # calculate difference in most recent labs and most recent dates
  labs_cot_test <- labs_cot %>%
    group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>%
    arrange(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS) %>%
    summarise(CHANGE_LAB_NORM = diff(RESULT_VALUE_NORM),
              TIME_DIFF = diff(EVENT_TS)) # convert from days to hours
  # calculate change over time per lab value
  cot_labs <- na.omit(labs_cot_test) %>% 
    mutate(CHANGE_OVER_TIME = abs(CHANGE_LAB_NORM / as.numeric(TIME_DIFF)))
  # calculate change over time per patient via sum of CoT per lab per patient
  cot_labs_total <- cot_labs %>% 
    group_by(ENCRYPTED_PAT_MRN_ID) %>% 
    summarise(CoT = sum(CHANGE_OVER_TIME, na.rm = T))
  
  # normalize cot score by dividing all values by maximum cot score
  # max cot score
  max_cot <- max(cot_labs_total$CoT)
  # add column with values scaled to 100 max
  cot_labs_total <- cot_labs_total %>% 
    mutate(CoT_norm =(CoT / max_cot) * 100)
}
# ------------------------------------------------------------------------------
#                 Random Time Sample
# ------------------------------------------------------------------------------
# Random sampling of timestamp
set.seed(2)
labs_random <- labs %>% 
  filter(!is.na(EVENT_TS)) %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>% 
  sample_n(1) %>% 
  select(ENCRYPTED_PAT_MRN_ID, EVENT_TS)

# Rename the timestamp to a new name for joining
labs_random <- labs_random %>% 
  rename(TS = EVENT_TS)

# join the random timestamp to original labs dataframe
labs <- left_join(labs, labs_random)

# create df with dates clos
df1 <- labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(EVENT_TS == max(EVENT_TS))

# Finding the second most recent lab draw values
# create df filtering out most recent lab draw date
df2 <- labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(EVENT_TS != max(EVENT_TS))
# filter out only the most recent lab draws from df2
df2 <- df2 %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(EVENT_TS == max(EVENT_TS))

# merge df1 and df2 to create df w/ labs draws from most recent date and next
# most recent date
labs_cot <- rbind(df1, df2)
# filter out any lab value that does not have two results
labs_cot <- labs_cot %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>% 
  filter(n() > 1)
# just looking at the data
labs_cot <- labs_cot %>% 
  arrange(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS)
# combine same lab type data on same day by finding mean
labs_cot <- labs_cot %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS) %>% 
  filter(RESULT_VALUE == mean(RESULT_VALUE))

# add normalized lab values
labs_cot <- labs_cot %>% 
  mutate(RESULT_VALUE_NORM = (RESULT_VALUE - NORMAL_MEAN) / (NORMAL_HIGH - NORMAL_LOW))

labs_cot_time_diff <- labs_cot %>%
  group_by(ENCRYPTED_PAT_MRN_ID, EVENT_NAME) %>%
  arrange(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, EVENT_TS) %>%
  summarise(TIME_DIFF = diff(EVENT_TS)) 

labs_cot_time_diff <- left_join(labs_cot_time_diff, labs_cot, by=c("ENCRYPTED_PAT_MRN_ID", "EVENT_NAME"))
labs_cot_time_diff <- labs_cot_time_diff %>% 
  select(c(ENCRYPTED_PAT_MRN_ID, EVENT_NAME, TIME_DIFF, ENCOUNTER_TYPE)) %>% 
  unique() %>% 
  filter(TIME_DIFF > 0)

time_summary_cot <- labs_cot_time_diff %>% 
  group_by(ENCOUNTER_TYPE) %>% 
  summarise(median_time = median(TIME_DIFF),
            min_time = min(TIME_DIFF),
            max_time = max(TIME_DIFF))

write_csv(time_summary_cot, "time_summary_cot.csv")






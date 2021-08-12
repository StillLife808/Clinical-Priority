# import useful libraries
library("tidyverse")
library("tibbletime")

# set working directory
setwd("/Users/michael/data/OneDrive_1_2-15-2021")

# read in csv files
labs <- read_csv("labs_clean.csv") # look at
cot_labs <- read_csv("cot_labs.csv")
z_score_labs <- read_csv("z_score_labs.csv")
mpews <- read_csv("mpews_deid.csv")

# ------------------------------------------------------------------------------
#                 Using MPEWS timeframe to calculate acuity
# ------------------------------------------------------------------------------
# add MPEWS dataframe to labs dataframe
mpews_labs <- right_join(labs, mpews) %>% 
  select(-c(ENCRYPTED_PAT_ENC_CSN_ID, EVNT_DESCR))
# filter out lab time within four hours of mpews time
mpews_labs <- mpews_labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS) %>% 
  filter(EVENT_TS >= EVNT_END_TS - 4*60*60 && EVENT_TS <= EVNT_END_TS + 4*60*60) # 14400 is number of seconds in four hours

# create column of time difference between mpews timestamp and labs timestamp
# filter out lab values w/in four hours of mpews score
mpews_labs <- mpews_labs %>% 
  #group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS) %>% 
  mutate(date_minimum = abs(EVENT_TS - EVNT_END_TS)) %>% 
  filter(date_minimum <= 4*60*60)

# find lab date per lab closest to mpews date
mpews_labs <- mpews_labs %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS, EVENT_NAME) %>% 
  filter(date_minimum == min(date_minimum))

# calculate z-score per lab value
mpews_labs_z <-  mpews_labs %>% 
  mutate(z_score = abs((RESULT_VALUE - NORMAL_MEAN) / ((NORMAL_HIGH - NORMAL_MEAN) / 2)))

# calculate z-score per patient
mpews_labs_z_score <- mpews_labs_z %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS) %>% 
  mutate(z_score_total = sum(z_score))

mpews_labs_z_score <- mpews_labs_z_score %>% 
  select(ENCRYPTED_PAT_MRN_ID, ENCOUNTER_TYPE, EVNT_END_TS, EVNT_TAG, z_score_total)

# consolidate duplicate rows to unique z-scores per patient
mpews_labs_z_score <- unique(mpews_labs_z_score)

mpews_labs_z_score <- mpews_labs_z_score %>% 
  mutate(log_z_score = log(z_score_total)) %>% 
  mutate(mpews_cat = ifelse(EVNT_TAG <= 2, 1, 
                            ifelse(EVNT_TAG >= 3 & EVNT_TAG <= 5, 2,
                                   ifelse(EVNT_TAG >= 6 & EVNT_TAG <= 7, 3, 4))))

mpews_labs_z_score$mpews_cat <- as.factor(mpews_labs_z_score$mpews_cat)
mpews_labs_z_score$EVNT_TAG <- as.factor(mpews_labs_z_score$EVNT_TAG)

# find the variance of acuity score
mpews_var <- na.omit(mpews_labs_z_score) %>% 
  group_by(EVNT_TAG) %>% 
  mutate(variance = var(log_z_score))

mpews_var <- na.omit(mpews_var)
coeff <- diff(range(mpews_var$log_z_score)) / diff(range(mpews_var$variance))
coeff <- max(mpews_var$log_z_score) / max(mpews_var$variance)

# plot acuity vs. MPEWS superimposed with standard deviation vs. MPEWS
ggplot(mpews_var, aes(EVNT_TAG)) +
  geom_violin(aes(y=log_z_score)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, aes(y=log_z_score)) +
  geom_smooth(aes(x=as.numeric(EVNT_TAG), y=variance*coeff), method = "lm", se=F) +
  scale_y_continuous(
    # Features of the first axis
    name = "log(Acuity)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Variance")
  ) +
  xlab("MPEWS") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# find the standard deviation of acuity score
mpews_std <- na.omit(mpews_labs_z_score) %>% 
  group_by(EVNT_TAG) %>% 
  mutate(std= sd(log_z_score))

mpews_std <- na.omit(mpews_std)
coeff <- diff(range(mpews_std$log_z_score)) / diff(range(mpews_std$std))
coeff <- max(mpews_std$log_z_score) / max(mpews_std$std)

# plot acuity vs. MPEWS superimposed with standard deviation vs. MPEWS
ggplot(mpews_std, aes(EVNT_TAG)) +
  geom_violin(aes(y=log_z_score)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, aes(y=log_z_score)) +
  geom_smooth(aes(x=as.numeric(EVNT_TAG), y=std*coeff), method = "lm", se=F) +
  scale_y_continuous(
    # Features of the first axis
    name = "log(Acuity)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Standard Deviation")
  ) +
  xlab("MPEWS") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# ------------------------------------------------------------------------------
#               Calculate p-value for Variance/Sd vs. MPEWS
# ------------------------------------------------------------------------------
# use linear regression model to find r squared and p-value of
# standard deviation vs. MPEWS
m <- lm(std ~ as.numeric(EVNT_TAG), mpews_std)
summary(m)$r.squared
summary(m)

# use linear regression model to find r squared and p-value of
# variance vs. MPEWS
p <- lm(variance ~ as.numeric(EVNT_TAG), mpews_var)
summary(p)$r.squared
summary(p)

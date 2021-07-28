# import useful libraries
library("tidyverse")

# set working directory
setwd("/Users/michael/data/OneDrive_1_2-15-2021")

# read in csv files
labs <- read_csv("labs_clean.csv") # look at
prism <- na.omit(read_csv("PRISM_encrypted.csv"))
cot_labs <- read_csv("cot_labs.csv")
z_score_labs <- read_csv("z_score_labs.csv")

# prism date formatting
prism$HOSPITAL_ADMIT <- as.Date(prism$HOSPITAL_ADMIT, format = "%m/%d/%y")
prism$ICU_ADMIT <- as.Date(prism$ICU_ADMIT, format = "%m/%d/%y")
# ------------------------------------------------------------------------------
#                 Using PRISM timeframe to calculate acuity
# ------------------------------------------------------------------------------
# add MPEWS dataframe to labs dataframe
prism_labs <- na.omit(right_join(labs, prism))

# create new column that changes labs timestamp to month-day-year
prism_labs$EVENT_DATE <- format(as.POSIXct(prism_labs$EVENT_TS,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
prism_labs$EVENT_DATE <- as.Date(prism_labs$EVENT_DATE, format = "%m/%d/%y")

# filter only dates where labs were drawn and PRISM score calculated
prism_labs <- prism_labs %>% 
  filter(EVENT_DATE == HOSPITAL_ADMIT)

# filter the first labs per date
prism_labs <- prism_labs %>% 
  group_by(ENCRYPTED_PAT_ENC_CSN_ID, HOSPITAL_ADMIT, EVENT_NAME) %>% 
  filter(EVENT_TS == min(EVENT_TS))

# calculate z-score per lab value
prism_labs_z <-  prism_labs %>% 
  mutate(z_score = abs((RESULT_VALUE - NORMAL_MEAN) / ((NORMAL_HIGH - NORMAL_MEAN) / 2)))

# calculate z-score per patient
prism_labs_z_score <- prism_labs_z %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, HOSPITAL_ADMIT) %>% 
  mutate(z_score_total = sum(z_score))

# select columns of interest
prism_labs_z_score <- prism_labs_z_score %>% 
  select(ENCRYPTED_PAT_MRN_ID, ENCOUNTER_TYPE, HOSPITAL_ADMIT, PRISM, z_score_total)

# consolidate duplicate rows to unique z-scores per patient
prism_labs_z_score <- unique(prism_labs_z_score)

# add column of log(z_scores)
prism_labs_z_score <- prism_labs_z_score %>% 
  mutate(log_z_score = log(z_score_total))

# make prism scores as factors and elminate infinite values
prism_labs_z_score$PRISM <- as.factor(prism_labs_z_score$PRISM)
prism_labs_z_score <- prism_labs_z_score %>% 
  filter(log_z_score != -Inf)

# VARIANCE PLOT
# create new dataframe - add column of variance of log(z_score)
prism_var <- prism_labs_z_score %>% 
  group_by(PRISM) %>% 
  mutate(variance = var(log_z_score))

# omit any NA's
prism_var <- na.omit(prism_var)
# calculate coefficient offset for variance range
coeff <- diff(range(prism_var$log_z_score)) / diff(range(prism_var$variance))

# plot log(acuity) vs PRISM and Variance vs PRISM w/ coeff offset on variance y-axis
ggplot(prism_var, aes(x=PRISM)) +
  geom_violin(aes(y=log_z_score)) +
  coord_cartesian(xlim = c(1, 28)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, aes(y=log_z_score)) +
  geom_smooth(aes(x=as.numeric(PRISM), y=variance*coeff), method = "lm", se=F) +
  scale_y_continuous(
    # Features of the first axis
    name = "log(Acuity)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Variance")
  ) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  
# STANDARD DEVIATION PLOT
# create new dataframe - add column of standard deviation of log(z_score)
prism_std <- prism_labs_z_score %>% 
  group_by(PRISM) %>% 
  mutate(std = sd(log_z_score))

# omit NA's
prism_std <- na.omit(prism_std)
# calculate coefficient offset for standard deviation range
coeff <- diff(range(prism_std$log_z_score)) / diff(range(prism_std$std))
coeff <- max(prism_std$log_z_score) / max(prism_std$std)

# log(acuity) vs PRISM and standard deviation vs PRISM plot w/ coeff offset on std y-axis
ggplot(prism_std, aes(x=PRISM)) +
  geom_violin(aes(y=log_z_score)) +
  coord_cartesian(xlim = c(1, 28)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, aes(y=log_z_score)) +
  geom_smooth(aes(x=as.numeric(PRISM), y=std*coeff), method = "lm", se=F) +
  scale_y_continuous(
    # Features of the first axis
    name = "log(Acuity)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./coeff, name="Standard Deviation")
  ) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
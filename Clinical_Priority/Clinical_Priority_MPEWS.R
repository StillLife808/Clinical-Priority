# import useful libraries
library("tidyverse")
library("tibbletime")

# set working directory
setwd("/Users/michael/Clinical_Priority/data/OneDrive_1_2-15-2021")

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

mpews_lm <- lm(variance ~ EVNT_TAG, mpews_var)

ggplot(na.omit(mpews_labs_z_score), aes(x=EVNT_TAG, y=log_z_score)) +
  geom_violin() +
  geom_smooth(method = "lm") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  xlab("MPEWS") +
  ylab("Acuity Score") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


mpews_var <- na.omit(mpews_labs_z_score) %>% 
  group_by(EVNT_TAG) %>% 
  mutate(variance = var(log_z_score))

mpews_var <- na.omit(mpews_var)
coeff <- diff(range(mpews_var$log_z_score)) / diff(range(mpews_var$variance))
coeff <- max(mpews_var$log_z_score) / max(mpews_var$variance)

lm_eqn <- lm(variance ~ EVNT_TAG, mpews_var)
lm_eqn <- lm(variance ~ PRISM, prism_var)

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

mpews_std <- na.omit(mpews_labs_z_score) %>% 
  group_by(EVNT_TAG) %>% 
  mutate(std= sd(log_z_score))

mpews_std <- na.omit(mpews_std)
coeff <- diff(range(mpews_std$log_z_score)) / diff(range(mpews_std$std))
coeff <- max(mpews_std$log_z_score) / max(mpews_std$std)

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
#                       Joining Acuity and CoT scores
# ------------------------------------------------------------------------------
# merge acuity and change-over-time scores
labs_score <- right_join(cot_labs, z_score_labs)
labs_score <- na.omit(labs_score)
# get most recent MPEWS score per patient
mpews_add <- left_join(labs_score, mpews) %>% 
  select(-c(ENCRYPTED_PAT_ENC_CSN_ID, EVNT_DESCR)) %>% 
  filter(ENCOUNTER_TYPE == "ICU_VISIT_HOSPITAL")
# filter MPEWS scores within an hour of most recent acuity score
mpews_new <- mpews_add %>% 
  filter(EVNT_END_TS >= EVENT_TS - 60*60*4, EVNT_END_TS <= EVENT_TS + 60*60*4) #60*60*60 is seconds to day conversion

mpews_norm <- mpews_new %>% 
  mutate(CoT_norm = CoT / max(CoT) * 100,
         acuity_score_norm = acuity_score / max(acuity_score) * 100,
         mpews_normal = EVNT_TAG / max(EVNT_TAG) * 100)

# find the average of MPEWS scores within a day
MPEWS_AVG_PER_PT = group_by(mpews_norm, ENCRYPTED_PAT_MRN_ID) %>% summarise(mpews_avg = mean(EVNT_TAG))
# join the mpews average per patient with larger dataframe
mpews_analysis <- right_join(mpews_norm, MPEWS_AVG_PER_PT)
# select only then important columns and add log(acuity) and log(CoT)
mpews_analysis <- mpews_analysis %>% 
  select(ENCRYPTED_PAT_MRN_ID, CoT, acuity_score, EVENT_TS, mpews_avg) %>% 
  mutate(log_acuity = log(acuity_score),
         log_CoT = log(CoT))
  distinct()

mpews_analysis <- mpews_analysis %>% 
  mutate(mpews_cat = ifelse(mpews_avg <= 2, 1, 
         ifelse(mpews_avg >= 3 & mpews_avg <= 5, 2,
         ifelse(mpews_avg >= 6 & mpews_avg <= 7, 3, 4))))
# ------------------------------------------------------------------------------
#                         Plot MPEW scores vs CoT
# ------------------------------------------------------------------------------
ggplot(mpews_norm, aes(y=acuity_score_norm, x=MPEWS_cat)) +
  #geom_point(aes(y=acuity_score_norm, colour="blue"), alpha=0.6) +
  #geom_point(aes(y=CoT_norm, colour="green"), alpha=0.6) +
  #geom_point(aes(y=mpews_normal)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) + #fix this
  xlab("MPEWS") +
  ylab("log(Acuity)") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

mpews_log <- mpews_new %>% 
  mutate(log_acuity = log(acuity_score),
         log_CoT = log(CoT),
         log_mpews = log(EVNT_TAG)) %>% 
  select(-c(acuity_score, acuity_score_norm, CoT, CoT_norm, EVNT_TAG))

ggplot(mpews_log, aes(x=log_CoT)) +
  geom_point(aes(y=log_acuity), color="blue") +
  geom_point(aes(y=log_mpews), color="red") +
  xlab("Degree of Change") +
  ylab("Acuity and MPEW scores") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
# ------------------------------------------------------------------------------
#                     Filter Most Recent MPEW scores
# ------------------------------------------------------------------------------
# merge MPEWS with labs_score
# filter out most recent MPEWS score per patient
mpews_recent <- mpews %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>% 
  filter(EVNT_END_TS == max(EVNT_END_TS)) %>% 
  select(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS, EVNT_TAG)
# filter out the second most recent MPEWS score per patient
mpews_second_recent <- mpews %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>% 
  filter(EVNT_END_TS != max(EVNT_END_TS))
# filter out only the most recent lab draws from df2
mpews_second_recent <- mpews_second_recent %>% 
  group_by(ENCRYPTED_PAT_MRN_ID) %>% 
  filter(EVNT_END_TS == max(EVNT_END_TS)) %>% 
  select(ENCRYPTED_PAT_MRN_ID, EVNT_END_TS, EVNT_TAG)
# join MPEW scores
mpews_join <- rbind(mpews_recent, mpews_second_recent)
# filter out any patient that does not have two MPEW scores
mpews_join <- mpews_join %>% 
  group_by(ENCRYPTED_PAT_MRN_ID, EVNT_TAG) %>% 
  filter(n() > 1)
# Observation: time intervals between MPEW calculations are very short so
# scores don't change over the two most recent intervals.
# ------------------------------------------------------------------------------
#                 Join MPEW scores w/ Clinical Priority Scores
# ------------------------------------------------------------------------------
labs_score_mpew <- right_join(labs_score, mpews_recent)
labs_score_mpew <- labs_score_mpew %>% 
  filter(EVENT_TS == EVNT_END_TS)
# ------------------------------------------------------------------------------
#                 Categorize Acuity vs CoT Score by Quadrants
# ------------------------------------------------------------------------------
# filter out any infinite value
mpews_analysis <- mpews_analysis %>% 
  filter(log_CoT != -Inf)

# create categories for each of the acuity vs CoT quadrants
# 1 = low priority
# 2 = chronic but stable
# 3 = recovering?
# 4 = acutely sick
mpews_analysis <- mpews_analysis %>% 
  mutate(priority_cat = ifelse(log_acuity < median(log_acuity) & log_CoT < median(log_CoT), 1,
                               ifelse(log_acuity > median(log_acuity) & log_CoT < median(log_CoT), 2,
                                      ifelse(log_acuity > median(log_acuity) & log_CoT > median(log_CoT), 4, 3))))

mpews_analysis$mpews_cat <- as.factor(mpews_analysis$mpews_cat)
mpews_analysis$priority_cat <- as.factor(mpews_analysis$priority_cat)

mpews_analysis <- na.omit(mpews_analysis)

# chi square test of mpews categories vs clinical priority quadrants
chisq.test(mpews_analysis$mpews_cat, mpews_analysis$priority_cat, correct=FALSE)

# plot of mpews categories vs priority categories
ggplot(mpews_analysis, aes(y=priority_cat, x=mpews_cat)) +
  geom_count() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  xlab("MPEWS Categorized") +
  ylab("Priority Categorized") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# plot of mpews categories vs log(acuity)
ggplot(mpews_analysis, aes(x=mpews_cat, y=log_acuity)) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  xlab("MPEWS Categorized") +
  ylab("Acuity Score") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# plot of log(acuity) vs. log(CoT)
ggplot(mpews_analysis, aes(x=log_CoT, y=log_acuity)) +
  geom_point(colour="cornflowerblue", alpha=0.6) +
  geom_hline(yintercept=median(mpews_analysis$log_acuity), alpha=0.4, color = "black") +
  geom_vline(xintercept=median(mpews_analysis$log_CoT), alpha=0.4, color = "black") +
  xlab("log(Degree of Change)") +
  ylab("log(Acuity)") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# select only the log scores for kmeans analysis
mpews_kmeans <- mpews_analysis %>% 
  select(log_acuity, log_CoT)

# swap variables so that log_CoT is our x
mpews_kmeans <- mpews_kmeans[c("log_CoT", "log_acuity")]

# kmeans calculation and plot
km.out = kmeans(mpews_kmeans,4,nstart=20)
plot(mpews_kmeans, col=(km.out$cluster+1), main="K-Means Clustering Results with K=4",
     xlab="log(Degree of Change)", ylab = "log(Acuity)", pch=20, cex=1)

# add kmeans grouping to mpews_analysis dataframe
kmeans_group <- km.out$cluster
mpews_analysis$kmeans <- kmeans_group

# chi square test of mpews categories and kmeans
chisq.test(mpews_analysis$mpews_cat, mpews_analysis$kmeans, correct=FALSE)

ggplot(mpews_analysis, aes(x=mpews_cat, y=kmeans_group)) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  xlab("MPEWS Categorized") +
  ylab("K-Means") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


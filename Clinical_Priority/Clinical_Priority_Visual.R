# import useful libraries
library("tidyverse")

# set working directory
setwd("/Users/michael/Clinical_Priority/data/OneDrive_1_2-15-2021")

# read in csv files
cot_labs <- read_csv("cot_labs.csv")
z_score_labs <- read_csv("z_score_labs.csv")
mpews <- read_csv("mpews_deid.csv")

# ------------------------------------------------------------------------------
#                     Combining Acuity and CoT scores
# ------------------------------------------------------------------------------
# merge acuity and change-over-time scores
labs_score <- right_join(cot_labs, z_score_labs)
labs_score <- na.omit(labs_score)
# ------------------------------------------------------------------------------
#                           Plotting Data (raw)
# ------------------------------------------------------------------------------
# find the median of acuity and change over time scores to create quadrants
acuity_median <- median(labs_score$acuity_score_norm, na.rm = T)
CoT_median <- median(labs_score$CoT_norm, na.rm = T)

# plot acuity score vs chane-over-time score
ggplot(labs_score, aes(x=CoT_norm, y=acuity_score_norm, color=ENCOUNTER_TYPE)) +
  geom_point() +
  geom_hline(yintercept=acuity_median, alpha=0.4, color = "black") +
  geom_vline(xintercept=CoT_median, alpha=0.4, color = "black") +
  xlab("Degree of Change") +
  ylab("Acuity") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# ------------------------------------------------------------------------------
# elminate "extreme values"
labs_score_no <- labs_score %>% 
  filter(CoT_norm < 40 & CoT_norm >= 1 & acuity_score_norm >= 1 & acuity_score_norm < 75)

max_acuity <- max(labs_score_no$acuity_score)
max_cot <- max(labs_score_no$CoT)

labs_score_no$acuity_score_norm <- (labs_score_no$acuity_score / max_acuity) * 100
labs_score_no$CoT_norm <- (labs_score_no$CoT / max_cot) * 100

# find median
acuity_median <- median(labs_score_no$acuity_score_norm)
CoT_median <- median(labs_score_no$CoT_norm)

# plot acuity vs change-over-time w/o extreme values
ggplot(labs_score_no, aes(x=CoT_norm, y=acuity_score_norm, color=ENCOUNTER_TYPE)) +
  geom_point() +
  geom_hline(yintercept=acuity_median, alpha=0.4, color = "black") +
  geom_vline(xintercept=CoT_median, alpha=0.4, color = "black") +
  xlab("Degree of Change") +
  ylab("Acuity") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  
# add log scale to acuity and CoT scores
labs_score <- labs_score %>% 
  mutate(log_acuity = log(acuity_score),
         log_CoT = log(CoT))

max_log_cot <- max(labs_score$log_CoT)
max_log_acuity <- max(labs_score$log_acuity)

labs_score <- labs_score %>% 
  mutate(log_acuity_norm = (log_acuity / max_log_acuity) * 100,
         log_CoT_norm = (log_CoT / max_log_cot) * 100)

acuity_median <- median(labs_score$log_acuity, na.rm = T)
CoT_median <- median(labs_score$log_CoT, na.rm = T)

ggplot(labs_score, aes(x=log_CoT, y=log_acuity, color=ENCOUNTER_TYPE)) +
  geom_point() +
  geom_hline(yintercept=acuity_median, alpha=0.4, color = "black") +
  geom_vline(xintercept=CoT_median, alpha=0.4, color = "black") +
  xlab("Degree of Change") +
  ylab("Acuity") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# ------------------------------------------------------------------------------
#   Function to Plot Acuity vs Change over time (filtering by encounter type)
# ------------------------------------------------------------------------------
plot_encounter <- function(df, encounter) {
  if(missing(encounter)) {
    # find the median of acuity and change over time scores to create quadrants
    acuity_median <- median(df$acuity_score_norm)
    CoT_median <- median(df$CoT_norm)
    
    # plot acuity score vs chane-over-time score
    ggplot(df, aes(x=CoT_norm, y=acuity_score_norm, color=ENCOUNTER_TYPE)) +
      geom_point() +
      geom_hline(yintercept=acuity_median, alpha=0.4, color = "black") +
      geom_vline(xintercept=CoT_median, alpha=0.4, color = "black") +
      #ggtitle(encounter) +
      xlab("Degree of Change") +
      ylab("Acuity") +
      labs(color = "Encounter") +
      scale_color_manual(labels = c("ICU",
                                    "Nephrology outpatient",
                                    "Nephrology inpatient",
                                    "Non-nephrology inpatient",
                                    "Non-nephrology outpatient"),
                         values = c("#f8766d",
                                    "gold2",
                                    "orange1",
                                    "darkorchid2",
                                    "cornflowerblue")) +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  } else {
    df <- df %>% 
      filter(ENCOUNTER_TYPE == encounter)
    # find the median of acuity and change over time scores to create quadrants
    acuity_median <- median(df$acuity_score_norm)
    CoT_median <- median(df$CoT_norm)
    
    # plot acuity score vs chane-over-time score
    ggplot(df, aes(x=CoT_norm, y=acuity_score_norm)) +
      geom_point(color="cornflowerblue") +
      geom_hline(yintercept=acuity_median, alpha=0.4, color = "black") +
      geom_vline(xintercept=CoT_median, alpha=0.4, color = "black") +
      ggtitle(encounter) +
      xlab("Degree of Change") +
      ylab("Acuity") +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  }
}

# ------------------------------------------------------------------------------
# Function to Plot log(Acuity) vs log(Change over time) (filtering by encounter type)
#                     (Main Method for Plotting)
# ------------------------------------------------------------------------------
plot_log <- function(df, encounter, color_select) {
  if(missing(encounter) & missing(color_select)) {
    df <- df %>% 
      mutate(log_acuity = log(acuity_score),
             log_CoT = log(CoT))
    
    max_log_cot <- max(df$log_CoT)
    max_log_acuity <- max(df$log_acuity)
    
    df <- df %>% 
      mutate(log_acuity_norm = (log_acuity / max_log_acuity) * 100,
             log_CoT_norm = (log_CoT / max_log_cot) * 100)
    
    acuity_median <- median(df$log_acuity, na.rm = T)
    CoT_median <- median(df$log_CoT, na.rm = T)
    
    ggplot(df, aes(x=log_CoT, y=log_acuity, color=ENCOUNTER_TYPE)) +
      geom_point(alpha=0.4) +
      geom_hline(yintercept=acuity_median, alpha=0.4, color = "black") +
      geom_vline(xintercept=CoT_median, alpha=0.4, color = "black") +
      xlab("log(Degree of Change)") +
      ylab("log(Acuity)") +
      labs(color = "Encounter") +
      scale_color_manual(labels = c("ICU",
                                    "Nephrology outpatient",
                                    "Nephrology inpatient",
                                    "Non-nephrology inpatient",
                                    "Non-nephrology outpatient"),
                         values = c("#f8766d",
                                    "gold2",
                                    "cornflowerblue",
                                    "darkorchid2",
                                    "chartreuse2")) +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  } else {
    # merge acuity and change-over-time scores
    df_filter <- df %>% 
      filter(ENCOUNTER_TYPE %in% encounter)
    
    df_filter <- df_filter %>% 
      mutate(log_acuity = log(acuity_score),
             log_CoT = log(CoT))
    
    max_log_cot <- max(df_filter$log_CoT)
    max_log_acuity <- max(df_filter$log_acuity)
    
    df_filter <- df_filter %>% 
      mutate(log_acuity_norm = (log_acuity / max_log_acuity) * 100,
             log_CoT_norm = (log_CoT / max_log_cot) * 100)
    
    #acuity_median <- median(df_filter$log_acuity, na.rm = T)
    #CoT_median <- median(df_filter$log_CoT, na.rm = T)
    
    ggplot(df_filter, aes(x=log_CoT, y=log_acuity, color=ENCOUNTER_TYPE)) +
      geom_point(alpha=0.5) +
      geom_hline(yintercept=acuity_median, alpha=0.4, color = "black") +
      geom_vline(xintercept=CoT_median, alpha=0.4, color = "black") +
      #ggtitle(encounter) +
      xlab("log(Degree of Change)") +
      ylab("log(Acuity)") +
      labs(color = "Encounter") +
      scale_color_manual(labels = c(encounter),
                         values = c(color_select)) +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
            legend.position = "none")
  }
}
plot_encounter(labs_score)
plot_log(labs_score)
ggsave("all_labs.pdf")

# ------------------------------------------------------------------------------
#                   Plotting Data (ICU_HOSPITAL_VISIT)
# ------------------------------------------------------------------------------
plot_encounter(labs_score, "ICU_VISIT_HOSPITAL")
# log plot
icu_plot <- plot_log(labs_score,"ICU_VISIT_HOSPITAL", "#f8766d")
icu_plot
ggsave("icu.pdf")

# ------------------------------------------------------------------------------
#                   Plotting Data (NEPH_OUTPATIENT)
# ------------------------------------------------------------------------------
plot_encounter(labs_score, "NEPH_OUTPATIENT")
# log plot
neph_out_plot <- plot_log(labs_score, "NEPH_OUTPATIENT", "gold2")
neph_out_plot
ggsave("neph_outpatient.pdf")

# ------------------------------------------------------------------------------
#                 Plotting Data (NEPH_HOSPITAL_VISIT)
# ------------------------------------------------------------------------------
plot_encounter(labs_score, "NEPH_VISIT_HOSPITAL")
# log plot
neph_in_plot <- plot_log(labs_score, "NEPH_VISIT_HOSPITAL", "cornflowerblue")
neph_in_plot
ggsave("neph_inpatient.pdf")

# ------------------------------------------------------------------------------
#               Plotting Data (NON_NEPH_NON_ICU_HOSPITAL)
# ------------------------------------------------------------------------------
plot_encounter(labs_score, "NON_NEPH_NON_ICU_HOSPITAL")
# log plot
non_neph_in_plot <- plot_log(labs_score, "NON_NEPH_NON_ICU_HOSPITAL", "darkorchid2")
non_neph_in_plot
ggsave("non_neph_inpatient.pdf")

# ------------------------------------------------------------------------------
#               Plotting Data (NON_NEPH_NON_ICU_OUTPATIENT)
# ------------------------------------------------------------------------------
plot_encounter(labs_score, "NON_NEPH_NON_ICU_OUTPATIENT")
# log plot
non_neph_out_plot <- plot_log(labs_score, "NON_NEPH_NON_ICU_OUTPATIENT", "chartreuse2")
non_neph_out_plot
ggsave("non_neph_outpatient.pdf")

# ------------------------------------------------------------------------------
#               Plotting Data (All inpatient encounters)
# ------------------------------------------------------------------------------
# log plot
inpatient_plot <- plot_log(labs_score, 
                     c("ICU_VISIT_HOSPITAL", "NEPH_VISIT_HOSPITAL", "NON_NEPH_NON_ICU_HOSPITAL"),
                     c("#f8766d", "cornflowerblue", "darkorchid2"))
inpatient_plot
ggsave("all_inpatient.pdf")

# ------------------------------------------------------------------------------
#               Plotting Data (All outpatient encounters)
# ------------------------------------------------------------------------------
# log plot
outpatient_plot <- plot_log(labs_score, 
                           c("NEPH_OUTPATIENT", "NON_NEPH_NON_ICU_OUTPATIENT"),
                           c("gold2", "chartreuse2"))
outpatient_plot
ggsave("all_outpatient.pdf")

# ------------------------------------------------------------------------------
#                       K-Means Clustering
# ------------------------------------------------------------------------------
labs_score <- labs_score %>% 
  filter(log_CoT != Inf,
         log_CoT != -Inf,
         log_acuity != -Inf)

labs_kmeans <- labs_score %>% 
  select(log_acuity, log_CoT) %>% 
  filter(log_CoT != Inf,
         log_CoT != -Inf)

labs_kmeans <- labs_kmeans[c("log_CoT", "log_acuity")]

km.out = kmeans(labs_kmeans,2,nstart=20)
plot(labs_kmeans, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2",
     xlab="log(Degree of Change)", ylab = "log(Acuity)", pch=20, cex=1)


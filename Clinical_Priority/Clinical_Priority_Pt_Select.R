# import useful libraries
library("tidyverse")
library("plotly")

# set working directory
setwd("/Users/michael/Clinical_Priority/data/OneDrive_1_2-15-2021")

# read in csv files
cot_labs <- read_csv("cot_labs.csv")
z_score_labs <- read_csv("z_score_labs.csv")
pt_labs <- read_csv("labs_most_recent.csv")

# merge acuity and change-over-time scores
labs_score <- na.omit(right_join(cot_labs, z_score_labs))

# ------------------------------------------------------------------------------
#                 Select Random Patients by Quadrant Function
# ------------------------------------------------------------------------------
# function to select random pt's from each quadrant
select_pt <- function(df, num, encounter) {
  # set seed for reproducible random sampling
  set.seed(6)
  if(missing(encounter)){
    # find median values for acuity and CoT
    acuity_median <- median(df$acuity_score_norm)
    CoT_median <- median(df$CoT_norm)
    # create vector of unique pt id's per quadrant
    pt_ids_1 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm > acuity_median & df$CoT_norm > CoT_median]
    pt_ids_2 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm > acuity_median & df$CoT_norm < CoT_median]
    pt_ids_3 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm < acuity_median & df$CoT_norm > CoT_median]
    pt_ids_4 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm < acuity_median & df$CoT_norm < CoT_median]
    # randomly sample n pts from vector 
    pt_ids_1 <- sample(pt_ids_1, size = num)
    pt_ids_2 <- sample(pt_ids_2, size = num)
    pt_ids_3 <- sample(pt_ids_3, size = num)
    pt_ids_4 <- sample(pt_ids_4, size = num)
    # get subset of df using vector created above as filter
    new_df <- subset(df, ENCRYPTED_PAT_MRN_ID %in% c(pt_ids_1, pt_ids_2, pt_ids_3, pt_ids_4))
  } else {
    # filter by encounter type
    df <- df %>% 
      filter(ENCOUNTER_TYPE == encounter)
    # find median values for acuity and CoT
    acuity_median <- median(df$acuity_score_norm)
    CoT_median <- median(df$CoT_norm)
    # create vector of unique pt id's per quadrant
    pt_ids_1 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm > acuity_median & df$CoT_norm > CoT_median]
    pt_ids_2 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm > acuity_median & df$CoT_norm < CoT_median]
    pt_ids_3 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm < acuity_median & df$CoT_norm > CoT_median]
    pt_ids_4 <- df$ENCRYPTED_PAT_MRN_ID[df$acuity_score_norm < acuity_median & df$CoT_norm < CoT_median]
    # randomly sample n pts from vector 
    pt_ids_1 <- sample(pt_ids_1, size = num)
    pt_ids_2 <- sample(pt_ids_2, size = num)
    pt_ids_3 <- sample(pt_ids_3, size = num)
    pt_ids_4 <- sample(pt_ids_4, size = num)
    # get subset of df using vector created above as filter
    new_df <- subset(df, ENCRYPTED_PAT_MRN_ID %in% c(pt_ids_1, pt_ids_2, pt_ids_3, pt_ids_4))
  }
}

# ------------------------------------------------------------------------------
#                     Examples of Function Use
# ------------------------------------------------------------------------------
find_pt_labs <- function(df) {
  pt_id <- df$ENCRYPTED_PAT_MRN_ID
  
  subset(pt_labs, ENCRYPTED_PAT_MRN_ID %in% pt_id)
}

# example of selecting from all encounter types
df <- select_pt(labs_score, 3)
# plot linear and log adjusted models (make sure to load in function from clinical_priority_visual.R)
plot_encounter(df)
plot_log(df)
# pt subset df
pt_df <- find_pt_labs(df)

# example of selecting from only ICU encounter types
df <- select_pt(labs_score, 3, "ICU_VISIT_HOSPITAL")
# plot linear and log adjusted models (make sure to load in function from clinical_priority_visual.R)
plot_encounter(df)
plot_log(df)

# Using plotly
fig <- df %>%
  plot_ly(
    type = 'scatter',
    mode = 'markers',
    x = ~CoT_norm,
    y = ~acuity_score_norm,
    #marker = list(size = ~acuity_score_norm, sizeref = 4000, sizemode = 'area'),
    color = ~ENCOUNTER_TYPE,
    text = ~ENCRYPTED_PAT_MRN_ID,
    hovertemplate = paste(
      "<b>Pt ID: %{text}</b><br><br>",
      "%{yaxis.title.text}: %{y:.3f}<br>",
      "%{xaxis.title.text}: %{x:.3f}<br>",
      "<extra></extra>"
    )
  )
fig <- fig %>%
  layout(legend = list(orientation = 'h', y = -0.3))
# plot
fig


########################################
# Computing correlations for review paper

library(tidyverse)     # create plots with ggplot, manipulate data, etc.
library(modelsummary)  # combine multiple regression models into a single table
library(readxl)        # read excel files
library(rstatix)       # get_summary_stats function
library(haven)         # for reading SPSS files 

# In this document, we compute an average correlation of participants' 
# fake and true news accuracy ratings. 

# Among the studies we had raw data on from our data extraction process, 
# we only select those studies that tested `veracity` (i.e. true vs. fake) 
# within participants.

# For each sample, we extract :
# - id (of individual participants)
# - veracity (identifying fake and true news)
# - condition (treatment vs. control)
# - accuracy (participants accuracy ratings)
# and we add:
# sample_id
# paper_id

# NOTE: `sample_id` is coding for studies. 
# That means that several conditions in one and the same study share the same sample_id.
# We should consider changing that to study_id. From then we can compute a unique_sample_id
# that corresponds to the one we use in the main meta analysis. 

# Bago, B., Rosenzweig, L. R., Berinsky, A. J., & Rand, D. G. (2022).
# Emotion may predict susceptibility to fake news but emotion regulation does not seem to help.
# Cognition and Emotion, 1–15. https://doi.org/10.1080/02699931.2022.2090318
#####

## Study 1

d <- read_csv("./data_from_papers/study1.csv")
head(d)

# check levels of veracity variable
levels(as.factor(d$reality))

d1 <- d %>% 
  mutate(id = ID, 
         veracity = ifelse(reality == "fake", "fake", "true"), 
         condition = "control", 
         accuracy = perceived_accuracy, 
         # only one sample in data set
         sample_id = 1, 
         paper_id = "Bago_2022") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

## Study 2

d2 <- read_csv("./data_from_papers/study2.csv")
head(d2)

# check levels of condition variable
levels(as.factor(d2$condition))

d2 <- d2 %>% 
  mutate(id = ID, 
         veracity = ifelse(reality == "fake", "fake", "true"),
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = case_when(condition == "Control" ~ 2, 
                               condition == "Reappraisal" ~ 3,
                               condition == "Suppression" ~ 4
                               ),
         condition = ifelse(condition == "Control", "control", "treatment"), 
         accuracy = perceived_accu, 
         paper_id = "Bago_2022") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

## Study 3

d3 <- read_csv("./data_from_papers/study3.csv")
names(d3)

# check levels of condition variable
levels(as.factor(d3$condition))

d3 <- d3 %>% 
  mutate(id = ID, 
         veracity = ifelse(reality == "fake", "fake", "true"),
         condition = ifelse(condition == "Control", "control", "treatment"), 
         accuracy = perceived_accu, 
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = ifelse(condition == "control", 5, 6), 
         paper_id = "Bago_2022") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

## Study 4

d4 <- read_csv("./data_from_papers/study4.csv")
head(d4)

d4 <- d4 %>% 
  mutate(id = ID, 
         veracity = ifelse(reality == "fake", "fake", "true"),
         condition = ifelse(condition == "Control", "control", "treatment"), 
         accuracy = perceived_accu, 
         # only one sample in data set
         sample_id = 7, 
         paper_id = "Bago_2022") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))


## Combine + add scale variable
Bago_2022 <- rbind(d1, d2, d3, d4) %>% 
  mutate(scale = "binary")
#####

# Bago, B., Rand, D. G., & Pennycook, G. (2020). Fake news, fast and slow: 
# Deliberation reduces belief in false (but not true) news headlines. 
# Journal of Experimental Psychology: General, 149(8), 1608–1613. 
# https://doi.org/10.1037/xge0000729
#####

## Study 1 (control)
d1 <- read_csv("./data_from_papers/study_1_final_only.csv")
names(d1)

# check levels of condition variable
levels(as.factor(d1$group))

d1 <- d1 %>% 
  mutate(id = ID, 
         veracity = ifelse(reality == "fake", "fake", "true"),
         condition = "control", 
         accuracy = perceived_accu, 
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = 1, 
         paper_id = "Bago_2020") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))


## Study 2 (control)
d2 <- read_csv("./data_from_papers/study_2_final_only.csv")
head(d2)

d2 <- d2 %>% 
  mutate(id = ID, 
         veracity = ifelse(reality == "fake", "fake", "true"),
         condition = "control", 
         accuracy = perceived_accu, 
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = 3, 
         paper_id = "Bago_2020") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

## Combine
Bago_2020 <- rbind(d1, d2) %>% 
  mutate(scale = "binary")
#####

# Chen, C. X., Pennycook, G., & Rand, D. G. (2021). What Makes News Sharable on 
# Social Media? [Preprint]. PsyArXiv. https://doi.org/10.31234/osf.io/gzqcd
#####
## Study 2
d2 <- read_csv("./data_from_papers/S2_data_headline_level.csv")
head(d2)

d2 <- d2 %>% 
  mutate(id = subjectID, 
         veracity = case_when(news_type == "fake" ~ "fake", 
                              news_type == "real" ~ "true"),
         condition = "control", 
         accuracy = true, 
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = 2, 
         paper_id = "Chen_2021") %>% 
  drop_na(veracity) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

## Combine
Chen_2021 <- d2 %>% 
  mutate(scale = 6)
#####

# Sultan, M., Tump, A. N., Geers, M., Lorenz-Spreen, P., Herzog, S., & 
# Kurvers, R. (2022). Time Pressure Reduces Misinformation Discrimination 
# Ability But Not Response Bias. 
# PsyArXiv. https://doi.org/10.31234/osf.io/brn5s
#####
dat <- read_excel("./data_from_papers/cleaned_data.xlsx")
names(dat)
### !for much of the following we simply copy the authors analysis script! ###

# add dat from pretest
# item numbers¸
item_number_index <- unique(dat$item_number)
pretest_batch_1 <- read_excel("./data_from_papers/final_items_after_pretest_batch_1.xlsx", sheet = 1, col_names = TRUE, col_types = NULL)
pretest_batch_2 <- read_excel("./data_from_papers/final_items_after_pretest_batch_2.xlsx", sheet = 1, col_names = TRUE, col_types = NULL)
pretest <- rbind(pretest_batch_1, pretest_batch_2)
# cleaning up workspace
rm(list = c("pretest_batch_1", "pretest_batch_2"))

# adding whether the item is true or false, which politicial party does the 
# item favour, and participant accuracy

# setting up a col
dat$item_accuracy <- NA
dat$item_political_leaning <- NA

# string detect all of the values that are item number x and then add that to item_accuracy of my df
for (i in 1:length(item_number_index)) {
  # get index of all of those that have item_number i as item number
  index_my_df <- str_detect(dat$item_number, paste0("\\b", item_number_index[i], "\\b"))
  # get value of whether item number i is true or false
  true_false_value_from_df_pretest <- pretest$Category[pretest$item_number == item_number_index[i]]
  item_dem_rep_value_from_df_pretest <- pretest$pretest_combined_categ[pretest$item_number == item_number_index[i]]
  # subset using index and value
  dat$item_accuracy[index_my_df] <- true_false_value_from_df_pretest
  dat$item_political_leaning[index_my_df] <- item_dem_rep_value_from_df_pretest
}

# adjusting values to True News and False News
dat$item_accuracy[str_detect(dat$item_accuracy, "True")] <- "True News"
dat$item_accuracy[str_detect(dat$item_accuracy, "False")] <- "False News"

# making into factor 
dat$item_accuracy <- factor(dat$item_accuracy, levels = c("False News", "True News"))

### !Here we differ from the authors analysis! ###
# we want to create a binary variable the codes 0 when participants rated 
# 'accuracy_response' as 'No', and 1 when 'Yes'. We later then take the overall mean 
# grouped by condition
dat <- dat %>% 
  mutate(accuracy_ratings = ifelse(accuracy_response == "No", 0, 1))

# check
dat[1:100, c("accuracy_response", "accuracy_ratings")]

# doing some additional stuff 
# changing Time Pressure to Time Pressure and such
dat <- dat %>% mutate(condition = 
                        case_when(condition == "time_pressure" ~ "Time Pressure",
                                  condition == "non_time_pressure" ~ "Control"))

# doing more additional stuff to get to a political congruence variable

# adding political leaning column
# values for scale
# 1 = Strongly Democratic
# 2 = Moderately Democratic
# 3 = Lean Democratic
# 4 = Strongly Republican
# 5 = Moderately Republican
# 6 = Lean Republican
dat$political_identification_scale <- as.numeric(dat$political_identification_scale)
dat <- dat %>% mutate(political_identity = ifelse(political_identification_scale <= 3, "Democrat", "Republican"))

# recoding to include scale labels
dat <- dat %>% mutate(political_identification_scale = 
                        case_when(political_identification_scale == 1 ~ "Strongly Democratic",
                                  political_identification_scale == 2 ~ "Moderately Democratic",
                                  political_identification_scale == 3 ~ "Lean Democratic",
                                  political_identification_scale == 4 ~ "Lean Republican",
                                  political_identification_scale == 5 ~ "Moderately Republican",
                                  political_identification_scale == 6 ~ "Strongly Republican"
                        )
)


# adding class to gender and political leaning
dat$political_identity <- factor(dat$political_identity, levels = c("Republican", "Democrat"))
dat$political_identification_scale <- factor(dat$political_identification_scale, levels = c("Strongly Republican", 
                                                                                            "Moderately Republican", 
                                                                                            "Lean Republican",
                                                                                            "Lean Democratic",
                                                                                            "Moderately Democratic",
                                                                                            "Strongly Democratic"
))

# adding congruency
dat <- dat %>% mutate(congruency = 
                        case_when(political_identity == "Democrat" & item_political_leaning == "Dem Favoured" ~ "Congruent",
                                  political_identity == "Democrat" & item_political_leaning == "Rep Favoured" ~ "Incongruent",
                                  political_identity == "Republican" & item_political_leaning == "Rep Favoured" ~ "Congruent",
                                  political_identity == "Republican" & item_political_leaning == "Dem Favoured" ~ "Incongruent"))



dat <- dat %>% mutate(congruency_non_binary = 
                        # for congruent 
                        case_when(political_identification_scale == "Strongly Democratic" & item_political_leaning == "Dem Favoured" ~ "Strongly Congruent",
                                  political_identification_scale == "Moderately Democratic" & item_political_leaning == "Dem Favoured" ~ "Moderately Congruent",
                                  political_identification_scale == "Lean Democratic" & item_political_leaning == "Dem Favoured" ~ "Lean Congruent",
                                  political_identification_scale == "Lean Republican" & item_political_leaning == "Rep Favoured" ~ "Lean Congruent",
                                  political_identification_scale == "Moderately Republican" & item_political_leaning == "Rep Favoured" ~ "Moderately Congruent",
                                  political_identification_scale == "Strongly Republican" & item_political_leaning == "Rep Favoured" ~ "Strongly Congruent",
                                  
                                  # for incongruent
                                  political_identification_scale == "Strongly Democratic" & item_political_leaning == "Rep Favoured" ~ "Strongly Incongruent",
                                  political_identification_scale == "Moderately Democratic" & item_political_leaning == "Rep Favoured" ~ "Moderately Incongruent",
                                  political_identification_scale == "Lean Democratic" & item_political_leaning == "Rep Favoured" ~ "Lean Incongruent",
                                  political_identification_scale == "Lean Republican" & item_political_leaning == "Dem Favoured" ~ "Lean Incongruent",
                                  political_identification_scale == "Moderately Republican" & item_political_leaning == "Dem Favoured" ~ "Moderately Incongruent",
                                  political_identification_scale == "Strongly Republican" & item_political_leaning == "Dem Favoured" ~ "Strongly Incongruent"
                        ))

# making factor
dat$congruency <- factor(dat$congruency, levels = c("Incongruent", "Congruent"))
dat$congruency_non_binary <- factor(dat$congruency_non_binary, levels = c("Strongly Incongruent", "Moderately Incongruent", "Lean Incongruent", "Lean Congruent", "Moderately Congruent", "Strongly Congruent"))

# the authors removed RTs exceeding 6 seconds (n = 208/11883; 2%) for the
# treatment group (time pressure) and 60 seconds (n = 190/12021; 2%) for the
# control group. 

# capping accuracy timer removing those trial that go beyond 6 seconds in time pressure
dat <- dat %>% 
  mutate(accuracy_timer_cap = F,
         accuracy_timer_cap = ifelse(condition == "Control" & accuracy_timer > 60, T,
                                     ifelse(condition == "Time Pressure" & accuracy_timer > 6, T, accuracy_timer_cap)))

# checking count
dat %>% group_by(condition) %>% count(accuracy_timer_cap)

# removing from dat
dat <- dat %>%
  filter(accuracy_timer_cap != T)

# check whether we achieved the same final sample size
dat %>% 
  group_by(ID, condition) %>% 
  summarise(n = n()) %>% 
  group_by(condition) %>% 
  summarise(n = n()) 

# visualize items per participants
dat %>% 
  group_by(ID, condition) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = n)) + 
  geom_histogram() + 
  ggtitle("Distribution of observations per participant")

# get summary stats
dat %>% group_by(condition, item_accuracy) %>% 
  get_summary_stats(accuracy_ratings, type = "mean_sd")

# extract data
# check levels of condition
levels(as.factor(dat$condition))
# check levels of veracity 
levels(as.factor(dat$item_accuracy))

d1 <- dat %>% 
  mutate(id = ID, 
         veracity = ifelse(item_accuracy == "False News", "fake", "true"),
         condition = ifelse(condition == "Control", "control", "treatment"), 
         accuracy = accuracy_ratings,
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = ifelse(condition == "control", 1, 2), 
         paper_id = "Sultan_2022") %>% 
  drop_na(veracity) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

## Combine
Sultan_2022 <- d1 %>% 
  mutate(scale = "binary")
#####

# Rathje, S., Roozenbeek, J., Van Bavel, J. J., & Van Der Linden, S. (2023). 
# Accuracy and social motivations shape judgements of (mis)information. 
# Nature Human Behaviour. https://doi.org/10.1038/s41562-023-01540-w

#####
### !for much of the following we simply copy the authors analysis script! ###

## Study 1

data <- read_csv("./data_from_papers/Study1DataAnonymized.csv")
head(data)

# Exclusions

#Exclude those who failed the attention check and responded randomly (pre-registered exclusion criteria)
count(subset(data, data$Q114 == 35))
data <- subset(data, data$Q114 == 35)
count(subset(data, data$Random == 2))
data <- subset(data, data$Random == 2)

## Recode Important Variables 

#Recode political oriention into binary (is Repub) variable
data$pol_orientation <- ifelse(data$DemRep_C > 3, 1, 0)
#Recode Orientation Variable as factor(0 = Republican, 0 = Democrat)
data$pol_orientation <- as.factor(data$pol_orientation)
levels(data$pol_orientation) <- c("Democrat", "Republican")

# check coding
data[data$pol_orientation == "Republican", c("pol_orientation", "DemRep_C")]

#Recode Condition Variable (0 = Experimental, 1 = Control)
data$Condition <- as.factor(data$random)
levels(data$Condition) <- c("Experimental", "Control")


# Accuracy measures
# HERE WE DIFFER FROM THEIR ANALYSIS
# we want to group by political concordance
# we therefore have to go into a long format

# First, build an easy-to-read ID variable for subjects (to calculate sample
# size later); we can do this since data is in wide format (one line per participant)
data <- data %>% 
  mutate(ID = as.factor(1:nrow(.)))

# Then go into long format
data_long <- data %>% 
  select(ID, starts_with("Accuracy"), Condition, pol_orientation) %>% 
  pivot_longer(-c("Condition", "pol_orientation", "ID"), names_to = "item_type",
               values_to = "ratings")

data_long <- data_long %>% 
  mutate(news_orientation = ifelse(grepl(item_type, pattern = "D", fixed = TRUE), 
                                   "Democrat", "Republican"),
         news_type = ifelse(grepl(item_type, pattern = "F", fixed = TRUE), 
                            "Fake", "True"), 
         binary_rating = ifelse(ratings > 3, 1, 0),
         congruence = ifelse(pol_orientation == news_orientation,"concordant", "discordant"))

# extract data
# levels veracity
levels(as.factor(data_long$news_type))
# levels condition
levels(as.factor(data_long$Condition))

d1 <- data_long %>% 
  mutate(id = ID, 
         veracity = ifelse(news_type == "Fake", "fake", "true"),
         condition = ifelse(Condition == "Control", "control", "treatment"), 
         accuracy = ratings, 
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = ifelse(condition == "treatment", 1, 2), 
         paper_id = "Rathje_2023") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))


## Study 2

data <- read_csv("./data_from_papers/Study2AnonymizedData.csv")
names(data)

# Exclusions
#Exclude those who failed the attention check and responded randomly (pre-registered exclusion criteria)
count(subset(data, data$Q114 == 35))
data <- subset(data, data$Q114 == 35)
count(subset(data, data$Random == 2))
data <- subset(data, data$Random == 2)

## Recode Important Variables 

#Recode political oriention into binary (is Repub) variable
data$pol_orientation <- ifelse(data$DemRep_C > 3, 1, 0)
#Recode Orientation Variable as factor(0 = Republican, 0 = Democrat)
data$pol_orientation <- as.factor(data$pol_orientation)
levels(data$pol_orientation) <- c("Democrat", "Republican")

# check coding
data[data$pol_orientation == "Republican", c("pol_orientation", "DemRep_C")]

#Recode Condition Variable 
data$Condition <- factor(data$random, levels = c(0, 1, 2, 3), 
                         labels = c("Accuracy","Control", "Social", "Mixed"))


# Accuracy measures

# we want to group by political concordance
# we therefore have to go into a long format

# First, build an easy-to-read ID variable for subjects (to calculate sample
# size later)
data <- data %>% 
  mutate(ID = as.factor(1:nrow(.)))

# Then go into long format
data_long <- data %>% 
  select(ID, starts_with("Accuracy"), Condition, pol_orientation) %>% 
  pivot_longer(-c("Condition", "pol_orientation", "ID"), names_to = "item_type",
               values_to = "ratings")

data_long <- data_long %>% 
  mutate(news_orientation = ifelse(grepl(item_type, pattern = "D", fixed = TRUE), 
                                   "Democrat", "Republican"),
         news_type = ifelse(grepl(item_type, pattern = "F", fixed = TRUE), 
                            "Fake", "True"), 
         binary_rating = ifelse(ratings > 3, 1, 0),
         congruence = ifelse(pol_orientation == news_orientation,"concordant", "discordant"))


# extract data
# levels veracity
levels(as.factor(data_long$news_type))
# levels condition
levels(as.factor(data_long$Condition))

d2 <- data_long %>% 
  mutate(id = ID, 
         veracity = ifelse(news_type == "Fake", "fake", "true"),
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = case_when(Condition == "Accuracy" ~ 3,
                               Condition == "Control" ~ 4,
                               Condition == "Social" ~ 5,
                               Condition == "Mixed" ~ 6
                               ),
         condition = ifelse(Condition == "Control", "control", "treatment"), 
         accuracy = ratings, 
         paper_id = "Rathje_2023") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))


## Study 3 

data <- read_csv("./data_from_papers/Study3AnonymizedData.csv")
names(data)

# Exclusions
#Exclude those who failed the attention check and responded randomly (pre-registered exclusion criteria)
count(subset(data, data$Q114 == 35))
data <- subset(data, data$Q114 == 35)
count(subset(data, data$Random == 2))
data <- subset(data, data$Random == 2)

## Recode Important Variables 

#Recode political oriention into binary (is Repub) variable
data$pol_orientation <- ifelse(data$DemRep_C > 3, 1, 0)
#Recode Orientation Variable as factor(0 = Republican, 0 = Democrat)
data$pol_orientation <- as.factor(data$pol_orientation)
levels(data$pol_orientation) <- c("Democrat", "Republican")

# check coding
data[data$pol_orientation == "Republican", c("pol_orientation", "DemRep_C")]

#Recode Condition Variable (0 = Experimental, 1 = Control)
data$Condition <- factor(data$condition, levels = c(0, 1, 2, 3), 
                         labels = c("Accuracy (Source)","Control (Source)", 
                                    "Accuracy (No Source)", 
                                    "Control (No Source)"))

# Accuracy measures

# we want to group by political concordance
# we therefore have to go into a long format

# First, build an easy-to-read ID variable for subjects (to calculate sample
# size later)
data <- data %>% 
  mutate(ID = as.factor(1:nrow(.)))

# Then go into long format
data_long <- data %>% 
  select(ID, starts_with("Accuracy"), Condition, pol_orientation) %>% 
  pivot_longer(-c("Condition", "pol_orientation", "ID"), names_to = "item_type",
               values_to = "ratings")

data_long <- data_long %>% 
  mutate(news_orientation = ifelse(grepl(item_type, pattern = "D", fixed = TRUE), 
                                   "Democrat", "Republican"),
         news_type = ifelse(grepl(item_type, pattern = "F", fixed = TRUE), 
                            "Fake", "True"), 
         binary_rating = ifelse(ratings > 3, 1, 0),
         congruence = ifelse(pol_orientation == news_orientation,"concordant", "discordant"))



# we want to primarily report the original 6-scale measure though
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(ratings, type = "mean_sd")

# extract data
# levels veracity
levels(as.factor(data_long$news_type))
# levels condition
levels(as.factor(data_long$Condition))

d3 <- data_long %>% 
  mutate(id = ID, 
         veracity = ifelse(news_type == "Fake", "fake", "true"),
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = case_when(Condition == "Accuracy (Source)" ~ 7,
                               Condition == "Control (Source)" ~ 8,
                               Condition == "Accuracy (No Source)" ~ 9,
                               Condition == "Control (No Source)" ~ 10
         ),
         condition = ifelse(Condition == "Control (Source)" | 
                              Condition == "Control (No Source)", "control", "treatment"), 
         accuracy = ratings, 
         paper_id = "Rathje_2023") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))


## Study 4 

data <- read_csv("./data_from_papers/Study4Anonymized.csv")
names(data)

# Exclusions
#Exclude those who failed the attention check and responded randomly (pre-registered exclusion criteria)
count(subset(data, data$Q114 == 35))
data <- subset(data, data$Q114 == 35)
count(subset(data, data$Random == 2))
data <- subset(data, data$Random == 2)

## Recode Important Variables 

#Recode political oriention into binary (is Repub) variable
data$pol_orientation <- ifelse(data$DemRep_C > 3, 1, 0)
#Recode Orientation Variable as factor(0 = Republican, 0 = Democrat)
data$pol_orientation <- as.factor(data$pol_orientation)
levels(data$pol_orientation) <- c("Democrat", "Republican")

# check coding
data[data$pol_orientation == "Republican", c("pol_orientation", "DemRep_C")]

#Recode Condition Variable (0 = Experimental, 1 = Control)
data$Condition <- factor(data$condition, levels = c(0, 1, 2), 
                         labels = c("Control","Accuracy (Financial)", 
                                    "Accuracy (Non-Financial)"))

# Accuracy measures

# we want to group by political concordance
# we therefore have to go into a long format

# First, build an easy-to-read ID variable for subjects (to calculate sample
# size later)
data <- data %>% 
  mutate(ID = as.factor(1:nrow(.)))

# Then go into long format
data_long <- data %>% 
  select(ID, starts_with("Accuracy"), Condition, pol_orientation) %>% 
  pivot_longer(-c("Condition", "pol_orientation", "ID"), names_to = "item_type",
               values_to = "ratings")

data_long <- data_long %>% 
  mutate(news_orientation = ifelse(grepl(item_type, pattern = "D", fixed = TRUE), 
                                   "Democrat", "Republican"),
         news_type = ifelse(grepl(item_type, pattern = "F", fixed = TRUE), 
                            "Fake", "True"), 
         binary_rating = ifelse(ratings > 3, 1, 0),
         congruence = ifelse(pol_orientation == news_orientation,"concordant", "discordant"))

# extract data
# levels veracity
levels(as.factor(data_long$news_type))
# levels condition
levels(as.factor(data_long$Condition))

d4 <- data_long %>% 
  mutate(id = ID, 
         veracity = ifelse(news_type == "Fake", "fake", "true"),
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = case_when(Condition == "Control" ~ 11,
                               Condition == "Accuracy (Financial)" ~ 12,
                               Condition == "Accuracy (Non-Financial)" ~ NA
         ),
         condition = ifelse(Condition == "Control", "control", "treatment"), 
         accuracy = ratings, 
         paper_id = "Rathje_2023") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

Rathje_2023 <- rbind(d1, d2, d3, d4) %>% 
  mutate(scale = 6)
#####

# Erlich, A., & Garner, C. (2023). Is pro-Kremlin Disinformation Effective? 
# Evidence from Ukraine. The International Journal of Press/Politics, 28(1), 
# 5–28. https://doi.org/10.1177/19401612211045221
#####
## Study 1

d <- read_rds("./data_from_papers/long_media_data_s1.rds")
names(d)

# check what's the indicator of true vs. fake

# two candidates, according to their variable names
table(d$true_story, d$narr_strat_true) # suggests they are the same
# check more thoroughly 
test <- d %>% select(true_story, narr_strat_true) %>% 
  mutate(test = ifelse(true_story == narr_strat_true, TRUE, FALSE))
table(test$test) # they are identical

# question is: Does 0 mean fake and 1 true, or vice versa? check against another
# candidate variable, 'strategy'
table(d$strategy, d$true_story) # '1' clearly means true

# sample size
length(levels(as.factor(d$Respondent_Serial)))

# number of items per participant and veracity
d %>% group_by(Respondent_Serial, true_story) %>% 
  summarize(n = n()) # 4 true, 12 false, as reported in paper


d %>% 
  select(
    # outcome measure, originally from 1 (completely true) to 6 (completely false)
    # according to the article. However, to be coherent with what's reported in 
    # the article, it must have been reverse-coded in the data at hand.
    # Because true news were rated as higher than fake news. Impossible to check.
    response,
    # indicator of whether the item is true
    true_story, 
    # news subject
    theme) %>% 
  group_by(true_story, theme) %>% 
  get_summary_stats(response, type = "mean_sd") %>% 
  # take into account
  mutate(n_observations = n, 
         n_participants = n_observations / 16)


# extract data
# levels veracity
levels(as.factor(d$true_story))

d1 <- d %>% 
  mutate(id = Respondent_Serial, 
         veracity = ifelse(true_story == 0, "fake", "true"),
         condition = "control", 
         accuracy = response, 
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = 1, 
         paper_id = "Erlich_2023") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

Ehrlich_2023 <- d1 %>% 
  mutate(scale = 6)
#####

# Altay, S., Lyons, B., & Modirrousta-Galian, A. (2023). 
# Exposure to Higher Rates of False News Erodes Media Trust and Fuels 
# Skepticism in News Judgment. https://doi.org/10.31234/osf.io/t9r43
#####

# Study 1
d <- read_csv("./data_from_papers/Study_1_Data_Long_Clean.csv")
names(d)

# levels condition
levels(as.factor(d$Condition))
# levels veracity
levels(as.factor(d$Veracity))


d1 <- d %>% 
  mutate(id = Participants, 
         veracity = ifelse(Veracity == "FALSE", "fake", "true"),
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = case_when(Condition == "False" ~ 2,
                               Condition == "Mix" ~ 3,
                               Condition == "True" ~ 4
                               ),
         condition = "control", 
         accuracy = Accuracy, 
         paper_id = "Altay_2023") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

# Study 2
d <- read_csv("./data_from_papers/Study_2_Data_Long_Clean.csv")
names(d)

# levels condition
levels(as.factor(d$Condition))
# levels veracity
levels(as.factor(d$Veracity))


d2 <- d %>% 
  mutate(id = ResponseId, 
         veracity = ifelse(Veracity == "FALSE", "fake", "true"),
         # assign sample id's to each of the sample groups 
         # as identified in meta data
         sample_id = case_when(Condition == "StrongFalse" ~ 5,
                               Condition == "False" ~ 6,
                               Condition == "Mix" ~ 7, 
                               Condition == "True" ~ 8,
                               Condition == "StrongTrue" ~ 9,
         ),
         condition = "control", 
         accuracy = Accuracy, 
         paper_id = "Altay_2023") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

Altay_2023 <- rbind(d1, d2) %>% 
  mutate(scale = 4)
#####

# Luo, M., Hancock, J. T., & Markowitz, D. M. (2022). 
# Credibility Perceptions and Detection Accuracy of Fake News Headlines on 
# Social Media: Effects of Truth-Bias and Endorsement Cues. 
# Communication Research, 49(2), 171–195. https://doi.org/10.1177/0093650220921321

#####
# Study 1
d <- read_csv("./data_from_papers/Study1_data0220.csv")

# check different sample id's (all control, 
# differed in the topic of news)
d %>% reframe(unique(Topic))

d <- d %>% 
  mutate(id = ResponseId, 
         veracity = ifelse(Veracity == "fake", "fake", "true"),
         condition = "control", 
         accuracy = Judgment, 
         # assign sample id's to each of the three samples groups 
         # as identified in meta data
         sample_id = case_when(Topic == "P" ~ 1, 
                               Topic == "H" ~ 2,
                               Topic == "S" ~ 3,
                               ), 
         paper_id = "Luo_2022") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

# Study 2
d2 <- read_csv("./data_from_papers/Study1_data0220.csv")

levels(as.factor(d2$Veracity))

d2 <- d2 %>% 
  mutate(id = ResponseId, 
         veracity = ifelse(Veracity == "fake", "fake", "true"),
         condition = "treatment", 
         accuracy = Judgment, 
         sample_id = 1, 
         paper_id = "Luo_2022") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id))

# combine
Luo_2022 <- rbind(d, d2) %>% 
  mutate(scale = 7)

#####
# Hoes-Altay-Angelis 2023
#####
d <- read_excel("./data_from_papers/Hoes-Altay-Angelis_data_clean.xlsx")
names(d)

# inspect key variables to get an overview
d %>% 
  select(PROLIFIC_PID, ResponseId, Veracity, Ratings, Condition, ITEM, 
         Headline, starts_with("head")) %>% 
  arrange(PROLIFIC_PID)

# clean data
Hoes.Altay_2023 <- d %>% 
  mutate(id = PROLIFIC_PID, 
         veracity = ifelse(Veracity == "False", "fake", "true"),
         condition = ifelse(Condition == "Control", "control", "treatment"), 
         accuracy = Ratings, 
         sample_id = 1, 
         paper_id = "Hoes-Altay-Angelis_2023", 
         scale = 6) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))
#####

#####
# Altay-Gilardi 2023
#####
d <- read_excel("./data_from_papers/Altay-Gilardi_data_long_clean.xlsx")
names(d)

# inspect key variables to get an overview
d %>% 
  select(PROLIFIC_PID, True_False,Condition, DV, Ratings, News_number) %>% 
  arrange(PROLIFIC_PID)

# long format data, `Ratings` codes the outcome for both sharing and accuracy, 
# so we have to filter DV == Accuracy. 
# Also, remove some treatment conditions that are irrelevant for our study
d <- d %>% 
  filter(DV == "Accuracy" & 
           Conditions %in% c("Control", "FalseLabel"))

# clean data
Altay.Gilardi_2023 <- d %>% 
  mutate(id = PROLIFIC_PID, 
         veracity = ifelse(True_False == "false", "fake", "true"),
         condition = ifelse(Condition == "Control", "control", "treatment"), 
         accuracy = Ratings, 
         sample_id = 1, 
         paper_id = "Hoes-Altay-Angelis_2023", 
         scale = 6) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))
#####

#####
# Espina Mairal, S., Bustos, F., Solovey, G., & Navajas, J. (2023). 
# Interactive crowdsourcing to fact-check politicians. 
# Journal of Experimental Psychology: Applied. 
# https://doi.org/10.1037/xap0000492
#####
# Study 1
d1 <- read_csv("./data_from_papers/mairal_database.csv")

# Thanks to their codebook (great documentation!), we know that `answer_chequeado`
# codes veracity and that `answer_1` is the original answer (before any manipulation).
# Since participant's initial rating happened under the same (unmanipulated) 
# circumstances for each condition, we consider them all as control. The `control` 
# variable is thus not relevant for us. 

d1 <- d1 %>% 
  mutate(id = id_subject, 
         veracity = ifelse(answer_chequeado == "V", "true", "fake"),
         condition = "control", 
         accuracy = ifelse(answer_1 == "V", 1, 0), 
         sample_id = 1, 
         paper_id = "Mairal_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))


# Study 2
d2 <- read_csv("./data_from_papers/mairal_database_study2.csv")

d2 <- d2 %>% 
  mutate(id = id_subject, 
         veracity = ifelse(answer_chequeado == "V", "true", "fake"),
         condition = "control", 
         accuracy = ifelse(answer_1 == "V", 1, 0), 
         sample_id = 2, 
         paper_id = "Mairal_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

# combine
Mairal_2023 <- rbind(d1, d2)
#####

#####
# Gawronski, B., Ng, N. L., & Luke, D. M. (2023). 
# Truth sensitivity and partisan bias in responses to misinformation. 
# Journal of Experimental Psychology: General, 152(8), 2205–2236. 
# https://doi.org/10.1037/xge0001381
#####

#####
# Experiment 1

d1 <- read_sav("./data_from_papers/Gawronski-Experiment1.sav")

# No codebook available, but we get variable names and answer codes from the 
# supplemental material with the original survey.
# Data in wide format. 
# For the rating variables (Pro..._S/T), 
# S indicates that participants were assigned to "Sharing" condition and T 
# to accuracy (or Truth) judgement. 
# We know that for the Political identification variable `PO_Part`, 1 == Republican
# and 2 == Democrat

d1 <- d1 %>% 
  # remove failed attention checks (according to paper)
  filter(is.na(Attn)) %>% 
  # restrict to key variables
  select(ResponseId, PO_Part, starts_with("Pro")) %>% 
  # bring into long format and separate variables
  pivot_longer(ProDemReal_01_T:ProRepFake_15_S,
               names_to = "SlantVeracity_number_condition", 
               values_to = "outcome") %>% 
  separate_wider_delim(SlantVeracity_number_condition, delim = "_", 
                       names = c("SlantVeracity", "number", "condition")) %>% 
  separate_wider_position(SlantVeracity, c(slant = 6, veracity = 4)) %>% 
  # add a headline id
  arrange(ResponseId) %>% 
  mutate(headline_id = rep(1:60, nrow(.)/60)) %>% 
  # get only accuracy rating participants (not sharing ones)
  filter(condition == "T") %>% 
  rename(accuracy = outcome) %>% 
  group_by(ResponseId) %>% 
  mutate(na_count = sum(is.na(accuracy))) %>%
  filter(na_count != 60) %>%
  ungroup() %>% 
  mutate(
    # Make nicer political identity variable
    political_identity = ifelse(PO_Part == 1, "ProRep", "ProDem"), 
    # Make concordance variable
    concordance = ifelse(political_identity == slant, "concordant", "discordant")
  ) 

d1 <- d1 %>% 
  mutate(id = ResponseId, 
         veracity = ifelse(veracity == "Real", "true", "fake"),
         condition = "control", 
         accuracy = accuracy, 
         sample_id = 1, 
         paper_id = "Gawronski_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

# Experiment 4

d2 <- read_sav("./data_from_papers/Gawronski-Experiment4.sav")

# For the rating variables (Pro..._S/T), we can deduce from the supplemental information
# that for participants in the sharing condition (irrelevant), the ending is "_C". 
# For participants in the truth prime condition (the one we are interested in), 
# as in experiment 1, 
# S indicates that participants were assigned to "Sharing" condition and T 
# to accuracy (or Truth) judgement. 

d2 <- d2 %>% 
  # remove failed attention checks (according to paper)
  filter(is.na(Attn)) %>% 
  # restrict to key variables
  select(ResponseId, PO_Part, starts_with("Pro")) %>% 
  # bring into long format and separate variables
  pivot_longer(ProDemFake_09_T:ProDemReal_12_S,
               names_to = "SlantVeracity_number_condition", 
               values_to = "outcome") %>% 
  separate_wider_delim(SlantVeracity_number_condition, delim = "_", 
                       names = c("SlantVeracity", "number", "condition")) %>% 
  separate_wider_position(SlantVeracity, c(slant = 6, veracity = 4)) %>% 
  # add a headline id
  arrange(ResponseId) %>% 
  mutate(headline_id = rep(1:60, nrow(.)/60)) %>% 
  # get only accuracy rating participants (not sharing once)
  filter(condition == "T") %>% 
  rename(accuracy = outcome) %>% 
  group_by(ResponseId) %>% 
  mutate(na_count = sum(is.na(accuracy))) %>%
  filter(na_count != 60) %>%
  ungroup() %>% 
  mutate(
    # Make nicer political identity variable
    political_identity = ifelse(PO_Part == 1, "ProRep", "ProDem"), 
    # Make concordance variable
    concordance = ifelse(political_identity == slant, "concordant", "discordant")
  ) %>% 
  select(ResponseId, veracity, accuracy, concordance)

d2 <- d2 %>% 
  mutate(id = ResponseId, 
         veracity = ifelse(veracity == "Real", "true", "fake"),
         condition = "control", 
         accuracy = accuracy, 
         sample_id = 2, 
         paper_id = "Gawronski_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

# combine
Gawronski_2023 <- rbind(d1, d2)
#####

#####
# Garrett, R. K., & Bond, R. M. (2021). 
# Conservatives’ susceptibility to political misperceptions. 
# Science Advances, 7(23), eabf1234. https://doi.org/10.1126/sciadv.abf1234
#####

#####
# No codebook, but the stata labels provide some insight.

# data with panel responses
d <- read_dta("./data_from_papers/Garret_panelPublic.dta")

d <- d %>% 
  # Remove variable labels (they slow down computation)
  haven::zap_labels() %>% 
  # add participant id
  mutate(participant_id = 1:nrow(.), 
         scale = 4) %>% 
  # bring to long format
  pivot_longer(matches("^[A-Z]\\d+"), 
               names_to = "VeracityNumberWeek_Wave",
               values_to = "accuracy") %>% 
  # assign a headline_id
  mutate(headline_id = VeracityNumberWeek_Wave) %>% 
  # split the headline capturing variable in variables meaningful for 
  # analysis
  separate_wider_regex(VeracityNumberWeek_Wave,
                       patterns  = c(veracity = "T|F",
                                     number = "\\d+",
                                     variant = "b|bn|e",
                                     wave = "_W\\d+")) %>% 
  # clean some variables
  mutate(wave = as.numeric(str_extract(wave, "\\d+")), 
         veracity = ifelse(veracity == "F", "fake", "true"),
         # remove NA values for accuracy and political identity
         across(c(accuracy, polid_W1), function(x) (ifelse (x %in% c(8,9), NA, x))),
         # from their supplemental material (and it fits with plausibility check)
         # they reverse coded accuracy (1 = probably true). We reverse-code it to 
         # be in line with other studies
         accuracy = 5-accuracy,
         political_party = case_when(pid3 == 1 ~ "democrat", 
                                     pid3 == 2 ~ "republican", 
                                     TRUE ~ NA_character_
         )
  ) %>% 
  # select key variables
  select(participant_id, headline_id, veracity, accuracy, wave, variant, political_party)

# check political party 
table(d$political_party, useNA  = "always")

# check single participant single wave
single_participant_single_wave <- d %>% 
  filter(participant_id == 1, wave == 1) %>% 
  pivot_wider(names_from = variant, 
              values_from = accuracy)
# Cross-checking question labels, ratings, and the supplemental material, it seems that `variant` == e
# encodes whether participant had encountered the headline or not (it only uses binary codes), 
# `variant` == "b" means encountered before, and `variant` == `bn` means unfamiliar
d <- d %>% 
  filter(variant != "e") %>% 
  drop_na(accuracy) %>% 
  rename(encountered_before = variant) %>% 
  mutate(encountered_before = ifelse(encountered_before == "b", TRUE, FALSE), 
         # change headline id, remove "e," "bn," or "b" from strings in the "Variable" column
         headline_id = gsub("[ebn]", "", headline_id)
  )

# (result) data identifying political slant of headlines
slant <- read_dta("./data_from_papers/Garret_AMTstatementSlant.dta")

# To be able to classify politically concordant and discordant ratings, 
# we need to match both.

# give matching id the same name as in the main data
slant <- slant %>% 
  rename(headline_id = statement_num) %>% 
  # change headline id, remove "e," "bn," or "b" from strings in the "Variable" column
  mutate(headline_id = gsub("[ebn]", "", headline_id))

# For the slant data, it seems that `rFavor` are is the aggregated rating from republicans, 
# `dFavor` from demcorats, and `pFavor` some resulting compromise. 
# This interpretation fits with what they write in the paper: 
# "We labeled each statement as favoring the party that benefited more or was 
# hurt less according to the two groups of partisan workers. When both groups 
# said that neither party benefited more, or when they gave contradictory assessments, 
# we labeled the statements as favoring neither party"

# check conflicting cases
slant %>% 
  mutate(conflict = case_when(rFavor == 1 & dFavor == 2 ~ TRUE, 
                              rFavor == 2 & dFavor == 1 ~ TRUE,
                              TRUE ~ FALSE
  )
  ) %>% 
  filter(conflict == TRUE)

# rename slant variable and labels
slant <- slant %>% 
  rename(slant = pFavor) %>% 
  mutate(slant = case_when(slant == 1 ~ "democrat", 
                           slant == 2 ~ "republican", 
                           slant == 3 ~ "neutral"))


full_d <- left_join(d, slant) 

# to assign the same sample id's as in the summary meta data, 
# we split the sample in the same way we did for the meta data

# we split the sample in those who identify as democrat or republican and those who don't
political_d <- full_d %>% 
  filter(!is.na(political_party)) %>% 
  mutate(concordance = case_when(slant == political_party ~ "concordant", 
                                 slant == "neutral" ~ "neutral", 
                                 slant != political_party ~ "discordant"))

Apolitical_d <- full_d %>% 
  filter(is.na(political_party))

# Get summary data
d1 <- political_d  %>% 
  mutate(id = participant_id, 
         veracity = veracity,
         condition = "control", 
         accuracy = accuracy, 
         sample_id = 1, 
         paper_id = "Garrett_2021", 
         scale = 4) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

d1 <- Apolitical_d  %>% 
  mutate(id = participant_id, 
         veracity = veracity,
         condition = "control", 
         accuracy = accuracy, 
         sample_id = 2, 
         paper_id = "Garrett_2021", 
         scale = 4) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Garrett_2021 <- rbind(d1, d2)
#####

#####
# Pereira, F. B., Bueno, N. S., Nunes, F., & Pavão, N. (2023). 
# Inoculation Reduces Misinformation: Experimental Evidence from Multidimensional 
# Interventions in Brazil. Journal of Experimental Political Science, 1–12. 
# https://doi.org/10.1017/XPS.2023.11
#####

#####

# Study 2 (study 1 not relevant)
d <- read_dta("./data_from_papers/Pereira_2023-study2.dta")

d <- d %>% 
  # add easy id
  mutate(id = 1:nrow(.))

# first wave ratings are pre-treatment, for both the treatment and the control group: 
# "After completing the first wave questionnaire, respondents were randomly assigned, 
# via simple randomization, to one of two conditions. After the first wave of the survey 
# and before the second, the treatment group (n = 575) received the main experimental stimuli 
# comprised of a multidimensional intervention seeking to reduce rumor acceptance."


# for the second wave, we only want those news headlines that are not repeated.
# According to the codebook, these are these "alterantive" news are the ones that contain "b" in the variable name
wave1 <- d %>% 
  select(-starts_with("w2")) 

wave2 <- d %>% 
  select(id, c(starts_with("w2") & ends_with("b"))) %>%
  # remove "w2_" name prefix
  rename_with(~gsub("w2_", "", .), starts_with("w2_"))

# merge back to reduced data frame
d <- left_join(wave1, wave2, by = join_by(id))

# bring to long format
d <- d %>% 
  pivot_longer(c(starts_with("news") & !contains("_")), 
               names_to = "headline", 
               values_to = "accuracy"
  ) %>% 
  # add veracity and recode accuracy variable (from codebook we know that (1 = true and 2 = False))
  mutate(veracity = ifelse(grepl("f", headline), "false", "true"), 
         # make 1 = rated as true and 0 = rated as false
         accuracy = 2 - accuracy, 
         # recode condition variable (from codebook we know that treatment indicator (1 = treatment, 0 = control))
         condition = ifelse(treatment == 1, "treatment", "control"), 
         # code wave
         wave = ifelse(grepl("b", headline), 2, 1)
  ) 

# Wave 1
# In order to be consistent in the sample id's, we nevertheless split the the first wave results into two samples, 
# one for the control and one for the treatment group.
# summary stats (when grouping not by condition but by headline, we replicate descriptive stats in appendix)
d %>%
  filter(wave == 1) %>% 
  group_by(wave, condition, veracity) %>% 
  drop_na(accuracy) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id)) 

# Wave 2
d %>%
  filter(wave == 2) %>% 
  group_by(wave, condition, veracity) %>% 
  drop_na(accuracy) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id), 
            n_headlines = n_distinct(headline)) 



# Get data

# Wave 1 (even the treatment group can be considered control here, 
# as it's the pre-intervention measure)
d1 <- d %>%
  filter(wave == 1) %>%  
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         # for sample id, distinguish between treatment and control
         sample_id = ifelse(condition == "control", 1, 2),
         # now, mak all control
         condition = "control", 
         accuracy = accuracy, 
         paper_id = "Pereira_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

# Wave 2 
d2 <- d %>%
  filter(wave == 2) %>%  
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         # for sample id, distinguish between treatment and control
         sample_id = ifelse(condition == "control", 1, 2),
         condition = condition, 
         accuracy = accuracy, 
         paper_id = "Pereira_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Pereira_2023 <- rbind(d1, d2)
#####

#####
# Epstein, Z., Sirlin, N., Arechar, A., Pennycook, G., & Rand, D. (2023). 
# The social media context interferes with truth discernment. 
# Science Advances, 9(9), eabo6169. https://doi.org/10.1126/sciadv.abo6169
#####

#####

# Read data
d <- read_csv("./data_from_papers/Epstein_2023.csv")

# check data
d %>% group_by(ResponseId) %>%
  count() %>% # id = ResponseId
  print(n = 50)

# Note that some participants have up to 50 observations, because they still include sharing ratings

# remove condition two because not reported in figures in paper
d <- d %>% 
  filter(Condition != 2) %>%
  # remove sharing as we focus on accuracy
  filter(engagement_type == "Accuracy") 

# We can see that condition 1 is surely only on accuracy, and condition 2 surely only on sharing. 
# We don't know what conditions 3 and 4 correspond to - we need to identifiy the one where participants rated accuracy first.
# To do so, we calculate means and compare with figures. 

d %>% 
  group_by(wave, Condition, veracity) %>% 
  summarise(across(response, ~mean(.x))) %>% 
  pivot_wider(values_from = response, 
              names_from = veracity) %>% 
  mutate(discernment = `1` - `0`)

# From comparison with the figures in the paper, it seems that condition 3 
# is the one where participants rated accuracy first. 

# we suppose that when concordance = -0.5, this corresponds to discordant and conversly for concordant

# summary stats 
d <- d %>%
  mutate(accuracy = response, 
         veracity = ifelse(veracity == 1, "true", "fake"),
         concordance = case_when(concord == 0.5 ~ "concordant", 
                                 concord == -0.5 ~"discordant", 
                                 TRUE ~ as.character(concord)), 
         condition_detail = case_when(Condition == 1 ~ "accuracy only", 
                                      Condition == 3 ~ "accuracy first, then sharing", 
                                      Condition == 4 ~ "sharing first, then accuracy"), 
         condition = ifelse(Condition %in% c(1,3), "control", "treatment"), 
         topic = ifelse(wave == 1, "covid", "political")
  ) 

# extract a sample id
d <- d %>% 
  mutate(sample_id = case_when(
    # need to go from most to least specific
    condition_detail == "accuracy first, then sharing" & topic == "political" & 
      is.na(concordance) ~ 4,
    condition_detail == "accuracy only" & topic == "political" & 
      is.na(concordance) ~ 6,
    condition_detail == "accuracy first, then sharing" & topic == "covid" ~ 1,
    condition_detail == "accuracy only" & topic == "covid" ~ 2,
    condition_detail == "accuracy first, then sharing" & topic == "political" ~ 3,
    condition_detail == "accuracy only" & topic == "political" ~ 5,
    )
    )

# get clean data
d <- d %>%
  mutate(id = ResponseId, 
         veracity = veracity,
         sample_id = sample_id,
         condition = condition, 
         accuracy = accuracy, 
         paper_id = "Epstein_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Epstein_2023 <- d
#####

#####

#####
# Aslett, K., Sanderson, Z., Godel, W., Persily, N., Nagler, J., & Tucker, J. A. (2024). 
# Online searches to evaluate misinformation can increase its perceived veracity. 
# Nature, 625(7995), 548–556. https://doi.org/10.1038/s41586-023-06883-y
#####

#####

# Note that here, we will only inlcude study 1 and 5, as these are the only within-participant
# design studies regarding news veracity.

# From the authors' analysis script, we know that: 

#Study 1 (False/Misleading Article Data): Study_1_df_FM.csv
#Study 2 (False/Misleading Article Data): Study_2_df_FM.csv
#Study 3 (False/Misleading Article Data): Study_3_df_FM.csv
#Study 4 (False/Misleading Article Data): Study_4_df_FM.csv
#Study 5 (False/Misleading Article Data): Study_5_df_FM.csv
#Study 5 (Web Tracking Data): Study_5_treat_data.csv
#Fact-Checker Ideology Data: FC_Ideo_Data.csv
#Study 5 (Only Control Data): Control_Data_Study_5.csv
# Study 5 Web Tracking Data (Only Control Data): output_Control_Survey_2.csv
#Search Term - Headline Coding: Headline_Coding_4.csv
#Study 1 (True Article Data): Study_1_df_T.csv
#Study 2 (True Article Data): Study_2_df_T.csv
#Study 3 (True Article Data): Study_3_df_T.csv
#Study 4 (True Article Data): Study_4_df_T.csv
#Study 5 (True Article Data): Study_5_df_T.csv

# Study 1
d_false <- read_csv("./data_from_papers/Aslett_2023_Study_1_df_FM.csv") %>% 
  mutate(veracity = "false")

d_true <- read_csv("./data_from_papers/Aslett_2023_Study_1_df_T.csv") %>% 
  mutate(veracity = "true")

# Double check with paper:
# Over these 10 days, 13 different false/misleading articles were evaluated by 
# individuals in our control group who were not requested to search online 
# (resulting in 1,145 evaluations from 876 unique respondents) and those in our 
# treatment group who were requested to search online 
# (resulting in 1,130 evaluations from 872 unique respondents).

d_false %>% 
  # they omit NA's in their analysis
  na.omit() %>% 
  group_by(Treat_Search) %>% 
  summarize(n_evaluations = n(), 
            n_participants = n_distinct(ResponseId))

# For false news, it seems like `Article_day` identifies false headlines (13 different entries, matching paper description)
table(d_false$Article_day)
length(levels(as.factor(d_false$Article_day)))

# They don't report number of true news by studies, but from appendix section A we can count all headlines. 
# There are 50 in total, so there should be 50 - 13 False/Misleading - 1 Could not determine = 36 true headlines. 

# several candidates:
table(d_true$Article)
table(d_true$article_number)
table(d_true$day)
table(d_true$Article_day)
length(levels(as.factor(d_true$Article_day))) 
# it seems like it's also "Article_day" that is the headline identifier

# check how many answers per participant
d_false %>% group_by(ResponseId) %>% count() %>% 
  group_by(n) %>% 
  summarize(n_distinct(ResponseId))

d_true %>% group_by(ResponseId) %>% count() %>% 
  group_by(n) %>% 
  summarize(n_distinct(ResponseId))

# From their code, Article seems to identify whether true news came from mainstream or low quality sources
# T_LQ_Data_Study_1 <- Study_1_df_T %>% filter(Article == "1" | Article == "2" | Article == "3")
# T_Mainstream_Data_Study_1 <- Study_1_df_T %>% filter(Article == "4" | Article == "5")
# We will code 1, 3 and 3 as low qualitiy, and 4 and 5 as mainstream sources

d_true <- d_true %>% 
  mutate(source = ifelse(Article %in% c(1,2,3), "low quality", "mainstream"))

# merge
d <- bind_rows(d_false, d_true_selected <- d_true %>% 
                 select(names(d_false), source))

# check if 49 unique headlines
length(levels(as.factor(d$Article_day))) 

# Recode key variables
# we know from the paper that "treatment dummy (1 = treatment group; 0 = control group)"
d <- d %>% 
  mutate(condition = ifelse(Treat_Search == 0, "control", "treatment")) %>% 
  rename(accuracy = Likert_Evaluation, 
         headline = Article_day)

# Did participants answer both true and false news? 
d %>% 
  group_by(ResponseId, veracity) %>% 
  count() %>%
  pivot_wider(names_from = veracity, 
              values_from = n) %>% 
  mutate(seen_veracity_types = case_when(is.na(true) ~ "false only", 
                                         is.na(false) ~ "true only",
                                         !is.na(true) & !is.na(false) ~ "both", 
                                         TRUE ~ NA_character_
  )) %>% 
  group_by(seen_veracity_types) %>% 
  summarize(n_subjects = n_distinct(ResponseId))

# We will subset the data on those participants who saw both types of news
valid_respondents <- d %>% 
  group_by(ResponseId, veracity) %>% 
  count() %>%
  pivot_wider(names_from = veracity, 
              values_from = n) %>% 
  mutate(seen_veracity_types = case_when(is.na(true) ~ "false only", 
                                         is.na(false) ~ "true only",
                                         !is.na(true) & !is.na(false) ~ "both", 
                                         TRUE ~ NA_character_
  )) %>% 
  filter(seen_veracity_types == "both") %>% 
  pull(ResponseId)

d <- d %>% 
  filter(ResponseId %in% valid_respondents) %>% 
  arrange(ResponseId)

# check total number of respondents
d %>% 
  reframe(n_subjects = n_distinct(ResponseId))

# note that this also diminishes the number of unique true headlines that we have
length(levels(as.factor(d$headline)))

# Just because they have seen all the answers, it doesn't mean that they have answered in both conditions.
# Any participants with no valid answers at all? 
d %>% 
  group_by(ResponseId) %>% 
  summarise(NAs = sum(is.na(accuracy)), 
            total_ratings = n(), 
            share_NAs = NAs/total_ratings) %>% 
  summarise(n_subjects_with_only_NAs = sum(share_NAs == 1)) # No


# Did all respondents have valid answer for both true and false?
d %>% 
  group_by(ResponseId, veracity) %>% 
  summarise(NAs = sum(is.na(accuracy)), 
            total_ratings = n(), 
            share_NAs = NAs/total_ratings
  ) %>% 
  group_by(veracity) %>%
  summarize(participants_with_only_NAs = sum(share_NAs == 1)) #No

# check NAs for all variables

# Summarize number of NAs across all variables in a data frame
na_summary <- colSums(is.na(d))
# Print the summary
print(na_summary)

# Which of the remaining headlines are from low and which from high quality sources?
d %>% 
  filter(veracity == "true") %>% 
  group_by(source, headline) %>% 
  count() %>% 
  print(n = 30)
# We cannot compare the headline identifier variable with the table in ESM A.

# check how many answers per participant
d %>% 
  group_by(ResponseId, condition) %>% 
  summarize(n_valid_answers = sum(!is.na(accuracy))) %>% 
  group_by(n_valid_answers,condition) %>% 
  summarize(n_distinct(ResponseId))

# most participants gave 3 valid answers

# What's the average share of true news participants rated (valid ratings, not just seen)? 
share_true_news <- d %>% 
  group_by(ResponseId, veracity) %>% 
  summarise(valid_answers = sum(!is.na(accuracy))) %>% 
  pivot_wider(names_from = veracity,
              values_from = valid_answers) %>% 
  mutate(
    n_total_valid_responses = false + true,
    share_true = true / n_total_valid_responses)  %>% 
  ungroup()

share_true_news %>% 
  group_by(share_true) %>% 
  summarise(n_subjects = n_distinct(ResponseId))

# Most participants saw 2/3 true news

# Here is the mean
share_true_news %>% 
  summarize(mean_share_true = mean(share_true))

# In our data frame we will take the modal values, i.e. 3 items per participant, and 2/3 share of true news

# check summary statistics
d %>% 
  # in our report, we average the sample size between respondents with valid responses between false and true news
  # we remove NAs here to make sure participants with only invalid responses do not appear in the n_subj count
  drop_na(accuracy) %>% 
  group_by(condition, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(ResponseId), 
            n_headlines = n_distinct(headline)) # note that the n_subj differs from the paper because we do not use (omitNA)


# get clean data
d1 <- d %>%
  mutate(id = ResponseId, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = 1,
         condition = condition, 
         accuracy = accuracy, 
         paper_id = "Aslett_2024", 
         scale = 7) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

# Study 5
d_false <- read_csv("./data_from_papers/Aslett_Study_5_df_FM.csv") %>% 
  mutate(veracity = "false")

d_true <- read_csv("./data_from_papers/Aslett_Study_5_df_T.csv") %>% 
  mutate(veracity = "true")

# From the paper, we know:
# "his study was almost identical to study 1, but we used a custom plug-in to 
# collect digital trace data and encouraged the respondents to specifically search 
# online using Google (our web browser plug-in could collect search results only 
# from a Google search result page). Similar to study 1, we measured the effect of 
# SOTEN on belief in misinformation in a randomized controlled trial that ran on 
# 12 separate days from 13 July 2021 to 9 November 2021, during which we asked two 
# different groups of respondents to evaluate the same false/ misleading or true 
# articles in the same 24 h window. The treatment group was encouraged to search 
# online, while the control group was not."
# "A total of 17 different false/misleading articles were evaluated by individuals 
# in our control group who were not encouraged to search online (877 evaluations 
# from 621 unique respondents) and those in our treatment group who were encouraged 
# to search online (608 evaluations from 451 unique respondents)."

d_false %>% 
  group_by(Treatment) %>% 
  summarize(n_evaluations = n(), 
            n_participants = n_distinct(ResponseId))

# For false news, it seems like `Article_day` identifies false headlines (13 different entries, matching paper description)
table(d_false$Article_day)
length(levels(as.factor(d_false$Article_day)))

# They don't report number of true news by studies, but from appendix section A we can count all headlines. 
table(d_true$Article_day)
length(levels(as.factor(d_true$Article_day))) 

# check how many answers per participant
d_false %>% group_by(ResponseId) %>% count() %>% 
  group_by(n) %>% 
  summarize(n_distinct(ResponseId))

d_true %>% group_by(ResponseId) %>% count() %>% 
  group_by(n) %>% 
  summarize(n_distinct(ResponseId))

# From their code, Article_Num seems to identify whether true news came from mainstream or low quality sources
# We will code 1, 3 and 3 as low qualitiy, and 4 and 5 as mainstream sources

d_true <- d_true %>% 
  mutate(source = ifelse(Article_Num %in% c(1,2,3), "low quality", "mainstream"))

# merge
# Get the names of columns in d_false excluding FC_eval
selected_columns <- setdiff(names(d_false), "FC_Eval")

# Select columns from d_true that match the names in selected_columns
d_true_selected <- d_true %>%
  select(all_of(selected_columns), source)

# Bind rows of d_false and d_true_selected
d <- bind_rows(d_false[selected_columns], d_true_selected)


# check number headlines
length(levels(as.factor(d$Article_day))) 

# Recode key variables
# we know from the paper that "treatment dummy (1 = treatment group; 0 = control group)"
d <- d %>% 
  mutate(condition = ifelse(Treatment == 0, "control", "treatment")) %>% 
  rename(accuracy = Seven_Ordinal, 
         headline = Article_day)

# Did participants answer both true and false news? 
d %>% 
  group_by(ResponseId, veracity) %>% 
  count() %>%
  pivot_wider(names_from = veracity, 
              values_from = n) %>% 
  mutate(seen_veracity_types = case_when(is.na(true) ~ "false only", 
                                         is.na(false) ~ "true only",
                                         !is.na(true) & !is.na(false) ~ "both", 
                                         TRUE ~ NA_character_
  )) %>% 
  group_by(seen_veracity_types) %>% 
  summarize(n_subjects = n_distinct(ResponseId))

# We will subset the data on those participants who saw both types of news
valid_respondents <- d %>% 
  group_by(ResponseId, veracity) %>% 
  count() %>%
  pivot_wider(names_from = veracity, 
              values_from = n) %>% 
  mutate(seen_veracity_types = case_when(is.na(true) ~ "false only", 
                                         is.na(false) ~ "true only",
                                         !is.na(true) & !is.na(false) ~ "both", 
                                         TRUE ~ NA_character_
  )) %>% 
  filter(seen_veracity_types == "both") %>% 
  pull(ResponseId)

d <- d %>% 
  filter(ResponseId %in% valid_respondents) %>% 
  arrange(ResponseId)

# check total number of respondents
d %>% 
  reframe(n_subjects = n_distinct(ResponseId))

# note that this also diminishes the number of unique true headlines that we have
length(levels(as.factor(d$headline)))

# Just because they have seen all the answers, it doesn't mean that they have answered in both conditions.
# Any participants with no valid answers at all? 
d %>% 
  group_by(ResponseId) %>% 
  summarise(NAs = sum(is.na(accuracy)), 
            total_ratings = n(), 
            share_NAs = NAs/total_ratings) %>% 
  summarise(n_subjects_with_only_NAs = sum(share_NAs == 1)) # No


# Did all respondents have valid answer for both true and false?
d %>% 
  group_by(ResponseId, veracity) %>% 
  summarise(NAs = sum(is.na(accuracy)), 
            total_ratings = n(), 
            share_NAs = NAs/total_ratings
  ) %>% 
  group_by(veracity) %>%
  summarize(participants_with_only_NAs = sum(share_NAs == 1)) #Yes

# check NAs for all variables

# Summarize number of NAs across all variables in a data frame
na_summary <- colSums(is.na(d))
# Print the summary
print(na_summary)

# Which of the remaining headlines are from low and which from high quality sources?
d %>% 
  filter(veracity == "true") %>% 
  group_by(source, headline) %>% 
  count() %>% 
  print(n = 30)
# We cannot compare the headline identifier variable with the table in ESM A.

# check how many answers per participant
d %>% 
  group_by(ResponseId, condition) %>% 
  summarize(n_valid_answers = sum(!is.na(accuracy))) %>% 
  group_by(n_valid_answers,condition) %>% 
  summarize(n_distinct(ResponseId))

# most participants gave 3 valid answers

# check how many answers per participant
d %>% 
  group_by(ResponseId, veracity) %>% 
  summarize(n_valid_answers = sum(!is.na(accuracy))) %>% 
  group_by(n_valid_answers,veracity) %>% 
  summarize(n_distinct(ResponseId))

# What's the average share of true news participants rated (valid ratings, not just seen)? (Control group only)
share_true_news <- d %>% 
  filter(condition == "control") %>% 
  group_by(ResponseId, veracity) %>% 
  summarise(valid_answers = sum(!is.na(accuracy))) %>% 
  pivot_wider(names_from = veracity,
              values_from = valid_answers) %>% 
  mutate(
    n_total_valid_responses = false + true,
    share_true = true / n_total_valid_responses)  %>% 
  ungroup()

share_true_news %>% 
  group_by(share_true) %>% 
  summarise(n_subjects = n_distinct(ResponseId))

# Most participants saw 2/3 true news. But this is only about half of the participants, so we will rely on the mean in our data

# Here is the mean
share_true_news %>% 
  summarize(mean_share_true = mean(share_true))

# In our data frame we will take the modal values, i.e. 3 items per participant, and 2/3 share of true news

# check summary statistics
d %>% 
  # in our report, we average the sample size between respondents with valid responses between false and true news
  # we remove NAs here to make sure participants with only invalid responses do not appear in the n_subj count
  drop_na(accuracy) %>% 
  group_by(condition, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(ResponseId), 
            n_headlines = n_distinct(headline)) # note that the n_subj differs from the paper because we do not use (omitNA)

# get clean data
d2 <- d %>%
  mutate(id = ResponseId, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = 5,
         condition = condition, 
         accuracy = accuracy, 
         paper_id = "Aslett_2024", 
         scale = 7) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Aslett_2024 <- rbind(d1, d2)

#####

#####

#####
# Allen, J., Arechar, A. A., Pennycook, G., & Rand, D. G. (2021). 
# Scaling up fact-checking using the wisdom of crowds. 
# Science Advances, 7(36), eabf4393.
#####

#####
# read what appears to be the main data
d <- read.csv("./data_from_papers/Allen_2021-RepAB.csv") 

# We know from their code that headline id's are coded under `msg_id`
d <- d %>% 
  rename(headline = msg_id)

# Further, from the code and looking at their data, it looks like the first three respondents are the three professional fact checkers they hired
d %>% 
  group_by(w_id) %>% 
  count() %>%
  rename(n_ratings_per_subject = n) %>% 
  group_by(n_ratings_per_subject) %>% 
  summarize(n_participants = n_distinct(w_id))

# First, we need to identify veracity of news statements based on the fact checker ratings
# From their code we know that they calculate an average veracity score based on 7 items. 
professional_factcheckers <- d %>%
  filter(w_id <= 3) %>%
  mutate(mean_veracity = (mru+ mro+ mra+ mrc+ mrr+ mrw+ mrh)/7) 

# We now calculate an average of the individual fact checker averages for each headline. 
# We code a binary veracity indicator, indicating `false` if the value is below the scale midpoint of 4, and `true` if above.
headline_veracity <- professional_factcheckers %>% 
  group_by(headline) %>% 
  summarise(average_prof_factcheckers = mean(mean_veracity)) %>% 
  mutate(veracity = case_when(average_prof_factcheckers < 4 ~ "false", 
                              average_prof_factcheckers > 4 ~ "true", 
                              average_prof_factcheckers == 4 ~ NA_character_, 
                              TRUE ~ NA_character_
  ))
# check
table(headline_veracity$veracity, useNA = "always")

# One NA, check that 
headline_veracity %>% 
  filter(is.na(veracity))

# There is one headline for which the average rating across fact checkers is exactly 4. 
# We will exclude that item in our summarized data later on

# Now, we add veracity to the data frame for participants (removeing the fact checkers).
d <- left_join(d %>% filter(w_id > 3), headline_veracity, by = "headline")

# Now, we will look only at the accuracy variable, 
# because this item seems to be closest to what we are after and what other articles report.
# From the survey on the OSF, we know that `mrc` captures accuracy 
# 'Do you think this story is accurate ?' (1-Denfinitely No, 7- Definitely Yes)
d <- d %>% 
  rename(accuracy = mrc, 
         id = w_id)

# check how many real and false news items, on average
# What's the average share of true news participants rated (valid ratings, not just seen)? (Control group only)
share_true_news <- d %>% 
  # remove that one item where veracity could not be established (average fact checker rating exactly 4)
  drop_na(veracity) %>% 
  group_by(id, veracity) %>% 
  summarise(valid_answers = sum(!is.na(accuracy))) %>% 
  pivot_wider(names_from = veracity,
              values_from = valid_answers) %>% 
  mutate(
    n_total_valid_responses = false + true,
    share_true = true / n_total_valid_responses)  %>% 
  ungroup()

share_true_news %>% 
  summarise(mean_share_true = mean(share_true))

# sample size
d %>% summarize(n_distinct(id))

# get summary statistics
d %>% 
  # remove that one item where veracity could not be established (average fact checker rating exactly 4)
  drop_na(veracity) %>% 
  group_by(veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id), 
            n_headlines = n_distinct(headline)) 

# get clean data
d <- d %>%
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = 1,
         condition = "control", 
         accuracy = accuracy, 
         paper_id = "Allen_2021", 
         scale = 7) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Allen_2021 <- d

#####

#####

#####
# Lyons, B., Modirrousta-Galian, A., Altay, S., & Salovich, N. A. (2024). 
# Reduce blind spots to improve news discernment? Performance feedback reduces 
# overconfidence but does not improve subsequent discernment. 
# https://doi.org/10.31219/osf.io/kgfrb
#####

#####
# Study 1 (first wave)
d1 <- read_csv("./data_from_papers/Lyons_2024-feedbackw1_lightlycleaned.csv")

# Study 2 (second wave)
d2 <- read_csv("./data_from_papers/Lyons_2024-feedbackw2_lightlycleaned.csv")

# merge
d <- left_join(d1, d2 %>% select(pid, starts_with("acc")), 
               by = join_by(pid))

# Code political congruence and veracity. 
# We construct following code meanings from the authors' analysis file and the survey file.
pro_democrat_headlines <- c("acc1", "acc2", "acc5", "acc6", "acc7", "acc8", 
                            "acc14" , "acc15" , "acc17" , "acc18", "acc19", 
                            "acc21", "acc25", "acc28", "acc29", "acc30", "acc31", "acc32")

true_items <- c("acc5", "acc6", "acc7", "acc8", "acc9", "acc10", "acc11", "acc12", 
                "acc17", "acc18", "acc19", "acc20", "acc21", "acc22", "acc23", "acc24",
                "acc29", "acc30", "acc31", "acc32", "acc33", "acc34", "acc35", "acc36")

# Bring to long format
d <- d %>% 
  # make id variable
  mutate(subject_id = 1:nrow(.)) %>% 
  pivot_longer(starts_with("acc"), names_to = "headline", values_to = "accuracy") %>% 
  mutate(veracity = ifelse(headline %in% true_items, "true", "false"), 
         slant = ifelse(headline %in% pro_democrat_headlines, "democrat", "republican"))

# code binary party variable, political concordance, and condition variable
d <- d %>% 
  mutate(political_party = case_when(party1 == 1 ~ "democrat", 
                                     party1 == 2 ~ "republican",
                                     party1 %in% c(3,4,5) ~ "independent/other"), 
         concordance = case_when(slant == political_party ~ "concordant", 
                                 political_party == "independent/other" ~ "political", 
                                 slant != political_party ~ "discordant"), 
         condition = ifelse(feed == 0, "control", "treatment")
  )

# We want to be able to distinguish between prior and post ratings, since (for the treatment group) the treatment happened after the first 12 items.

# From correpondance with the authors, we know that:
# "control1 means that they rated set A (a1-12) before set B (a13-24) while control2 means they rated set B before set A."
# We assume that the same endings "1" and "2" correponds to the same set of headlines for the interventions group.


# We make a variable that indicates whether a news headline has been seen prior or post to treatment. 
d <- d %>% 
  mutate(
    # make a pure numeric headline identifier
    headline = str_extract(headline, "\\d+"), 
    # make a pure numeric order identifier, 
    order_group = str_extract(feedback, "\\d+"),
    # make a variable that identifies at which point in time (pre, post, week-after) a participant saw each headline
    timing = case_when(headline %in% c(1:12) & order_group == 1 ~ "pre-treatment", 
                       headline %in% c(1:12) & order_group == 2 ~ "post-treatment", 
                       headline %in% c(13:24) & order_group == 2 ~ "pre-treatment", 
                       headline %in% c(13:24) & order_group == 1 ~ "post-treatment", 
                       headline %in% c(25:36) ~ "week-after"
    )) 

# for our study, we can include all headlines from the two control groups and the "pre-treatment" headlines from the treatment groups
# summary stats 
d %>% 
  filter(condition == "control" | timing == "pre-treatment") %>% 
  group_by(condition, timing, concordance, veracity) %>% 
  drop_na(accuracy) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(subject_id)) %>% 
  print(n = 30)

# extract a sample id
d <- d %>% 
  mutate(sample_id = case_when(
    # need to go from most to least specific
    condition == "control" & concordance == "political" ~ 2,
    condition == "treatment" & timing == "pre-treatment" & 
      concordance == "political" ~ 4,
    condition == "treatment" & timing == "pre-treatment" ~ 3,
    condition == "control" ~ 1
    # note that this is not an exhaustive list, but all the other categories 
    # are treatment ones and we don't care about them here
  )
  )

# get clean data
d <- d %>%
  filter(!is.na(sample_id)) %>% 
  mutate(id = subject_id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = sample_id,
         # we code condition as "control" since we filtered out treatments with 
         # only considering valid sample ids; the ratings from the treatment 
         # group here are pre-treatment, hence control
         condition = "control", 
         accuracy = accuracy, 
         paper_id = "Lyons_2024", 
         scale = 4) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Lyons_2024 <- d

#####

#####

#####
# Modirrousta-Galian, A., Higham, P. A., & Seabrooke, T. (2023). 
# Effects of inductive learning and gamification on news veracity discernment. 
# Journal of Experimental Psychology: Applied, 29(3), 599–619. 
# https://doi.org/10.1037/xap0000458
#####

#####
# read data from baseline (control condition)
d <- read.csv("./data_from_papers/Modirrousta-Galian_2023-baseline cleaned data.csv") 

# Weirdly, there is a participant id variable missing. In their analysis script, the authors simply do this
# "Each participant rated 36 news headlines, so each participant ID needs to be repeated 36 times in the data frame"

# Looking at the data frame, condition == gender, this seems to fit their data structure 
# (i.e. a new gender is revealed every 41 rows, with 5 items being attention checks or things other than news ratings, 
# leaving 36 ratings as stated in the article)
d <- d %>%
  mutate(id = rep(1:(nrow(.)/41), each = 41)
  )

# inspect
# from table 4 in paper, we know that baseline condition should have: 40 male, 31 female, 1 other = 72 subjects
n_distinct(d$id)

# now we reduce data to actual news ratings, which should be achieved by 
# (i) filtering `condition` to only "experiment" and
# (ii) filtering `stimulus_type` to NOT "Attention"
d <- d %>% 
  filter(condition == "experiment" & stimulus_type != "Attention")

# check
d %>% 
  group_by(id) %>% 
  summarise(headlines_per_subject = n()) %>% 
  group_by(headlines_per_subject) %>% 
  summarize(subjects = n_distinct(id))

# rename key variables
d <- d %>% 
  rename(accuracy = response, 
         headline = stimulus) %>% 
  mutate(veracity = tolower(stimulus_type))

# get summary statistics
d %>% 
  group_by(veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id), 
            n_headlines = n_distinct(headline)) 

# get clean data
d <- d %>%
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = 1,
         condition = "control", 
         accuracy = accuracy, 
         paper_id = "Modirrousta-Galian_2023_b", 
         scale = 7) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Modirrousta.Galian_2023_b <- d

#####

#####

#####
# Kreps, S. E., & Kriner, D. L. (2023). 
# Assessing misinformation recall and accuracy perceptions: Evidence from the COVID-19 pandemic. 
# Harvard Kennedy School Misinformation Review. https://doi.org/10.37016/mr-2020-123
#####

#####
# read data
d <- read_dta("./data_from_papers/Kreps_2023-kk_misinformation_data.dta")

# We know that:
# "Four blocks contained one real and one misinformation story from each of the two substantive categories 
# (i.e., origins/response and treatments). For example, the first such block contained headlines 1, 5, 12, 
# and 16 from Table A2. Each respondent was asked to evaluate two of these, chosen at random."
# So, each block contained 2 true and 2 false headlines, and participants saw two of these (and additional placebo blocks), 
# so each participant rated 4 true and 4 false headlines

# get labels
labels <- sjlabelled::get_label(d)
labels

# check labels
data_labels <- data.frame(names(d), labels)

# filter out all variables that contain "true" 
# (which are presumably accuracy ratings) to get a better overview
data_labels <- data_labels %>% 
  filter(grepl("true", names.d.))

# remove placebo names and attempt to differentiate between true and false
reduced_labels <- data_labels %>% 
  filter(!grepl("placebo", names.d.)) %>% 
  mutate(veracity = case_when(grepl("mis", names.d.) ~ "false", 
                              grepl("fact", names.d.) ~ "true",
                              grepl("fake", names.d.) ~ "false", 
                              grepl("real", names.d.) ~ "true",
                              TRUE ~ NA_character_
  ), 
  # focal candidates (based abbreviations for their thematic categories)
  # likely that we can focus on the variables starting with either "o_" (presumably for origins), 
  # or "t_" (presumably for treatment), "r_" (presumably for response) 
  focal = case_when(grepl("^t_", names.d.) | grepl("^o_", names.d.) | grepl("^r_", names.d.) ~ TRUE, 
                    TRUE ~ FALSE)
  ) 

# make a vector with candidate variables
candidates <- reduced_labels %>% 
  filter(focal == TRUE & !is.na(veracity)) %>% 
  pull(names.d.)

# Let's check the variables in the original data
# in the paper, they only talk about binary ratings, so we should focus on variables containing "true_bin"
# My hunch is that `o_fake1_true_bin` is the same as `miscon1_true_bin` or `misconm1_true_bin`
d %>% 
  select(o_fake1_true_bin, miscon1_true_bin, miscom1_true_bin) %>% 
  print(n = 100) # the first two are the same

# So `t_fake1_true_bin` is probably the same as `miscom1_true_bin`
d %>% 
  select(t_fake1_true_bin, miscom1_true_bin) %>% 
  print(n = 100) # check

# It seems relatively safe to say that our candidate variables before represent our items of interest
d <- d %>% 
  select(StartDate, ResponseId, all_of(candidates)) 

# something weird: there seem to be 25 observations per participant, although it seems the data is in wide format 
d %>% group_by(ResponseId) %>% count()

d %>% group_by(StartDate, ResponseId) %>% count()

# let's check entries for one participant only
single_respondent <- d %>% 
  filter(ResponseId == "R_3ExTZTIvRRR3409")

# after doing this for a couple of participants, it seems safe to say that they just recored the same response 25 times

# check number of unique participants
nrow(d)
n_distinct(d$ResponseId)

# we just take the mean per participant to only have on row
d <- d %>% 
  group_by(ResponseId) %>% 
  summarise(across(contains("true_bin"), ~mean(.x)))

# bring to long format
d <- d %>% 
  pivot_longer(contains("true_bin"), 
               names_to = "headline_cryptic", 
               values_to = "accuracy")

# merge with better headline identifiers from labels
d <- left_join(d, 
               reduced_labels %>% 
                 rename(headline_cryptic = names.d.,
                        headline = labels) %>% 
                 select(headline_cryptic, headline, veracity)) %>% 
  rename(id = ResponseId) %>% 
  arrange(id)

# check how many ratings per participant
d %>%  
  group_by(id) %>% 
  summarise(sum(is.na(accuracy)))
# correponds to the 8 ratings that we deduced from the descriptions in the paper

# get summary statistics
d %>% 
  group_by(veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id), 
            n_headlines = n_distinct(headline)) 

# get clean data
d <- d %>%
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = 1,
         condition = "control", 
         accuracy = accuracy, 
         paper_id = "Kreps_2023", 
         scale = "binary") %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Kreps_2023 <- d

#####

#####

#####
# Stagnaro, M., Pink, S., Rand, D. G., & Willer, R. (2023). 
# Increasing accuracy motivations using moral reframing does not reduce Republicans’ belief in false news. 
# Harvard Kennedy School Misinformation Review. 
# https://doi.org/10.37016/mr-2020-128
#####

#####
# read data
d <- read.csv("./data_from_papers/Stagnaro_2023-HKS Misinformation review_Framing Accuracy as Conservative Political Identity_usingTemplet_data_20231005.csv") 

# no documentation or analysis script
# data appears to be in longformat; rename what appear to be the key variables

d <- d %>% 
  rename(id = sub,
         headline = Headline,
         accuracy = Rating,
         condition = exp_con
  )

# check subjects and headlines per subject
n_distinct(d$id)
# matches their paper

# headline ratings per participant
d %>% 
  group_by(id) %>% 
  summarize(ratings_per_participant = sum(!is.na(accuracy))) %>% 
  group_by(ratings_per_participant) %>% 
  summarise(subjects = n_distinct(id))

# Vast majority rated 20, which matches their report
# One individual rated 101 - they probably forgot to exclude some pilot run they did themselves. 

# We exclude that participant

# identify participant
invalid_participant <- d %>% 
  group_by(id) %>% 
  summarize(ratings_per_participant = sum(!is.na(accuracy))) %>% 
  filter(ratings_per_participant > 20) %>% 
  pull(id)

# remove participant
d <- d %>% 
  filter(id != invalid_participant)

# We need a veracity variable. Probably, the `Real` and `Fake` variables are the same
d %>% 
  select(Real, Fake) %>% 
  mutate(identical = ifelse(Real == Fake, TRUE, FALSE)) %>% 
  group_by(identical) %>% 
  count()

# they are the same. Rename one of them veracity
d <- d %>% 
  rename(veracity = Real)

# Weirdly, there seem to be 101, and not 100 headlines as they report. Are there valid observations for all headlines ? 
d %>% 
  group_by(headline) %>% 
  summarize(ratings_per_headline = sum(!is.na(accuracy))) %>% 
  print(n = 101)

# headline 101 did not receive any valid rating - exclude
d <- d %>% 
  filter(headline != 101)


# get summary statistics
d %>% 
  group_by(condition, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id), 
            n_headlines = n_distinct(headline)) 

# Annoyingly, their accuracy measure seems to be not coded as they write in the paper 
# (from 1 to 4), but from 0 to three
table(d$accuracy, useNA = "always")

d <- d %>% 
  # bring on a scale from 1 to 4
  mutate(accuracy = 1 + accuracy)


# get clean data
d <- d %>%
  mutate(id = id, 
         veracity = ifelse(veracity == "Real", "true", "fake"),
         sample_id = 1,
         condition = "control", 
         accuracy = accuracy, 
         paper_id = "Stagnaro_2023", 
         scale = 4) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Stagnaro_2023 <- d

table(d$veracity)
  

#####

#####

#####
# Guess, A., McGregor, S., Pennycook, G., & Rand, D. (2024). 
# Unbundling Digital Media Literacy Tips: Results from Two Experiments. 
# OSF. https://doi.org/10.31234/osf.io/u34fp
#####

#####
# read data from study 1 
d <- read_csv("./data_from_papers/Guess_2024-clean_acc_headline_resplevel.csv") 

# check sample size
# total sample size reported in paper is N = 4,926, 
# but that is sharing and accuracy condition combined - we should thus expect roughly half
n_distinct(d$caseid)

# recode key variables
d <- d %>% 
  rename(id = caseid, 
         accuracy = belief_accuracy) %>% 
  mutate(condition = ifelse(treated == 0, "control", "treatment"), 
         veracity = ifelse(type == FALSE, "false", "true"), 
         concordance = ifelse(congenial == 0, "discordant", "concordant"), 
  )

# how many participants in control group
d %>% 
  group_by(condition, party, concordance) %>% 
  summarise(n_subj = n_distinct(id)) 

# note that there are more participants in the discordant than in the concordant group,
# because all non-republican and non-democrat participants have always been counted as "discordant"

# we will change that measure concordance, and we'll code NA instead for those participants
# who are neither democrat nor republican, as we did for other studies

d <- d %>% 
  mutate(concordance = ifelse(grepl("Republican", party) | grepl("Democrat", party), 
                              concordance, NA), 
  ) 

# get summary statistics
d %>% 
  group_by(condition, concordance, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id), 
            n_headlines = n_distinct(headline)) 

# extract a sample id
d <- d %>% 
  mutate(sample_id = case_when(
    # need to go from most to least specific
    condition == "control" & !is.na(concordance) ~ 1,
    condition == "control" & is.na(concordance) ~ 2
    # note that this is not an exhaustive list, but all the other categories 
    # are treatment ones and we don't care about them here
  )
  )

# get clean data
d1 <- d %>%
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = sample_id,
         condition = condition, 
         accuracy = accuracy, 
         paper_id = "Guess_2024", 
         scale = 6) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

# read data from study 2
d <- read_csv("./data_from_papers/Guess_2024-share_hl_resplevel_exp2.csv")

# check sample size
# total sample size reported in paper is N = 2,524 (of which ~20% in the control group), 
# but that is sharing and accuracy condition combined - we should thus expect roughly half
n_distinct(d$caseid)

# recode key variables
d <- d %>% 
  rename(id = caseid, 
         accuracy = belief_accuracy) %>% 
  mutate(condition = ifelse(treated == 0, "control", "treatment"), 
         veracity = ifelse(type == FALSE, "false", "true"), 
         concordance = ifelse(congenial == 0, "discordant", "concordant"), 
  )

# how many participants in control group
d %>% 
  group_by(condition) %>% 
  summarise(n_subj = n_distinct(id)) 

# note that there are more participants in the discordant than in the concordant group,
# because all non-republican and non-democrat participants have always been counted as "discordant"
d %>% 
  group_by(condition, party, concordance) %>% 
  summarise(n_subj = n_distinct(id)) 

# we will change that measure concordance, and we'll code NA instead for those participants
# who are neither democrat nor republican, as we did for other studies
d <- d %>% 
  mutate(concordance = ifelse(grepl("Republican", party) | grepl("Democrat", party), 
                              concordance, NA), 
  ) 

# how many headlines per participant? 
d %>% 
  group_by(id) %>% 
  summarise(n_headlines = n_distinct(headline)) 

# 60 is the newspool, but there should only be 20 valid observations
d %>% 
  group_by(id) %>% 
  summarise(valid_n = sum(!is.na(accuracy))) %>% 
  group_by(valid_n) %>% 
  summarise(n_participants = n_distinct(id))

# most people have 20 observations, which is what is reported in the paper. 
# But but there are also quite a few people with more than that, namely 60 
# observations
ids_with_60_answers <- d %>% 
  group_by(id) %>% 
  summarise(valid_n = sum(!is.na(accuracy))) %>% 
  filter(valid_n == 60) %>% 
  pull(id)

# extract a sample id
d <- d %>% 
  mutate(
    # prior, indicate who rated 60 items and who not
    n_60 = ifelse(id %in% ids_with_60_answers, TRUE, FALSE),
    sample_id = case_when(
    # need to go from most to least specific
    n_60 == FALSE & condition == "control" & !is.na(concordance) ~ 3,
    n_60 == FALSE & condition == "control" & is.na(concordance) ~ 4,
    n_60 == TRUE & condition == "control" & !is.na(concordance) ~ 5,
    n_60 == TRUE & condition == "control" & is.na(concordance) ~ 6,
    # note that this is not an exhaustive list, but all the other categories 
    # are treatment ones and we don't care about them here
  )
  )

# check
d %>% 
  group_by(sample_id) %>% 
  summarise(n_participants = n_distinct(id))

# get clean data
d2 <- d %>%
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = sample_id,
         condition = condition, 
         accuracy = accuracy, 
         paper_id = "Guess_2024", 
         scale = 6) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Guess_2024 <- rbind(d1, d2)

#####

#####

#####
# Lyons, B., King, A. J., & Kaphingst, K. (2024). 
# A health media literacy intervention increases skepticism of both inaccurate 
# and accurate cancer news among U.S. adults. 
# https://doi.org/10.31219/osf.io/hm9ty
#####

#####
# read data
d <- read_dta("./data_from_papers/Lyons_2024_b-boast.dta") 

# it seems that variables containing "assign" identify which headlines a participant has seen, 
# with "1" probably coding "seen", and "9" probably coding "not seen" 

# from their analysis state code, it seems that variables containing "fake"/"true" 
# and "p1" code accuracy (while "p2" variables code sharing - 
# this seems to match with variable "prompt_assignment"); this is confirmed by their codebook

# from the codebook, we also know that the `Q20_21_split` is the treatment indicator variable
# with code `1` meaning no treatment, i.e. control

# restrict to key variables and bring to long format
d <- d %>% 
  select(caseid, q20_21_split, contains("p1")) %>% 
  pivot_longer(contains("p1"), 
               names_to = "headline", 
               values_to = "accuracy"
  ) %>% 
  # add veracity and recode accuracy variable (from codebook we know that (1 = true and 2 = False))
  mutate(veracity = ifelse(grepl("fake", headline), "false", "true"), 
         # accuracy NA's are coded as `999` but also as 998 - change that to NA
         accuracy = ifelse(accuracy > 4, NA, accuracy), 
         condition = ifelse(q20_21_split == 1, "control", "treatment")
  ) %>% 
  # remove participants from the sharing condition
  drop_na(accuracy) %>% 
  rename(id = caseid) %>% 
  select(-q20_21_split)

# some checks
d %>% 
  group_by(condition, id, veracity) %>% 
  summarise(n_news = n()) %>% 
  group_by(condition, veracity, n_news) %>% 
  summarise(n_participants = n_distinct(id))

# get summary statistics 
d %>% 
  group_by(condition, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id), 
            n_headlines = n_distinct(headline)) 

# get clean data
d <- d %>%
  mutate(id = id, 
         veracity = ifelse(veracity == "false", "fake", veracity),
         sample_id = 1,
         condition = condition, 
         accuracy = accuracy, 
         paper_id = "Lyons_2024_b", 
         scale = 4) %>% 
  select(c(id, veracity, condition, accuracy, sample_id, paper_id, scale))

Lyons_2024_b <- d


#####
# Make composite data frame 
# Combine all data and add a unique sample id and a unique participant id
d <- rbind(Bago_2020, Bago_2022, Chen_2021, Rathje_2023, Sultan_2022, 
           Ehrlich_2023, Altay_2023, Luo_2022, Hoes.Altay_2023, Altay.Gilardi_2023,
           Mairal_2023, Gawronski_2023, Garrett_2021, Pereira_2023, Epstein_2023, 
           Aslett_2024, Allen_2021, Lyons_2024, Modirrousta.Galian_2023_b, Kreps_2023, 
           Stagnaro_2023, Guess_2024, Lyons_2024_b) %>% 
  # for now, samples are coded within papers (i.e. all papers have a sample
  # simply called `1`). To uniquely identify a sample with just one variable, 
  # we build `unique_sample_id`
  mutate(unique_sample_id = paste(paper_id, sample_id, sep = "_"),
         # for now we can identify unique participants within samples; 
         # the coding strategy varies between samples; 
         # the following variable makes sure that across all papers, 
         # we can identify each unique participant
         unique_participant_id = paste(paper_id, sample_id, id, sep = "_"),
         # identify unique observations
         observation_id = 1:nrow(.)
         ) %>% 
  # remove NA's for accuracy
  drop_na(accuracy) %>% 
  # remove NA's for veracity 
  # (one study, Allen_2021, has non-established veracity for some items)
  drop_na(veracity)


# Calculate `error` variable
# Error is derived from the `accuracy` ratings. 
# The way error is calculated depends on `veracity` and `scale` 

# first, we need a numeric version of scale
d <- d %>% 
  mutate(
    # make numeric version of scale
    # (this will turn "binary" values to NA)
    scale_numeric = as.numeric(scale)
    )

# then we can calculate the error
d <- d %>% 
  mutate(error = case_when(veracity == "true" ~ ifelse(scale == "binary", 
                                                       # for binary scales, the maximum is 1
                                                       1 - accuracy, 
                                                       # for continuous scales, the maximum is the value of
                                                       # `accuracy_scale_numeric`
                                                       scale_numeric - accuracy
                                                       ), 
                           veracity == "fake" ~ ifelse(scale == "binary", 
                                                       # for binary scales, the minimum is 0
                                                       accuracy - 0,
                                                       # for continuous scales, the minimum is 1
                                                       accuracy - 1)
                           )
         )

# for binary outcomes, we add a variable called `correct`. 
# this variable takes on the value of accuracy for true news and 1-accuracy 
# for fake news
d <- d %>% 
  mutate(correct = case_when(
    scale == "binary" ~ ifelse(veracity == "fake", 
                               # for fake news "accurate" is the wrong response
                               1-accuracy, 
                               # for true news "accurate" is correct response
                               accuracy)
    )
    )

# calculate correlation by sample
#####
# calculate the average correlation between fake news accuracy ratings and true news accuracy ratings for each sample.
correlations_by_sample <- d %>%
  # step 1: for each participant, calculate means of fake and true
  group_by(paper_id, sample_id, id, veracity, condition, scale) %>% 
  summarize(mean_accuracy = mean(accuracy)) %>% 
  pivot_wider(names_from = veracity, values_from = mean_accuracy, names_prefix = "accuracy_") %>% 
  # there is one NA value in Bago_2022 that we need to remove
  drop_na(accuracy_true, accuracy_fake) %>% 
  # step 2: for each sample, calculate correlations of means of fake and true
  group_by(paper_id, sample_id, condition, scale) %>% 
  summarize(r = cor(accuracy_fake, accuracy_true)) 

# step 3: get average by-sample correlation
average_correlation_by_sample <- correlations_by_sample %>% 
  ungroup() %>% 
  summarize(average_r = mean(r, na.rm=TRUE)) %>% 
  pull(average_r)
average_correlation_by_sample

ggplot(correlations_by_sample, aes(x = r)) + 
  geom_histogram() +
  geom_vline(xintercept = average_correlation_by_sample, colour = "red")
#####

# Export data
######
# individual level data
write_csv(d, "data/individual_level_subset.csv")

# all different correlations of studies
write_csv(correlations_by_sample, "data/correlations_by_sample.csv")
#####

# Check data
# subset <- read_csv("./individual_level_subset.csv")
# 
# 
# subset %>% group_by(paper_id) %>% 
#   summarize(studies_in_paper = max(sample_id)) %>% 
#   mutate(total_studies = sum(studies_in_paper))
# 
# subset %>% group_by(paper_id, condition) %>% 
#   summarize(studies_in_paper_by_condition = max(sample_id))
# Check plausibility:
# d %>% group_by(veracity, condition, paper_id, unique_sample_id) %>% 
#   summarize(mean_accuracy = mean(accuracy, na.rm=T), n = n_distinct(id)) %>% print(n=100)

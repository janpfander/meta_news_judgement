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
# Make composite data frame 
# Combine all data and add a unique sample id and a unique participant id
d <- rbind(Bago_2020, Bago_2022, Chen_2021, Rathje_2023, Sultan_2022, 
           Ehrlich_2023, Altay_2023, Luo_2022, Hoes.Altay_2023, Altay.Gilardi_2023,
           Mairal_2023, Gawronski_2023, Garrett_2021) %>% 
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
         )


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
  summarize(average_r = mean(r)) %>% 
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

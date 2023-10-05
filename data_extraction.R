# Extraction of descriptive data for Fake news review paper

library(tidyverse)     # create plots with ggplot, manipulate data, etc.
library(modelsummary)  # combine multiple regression models into a single table
library(readxl)        # read excel files
library(rstatix)       # get_summary_stats function
library(haven)        # read SPSS files

# Bago, B., Rosenzweig, L. R., Berinsky, A. J., & Rand, D. G. (2022). 
# Emotion may predict susceptibility to fake news but emotion regulation does not seem to help. 
# Cognition and Emotion, 1–15. https://doi.org/10.1080/02699931.2022.2090318


## Study 1

d <- read_csv("./data_from_papers/study1.csv")
head(d)

d <- d %>% 
  mutate(emotion = ifelse(anger == 1 | disgust == 1 | fear == 1 | happy == 1 | sad == 1 |
                            surprise == 1, "emotion", "no_emotion" )) 

# verify if coding was correct 
d %>% 
  select(emotion, anger, disgust, fear, happy, sad, surprise)

d %>%  group_by(emotion, concordancy, reality) %>% 
  summarise(mean_accuracy = mean(perceived_accuracy))

d %>%  group_by(emotion, concordancy, reality) %>%
  get_summary_stats(perceived_accuracy, type = "mean_sd")

## Study 2

d2 <- read_csv("./data_from_papers/study2.csv")
head(d2)

d2 %>%  group_by(concordancy, reality, condition) %>%
  get_summary_stats(perceived_accu, type = "mean_sd")

## Study 3

d3 <- read_csv("./data_from_papers/study3.csv")
head(d3)

d3 %>%  group_by(concordancy, reality, condition) %>%
  get_summary_stats(perceived_accu, type = "mean_sd")

## Study 4

d4 <- read_csv("./data_from_papers/study4.csv")
head(d4)

d4 %>%  group_by(concordancy, reality, condition) %>%
  get_summary_stats(perceived_accu, type = "mean_sd")

# Brashier, N. M., Pennycook, G., Berinsky, A. J., & Rand, D. G. (2021). 
# Timing matters when correcting fake news. 
# Proceedings of the National Academy of Sciences, 118(5), 
# e2020043118. https://doi.org/10.1073/pnas.2020043118

## Study 1

d <- read_excel("./data_from_papers/Experiment 1 Data.xlsx", 
                                sheet = "Means")

# get first wave results
d %>%  group_by(Condition) %>%
  get_summary_stats("False - Initial", "True - Initial", type = "mean_sd")

# get second wave results
d %>%  group_by(Condition) %>%
  get_summary_stats("False - Final", "True - Final", type = "mean_sd")

## Study 2

d2 <- read_excel("./data_from_papers/Experiment 2 Data.xlsx", 
                sheet = "Means")

# get first wave results
d2 %>%  group_by(Condition) %>%
  get_summary_stats("False - Initial", "True - Initial", type = "mean_sd")

# get second wave results
d2 %>%  group_by(Condition) %>%
  get_summary_stats("False - Final", "True - Final", type = "mean_sd")


# Bago, B., Rand, D. G., & Pennycook, G. (2020). Fake news, fast and slow: 
# Deliberation reduces belief in false (but not true) news headlines. 
# Journal of Experimental Psychology: General, 149(8), 1608–1613. 
# https://doi.org/10.1037/xge0000729

## Study 1 (politically neutral baseline; single response)
d <- read_csv("./data_from_papers/study_1_final_only.csv")
head(d)

d %>%  group_by(reality) %>%
  get_summary_stats(perceived_accu, type = "mean_sd")

## Study 1 (politically neutral baseline; double response)
d2 <- read_csv("./data_from_papers/study_1_two_response.csv")
head(d2)

d2 %>%  group_by(reality, response_number, ) %>%
  get_summary_stats(perceived_accu, type = "mean_sd")

## Study 2 (single response)
d3 <- read_csv("./data_from_papers/study_2_final_only.csv")
head(d3)

d3 %>%  group_by(reality, concordancy) %>%
  get_summary_stats(perceived_accu, type = "mean_sd")

## Study 2 (double response)
d4 <- read_csv("./data_from_papers/study_2_two_response.csv")
head(d4)

d4 %>%  group_by(reality, concordancy, response_number) %>%
  get_summary_stats(perceived_accu, type = "mean_sd")

# Calvillo, D. P., & Smelter, T. J. (2020). An initial accuracy focus reduces 
# the effect of prior exposure on perceived accuracy of news headlines. 
# Cognitive Research: Principles and Implications, 5(1), 55. 
# https://doi.org/10.1186/s41235-020-00257-y

# The authors provide means but no SDs
# We could recover SD's from conficence intervals

# calculate SD from 95% confidence intervals
# source: https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm

SD_from_95_CI <- function(lower_CI, upper_CI, sample_size) {
  SD = sqrt(sample_size)*(upper_CI - lower_CI)/3.92
  return(SD)
}

SD_from_95_CI(lower_CI = 2.73, upper_CI = 2.85, sample_size = 194)

# However, we can also check the raw data directly for SDs (more precise). 

# Study 1
d <- read_sav("./data_from_papers/initial ratings exp 1 data.sav")
d %>% arrange(age)

# Authors do not provide a codebook 
# the `fake...`/ `real...` columns seem to be the accuracy ratings
# the `...Prior`/ `...NoPrior` addition seems to indicate whether 
# headlines have been seen before or not+
# it's unclear what the `prior` column captures (perhaps which of two sets of 16
# a participant was familiarized with)
# `rating` column seems to indicate whether for the previous set, participants
# were asked to rate accuracy/truthfulness or interest
# We have to confirm this with comparing the output to table 1 from paper
d %>% 
  group_by(rating) %>% 
  summarise(across(fakePrior:realNoPrior, mean)) # matches table 1

# do some re-coding to get a more clear output table
d %>% 
  mutate (id = 1:nrow(.)) %>% 
  pivot_longer(fakePrior:realNoPrior, names_to = "veracity_familiar", 
               values_to = "accuracy") %>% 
  separate_wider_position(veracity_familiar, c(veracity = 4, familiar = 7), 
                          too_few = "align_start") %>% 
  group_by(rating, familiar, veracity) %>%
  summarise(across(accuracy, list(mean = mean, sd = sd)), 
            n = n())

# Study 2
d <- read_sav("./data_from_papers/initial ratings exp 2 data.sav")

# again, we have to figure out what means what - so annoying.
# the `...Prior`/ `...NPrior` columns seems to indicate accuracy ratings 
# (according to whether headlines have been seen before or not); 
# From the meta-data we know that within these columns, `F` = false, `T` = true, 
# `R` = republican, `D` = democrat. We assume the latter to mean `pro` respective party. 
# It's unclear what the `prior` column captures (perhaps which of two sets of 16
# a participant was familiarized with)
# `rating` column seems to indicate again whether for the previous set, participants
# were asked to rate accuracy/truthfulness or interest
# Crucially, we need to know each participant's political leaning. 
# We have 2 candidate columns that seem to encode this:
# `political_ideology` and `political_party` - but since both are numeric, 
# we don't know what they corresponds to. 
summary(d$pol_ideology )
table(d$pol_party) 

# do some re-coding 
d <- d %>% 
  mutate (id = 1:nrow(.)) %>% 
  pivot_longer(TDPrior:FRNPrior, names_to = "veracity_political_familiar", 
               values_to = "accuracy") %>% 
  separate_wider_position(veracity_political_familiar, c(veracity = 1, political = 1, familiar = 6), 
                          too_few = "align_start") %>%
  mutate(
    # recode veracity values
    veracity = ifelse(veracity == "T", "true", "fake")) %>% 
  arrange(id)

# We have to test this with comparing to the information given in the paper:
# "the effect of political concordance was greater with false headlines 
# (concordant M = 2.50; discordant: M = 1.88) than it was with true headlines 
# (concordant M = 3.05; discordant: M = 2.56)."

# For `pol_ideology` test two versions:
# (a) 1:3 = democrat, 4 is neutral, and 5:7 = republican
# (b) 1:3 = republican, 4 is neutral, and 5:7 = democrat
# both versions fail to replicate the above (see below)

# check `political_ideology`
# political_ideology_check <- d %>% 
#   mutate(    
#     # build concordance variable version (a)
#     concordance_a = case_when(pol_ideology > 3 & political == "R" ~ "concordant", 
#                               pol_ideology < 3 & political == "D" ~ "concordant",
#                               pol_ideology == 4 ~ NA_character_, 
#                               TRUE ~ "discordant"), 
#     # build concordance variable version (b)
#     concordance_b = case_when(pol_ideology > 3 & political == "D" ~ "concordant", 
#                               pol_ideology < 3 & political == "R" ~ "concordant",
#                               pol_ideology == 4 ~ NA_character_, 
#                               TRUE ~ "discordant")
#   ) %>% 
#   select(id, veracity, political, familiar, accuracy, pol_ideology , concordance_a, 
#          concordance_b) 
# 
# # version (a): try ro replicate the means given in paper
# political_ideology_check %>% 
#   group_by(concordance_a, veracity) %>% 
#   summarise(across(accuracy, list(mean = mean, sd = sd)), 
#             n = n()/4)
# 
# # version (b): try ro replicate the means given in paper
# political_ideology_check %>% 
#   group_by(concordance_b, veracity) %>% 
#   summarise(across(accuracy, list(mean = mean, sd = sd)), 
#             n = n()/4)

# For `pol_party` it seems that
# 1 = democrat, 2 = republican, 3 = neither
d$pol_party
# add concordance variable based on `pol_party`
d <- d %>% 
  mutate(    
    # build concordance variable
    concordance = case_when(pol_party == 1 & political == "D" ~ "concordant", 
                              pol_party == 2 & political == "R" ~ "concordant",
                              pol_party  == 3 ~ NA_character_, 
                              TRUE ~ "discordant")
  ) 

# check
d  %>% 
  select(id, veracity, political, familiar, accuracy, pol_party, concordance) 

# check values
d  %>% group_by(concordance, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd)), 
            n = n()/4) # matches

# now we can calculate summary stats
d  %>%  
  group_by(rating, familiar, veracity, concordance) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd)), 
            n = n_distinct(id)
            ) %>% 
  print(n = 30)

# Badrinathan, S. (2021). Educative Interventions to Combat Misinformation: 
# Evidence from a Field Experiment in India. 
# American Political Science Review, 115(4), 1325–1341. 
# https://doi.org/10.1017/S0003055421000459

## average belief in fake news (figure 3 paper)
(1.88+ 3.27 + 3.84 + 3.92 + 6.13 + 9.07 + 16.01 + 17.24 + 25.9 + 29.82 + 44.28
  + 47.55)/12 # = 17.4

## average belief in true news (appendix H)

(76 + 95)/2 # 85.5

# Pennycook, G., Binnendyk, J., Newton, C., & Rand, D. G. (2021). 
# A Practical Guide to Doing Behavioral Research on Fake News and Misinformation. 
# Collabra: Psychology, 7(1), 25293. https://doi.org/10.1525/collabra.25293

d1 <- read_csv("./data_from_papers/Pennycook et al. individual level data.csv")
head(d1)

d2 <- read_excel("./data_from_papers/Pennycook et al. item level data.xlsx")

## our variable of interest are combined (democrat & republican respondents) 
## ratings of Likelihood. The corresponding variable is '...13' 
d2$likelihood_ratings <- as.numeric(d2$...13)

## There are two news types: covid and political. The according variable is
## 'Category'. We want stats for true news and fake news likelihood ratings
## separately for those.
d2 %>% 
  group_by(Category) %>%  
  get_summary_stats(likelihood_ratings, type = "mean_sd")

# Maertens, R., Götz, F. M., Schneider, C. R., Roozenbeek, J., Kerr, J. R., 
# Stieger, S., McClanahan, W. P., Drabot, K., & Linden, S. van der. (2021). 
# The Misinformation Susceptibility Test (MIST): A psychometrically validated 
# measure of news veracity discernment [Preprint]. 
# PsyArXiv. https://doi.org/10.31234/osf.io/gk68h

## Scores range form 0 to 10. To have them range from 0 to 1 instead, we divide
## all scores by 10. Fake news scores represent fake news correctly identified 
## as false. 
## However, we want fake news falsely identified as true. We thus take 1-'f' 
## (the fake news score) for fake news

## Supplement S15 norm table US (general, average over 20-item scale)
mean_true = 6.87/10 
SD_true = 2.31/10

mean_fake = 1-7.17/10 
SD_fake = 2.47/10

## Supplement S15 norm table UK (general, average over 20-item scale)
mean_true = 5.91/10 
SD_true = 2.69/10

mean_fake = 1-7.38/10 
SD_fake = 2.43/10

# Chen, C. X., Pennycook, G., & Rand, D. G. (2021). What Makes News Sharable on 
# Social Media? [Preprint]. PsyArXiv. https://doi.org/10.31234/osf.io/gzqcd

## Study 1
d <- read_csv("./data_from_papers/S1_data_headline_level.csv")
head(d)

d %>%  group_by(news_type) %>%
  get_summary_stats(true, type = "mean_sd")

## Study 2
d2 <- read_csv("./data_from_papers/S2_data_headline_level.csv")
head(d)

d2 %>%  group_by(news_type) %>%
  get_summary_stats(true, type = "mean_sd")

# Basol, M., Roozenbeek, J., Berriche, M., Uenal, F., McClanahan, W. P., & 
# Linden, S. van der. (2021). Towards psychological herd immunity: 
# Cross-cultural evidence for two prebunking interventions against 
# COVID-19 misinformation. Big Data & Society, 8(1), 205395172110138.
# https://doi.org/10.1177/20539517211013868

## Study 1
d <- read_excel("./data_from_papers/Study 1 - final.xlsx")
names(d)

# scale is upside-down, i.e. 7 = highly manipulative
# we want this to be the other way around so we rebuild the variables
d$real_pre = 8 - d$`Real-Pre`
d$real_post = 8 - d$`Real-Post`
d$fake_pre = 8 - d$`Fake-Pre`
d$fake_post = 8 - d$`Fake-Post`

# check re-coding
d[1-100, c("real_pre", "Real-Pre")]
d[1-100, c("real_post", "Real-Post")]  
d[1-100, c("fake_pre", "Fake-Pre")]
d[1-100, c("fake_post", "Fake-Post")]

# get pre results
d %>% get_summary_stats(real_pre, fake_pre, type = "mean_sd")

# get post results
d %>% get_summary_stats(real_post, fake_post, type = "mean_sd")
  
## Study 2
d2 <- read_excel("./data_from_papers/Study 2 - final.xlsx")
names(d2)

# scale is upside-down, i.e. 7 = highly manipulative
# we want this to be the other way around so we rebuild the variables
d2$real_pre = 8 - d2$`Real-Avg-Manipulativeness-Pre`
d2$real_post = 8 - d2$`Real-Avg-Manipulativeness-Post`
d2$fake_pre = 8 - d2$`Fake-Avg-Manipulativeness-Pre`
d2$fake_post = 8 - d2$`Fake-Avg-Manipulativeness-Post`

# check re-coding
d2[1-100, c("real_pre", "Real-Avg-Manipulativeness-Pre")]
d2[1-100, c("real_post", "Real-Avg-Manipulativeness-Post")]  
d2[1-100, c("fake_pre", "Fake-Avg-Manipulativeness-Pre")]
d2[1-100, c("fake_post", "Fake-Avg-Manipulativeness-Post")]

# get pre results
d2 %>% group_by(Language, Condition) %>% 
  get_summary_stats(real_pre, fake_pre, type = "mean_sd")

# get post results
d2 %>% group_by(Language, Condition) %>% 
  get_summary_stats(real_post, fake_post, type = "mean_sd")

# get UK follow up results
d2$real_followup = 8 - d2$`Real-Avg-Manipulativeness-FollowUp`
d2$fake_followup = 8 - d2$`Fake-Avg-Manipulativeness-FollowUp`

# check re-coding
d2[1-100, c("real_followup", "Real-Avg-Manipulativeness-FollowUp")]
d2[1-100, c("fake_followup", "Fake-Avg-Manipulativeness-FollowUp")]

d2 %>% group_by(Language, Condition) %>% 
  get_summary_stats(real_followup, fake_followup, type = "mean_sd")

# Sultan, M., Tump, A. N., Geers, M., Lorenz-Spreen, P., Herzog, S., & 
# Kurvers, R. (2022). Time Pressure Reduces Misinformation Discrimination 
# Ability But Not Response Bias. 
# PsyArXiv. https://doi.org/10.31234/osf.io/brn5s

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

# get summary stats according to pol. congruency
dat %>% group_by(condition, item_accuracy, congruency) %>% 
  get_summary_stats(accuracy_ratings, type = "mean_sd")


# Rathje, S., Bavel, J. V., & Linden, S. van der. (2022). Accuracy and Social 
# Incentives Shape Belief in (Mis)Information [Preprint]. 
# In Review. https://doi.org/10.21203/rs.3.rs-1293101/v1

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

#First, use their accuracy measures for comparison
#Binary Accuracy Scores
data$DT1Bin <- ifelse(data$AccuracyDT1 > 3, 1, 0)
data$DT2Bin <- ifelse(data$AccuracyDT2 > 3, 1, 0)
data$DT3Bin <- ifelse(data$AccuracyDT3 > 3, 1, 0)
data$DT4Bin <- ifelse(data$AccuracyDT4 > 3, 1, 0)
data$RT1Bin <- ifelse(data$AccuracyRT1 > 3, 1, 0)
data$RT2Bin <- ifelse(data$AccuracyRT2 > 3, 1, 0)
data$RT3Bin <- ifelse(data$AccuracyRT3 > 3, 1, 0)
data$RT4Bin <- ifelse(data$AccuracyRT4 > 3, 1, 0)
data$DF1Bin <- ifelse(data$AccuracyDF1 > 3, 1, 0)
data$DF2Bin <- ifelse(data$AccuracyDF2 > 3, 1, 0)
data$DF3Bin <- ifelse(data$AccuracyDF3 > 3, 1, 0)
data$DF4Bin <- ifelse(data$AccuracyDF4 > 3, 1, 0)
data$RF1Bin <- ifelse(data$AccuracyRF1 > 3, 1, 0)
data$RF2Bin <- ifelse(data$AccuracyRF2 > 3, 1, 0)
data$RF3Bin <- ifelse(data$AccuracyRF3 > 3, 1, 0)
data$RF4Bin <- ifelse(data$AccuracyRF4 > 3, 1, 0)

# here we differ from their measure (we take an average score of the binary 
# accuracy variable as is typically done)
data$TrueBin <-     (data$DT1Bin +
  data$DT2Bin + 
  data$DT3Bin +
  data$DT4Bin +
  data$RT1Bin +
  data$RT2Bin + 
  data$RT3Bin +
  data$RT4Bin)/8

data$FalseBin <-    (data$DF1Bin +
  data$DF2Bin + 
  data$DF3Bin +
  data$DF4Bin +
  data$RF1Bin +
  data$RF2Bin + 
  data$RF3Bin +
  data$RF4Bin)/8

# check whether sample size corresponds to those reported in paper
nrow(data)

# get summary stats (attention, 0 = experimental, 1 = control)
data %>% group_by(Condition) %>% 
  get_summary_stats(FalseBin, TrueBin, type = "mean_sd")

# HERE WE DIFFER FROM THEIR ANALYSIS
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

# check if results are the same as the authors binary measures
data_long %>% group_by(Condition, news_type) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")
# note that we achieve the same results, but larger standard errors (which makes
# sense because our SD consider the whole data, which should match other standard
# deviations we coded)

# check whether we achieved the same final sample size
# (should be 228 experimental group and 234 Control group)
data_long %>% 
  group_by(ID, Condition) %>% 
  summarise(n = n()) %>% 
  group_by(Condition) %>% 
  summarise(n = n()) 

# now, Binary accuracy grouped by political concordance
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")

# we want to primarily report the original 6-scale measure though
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(ratings, type = "mean_sd")

# quick visualization
ggplot(data_long, aes(x = as.factor(ratings))) +
  geom_bar() +
  facet_wrap(~ news_type)

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

#Recode Condition Variable (0 = Experimental, 1 = Control)
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

# check if results are the same as the authors binary measures
data_long %>% group_by(Condition, news_type) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")
# note that we achieve the same results, but larger standard errors (which makes
# sense because our SD consider the whole data, which should match other standard
# deviations we coded)

# check sample size and whether it adds up to 998 (reported overall sample size)
data_long %>% 
  group_by(ID, Condition) %>% 
  summarise(n = n()) %>% 
  group_by(Condition) %>% 
  summarise(n = n()) 

# now, Binary accuracy grouped by political concordance
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")

# we want to primarily report the original 6-scale measure though
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(ratings, type = "mean_sd")

# quick visualization
ggplot(data_long, aes(x = as.factor(ratings))) +
  geom_bar() +
  facet_wrap(~ news_type)

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

# check if results are the same as the authors binary measures
data_long %>% group_by(Condition, news_type) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")
# note that we achieve the same results, but larger standard errors (which makes
# sense because our SD consider the whole data, which should match other standard
# deviations we coded)

# check sample size and whether it adds up to 998 (reported overall sample size)
data_long %>% 
  group_by(ID, Condition) %>% 
  summarise(n = n()) %>% 
  group_by(Condition) %>% 
  summarise(n = n()) 

# check how many news items there were
data_long %>% 
  group_by(ID, Condition, news_type, congruence) %>% 
  summarise(n = n()) # each individual saw 6 items per condition (24 in total)

# now, Binary accuracy grouped by political concordance
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")

# we want to primarily report the original 6-scale measure though
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(ratings, type = "mean_sd")

# quick visualization
ggplot(data_long, aes(x = as.factor(ratings))) +
  geom_bar() +
  facet_wrap(~ news_type)

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

# check results without taking congruence into account
data_long %>% group_by(Condition, news_type) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")


# check sample size by condition
data_long %>% 
  group_by(ID, Condition) %>% 
  summarise(n = n()) %>% 
  group_by(Condition) %>% 
  summarise(n = n()) 

# check how many news items there were
data_long %>% 
  group_by(ID, Condition, news_type, congruence) %>% 
  summarise(n = n()) # each individual saw 6 items per condition (24 in total)

# now, Binary accuracy grouped by political concordance
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(binary_rating, type = "mean_sd")

# we want to primarily report the original 6-scale measure though
data_long %>% group_by(Condition, news_type, congruence) %>% 
  get_summary_stats(ratings, type = "mean_sd")

# quick visualization
ggplot(data_long, aes(x = as.factor(ratings))) +
  geom_bar() +
  facet_wrap(~ news_type)


# Pehlivanoglu, D., Lin, T., Deceus, F., Heemskerk, A., Ebner, N. C., & 
# Cahill, B. S. (2021). The role of analytical reasoning and source credibility 
# on the evaluation of real and fake full-length news articles. 
# Cognitive Research: Principles and Implications, 6(1), 24. 
# https://doi.org/10.1186/s41235-021-00292-3

## Study 1

d <- read_excel("./data_from_papers/Study_1.xlsx")
head(d)

# Recode key variables

# news veracity variable as factor(0 = fake, 1 = true)  and
# source variable (1= credible, 0 = not credible)
# - which is which had to be tried out at first (see code below, basically 
# not credible and fake were expected to score lower in accuracy with their 
# respective comparisons)

d <- d %>% 
  mutate(
    # which levels correspond to which news type needed to be figured out by 
    # trying out and then comparing resulting accuracy levels to figure 1 
    # reported in paper; we know that true news = higher accuracy
    veracity = recode(VERACITY, `1` = "true", `0` = "fake"),
    # same, we need to try out but it's a bit trickier - the paper only reports 
    # source credibility effects in interactions, but figure 2 suggests 
    # a positive main effect of credibility (with both fake and true news)
    # we know that credible source = higher accuracy
    source_credibility = recode(SOURCE, `1` = "credible", 
                                `0` = "not credible"),
    # attention, accuracy is coded as wrong (0) and correct(1) with regard
    # to whether the person gave the correct answer
    # we, however, want a variable that indicates whether the person said
    # 'accurate' (1) or 'not accurate' (0)
    # we have to check this with the veracity variable
    accuracy = ifelse(ACC == 1 & veracity == "true", 1, 
                      # if a person was wrong & items was 'fake',
                      # the given answer was accurate
                      ifelse(ACC == 0 & veracity == "fake", 1, 0)),
    credibility = CRED)

### check this

## veracity
# a) check coding
d[, c("veracity", "VERACITY")] 

# b) check accuracy by veracity (true needs to be larger than fake)
d %>% 
  group_by(veracity) %>% 
  summarise(accuracy = mean(accuracy), 
            old_accuracy = mean(ACC),
            credibility = mean(credibility))
# looks good, but credibility ratings are weird; probably reverse coded;
# i.e. 1 = completely credible and 10 = not at all credible but let's check
# with source first

## veracity
# a) check coding
d[, c("source_credibility", "SOURCE")] 

d %>% 
  select(source_credibility, SOURCE) %>% 
  filter(SOURCE == 1)

# b) check accuracy by source credibility (credible to be larger than 
# not credible)
d %>% 
  group_by(source_credibility) %>% 
  summarise(accuracy = mean(accuracy), 
            credibility = mean(credibility))

# looks ok, but very close. To be sure group by veracity, too (distance should
# be slightly larger for true news according to fig. 2)

d %>% 
  group_by(veracity, source_credibility) %>% 
  summarise(accuracy = mean(accuracy), 
            credibility = mean(credibility))

# again, accuracy seems about right (i.e slightly higher for credible sources);
# but we can be quite certain now that credibility is reverse coded (because 
# slightly lower for credible sources).

# reversing credibility
d <- d %>% 
  mutate(credibility_old = credibility, 
         credibility = 11 - credibility)
# check
d[, c("credibility_old", "credibility")]

# attention: we only want real world news - that is, we can only keep those
# observations where true news have been paired with credible sources and 
# fake news with non-credible ones
# that is not technically real-world, exactly, but seems close enough to accept

# only look at observations where headlines were paired with matching source type
# (i.e. not credible for fake & credible for true)         

d_clean <- d %>% 
  filter((veracity == "fake" & source_credibility == "not credible") | 
           (veracity == "true" & source_credibility == "credible")  
  )
# check
table(d_clean$veracity, d_clean$source_credibility)

# here things get a bit complicated, because de facto this changes the design of 
# the study:
# "We created two experimental lists to control pairing of Veracity of the news 
# article (real vs. fake; within-subjects) and Credibility of the news source 
# (credible vs. non-credible; between-subjects)."

# In other words, for each participant, either all fake or all true news are
# mismatched in terms of source credibility. 
# Now that we have taken only matches between veracity and source_credibility,
# we thus have a between subject design for news veracity, and only 6 items 
# per participant. 

## check if that's the case
# number of observations should be half for new data frame 'd_clean'
nrow(d) # 3504
nrow(d_clean) # 1752

# number of participants
d %>% summarize(n_participants = n_distinct(SS_ID)) #357
d_clean %>% summarize(n_participants = n_distinct(SS_ID)) #357

# slightly varying between conditions
d_clean %>% 
  group_by(veracity) %>% 
  summarise(n_participants = n_distinct(SS_ID)) 
# How is that possible? There have been two lists, and apparently 
# the list with credible sources has been assigned more often 
# (hence more true items)

# we should have six items (strictly either fake or true per participant)
d_clean %>% 
  group_by(SS_ID, veracity) %>% 
  summarise(items = n_distinct(NEWS_ID))

## finally, get accuracy measures
# because it might be interesting to have a study that measures accuracy in two 
# different ways (accuracy AND credibility, we'll look at both separately)

d_clean %>% 
  group_by(veracity) %>% 
  get_summary_stats(accuracy, credibility, type = "mean_sd")


## Study 2

d <- read_excel("./data_from_papers/Study_2.xlsx")
head(d)

# Recode key variables

# news veracity variable as factor(0 = fake, 1 = true)  and
# source variable (1= credible, 0 = not credible)
# - which is which had to be tried out at first (see code below, basically 
# not credible and fake were expected to score lower in accuracy with their 
# respective comparisons)

d <- d %>% 
  mutate(
    # which levels correspond to which news type needed to be figured out by 
    # trying out and then comparing resulting accuracy levels to figure 1 
    # reported in paper; we know that true news = higher accuracy
    veracity = recode(VERACITY, `1` = "true", `0` = "fake"),
    # same, we need to try out but it's a bit trickier - the paper only reports 
    # source credibility effects in interactions, but figure 2 suggests 
    # a positive main effect of credibility (with both fake and true news)
    # we know that credible source = higher accuracy
    source_credibility = recode(SOURCE, `1` = "credible", 
                                `0` = "not credible"),
    # attention, accuracy is coded as wrong (0) and correct(1) with regard
    # to whether the person gave the correct answer
    # we, however, want a variable that indicates whether the person said
    # 'accurate' (1) or 'not accurate' (0)
    # we have to check this with the veracity variable
    accuracy = ifelse(ACC == 1 & veracity == "true", 1, 
                      # if a person was wrong & items was 'fake',
                      # the given answer was accurate
                      ifelse(ACC == 0 & veracity == "fake", 1, 0)),
    credibility = CRED)

### check this

## veracity
# a) check coding
d[, c("veracity", "VERACITY")] 

# b) check accuracy by veracity (true needs to be larger than fake)
d %>% 
  group_by(veracity) %>% 
  summarise(accuracy = mean(accuracy), 
            old_accuracy = mean(ACC),
            credibility = mean(credibility))
# looks good, but credibility ratings are weird; probably reverse coded;
# i.e. 1 = completely credible and 10 = not at all credible but let's check
# with source first

## veracity
# a) check coding
d[, c("source_credibility", "SOURCE")] 

d %>% 
  select(source_credibility, SOURCE) %>% 
  filter(SOURCE == 1)

# b) check accuracy by source credibility (credible to be larger than 
# not credible)
d %>% 
  group_by(source_credibility) %>% 
  summarise(accuracy = mean(accuracy), 
            credibility = mean(credibility))

# looks ok, but very close. To be sure group by veracity, too (distance should
# be slightly larger for true news according to fig. 2)

d %>% 
  group_by(veracity, source_credibility) %>% 
  summarise(accuracy = mean(accuracy), 
            credibility = mean(credibility))

# again, accuracy seems about right (i.e slightly higher for credible sources);
# but we can be quite certain now that credibility is reverse coded (because 
# slightly lower for credible sources).

# reversing credibility
d <- d %>% 
  mutate(credibility_old = credibility, 
         credibility = 11 - credibility)
# check
d[, c("credibility_old", "credibility")]

# attention: we only want real world news - that is, we can only keep those
# observations where true news have been paired with credible sources and 
# fake news with non-credible ones
# that is not technically real-world, exactly, but seems close enough to accept

# only look at observations where headlines were paired with matching source type
# (i.e. not credible for fake & credible for true)         

d_clean <- d %>% 
  filter((veracity == "fake" & source_credibility == "not credible") | 
           (veracity == "true" & source_credibility == "credible")  
  )
# check
table(d_clean$veracity, d_clean$source_credibility)

# here things get a bit complicated, because de facto this changes the design of 
# the study:
# "We created two experimental lists to control pairing of Veracity of the news 
# article (real vs. fake; within-subjects) and Credibility of the news source 
# (credible vs. non-credible; between-subjects)."

# In other words, for each participant, either all fake or all true news are
# mismatched in terms of source credibility. 
# Now that we have taken only matches between veracity and source_credibility,
# we thus have a between subject design for news veracity, and only 6 items 
# per participant. 

## check if that's the case
# number of observations should be half for new data frame 'd_clean'
nrow(d) # 4284
nrow(d_clean) # 2142
# slightly varying between conditions
d_clean %>% 
  group_by(veracity) %>% 
  summarise(n_participants = n_distinct(SS_ID)) 
# How is that possible? There have been two lists, and apparently 
# the list with credible sources has been assigned more often 
# (hence more true items)

# we should have six items (strictly either fake or true per participant)
d_clean %>% 
  group_by(SS_ID, veracity) %>% 
  summarise(items = n_distinct(NEWS_ID))

## finally, get accuracy measures
# because it might be interesting to have a study that measures accuracy in two 
# different ways (accuracy AND credibility, we'll look at both separately)

d_clean %>% 
  group_by(veracity) %>% 
  get_summary_stats(accuracy, credibility, type = "mean_sd")


# Erlich, A., & Garner, C. (2023). Is pro-Kremlin Disinformation Effective? 
# Evidence from Ukraine. The International Journal of Press/Politics, 28(1), 
# 5–28. https://doi.org/10.1177/19401612211045221

## Study 1

d <- read_rds("./data_from_papers/long_media_data_s1.rds")

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
  get_summary_stats(response, type = "mean_sd") 


## Study 2

d <- read_rds("./data_from_papers/long_media_data_s2.rds")

# Searching for accuracy measure
levels(as.factor(d$narr_believe)) 
# looks plausible, has 4 levels corresponding
# to the 7 item scale mentioned in the paper. No other variable looks plausible.
# Variable has been centered on 0 (instead of 4). Bring back to original scale:
d <- d %>% mutate(response = narr_believe + 4)

# Searching for veracity indicator
table(d$narr_true, d$narr_strat) # looks plausible
# check another candidate
table(d$narr_true, d$narr_strat_true) # suggests they are the same
# good enough to assum 'narr_true' to indicate veracity with 1 = true

# unclear what is a unique participant identifier here. So let's take reported sample size from paper:
# NDI funded a face-to-face, nationally representative survey of 9,474 respondents (who make 167,237 ratings)

# take number of answered items from study, too

d %>% 
  select(
    # Direction of scale is not explicitly described in paper.
    # However, to be coherent with what's reported in 
    # the article, it must have been coded such that higher = more true.
    # Because true news were rated as higher than fake news. Impossible to check.
    response,
    # indicator of whether the item is true
    narr_true, 
    # news subject
    narr_topic) %>% 
  group_by(narr_true, narr_topic) %>% 
  get_summary_stats(response, type = "mean_sd") 
# IMPORTANT: note that this output is 'unweighted' raw. In the paper, on other variables,

# Clayton, K., Blair, S., Busam, J. A., Forstner, S., Glance, J., Green, G., 
# Kawata, A., Kovvuri, A., Martin, J., Morgan, E., Sandhu, M., Sang, R., 
# Scholz-Bright, R., Welch, A. T., Wolff, A. G., Zhou, A., & Nyhan, B. (2020). 
# Real Solutions for Fake News? Measuring the Effectiveness of General Warnings 
# and Fact-Check Tags in Reducing Belief in False Stories on Social Media. 
# Political Behavior, 42(4), 1073–1095. https://doi.org/10.1007/s11109-019-09533-0

# import data 
load("./data_from_papers/warnings-tags-replication.RData")


# trying to find condition variable
# cross-reading paper and stata code, these seem to be the relevant variables for condition
table %>% select(nocorr_condition, disputed_condition, false_condition, flag_cond,cond,  purecontrol, warning, nowarning)

# cross-check sample sizes with paper to be sure these are the conditions
table %>% group_by(nocorr_condition, disputed_condition, false_condition, 
                   flag_cond,cond,  purecontrol, warning, nowarning) %>% 
  summarize(n_per_condition = n()) 

# make new easy-to-read condition variable
table <- table %>% 
  mutate(condition = case_when(cond == 1 ~ "pure_control", 
                               cond == 2 ~ "control_no_warning", 
                               cond == 3 ~ "control_warning", 
                               cond == 4 ~ "disputed_no_warning",
                               cond == 5 ~ "disputed_warning", 
                               cond == 6 ~ "false_no_warning", 
                               cond == 7 ~ "false_warning")
  )

# sample size
table %>% group_by(condition) %>% summarise(n = n())

# trying to identify news ratings  
test <- table %>% select(belief_old_fake_news, belief_real_news, real_civil_war_belief,
                 real_syria_belief, real_gorsuch_belief, draft_belief, bee_belief, 
                 chaf_belief, protester_belief, marines_belief, fbiagent_belief) 

# bring data to long format
long_table <- table %>% 
  pivot_longer(c(real_civil_war_belief, real_syria_belief, real_gorsuch_belief, 
                 draft_belief, bee_belief, chaf_belief, protester_belief, 
                 marines_belief, fbiagent_belief), 
               names_to = "item",
               values_to = "ratings") %>% 
  # make an binary 'veracity' variable identifying true and fake
  mutate(veracity = ifelse(grepl('real', item), 'true', 'fake'))

# check that veracity corresponds to correct items
long_table %>% group_by(item, veracity) %>% summarize(n = n())

# get summary stats
long_table %>% group_by(veracity, condition) %>% summarize(mean_rating = mean(ratings, na.rm=TRUE), 
                                                      sd = sd(ratings, na.rm = TRUE),
                                                      n = n())

# Badrinathan, S. (2021). Educative Interventions to Combat Misinformation: 
# Evidence from a Field Experiment in India. American Political Science Review, 
# 115(4), 1325–1341. https://doi.org/10.1017/S0003055421000459

# first, we follow the author's instructions given in the replication files:
# "To replicate, first run coding.R which merges the two data sets, cleans and 
# recodes variables as needed for final analysis." 
# The data reported here is thus the *cleaned* version using the author's "coding.R" file.
d <- read_csv("data_from_papers/CLEANED_bihar")

# Unfortunately, the documentation is only available in Hindi. 
# Luckily, from the cleaning document it is obvious which variables were the fake
# and which the true news.

# fake news (1 = responded 'not accurate', i.e. correctly identified as false): 
#dv1: CCTV : 'cctv'
#dv2: no terror attacks : 'attacks'
#dv3 : pulwama fake photos : 'pulwama'
#dv4 : ganga fake photos : 'ganga'
#dv5 : fake plastic finger : 'plastic' 
#dv6 : soldier : 'soldier'
#dv7 : gomutra : 'gomutra' 
#dv8 : rally : 'rally'
#dv9 : child kidnap : 'kidnap_dv'
#dv10 : 2000 note : 'note'
#dv11 : patel statue : 'patel'
#dv12 : flag on statue of liberty : 'flag'
#dv13 : evm hacking : 'evm'

# true news (1 = responded 'accurate', i.e. correctly identified as true): 
# true DVs
# dv14: man ki baat: 'true1'
# dv15: pulwama : 'true1'

# treatment (0 = control, 1 = one of the two treatments): 
# for treatment, we rely on the pooled treatment (grouping both treatment groups
# together) of the author. The reason is that she relies on this pooled variable
# mostly and the two treatments appear not to have yielded distinguishable effects
# treatment dummy : 'treatment' 

# bring data to long format and recode variables
long_d <- d %>% 
  pivot_longer(c(attacks, pulwama, ganga, plastic, soldier, gomutra, rally, 
                 kidnap_dv, note, patel, flag, evm, true1, true2), 
               names_to = "item",
               values_to = "ratings") %>% 
  # make an binary 'veracity' variable identifying true and fake
  mutate(veracity = ifelse(grepl('true', item), 'true', 'fake'), 
         # make treatment a factor
         condition = recode_factor(treatment, `0` = "control", `1` = "treatment"), 
         # recode accuracy responses for fake news
         # so that 1 = rated as accurate (just as is measured for true news)
         ratings_recoded = ifelse(veracity == 'fake', 
                                  ifelse(ratings == 1, 0, 1), 
                                  ratings)
         )

# check whether transformations were correct 
# (and get sample sizes for control vs. treatment groups)
long_d %>% 
  group_by(item, veracity, treatment, condition) %>% 
  summarize(n = n(), 
            mean_rating = mean(ratings, na.rm=TRUE),
            mean_rating_recoded = mean(ratings_recoded, na.rm=TRUE))

# get relevant summary stats
long_d %>% group_by(veracity, condition) %>% 
  summarize(mean_rating_recoded = mean(ratings_recoded, na.rm=TRUE),
            sd = sd(ratings, na.rm = TRUE),
            n = n())

# we can even go further and see political discordant vs. politically concordant
# from the cleaning document, we know that participants' party id = 'BJP' 
# problem is: we don't know what level (0,1) corresponds to which id; by repli-
# cating figure 5 (with the replication file accessible online) 
# from the paper, we figure out that 0 = non BJP and 1 = BJP

# we also know which fake news items are pro-BJP:
#  bihar$gomutra + bihar$attacks + bihar$pulwama + bihar$soldier + bihar$flag + bihar$note 
# and which fake news items are anti_BJP:
#  bihar$cctv + bihar$evm + bihar$ganga + bihar$kidnap_dv + bihar$plastic + bihar$patel
# regarding true news, we can combine the cleaning document and the supplement (table D.1)
# to know that `true1` (man ki baat) = pro BJP; and `true2` (pulwama) = anti BJP

pro_BJP <- c("gomutra", "attacks", "pulwama", "soldier", "flag", "note", "true1")

long_d <- long_d %>% 
  # make a binary variable indicating political slant of news
  mutate(political_slant = ifelse(item %in% pro_BJP, "pro_BJP", "anti_BJP"),
         # make a clearer party id variable
         party_id = recode_factor(BJP, `0` = "non_BJP", `1` = "BJP"),
         # combine party id and political slant 
         concordance = case_when(political_slant == "pro_BJP" & party_id == "BJP" ~ "concordant",
                   political_slant == "anti_BJP" & party_id == "non_BJP" ~ "concordant", 
                   TRUE ~ "discordant")
         )

# check coding
long_d %>% select(political_slant, party_id, concordance)

# get summary stats by `concordance`
long_d %>% group_by(veracity, condition, concordance) %>% 
  summarize(mean_rating_recoded = mean(ratings_recoded, na.rm=TRUE),
            sd = sd(ratings, na.rm = TRUE),
            n = n(),
            # add number of news items within each category
            # divide by two since for a single participant, only half of the 
            # news were concordant/discordant
            n_news = nlevels(as.factor(item))/2
            )

# standard errors seem large (because of the binary scale); plot this quickly
ggplot(long_d, aes(x = ratings_recoded, fill = veracity)) + 
  geom_histogram(alpha = 0.5)


# Calvillo, D. P., & Smelter, T. J. (2020). An initial accuracy focus reduces 
# the effect of prior exposure on perceived accuracy of news headlines. 
# Cognitive Research: Principles and Implications, 5(1), 55. 
# https://doi.org/10.1186/s41235-020-00257-y

# The authors provide means but no SDs
# We could recover SD's from conficence intervals

# calculate SD from 95% confidence intervals
# source: https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm

SD_from_95_CI <- function(lower_CI, upper_CI, sample_size) {
  SD = sqrt(sample_size)*(upper_CI - lower_CI)/3.92
  return(SD)
}

SD_from_95_CI(lower_CI = 2.73, upper_CI = 2.85, sample_size = 194)

# However, we can also check the raw data directly for SDs (more precise). 

# Study 1
d <- read_sav("./data_from_papers/fake news and pictures data (f18).sav")

# conditions are in the variable names
# bring data into long-format shape 
d <- d %>% 
  mutate (id = 1:nrow(.)) %>% 
  # attention: headline04 in set 2 has a typo
  rename(headline04truepic2 = headline04truenpic2) %>% 
  pivot_longer(starts_with("headline"), 
               names_to = "veracity_picture", 
               values_to = "accuracy", 
               names_prefix = "headline") %>% 
  separate_wider_regex(veracity_picture, 
                       c(headline_id = "\\d+", veracity = "true|fake", picture = "pic|nopic", set = "\\d+" ), 
                       too_few = "align_start")

# summary
d %>% group_by(veracity, picture) %>%
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_participants = n_distinct(id), 
            n_headlines = n_distinct(headline_id))

# overall conclusion about pictures
d %>% group_by(picture) %>%
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_participants = n_distinct(id), 
            n_headlines = n_distinct(headline_id))

# # old analysis from verification process - authors made a mistake and 
# # then uploaded a new data set in response to our mails
# ## do analysis that excludes people with at least one NA
# d <- read_sav("./data_from_papers/fake news and pictures data (f18).sav")
# 
# # pick only variables of interest
# d <- d %>% 
#   # make id variable 
#   mutate(id = 1:nrow(.)) %>% 
#   select(id, headline01truepic:headline28fakepic)
#   
# 
# # create a count variable of sum of index variables
# d$NAs <- rowSums(is.na(d))
# 
# # all participants should have 28 NAs if they answered all questions
# # (because they have been assigned to one of two sets of 28 questions)
# table(d$NAs)
# 
# # remove participants with NAs and re-do steps from above
# d <- d %>% 
#   filter(!(NAs > 28)) %>% 
#   # attention: headline02 has a typo
#   rename(headline02truepic = headine02truepic) %>% 
#   pivot_longer(starts_with("headline"), 
#                names_to = "veracity_picture", 
#                values_to = "accuracy", 
#                names_prefix = "headline") %>% 
#   separate_wider_regex(veracity_picture, 
#                        c(headline_id = "\\d+", veracity = "true|fake", picture = "pic|nopic", set))
# 
# # this does still not match the data in the paper
# d %>% group_by(veracity, picture) %>%
#   summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
#             n_participants = n_distinct(id), 
#             n_headlines = n_distinct(headline_id))
# 
# # same with overal conclusions
# d %>% group_by(picture) %>%
#   summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
#             n_participants = n_distinct(id), 
#             n_headlines = n_distinct(headline_id))


# Study 2 a
d <- read_sav("./data_from_papers/fake news fluency (exp 2a).sav")

# data is not available for single headlines, but only means by condition
# conditions are in the variable names; bring data into long-format shape 

d <- d %>% 
  mutate (id = 1:nrow(.)) %>%  
  pivot_longer(flu_true:disfl_false,
               names_to = "clarity_veracity", 
               values_to = "accuracy") %>% 
  separate_wider_delim(clarity_veracity, delim = "_", 
                       names = c("clarity", "veracity"))

d %>% group_by(clarity, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_participants = n_distinct(id))

# Study 2 b
d <- read_sav("./data_from_papers/fake news fluency (exp 2b).sav")

# data is not available for single headlines, but only means by condition
# conditions are in the variable names; bring data into long-format shape 
d <- d %>% 
  mutate (id = 1:nrow(.)) %>%
  rename(clarity = fluency_condition) %>% 
  pivot_longer(c(true, false),
               names_to = "veracity", 
               values_to = "accuracy")

d %>% group_by(clarity, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_participants = n_distinct(id))

# Study 3
d <- read_sav("./data_from_papers/fake news humor data sheet.sav")

# data is not available for single headlines, but only means by condition
# conditions are in the variable names; bring data into long-format shape 
d <- d %>% 
  slice(1:200) %>% 
  mutate (id = 1:nrow(.)) %>%
  pivot_longer(prior_true:nprior_false,
               names_to = "prior_veracity", 
               values_to = "accuracy") %>% 
  separate_wider_delim(prior_veracity, delim = "_", 
                       names = c("prior", "veracity"))

# summary stats
d %>% group_by(veracity, prior) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_participants = n_distinct(id))


# Luo, M., Hancock, J. T., & Markowitz, D. M. (2022). 
# Credibility Perceptions and Detection Accuracy of Fake News Headlines on 
# Social Media: Effects of Truth-Bias and Endorsement Cues. 
# Communication Research, 49(2), 171–195. https://doi.org/10.1177/0093650220921321

# Study 1
d <- read_csv("./data_from_papers/Study1_data0220.csv")

# rename first column
names(d)[1] <- "id"

# function to calculate SE to compare with paper table
se <- function(x, na.rm=TRUE) sd(x, na.rm = na.rm)/sqrt(length(x))

# summary stats
d %>% group_by(Topic, Veracity) %>% 
  summarise(across(Judgment, list(mean = mean, sd = sd, se = se), na.rm=TRUE), 
            n_participants = n_distinct(id))

# Study 2
d <- read_csv("./data_from_papers/S2_cleaned.csv")
# note that when using the non-cleaned file:
# d <- read_csv("./data_from_papers/Study2_data0220.csv")
# we cannot replicate the numbers. In the paper, they say that is due to 
# exclusion of participants who had already done study 1.

# function to calculate SE to compare with paper table
se <- function(x, na.rm=TRUE) sd(x, na.rm = na.rm)/sqrt(length(x))

# make more readable values
d <- d %>% 
  mutate(veracity = ifelse(type == "F", "fake", "true"))

# summary stats (for replicating paper table, also group by 'population')
d %>% group_by(topic, like, veracity) %>% 
  summarise(across(judgment, list(mean = mean, sd = sd, se = se), na.rm=TRUE)) %>% 
  print(n = nrow(.))

# Check number of participants
d %>% group_by(topic, like) %>%
  summarize(n_subj = n() /8) # each participant saw 8 news headlines

# Altay, S., Lyons, B., & Modirrousta-Galian, A. (2023). 
# Exposure to Higher Rates of False News Erodes Media Trust and Fuels 
# Skepticism in News Judgment. https://doi.org/10.31234/osf.io/t9r43

# Study 1
d <- read_csv("./data_from_papers/Study_1_Data_Long_Clean.csv")
names(d)

# levels condition
levels(as.factor(d$Condition))
# levels veracity
levels(as.factor(d$Veracity))

# summary stats 
d %>% group_by(Condition, Veracity) %>% 
  summarise(across(Accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(Participants))

# Study 2
d <- read_csv("./data_from_papers/Study_2_Data_Long_Clean.csv")
names(d)

# levels condition
levels(as.factor(d$Condition))
# levels veracity
levels(as.factor(d$Veracity))

# summary stats 
d %>% group_by(Condition, Veracity) %>% 
  summarise(across(Accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(ResponseId))

# Hoes-Altay-Angelis 2023
d <- read_excel("./data_from_papers/Hoes-Altay-Angelis_data_clean.xlsx")
names(d)

# levels condition
levels(as.factor(d$Condition))
# levels veracity
levels(as.factor(d$Veracity))

# beyond condition, proportion has been manipulated
levels(as.factor(d$Proportion))

# summary stats 
d %>% 
  mutate(Proportion = case_when(Proportion == "balanced" ~ 0.5, 
                                Proportion == "true" ~ 0.75, 
                                Proportion == "false" ~ 0.25,
                                )
         ) %>% 
  group_by(Condition, Proportion, Veracity) %>% 
  summarise(across(Ratings, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(PROLIFIC_PID)) %>% 
  print(n = 30)


# Altay-Gilardi 2023
d <- read_excel("./data_from_papers/Altay-Gilardi_data_long_clean.xlsx")
names(d)

# levels condition
levels(as.factor(d$Condition))
# levels veracity
levels(as.factor(d$True_False))

# long format data, `Ratings` codes the outcome for both sharing and accuracy, 
# so we have to filter DV == Accuracy. 
# Also, remove some treatment conditions that are irrelevant for our study
d <- d %>% 
  filter(DV == "Accuracy" & 
           Conditions %in% c("Control", "FalseLabel"))

# summary stats 
d %>% group_by(Condition, True_False) %>% 
  summarise(across(Ratings, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(PROLIFIC_PID))

# Espina Mairal, S., Bustos, F., Solovey, G., & Navajas, J. (2023). 
# Interactive crowdsourcing to fact-check politicians. 
# Journal of Experimental Psychology: Applied. 
# https://doi.org/10.1037/xap0000492

# Study 1
d <- read_csv("./data_from_papers/mairal_database.csv")
names(d)

# Thanks to their codebook (great documentation!), we know that `answer_chequeado`
# codes veracity and that `answer_1` is the original answer (before any manipulation).
# Since participant's initial rating happened under the same (unmanipulated) 
# circumstances for each condition, we consider them all as control. The `control` 
# variable is thus not relevant for us. 


# inspect key variables to get an overview
d %>% 
  select(id_subject, control, answer_chequeado, phrase_sign, subject_type, answer_1, 
         phrase_concordant) %>% 
  arrange(id_subject) 

# summary stats 
d %>%
  mutate(accuracy = ifelse(answer_1 == "V", 1, 0), 
         veracity = ifelse(answer_chequeado == "V", "true", "fake"),
         concordance = ifelse(phrase_concordant == 1, "concordant", "discordant")
         ) %>% 
  group_by(concordance, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id_subject))

# Study 2
d <- read_csv("./data_from_papers/mairal_database_study2.csv")
names(d)

# summary stats 
d %>%
  mutate(accuracy = ifelse(answer_1 == "V", 1, 0), 
         veracity = ifelse(answer_chequeado == "V", "true", "fake"),
         concordance = ifelse(phrase_concordant == 1, "concordant", "discordant")
  ) %>% 
  group_by(concordance, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(id_subject))

  
# Gawronski, B., Ng, N. L., & Luke, D. M. (2023). 
# Truth sensitivity and partisan bias in responses to misinformation. 
# Journal of Experimental Psychology: General, 152(8), 2205–2236. 
# https://doi.org/10.1037/xge0001381

# Experiment 1

d <- read_sav("./data_from_papers/Gawronski-Experiment1.sav")
names(d)

# No codebook available, but we get variable names and answer codes from the 
# supplemental material with the original survey.
# Data in wide format. 
# For the rating variables (Pro..._S/T), 
# S indicates that participants were assigned to "Sharing" condition and T 
# to accuracy (or Truth) judgement. 
# We know that for the Political identification variable `PO_Part`, 1 == Republican
# and 2 == Democrat

d <- d %>% 
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
  ) 
  
# summary data
d %>% 
  group_by(concordance, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(ResponseId))

# Experiment 4

d <- read_sav("./data_from_papers/Gawronski-Experiment4.sav")
names(d)

# For the rating variables (Pro..._S/T), we can deduce from the supplemental information
# that for participants in the sharing condition (irrelevant), the ending is "_C". 
# For participants in the truth prime condition (the one we are interested in), 
# as in experiment 1, 
# S indicates that participants were assigned to "Sharing" condition and T 
# to accuracy (or Truth) judgement. 

d <- d %>% 
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

# summary data
d %>% 
  group_by(concordance, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(ResponseId))


# Garrett, R. K., & Bond, R. M. (2021). 
# Conservatives’ susceptibility to political misperceptions. 
# Science Advances, 7(23), eabf1234. https://doi.org/10.1126/sciadv.abf1234

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

# we split the sample in those who identify as democrat or republican and those who don't
political_d <- full_d %>% 
  filter(!is.na(political_party)) %>% 
  mutate(concordance = case_when(slant == political_party ~ "concordant", 
                                 slant == "neutral" ~ "neutral", 
                                 slant != political_party ~ "discordant"))

Apolitical_d <- full_d %>% 
  filter(is.na(political_party))


# summary data political
political_d %>% 
  group_by(wave, concordance, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(participant_id), 
            n_headline = n_distinct(headline_id)) %>% 
  print(n = 90)

# summary data Apolitical
Apolitical_d %>% 
  group_by(wave, veracity) %>% 
  summarise(across(accuracy, list(mean = mean, sd = sd), na.rm=TRUE), 
            n_subj = n_distinct(participant_id), 
            n_headline = n_distinct(headline_id)) %>% 
  print(n = 30)


---
title: "Generate numbers for PRISMA flow diagram"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)
```

```
## Warning: package 'stringr' was built under R version 4.2.3
```

Load data sheets.

```r
# previous literature search
google <- read_csv("google.csv") 
scopus <- read_csv("scopus.csv") 
```

```
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```r
# Literature
included <- read_csv("../data/cleaned.csv") %>% 
  # control conditions only
  filter(condition == "control") %>% 
  group_by(paperID) %>% 
  summarize(reference = unique(reference)) %>% 
  arrange(paperID) %>% 
  ungroup() 
excluded <- read_csv("excluded.csv")

# revisions literature search
revisions_google <- read_csv("revisions_scholar.csv", col_types = cols(.default = col_character())) 
revisions_scopus <- read_csv("revisions_scopus.csv", col_types = cols(.default = col_character()))
```

Modify string format.

```r
# little helper function
make_strings_more_compatible <- function (string) {
  string <- tolower(string)   # no more capital letters
  string <- trimws(string)    # no more white spaces at the end
}

# edit strings
google <- google %>% 
  mutate(across(Title, make_strings_more_compatible))

scopus <- scopus %>% 
  mutate(across(Title, make_strings_more_compatible))

included <- included %>% 
  mutate(across(reference, make_strings_more_compatible))

excluded <- excluded %>% 
  mutate(across(reference, make_strings_more_compatible))

revisions_google <- revisions_google %>% 
  mutate(across(Title, make_strings_more_compatible))

revisions_scopus <- revisions_scopus %>% 
  mutate(across(Title, make_strings_more_compatible))
```

## Combine initial and revisions search

We want to have the total number of unique results from google scholar and scopus. That means, for both data bases respectively, we want to remove duplicates. 

### Google

Identify duplicates between revisions and prior search for google searches


```r
# Google scholar

# extract prior search results
prior_search_doi <- c(google$DOI )
prior_search_titles <- c(google$Title)

# identify duplicates from literature searches
revisions_google <- revisions_google %>% 
  mutate(duplicate_by_DOI = ifelse((DOI %in% prior_search_doi) & !is.na(DOI), TRUE, FALSE),
         duplicate_by_Title = ifelse((Title %in% prior_search_titles), TRUE, FALSE), 
         duplicate_by_either = ifelse((duplicate_by_DOI | duplicate_by_Title) == TRUE, TRUE, FALSE)
         )

# Overview of different matching methods
revisions_google %>% summarise(duplicate_by_DOI = sum(duplicate_by_DOI), 
                     duplicate_by_Title = sum(duplicate_by_Title), 
                     duplicate_by_either = sum(duplicate_by_either))
```

```
## # A tibble: 1 × 3
##   duplicate_by_DOI duplicate_by_Title duplicate_by_either
##              <int>              <int>               <int>
## 1              118                269                 269
```

```r
# store number of initial google results and revisions. 
n_google_first <- nrow(google)
n_google_second <- nrow(revisions_google)

# store the duplicates with prior search
duplicates_prior_google <- sum(revisions_google$duplicate_by_either)
```

Now we can calulate the overall number of unique google results as:


```r
n_google_total <- nrow(google) + nrow(revisions_google) - duplicates_prior_google

n_google_total
```

```
## [1] 1691
```

Finally, store a google data frame with all unique google results


```r
google <- bind_rows(google %>% 
                           mutate(across(everything(), ~as.character(.x))), 
                         revisions_google %>% 
                           rename(duplicate = duplicate_by_either) %>% 
                           filter(duplicate == FALSE))
```

### Scopus


```r
# Scopus

# extract prior search results
prior_search_doi <- c(scopus$DOI )
prior_search_titles <- c(scopus$Title)

# identify duplicates from literature searches
revisions_scopus <- revisions_scopus %>% 
  mutate(duplicate_by_DOI = ifelse((DOI %in% prior_search_doi) & !is.na(DOI), TRUE, FALSE),
         duplicate_by_Title = ifelse((Title %in% prior_search_titles), TRUE, FALSE), 
         duplicate_by_either = ifelse((duplicate_by_DOI | duplicate_by_Title) == TRUE, TRUE, FALSE)
         )

# Overview of different matching methods
revisions_scopus %>% summarise(duplicate_by_DOI = sum(duplicate_by_DOI), 
                     duplicate_by_Title = sum(duplicate_by_Title), 
                     duplicate_by_either = sum(duplicate_by_either))
```

```
## # A tibble: 1 × 3
##   duplicate_by_DOI duplicate_by_Title duplicate_by_either
##              <int>              <int>               <int>
## 1              604                625                 642
```

```r
# store number of initial google results and revisions. 
n_scopus_first <- nrow(scopus)
n_scopus_second <- nrow(revisions_scopus)

# store the duplicates with prior search
duplicates_prior_scopus <- sum(revisions_scopus$duplicate_by_either)
```

Now we can calulate the overall number of unique scopus results as:


```r
n_scopus_total <- nrow(scopus) + nrow(revisions_scopus) - duplicates_prior_scopus

n_scopus_total
```

```
## [1] 5159
```

Finally, store a scopus data frame with all unique scopus results


```r
scopus <- bind_rows(scopus %>% 
                           mutate(across(everything(), ~as.character(.x))), 
                         revisions_scopus %>% 
                           rename(duplicate = duplicate_by_either) %>% 
                           filter(duplicate == FALSE))
```

# Duplicates between searches

Identify duplicates from literature searches (i.e. between `scopus` and `google`). 


```r
# identify duplicates from literature searches
google <- google %>% 
  mutate(duplicate_by_DOI = ifelse((DOI %in% scopus$DOI) & !is.na(DOI), TRUE, FALSE),
         duplicate_by_Title = ifelse((Title %in% scopus$Title), TRUE, FALSE), 
         duplicate_by_either = ifelse((duplicate_by_DOI | duplicate_by_Title) == TRUE, TRUE, FALSE)
         )

# Overview of different matching methods
google %>% summarise(duplicate_by_DOI = sum(duplicate_by_DOI), 
                     duplicate_by_Title = sum(duplicate_by_Title), 
                     duplicate_by_either = sum(duplicate_by_either))
```

```
## # A tibble: 1 × 3
##   duplicate_by_DOI duplicate_by_Title duplicate_by_either
##              <int>              <int>               <int>
## 1              225                292                 338
```
We will take matches that occurred based on either `DOI` or `Title`. 


```r
all_search_results <- bind_rows(google %>% 
                                  select(DOI, Title, duplicate_by_either) %>% 
                                  rename(duplicate = duplicate_by_either), 
                                scopus %>% 
                                  mutate(duplicate = FALSE) %>% 
                                  select (DOI, Title, duplicate)
                                )
```

Combine the `included` and `excluded` sheet into a single `studies` data frame. 


```r
# make both sheets compatible
included_compatible <- included %>% select(paperID, reference) %>% mutate(included = "included")

excluded_compatible <- excluded %>% select(reference) %>% mutate(included = "excluded")

# combine sheets
studies <- bind_rows(included_compatible, excluded_compatible)

# add colums to combined data
studies <- left_join(studies, excluded %>% select(screening, extent, reference), by = "reference")
```

```
## Warning in left_join(studies, excluded %>% select(screening, extent, reference), : Detected an unexpected many-to-many relationship between `x` and `y`.
## ℹ Row 179 of `x` matches multiple rows in `y`.
## ℹ Row 1 of `y` matches multiple rows in `x`.
## ℹ If a many-to-many relationship is expected, set `relationship =
##   "many-to-many"` to silence this warning.
```

## Compute stats for PRISMA flowchart

### Identify appearance in searches

We write a function that allows us to identify overlaps between the literature searches. 

The function below checks whether a reference has a matching title in the google or the scopus searches. It creates two variables, `google` and `scopus`, that evaluate `TRUE` if there is a matching title for the reference, respectively. It creates two additional variables: 

- `identified_by_authors`, evaluates `TRUE` if reference does NOT appear in either `google` OR `scopus`
- `overlap`, evaluates `TRUE` if reference appears in both `google` AND `scopus`

To be more certain that we don't miss anything, we also search via `DOI`, not only `Title`. 


```r
identify_appearance_in_searches <- function(data, compare_by = "DOI and Title") {
  
  if (str_detect(compare_by, "Title")) {
  
 data$google_by_title <- sapply(data$reference, function(x) any(sapply(google$Title, 
                                                              grepl, x, 
                                                              fixed = TRUE)))
 
  data$scopus_by_title <- sapply(data$reference, function(x) any(sapply(scopus$Title, 
                                                              grepl, x, 
                                                              fixed = TRUE)))
    data <- data %>%
    mutate(overlap_by_title = ifelse((scopus_by_title & google_by_title) == TRUE,
                          TRUE, FALSE)
           )
  
  }
  
  if (str_detect(compare_by, "DOI")) {
  
 data$google_by_DOI <- sapply(data$reference, function(x) any(sapply(google$DOI, 
                                                              grepl, x, 
                                                              fixed = TRUE)))
 
  data$scopus_by_DOI <- sapply(data$reference, function(x) any(sapply(scopus$DOI, 
                                                              grepl, x, 
                                                              fixed = TRUE)))
  
  data <- data %>%
    mutate(overlap_by_DOI = ifelse((scopus_by_DOI & google_by_DOI) == TRUE,
                                     TRUE, FALSE)
    )
  
  }

  if (str_detect(compare_by, "(?=.*DOI)(?=.*Title).*")) {
  data <- data %>%
    mutate(identified_by_authors = 
             ifelse(
               (scopus_by_DOI | scopus_by_title | google_by_DOI | 
                  google_by_title)  == TRUE,
                                        FALSE,
                                        NA), 
           identified_by_authors = ifelse(is.na(identified_by_authors), TRUE, 
                                          identified_by_authors)
           )
  }
 
 return(data)
  
}
```

We apply this function to the `studies` data frame that contains all studies we assessed. 


```r
studies <- identify_appearance_in_searches(studies)
```

### Add source of summary statistics

We add where we got summary statistics from (paper, raw data, authors). This is not a relevant prisma statistic, but we will report it anyways.


```r
# make a data frame with source of summary stats
statistics_source <- read_csv("../data/cleaned.csv") %>% 
  group_by(paperID) %>% 
  summarize(values_from = first(values_from)) %>% 
  arrange(paperID) 

# add this variable to studies data frame
studies <- left_join(studies, statistics_source, by = "paperID") %>% 
  # This join will assign values to partly excluded studies. 
  # We have to code these as NAs separately
  mutate(values_from = ifelse(included == "excluded", NA, values_from))
```

### PRISMA


```r
# Studies per search and overal
prisma <- data.frame(
  # First vs. Second search
  scoups_initial = n_scopus_first, 
  scopus_revisions = n_scopus_second,
  duplicates_initial_revisions_scopus = duplicates_prior_scopus,
  google_initial = n_google_first, 
  google_revisions = n_google_second,
  duplicates_initial_revisions_google = duplicates_prior_google,
  # Identification
  scopus = nrow(scopus), 
  google = nrow(google), 
  overal = nrow(all_search_results), 
  duplicates = nrow(all_search_results %>% filter(duplicate == TRUE)),
  other_methods = nrow(studies %>% filter(identified_by_authors == TRUE)),
  # Screening
  screened = nrow(all_search_results %>% filter(duplicate != TRUE)),
  screening_excluded = nrow(all_search_results %>% filter(duplicate != TRUE)) - 
    nrow(studies %>% filter(identified_by_authors != TRUE)),
  retrieval_databases = nrow(studies %>% filter(identified_by_authors != TRUE)),
  exluded_databases = nrow(studies %>% filter((identified_by_authors != TRUE) &
                                                 included == "excluded"
                                               )
                                 ),
  exluded_databases_fulltext = nrow(studies %>% filter((identified_by_authors != TRUE) &
                                                 included == "excluded" &
                                                   screening == "full text"
                                               )
                                 ),
  exluded_databases_abstract = nrow(studies %>% filter((identified_by_authors != TRUE) &
                                                 included == "excluded" &
                                                   screening == "abstract"
                                               )
                                 ),
  retrieval_other = nrow(studies %>% filter(identified_by_authors == TRUE)),
  excluded_other = nrow(studies %>% filter((identified_by_authors == TRUE) &
                                                 included == "excluded"
                                               )
                                 ),
  studies_included = nrow(studies %>% filter(included == "included")), 
  # additional info on source of stats for included studies
  studies_included_paper = nrow(studies %>% filter(values_from == "paper")), 
  studies_included_authors = nrow(studies %>% filter(values_from == "authors")),
  studies_included_data = nrow(studies %>% filter(values_from == "raw data"))
) %>% 
  mutate(assessed_for_elegibility_databases = retrieval_databases,
         assessed_for_elegibility_other = retrieval_other,
         not_retrieved_databases = 0, 
         not_retrieved_other = 0 
         ) %>% 
  pivot_longer(everything(), names_to = "category", values_to = "n") 

prisma %>% print(n = 30)
```

```
## # A tibble: 27 × 2
##    category                                n
##    <chr>                               <dbl>
##  1 scoups_initial                       4002
##  2 scopus_revisions                     1799
##  3 duplicates_initial_revisions_scopus   642
##  4 google_initial                        980
##  5 google_revisions                      980
##  6 duplicates_initial_revisions_google   269
##  7 scopus                               5159
##  8 google                               1691
##  9 overal                               6850
## 10 duplicates                            338
## 11 other_methods                          47
## 12 screened                             6512
## 13 screening_excluded                   6248
## 14 retrieval_databases                   264
## 15 exluded_databases                     217
## 16 exluded_databases_fulltext            130
## 17 exluded_databases_abstract             87
## 18 retrieval_other                        47
## 19 excluded_other                         27
## 20 studies_included                       67
## 21 studies_included_paper                 21
## 22 studies_included_authors               15
## 23 studies_included_data                  31
## 24 assessed_for_elegibility_databases    264
## 25 assessed_for_elegibility_other         47
## 26 not_retrieved_databases                 0
## 27 not_retrieved_other                     0
```
## Export PRISMA


```r
write_csv(prisma, "prisma.csv")
```

## Export list of included studies


```r
write_csv(included, "included.csv")
```

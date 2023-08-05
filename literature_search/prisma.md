---
title: "Generate numbers for PRISMA flow diagram"
author: "Jan Pfänder & Sacha Altay"
date: "2023-04-27"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)
```

## Load and clean the different spreadsheets.

Load data sheets.

```r
# Literature searches
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
```

Modify string format.

```r
# little helper function
make_strings_more_compatible <- function (string) {
  string <- tolower(string)   # no more capital letters
  string <- trimws(string)    # no more white spaces at the end
}

# edit strings
# Literature searches
google <- google %>% 
  mutate(across(Title, make_strings_more_compatible))

scopus <- scopus %>% 
  mutate(across(Title, make_strings_more_compatible))

# Literature
included <- included %>% 
  mutate(across(reference, make_strings_more_compatible))

excluded <- excluded %>% 
  mutate(across(reference, make_strings_more_compatible))
```

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
## 1              112                130                 156
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
included_compatible <- included %>% select(reference) %>% mutate(included = "included")

excluded_compatible <- excluded %>% select(reference) %>% mutate(included = "excluded")

# combine sheets
studies <- bind_rows(included_compatible, excluded_compatible)

# add colums to combined data
studies <- left_join(studies, excluded %>% select(screening, extent, reference), by = "reference")
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
statistics_source <- read_csv("../data/data.csv") %>% 
  group_by(paperID) %>% 
  summarize(reference = unique(reference), 
            values_from = first(values_from)) %>% 
  arrange(paperID) %>% 
  # make references compatible
  mutate(across(reference, make_strings_more_compatible)) 

# add this variable to studies data frame
studies <- left_join(studies, statistics_source, by = "reference") %>% 
  # This join will assign values to partly excluded studies. 
  # We have to code these as NAs separately
  mutate(values_from = ifelse(included == "excluded", NA, values_from))
```

### PRISMA


```r
# Studies per search and overal
prisma <- data.frame(
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
## # A tibble: 21 × 2
##    category                               n
##    <chr>                              <dbl>
##  1 scopus                              4002
##  2 google                               980
##  3 overal                              4982
##  4 duplicates                           156
##  5 other_methods                         36
##  6 screened                            4826
##  7 screening_excluded                  4721
##  8 retrieval_databases                  105
##  9 exluded_databases                     74
## 10 exluded_databases_fulltext            69
## 11 exluded_databases_abstract             5
## 12 retrieval_other                       36
## 13 excluded_other                        19
## 14 studies_included                      48
## 15 studies_included_paper                20
## 16 studies_included_authors              12
## 17 studies_included_data                 16
## 18 assessed_for_elegibility_databases   105
## 19 assessed_for_elegibility_other        36
## 20 not_retrieved_databases                0
## 21 not_retrieved_other                    0
```
## Export PRISMA


```r
write_csv(prisma, "prisma.csv")
```

## Export list of included studies


```r
write_csv(included, "included.csv")
```


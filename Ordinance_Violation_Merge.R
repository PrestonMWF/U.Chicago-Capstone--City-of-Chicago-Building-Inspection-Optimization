## Load packages

library(tidyverse)

## Load Data

# building_violations <- read.csv('~/MS Analytics/Capstone/City_of_Chicago_building_violations.csv', stringsAsFactors = F)
# ordinance_violations <- read.csv('~/MS Analytics/Capstone/City_of_Chicago_ordinance_violations.csv', stringsAsFactors = F)

## Data Structure

head(building_violations)
head(ordinance_violations)
names(ordinance_violations)
names(building_violations)

## Merge Data

ordinance_violations <- ordinance_violations %>%
    mutate(VIOLATION.DATE = substring(VIOLATION.DATE, 1, 10),
           IMPOSED.FINE = as.numeric(IMPOSED.FINE))

building_violations <- building_violations %>%
    mutate(violation_code_number = ifelse(substring(violation_code, 1, 2) == 'ev' | substring(violation_code, 1, 2) == 'nc',
                                          violation_code,
                                          substring(violation_code, 3)))

merged_violations <- full_join(building_violations, ordinance_violations, 
                               by = c("violation_code_number" = "VIOLATION.CODE",
                                      "violation_date" = "VIOLATION.DATE", 
                                      "address" = "ADDRESS"))

## Data Summaries           


ord_group <- ordinance_violations %>%
    count(VIOLATION.CODE, VIOLATION.DESCRIPTION) %>%
    arrange(desc(n))
head(ord_group)

viol_group <- building_violations %>%
    count(violation_code, violation_description) %>%
    arrange(desc(n))
head(viol_group)

ord_fines <- ordinance_violations %>%
    group_by(VIOLATION.CODE, VIOLATION.DESCRIPTION) %>%
    summarize(avg = mean(IMPOSED.FINE)) %>%
    arrange(desc(avg))
head(ord_fines)

ord_dispostion <- ordinance_violations %>%
    count(CASE.DISPOSITION) %>%
    arrange(desc(n))
head(ord_dispostion)


## Other exploratory graphs

ggplot(ord_dispostion, aes(x = reorder(CASE.DISPOSITION, -n), y = n)) + geom_col() + coord_flip()






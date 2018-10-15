## Load packages

library(tidyverse)
library(sqldf)

## Load Data

 building_violations <- read.csv('City_of_Chicago_building_violations.csv', stringsAsFactors = F)
 ordinance_violations <- read.csv('City_of_Chicago_ordinance_violations.csv', stringsAsFactors = F)
 

## Data Structure

head(building_violations)
head(ordinance_violations)
names(ordinance_violations)
names(building_violations)

## Merge Data

ordinance_violations <- ordinance_violations %>%
    mutate(VIOLATION.DATE = substring(VIOLATION.DATE, 1, 10),
           IMPOSED.FINE = as.numeric(IMPOSED.FINE),
           VIOLATION.CODE = ifelse(substring(VIOLATION.CODE, 1, 2) == 'ev',
                                   paste0('24', substring(VIOLATION.CODE, 3)),
                                   VIOLATION.CODE),
           VIOLATION.CODE = ifelse(substring(VIOLATION.CODE, 1, 2) == 'nc',
                                   substring(VIOLATION.CODE, 3),
                                   VIOLATION.CODE),
           VIOLATION.CODE = ifelse(nchar(VIOLATION.CODE) == 3,
                                   paste0('000', VIOLATION.CODE),
                                   VIOLATION.CODE),
           VIOLATION.CODE = ifelse(nchar(VIOLATION.CODE) == 4,
                                   paste0('00', VIOLATION.CODE),
                                   VIOLATION.CODE),
           VIOLATION.CODE = ifelse(nchar(VIOLATION.CODE) == 5,
                                   paste0('0', VIOLATION.CODE),
                                   VIOLATION.CODE))

building_violations <- building_violations %>%
    mutate(violation_code_number = violation_code) %>%
    rename('id_building' = id) %>%
    rename('latitude_b' = latitude) %>%
    rename('longitude_b' = longitude) %>%
    rename('location_b' = location)

# create building violation rules

building_violations <- building_violations %>%
    mutate(violation_code_number = ifelse(substring(violation_code, 1, 2) == 'el',
                                          paste0('22', substring(violation_code, 3)),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'cn',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'pl',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'vt',
                                          paste0('25', substring(violation_code, 3)),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'br',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'rf',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'ir',
                                          paste0('20', substring(violation_code, 3)),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'ev',
                                          paste0('24', substring(violation_code, 3)),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 3) == 'aag',
                                          substring(violation_code, 4),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'nc',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 3) == 'cee',
                                          substring(violation_code, 4),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 3) == 'cel',
                                          substring(violation_code, 4),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'es',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'fc',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'fp',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'hv',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'vg',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'vv',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'vx',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'gb',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'eq',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'em',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'bc',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(substring(violation_code, 1, 2) == 'br',
                                          substring(violation_code, 3),
                                          violation_code_number),
           violation_code_number = ifelse(nchar(violation_code_number) == 2,
                                          paste0('0000', violation_code_number),
                                          violation_code_number),
           violation_code_number = ifelse(nchar(violation_code_number) == 3,
                                          paste0('000', violation_code_number),
                                          violation_code_number),
           violation_code_number = ifelse(nchar(violation_code_number) == 4,
                                          paste0('00', violation_code_number),
                                          violation_code_number),
           violation_code_number = ifelse(nchar(violation_code_number) == 5,
                                          paste0('0', violation_code_number),
                                          violation_code_number),
           violation_code_number = ifelse(violation_code_number == '000302',
                                          '000552',
                                          violation_code_number),
           violation_code_number = ifelse(violation_code_number == '000301',
                                          '000552',
                                          violation_code_number),
           violation_code_number = ifelse(violation_code_number == '003054',
                                          '073054',
                                          violation_code_number))




merged_violations <- full_join(building_violations, ordinance_violations, 
                               by = c("violation_code_number" = "VIOLATION.CODE",
                                      "violation_date" = "VIOLATION.DATE", 
                                      "address" = "ADDRESS"))

## Summaries

sqldf("SELECT
        ID is null as insull,
        COUNT(*)
      FROM
        merged_violations
      WHERE
        id_building is not null
      GROUP BY
        ID is null")

sqldf("SELECT 
        leftstr(violation_code_number, 2) as viol_num,
        COUNT(*)
    FROM
        building_violations
    GROUP BY
        leftstr(violation_code_number, 2)")

sqldf("SELECT 
        leftstr(ordinance_violations.'violation.code', 2) as viol_type,
        COUNT(*)
      FROM
        ordinance_violations
      GROUP BY
        leftstr(ordinance_violations.'violation.code', 2)")

## Data Summaries           


ord_group <- ordinance_violations %>%
    count(VIOLATION.CODE, VIOLATION.DESCRIPTION) %>%
    arrange(desc(n))
head(ord_group)

viol_group <- building_violations %>%
    count(violation_code_number, violation_description) %>%
    arrange(desc(n))
head(viol_group)

viol_comparison <- full_join(viol_group, ord_group, 
                               by = c("violation_code_number" = "VIOLATION.CODE"))

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






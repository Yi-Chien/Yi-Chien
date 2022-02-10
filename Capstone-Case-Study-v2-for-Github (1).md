Next Best States to Develop for Developer Company in The U.S.
================
Yi-Chien Tung
02/09/2022

## Business Task

This project is assuming that I am an analyst in a top 10 developer
company in the U.S., and it’s my job to find out what’s the next best
places/states for my company to develop. In order to gain a significant
return, my company will avoid those over-developed and over-crowded
cities and states, due to the cost are much higher.

In order to dig out the potential of a state, we study a state from
these aspects:

-   Population growth

-   Natural disaster frequency since 2015

Where natural disaster frequency is the most crucial factor for a better
place to live since we are in an era where climate change emerging, and
it’s affecting every aspects of human’s life, therefore this project
will be focusing more on it.

## Data Source

#### For naturaly disaster frequency:

The dataset for this project thanks to the Kaggler-Heads or Tails.
<https://www.kaggle.com/headsortails/datasets>.

He downloaded it from the FEMA website and applied a few simple data
cleaning and formatting measures. The full dataset with all rows and
columns: us_disaster_declarations.csv, the geographical resolution is
the county level, with FIPS codes being used to encode the counties. In
addition to the fips and timing features, the data provides the type of
disaster and also binary flags that indicate whether specific aid
programs were triggered in response. According to FEMA website: “There
are two types of disaster declarations. They are: emergency and major
disaster. While emergency is intended to avert a catastrophe through the
support of emergency actions. It does not include any restoration or
permanent repairs.” Therefore, “DR” stands for “Major Disaster” in
column ‘declaration_type’ is the only kind declaration type that I use.
The original dataset includes the year of disasters back to 1953, in
this project, we use data back to 2015 only.

Please note that the dataset also includes biological disasters
originally, in particular declarations made in response to the ongoing
Covid-19 pandemic, but Covid-19 is not a traditional natural disaster
for the analyzing purpose of a development company, therefore it is
removed from the processed dataset too.

#### For population growth:

Original dataset: state_pop_ori.csv, includes the most updated census
data from 2010-2020 which was downloaded from the U.S. Census Bureau.
There were many columns for population detail for that past 10 years,
and in this project we are using 2020 data only.

## Loading Packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.1.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(readr)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

## Prepare data

#### Natural disaster frequency dataset

Loading dataset and view the content

``` r
disasters_files <-read.csv(file = "/cloud/project/us_disaster_declarations.csv",header = T)
head(disasters_files)
```

    ##   X fema_declaration_string disaster_number state declaration_type
    ## 1 1                 DR-1-GA               1    GA               DR
    ## 2 2                 DR-2-TX               2    TX               DR
    ## 3 3                 DR-3-LA               3    LA               DR
    ## 4 4                 DR-4-MI               4    MI               DR
    ## 5 5                 DR-5-MT               5    MT               DR
    ## 6 6                 DR-6-MI               6    MI               DR
    ##      declaration_date fy_declared incident_type        declaration_title
    ## 1 1953-05-02 00:00:00        1953       Tornado                  Tornado
    ## 2 1953-05-15 00:00:00        1953       Tornado Tornado & Heavy Rainfall
    ## 3 1953-05-29 00:00:00        1953         Flood                    Flood
    ## 4 1953-06-02 00:00:00        1953       Tornado                  Tornado
    ## 5 1953-06-06 00:00:00        1953         Flood                   Floods
    ## 6 1953-06-09 00:00:00        1953       Tornado                  Tornado
    ##   ih_program_declared ia_program_declared pa_program_declared
    ## 1                   0                   1                   1
    ## 2                   0                   1                   1
    ## 3                   0                   1                   1
    ## 4                   0                   1                   1
    ## 5                   0                   1                   1
    ## 6                   0                   1                   1
    ##   hm_program_declared incident_begin_date   incident_end_date
    ## 1                   1 1953-05-02 00:00:00 1953-05-02 00:00:00
    ## 2                   1 1953-05-15 00:00:00 1953-05-15 00:00:00
    ## 3                   1 1953-05-29 00:00:00 1953-05-29 00:00:00
    ## 4                   1 1953-06-02 00:00:00 1953-06-02 00:00:00
    ## 5                   1 1953-06-06 00:00:00 1953-06-06 00:00:00
    ## 6                   1 1953-06-09 00:00:00 1953-06-09 00:00:00
    ##   disaster_closeout_date  fips place_code designated_area
    ## 1    1954-06-01 00:00:00 13000          0       Statewide
    ## 2    1958-01-01 00:00:00 48000          0       Statewide
    ## 3    1960-02-01 00:00:00 22000          0       Statewide
    ## 4    1956-02-01 00:00:00 26000          0       Statewide
    ## 5    1955-12-01 00:00:00 30000          0       Statewide
    ## 6    1956-03-30 00:00:00 26000          0       Statewide
    ##   declaration_request_number                                     hash
    ## 1                      53013 bb121323c9c29d3bef0c9a3f134bfd8b5ecff148
    ## 2                      53003 c879557e78d059e6847e7688388c339d10f51979
    ## 3                      53005 4fb19699fdbba1387ffa2263fcc4a4e37a1de6d6
    ## 4                      53004 87a0c1dd5da249767f545e0c0a43f917e4e9ca83
    ## 5                      53006 954449c15634fb45c8bea3ac975782793ccde050
    ## 6                      53007 fd3377e42a13e063a569a9b6fc25872dbeeaf099
    ##          last_refresh                       id
    ## 1 2021-07-13 23:01:19 60c3b7a9a0ee349d71025780
    ## 2 2021-07-13 23:01:19 60c3b7a9a0ee349d71025783
    ## 3 2021-07-13 23:01:19 60c3b7a9a0ee349d71025777
    ## 4 2021-07-13 23:01:19 60c3b7a9a0ee349d7102577a
    ## 5 2021-07-13 23:01:19 60c3b7a9a0ee349d71025774
    ## 6 2021-07-13 23:01:19 60c3b7a9a0ee349d71025766

Extract related columns for what the task needs only and set the year
record start from 2015

``` r
disasters_by_states <- disasters_files %>% 
  select(fy_declared,state, declaration_type, 
         declaration_date,incident_type) %>%   #'fy_declared' is fiscal year declared. 'declaration_date' with date and time included, helped to distinguish same incident type happened in same year.

  filter(declaration_type == "DR" & fy_declared >=2015 & 
          incident_type != "Biological") %>%   #"DR"= major disaster.  Incident_type 'Biological' is covid-19 event so excluded
  rename(event_year = fy_declared) %>%         #make it easier to understand
  arrange(event_year)

head(disasters_by_states)
```

    ##   event_year state declaration_type    declaration_date   incident_type
    ## 1       2015    NM               DR 2014-10-06 19:20:00 Severe Storm(s)
    ## 2       2015    NM               DR 2014-10-06 19:20:00 Severe Storm(s)
    ## 3       2015    NM               DR 2014-10-06 19:20:00 Severe Storm(s)
    ## 4       2015    NM               DR 2014-10-06 19:20:00 Severe Storm(s)
    ## 5       2015    NM               DR 2014-10-06 19:20:00 Severe Storm(s)
    ## 6       2015    NM               DR 2014-10-06 19:20:00 Severe Storm(s)

#### Population growth dataset

Loading dataset and view the content

``` r
state_pop <- read.csv("/cloud/project/state_pop_ori.csv",header = T)
str(state_pop)
```

    ## 'data.frame':    57 obs. of  73 variables:
    ##  $ SUMLEV           : int  10 20 20 20 20 40 40 40 40 40 ...
    ##  $ REGION           : chr  "0" "1" "2" "3" ...
    ##  $ DIVISION         : chr  "0" "0" "0" "0" ...
    ##  $ STATE            : int  0 0 0 0 0 1 2 4 5 6 ...
    ##  $ NAME             : chr  "United States" "Northeast Region" "Midwest Region" "South Region" ...
    ##  $ ESTIMATESBASE2010: int  308758105 55318414 66929737 114563042 71946912 4780118 710246 6392292 2916029 37254522 ...
    ##  $ POPESTIMATE2010  : int  309327143 55380764 66975328 114869421 72101630 4785514 713982 6407342 2921998 37319550 ...
    ##  $ POPESTIMATE2011  : int  311583481 55608318 67164092 116019483 72791588 4799642 722349 6473416 2941038 37636311 ...
    ##  $ POPESTIMATE2012  : int  313877662 55782661 67348275 117264196 73482530 4816632 730810 6556344 2952876 37944551 ...
    ##  $ POPESTIMATE2013  : int  316059947 55912775 67576524 118397213 74173435 4831586 737626 6634690 2960459 38253768 ...
    ##  $ POPESTIMATE2014  : int  318386329 56021339 67765576 119666248 74933166 4843737 737075 6732873 2968759 38586706 ...
    ##  $ POPESTIMATE2015  : int  320738994 56052790 67885682 121049223 75751299 4854803 738430 6832810 2979732 38904296 ...
    ##  $ POPESTIMATE2016  : int  323071755 56063777 68018175 122419547 76570256 4866824 742575 6944767 2991815 39149186 ...
    ##  $ POPESTIMATE2017  : int  325122128 56083383 68160342 123611036 77267367 4877989 740983 7048088 3003855 39337785 ...
    ##  $ POPESTIMATE2018  : int  326838199 56084543 68263019 124649156 77841481 4891628 736624 7164228 3012161 39437463 ...
    ##  $ POPESTIMATE2019  : int  328329953 56002934 68340091 125686544 78300384 4907965 733603 7291843 3020985 39437610 ...
    ##  $ POPESTIMATE2020  : int  329484123 55849869 68316744 126662754 78654756 4921532 731158 7421401 3030522 39368078 ...
    ##  $ NPOPCHG_2010     : int  569038 62350 45591 306379 154718 5396 3736 15050 5969 65028 ...
    ##  $ NPOPCHG_2011     : int  2256338 227554 188764 1150062 689958 14128 8367 66074 19040 316761 ...
    ##  $ NPOPCHG_2012     : int  2294181 174343 184183 1244713 690942 16990 8461 82928 11838 308240 ...
    ##  $ NPOPCHG_2013     : int  2182285 130114 228249 1133017 690905 14954 6816 78346 7583 309217 ...
    ##  $ NPOPCHG_2014     : int  2326382 108564 189052 1269035 759731 12151 -551 98183 8300 332938 ...
    ##  $ NPOPCHG_2015     : int  2352665 31451 120106 1382975 818133 11066 1355 99937 10973 317590 ...
    ##  $ NPOPCHG_2016     : int  2332761 10987 132493 1370324 818957 12021 4145 111957 12083 244890 ...
    ##  $ NPOPCHG_2017     : int  2050373 19606 142167 1191489 697111 11165 -1592 103321 12040 188599 ...
    ##  $ NPOPCHG_2018     : int  1716071 1160 102677 1038120 574114 13639 -4359 116140 8306 99678 ...
    ##  $ NPOPCHG_2019     : int  1491754 -81609 77072 1037388 458903 16337 -3021 127615 8824 147 ...
    ##  $ NPOPCHG_2020     : int  1154170 -153065 -23347 976210 354372 13567 -2445 129558 9537 -69532 ...
    ##  $ PPOPCHG_2010     : num  0.1843 0.1127 0.0681 0.2674 0.215 ...
    ##  $ PPOPCHG_2011     : num  0.729 0.411 0.282 1.001 0.957 ...
    ##  $ PPOPCHG_2012     : num  0.736 0.314 0.274 1.073 0.949 ...
    ##  $ PPOPCHG_2013     : num  0.695 0.233 0.339 0.966 0.94 ...
    ##  $ PPOPCHG_2014     : num  0.736 0.194 0.28 1.072 1.024 ...
    ##  $ PPOPCHG_2015     : num  0.7389 0.0561 0.1772 1.1557 1.0918 ...
    ##  $ PPOPCHG_2016     : num  0.7273 0.0196 0.1952 1.132 1.0811 ...
    ##  $ PPOPCHG_2017     : num  0.635 0.035 0.209 0.973 0.91 ...
    ##  $ PPOPCHG_2018     : num  0.52782 0.00207 0.15064 0.83983 0.74302 ...
    ##  $ PPOPCHG_2019     : num  0.456 -0.146 0.113 0.832 0.59 ...
    ##  $ PPOPCHG_2020     : num  0.3515 -0.2733 -0.0342 0.7767 0.4526 ...
    ##  $ NRANK_ESTBASE2010: chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2010 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2011 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2012 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2013 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2014 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2015 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2016 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2017 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2018 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2019 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_POPEST2020 : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2010  : chr  "X" "3" "4" "1" ...
    ##  $ NRANK_NPCHG2011  : chr  "X" "3" "4" "1" ...
    ##  $ NRANK_NPCHG2012  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2013  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2014  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2015  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2016  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2017  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2018  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2019  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_NPCHG2020  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2010  : chr  "X" "3" "4" "1" ...
    ##  $ NRANK_PPCHG2011  : chr  "X" "3" "4" "1" ...
    ##  $ NRANK_PPCHG2012  : chr  "X" "3" "4" "1" ...
    ##  $ NRANK_PPCHG2013  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2014  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2015  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2016  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2017  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2018  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2019  : chr  "X" "4" "3" "1" ...
    ##  $ NRANK_PPCHG2020  : chr  "X" "4" "3" "1" ...

Extract related columns for what the task needs only and set the year as
2020, the latest census data.

``` r
#Variables that we need for this project:
#'NAME' is states, and 
#'POPESTIMATE2020' is population total, and
#'PPOPCHG_2020' is population change in %

state_pop_selected <- state_pop %>% 
  select(NAME,POPESTIMATE2020,PPOPCHG_2020) %>% 
  rename(state = NAME, popest_2020 = POPESTIMATE2020, popchg_2020 = 
           PPOPCHG_2020) %>% 
  slice(6:57) %>%           #removing rows of United States and 4 regions
  arrange(desc(popchg_2020))
head(state_pop_selected)
```

    ##            state popest_2020 popchg_2020
    ## 1          Idaho     1826913    2.115804
    ## 2        Arizona     7421401    1.776752
    ## 3         Nevada     3138259    1.536445
    ## 4           Utah     3249879    1.451466
    ## 5          Texas    29360759    1.290122
    ## 6 South Carolina     5218040    1.169862

## Cleaning data

View values for each both datasets to find out the abnormal value

``` r
unique(disasters_by_states$event_year)        #No abnormal value
```

    ## [1] 2015 2016 2017 2018 2019 2020 2021

``` r
unique(disasters_by_states$state)             #No abnormal value           
```

    ##  [1] "NM" "MT" "MO" "HI" "NV" "AZ" "NY" "MS" "CA" "VT" "ME" "NH" "WV" "TN" "RI"
    ## [16] "CT" "MA" "GA" "KY" "OK" "TX" "GU" "NE" "AR" "WY" "LA" "CO" "KS" "NJ" "SD"
    ## [31] "IA" "MP" "SC" "WA" "AK" "ID" "AL" "OR" "DC" "MD" "VA" "DE" "PA" "WI" "FL"
    ## [46] "NC" "MN" "UT" "ND" "MI" "VI" "PR" "AS" "OH" "IN" "IL"

``` r
unique(disasters_by_states$declaration_date)  #No abnormal value
```

    ##   [1] "2014-10-06 19:20:00" "2014-10-09 21:35:00" "2014-10-29 09:00:00"
    ##   [4] "2014-10-31 13:10:00" "2014-11-03 17:00:00" "2014-11-05 12:00:00"
    ##   [7] "2014-11-05 18:00:00" "2014-12-22 09:56:00" "2015-01-07 15:18:00"
    ##  [10] "2015-01-27 12:24:00" "2015-02-03 17:00:00" "2015-03-12 13:55:00"
    ##  [13] "2015-03-25 12:45:00" "2015-03-31 17:06:00" "2015-04-02 09:20:00"
    ##  [16] "2015-04-03 14:30:00" "2015-04-08 16:35:00" "2015-04-13 16:35:00"
    ##  [19] "2015-04-20 12:40:00" "2015-04-30 17:35:00" "2015-05-01 17:37:00"
    ##  [22] "2015-05-12 17:12:00" "2015-05-14 18:05:00" "2015-05-18 11:25:00"
    ##  [25] "2015-05-21 19:30:00" "2015-05-26 13:31:00" "2015-05-29 23:02:00"
    ##  [28] "2015-06-05 18:10:00" "2015-06-25 14:08:00" "2015-06-26 11:37:00"
    ##  [31] "2015-07-07 20:55:00" "2015-07-13 16:58:00" "2015-07-16 14:32:00"
    ##  [34] "2015-07-20 16:00:00" "2015-07-22 15:30:00" "2015-07-29 19:55:00"
    ##  [37] "2015-07-30 17:00:00" "2015-07-31 14:58:00" "2015-08-05 19:54:00"
    ##  [40] "2015-08-07 15:37:00" "2015-08-07 15:45:00" "2015-08-07 20:00:00"
    ##  [43] "2015-08-12 12:10:00" "2015-09-22 17:21:00" "2015-10-05 18:30:00"
    ##  [46] "2015-10-15 19:00:00" "2015-10-20 19:05:00" "2015-10-30 17:44:00"
    ##  [49] "2015-11-25 11:00:00" "2015-12-23 09:11:00" "2015-12-29 18:00:00"
    ##  [52] "2016-01-04 13:02:00" "2016-01-15 19:27:00" "2016-01-21 18:18:00"
    ##  [55] "2016-01-21 20:30:00" "2016-02-01 17:00:00" "2016-02-02 17:27:00"
    ##  [58] "2016-02-05 19:09:00" "2016-02-09 10:45:00" "2016-02-10 18:16:00"
    ##  [61] "2016-02-17 11:30:00" "2016-02-17 20:10:00" "2016-02-26 16:35:00"
    ##  [64] "2016-03-04 13:50:00" "2016-03-04 14:35:00" "2016-03-07 18:07:00"
    ##  [67] "2016-03-13 16:41:00" "2016-03-14 19:37:00" "2016-03-16 18:45:00"
    ##  [70] "2016-03-19 17:35:00" "2016-03-23 16:20:00" "2016-03-25 16:00:00"
    ##  [73] "2016-04-25 15:46:00" "2016-05-06 12:45:00" "2016-05-24 13:35:00"
    ##  [76] "2016-06-11 13:23:00" "2016-06-25 15:24:00" "2016-07-15 12:15:00"
    ##  [79] "2016-08-03 18:45:00" "2016-08-09 18:45:00" "2016-08-14 18:45:00"
    ##  [82] "2016-08-26 16:23:00" "2016-09-16 10:30:00" "2016-09-28 12:18:00"
    ##  [85] "2016-09-29 18:45:00" "2016-10-06 17:00:00" "2016-10-08 20:53:00"
    ##  [88] "2016-10-08 20:59:00" "2016-10-10 16:27:00" "2016-10-11 14:30:00"
    ##  [91] "2016-10-20 10:15:00" "2016-10-20 11:38:00" "2016-10-31 18:35:00"
    ##  [94] "2016-11-02 10:30:00" "2016-11-02 12:15:00" "2016-12-02 09:46:00"
    ##  [97] "2016-12-15 18:00:00" "2017-01-25 13:58:00" "2017-01-25 14:30:00"
    ## [100] "2017-01-25 14:35:00" "2017-01-26 11:30:00" "2017-02-01 16:00:00"
    ## [103] "2017-02-10 14:09:00" "2017-02-11 14:30:00" "2017-02-14 18:15:00"
    ## [106] "2017-02-14 19:30:00" "2017-02-17 20:30:00" "2017-02-24 09:50:00"
    ## [109] "2017-03-16 08:48:00" "2017-03-21 20:47:00" "2017-03-27 21:45:00"
    ## [112] "2017-04-01 20:55:00" "2017-04-21 08:35:00" "2017-04-21 11:17:00"
    ## [115] "2017-05-02 18:00:00" "2017-05-18 13:40:00" "2017-05-22 14:30:00"
    ## [118] "2017-05-26 15:30:00" "2017-06-01 14:49:00" "2017-06-02 16:56:00"
    ## [121] "2017-06-15 08:55:00" "2017-06-16 11:51:00" "2017-06-23 19:20:00"
    ## [124] "2017-06-26 16:45:00" "2017-07-12 08:00:00" "2017-07-12 08:30:00"
    ## [127] "2017-07-25 19:00:00" "2017-08-01 12:15:00" "2017-08-02 10:05:00"
    ## [130] "2017-08-05 17:30:00" "2017-08-08 10:32:00" "2017-08-09 14:30:00"
    ## [133] "2017-08-16 19:00:00" "2017-08-18 22:15:00" "2017-08-25 21:35:00"
    ## [136] "2017-08-27 14:15:00" "2017-08-27 16:15:00" "2017-09-07 12:15:00"
    ## [139] "2017-09-10 08:05:00" "2017-09-10 14:45:00" "2017-09-15 20:25:00"
    ## [142] "2017-09-20 21:00:00" "2017-09-20 22:30:00" "2017-09-27 15:00:00"
    ## [145] "2017-10-07 14:25:00" "2017-10-07 16:10:00" "2017-10-10 12:40:00"
    ## [148] "2017-10-16 13:00:00" "2017-10-16 15:45:00" "2017-11-07 20:35:00"
    ## [151] "2017-11-14 07:00:00" "2017-11-16 19:45:00" "2017-11-22 16:00:00"
    ## [154] "2017-12-20 19:35:00" "2017-12-20 19:50:00" "2018-01-02 11:00:00"
    ## [157] "2018-01-02 20:25:00" "2018-01-02 20:30:00" "2018-01-02 20:40:00"
    ## [160] "2018-03-02 18:45:00" "2018-04-12 11:15:00" "2018-04-17 12:30:00"
    ## [163] "2018-04-17 14:00:00" "2018-04-26 18:35:00" "2018-04-26 19:20:00"
    ## [166] "2018-05-04 23:54:00" "2018-05-08 09:10:00" "2018-05-08 13:25:00"
    ## [169] "2018-05-11 20:41:00" "2018-05-30 09:17:00" "2018-06-08 18:04:00"
    ## [172] "2018-06-08 19:05:00" "2018-06-08 19:13:00" "2018-06-08 19:15:00"
    ## [175] "2018-06-25 09:20:00" "2018-06-25 09:21:00" "2018-06-25 09:22:00"
    ## [178] "2018-06-29 12:45:00" "2018-07-02 21:15:00" "2018-07-06 19:00:00"
    ## [181] "2018-07-12 10:58:00" "2018-07-19 08:40:00" "2018-07-30 10:41:00"
    ## [184] "2018-08-02 21:45:00" "2018-08-04 21:46:00" "2018-08-10 16:30:00"
    ## [187] "2018-08-17 12:33:00" "2018-08-20 12:01:00" "2018-08-20 12:50:00"
    ## [190] "2018-08-27 11:18:00" "2018-08-30 10:12:00" "2018-08-31 11:55:00"
    ## [193] "2018-09-05 12:15:00" "2018-09-05 14:14:00" "2018-09-12 12:55:00"
    ## [196] "2018-09-14 23:00:00" "2018-09-16 22:57:00" "2018-09-27 12:00:00"
    ## [199] "2018-09-29 13:15:00" "2018-10-01 12:56:00" "2018-10-01 17:10:00"
    ## [202] "2018-10-11 10:15:00" "2018-10-14 22:13:00" "2018-10-15 09:10:00"
    ## [205] "2018-10-18 12:15:00" "2018-10-19 09:55:00" "2018-10-26 18:20:00"
    ## [208] "2018-10-31 06:53:00" "2018-11-05 08:30:00" "2018-11-12 20:28:00"
    ## [211] "2018-11-27 13:15:00" "2018-11-30 11:09:00" "2018-12-05 11:03:00"
    ## [214] "2018-12-18 09:34:00" "2019-01-31 16:19:00" "2019-01-31 16:30:00"
    ## [217] "2019-02-01 11:00:00" "2019-02-14 16:10:00" "2019-02-25 11:25:00"
    ## [220] "2019-02-25 11:26:00" "2019-03-04 14:38:00" "2019-03-05 17:33:00"
    ## [223] "2019-03-21 13:35:00" "2019-03-23 14:35:00" "2019-03-26 14:00:00"
    ## [226] "2019-03-28 16:50:00" "2019-04-08 10:10:00" "2019-04-08 11:00:00"
    ## [229] "2019-04-17 12:39:00" "2019-04-17 14:53:00" "2019-04-17 14:56:00"
    ## [232] "2019-04-23 16:15:00" "2019-04-29 08:40:00" "2019-05-01 16:10:00"
    ## [235] "2019-05-02 10:55:00" "2019-05-07 14:20:00" "2019-05-17 23:59:00"
    ## [238] "2019-05-20 17:35:00" "2019-05-21 13:30:00" "2019-05-24 14:15:00"
    ## [241] "2019-06-01 21:30:00" "2019-06-03 13:17:00" "2019-06-07 22:41:00"
    ## [244] "2019-06-08 11:18:00" "2019-06-12 11:49:00" "2019-06-12 13:21:00"
    ## [247] "2019-06-12 13:38:00" "2019-06-14 14:38:00" "2019-06-17 11:02:00"
    ## [250] "2019-06-18 09:23:00" "2019-06-20 15:52:00" "2019-06-20 16:01:00"
    ## [253] "2019-06-20 16:05:00" "2019-07-09 11:05:00" "2019-07-09 11:24:00"
    ## [256] "2019-07-12 11:42:00" "2019-07-17 15:09:00" "2019-08-02 23:41:00"
    ## [259] "2019-08-07 09:40:00" "2019-08-15 21:15:00" "2019-08-27 11:37:00"
    ## [262] "2019-08-27 12:42:00" "2019-09-13 13:14:00" "2019-09-19 09:53:00"
    ## [265] "2019-09-19 11:00:00" "2019-09-23 12:21:00" "2019-09-30 13:11:00"
    ## [268] "2019-10-04 12:02:00" "2019-10-04 14:25:00" "2019-10-07 16:58:00"
    ## [271] "2019-10-21 19:57:00" "2019-11-18 09:48:00" "2019-12-06 09:50:00"
    ## [274] "2019-12-06 09:54:00" "2019-12-19 16:50:00" "2020-01-16 14:48:00"
    ## [277] "2020-01-17 10:25:00" "2020-01-21 12:17:00" "2020-03-05 18:28:00"
    ## [280] "2020-03-11 14:44:00" "2020-03-12 17:00:00" "2020-03-17 10:00:00"
    ## [283] "2020-04-03 18:05:00" "2020-04-16 21:25:00" "2020-04-23 15:05:00"
    ## [286] "2020-04-23 15:20:00" "2020-04-24 11:25:00" "2020-04-24 11:55:00"
    ## [289] "2020-05-01 21:25:00" "2020-05-08 15:05:00" "2020-05-08 15:20:00"
    ## [292] "2020-05-21 13:27:00" "2020-07-09 18:45:00" "2020-07-09 19:25:00"
    ## [295] "2020-07-09 19:55:00" "2020-07-09 20:00:00" "2020-07-09 20:10:00"
    ## [298] "2020-07-09 20:20:00" "2020-07-10 14:25:00" "2020-07-10 15:10:00"
    ## [301] "2020-07-10 15:20:00" "2020-08-17 16:20:00" "2020-08-22 16:00:00"
    ## [304] "2020-08-28 21:12:00" "2020-09-09 16:20:00" "2020-09-10 20:30:00"
    ## [307] "2020-09-15 18:25:00" "2020-09-20 12:40:00" "2020-09-23 21:40:00"
    ## [310] "2020-10-02 10:45:00" "2020-10-02 11:00:00" "2020-10-02 17:20:00"
    ## [313] "2020-10-14 10:25:00" "2020-10-16 20:49:00" "2020-11-05 16:30:00"
    ## [316] "2020-12-09 16:25:00" "2020-12-10 17:05:00" "2020-12-11 18:40:00"
    ## [319] "2020-12-21 12:45:00" "2020-12-31 21:35:00" "2021-01-12 12:58:00"
    ## [322] "2021-01-12 13:10:00" "2021-01-12 13:15:00" "2021-01-12 20:36:00"
    ## [325] "2021-01-15 14:36:00" "2021-02-04 10:05:00" "2021-02-04 18:08:00"
    ## [328] "2021-02-17 11:44:00" "2021-02-19 23:37:00" "2021-02-24 18:51:00"
    ## [331] "2021-03-03 23:00:00" "2021-03-04 15:24:00" "2021-03-09 10:08:00"
    ## [334] "2021-03-31 17:58:00" "2021-04-08 08:08:00" "2021-04-21 17:31:00"
    ## [337] "2021-04-23 15:44:00" "2021-04-26 12:22:00" "2021-04-28 08:51:00"
    ## [340] "2021-05-04 09:00:00" "2021-05-04 12:06:00" "2021-05-05 17:25:00"
    ## [343] "2021-05-08 10:15:00" "2021-05-10 11:49:00" "2021-05-13 09:47:00"
    ## [346] "2021-05-13 09:53:00" "2021-05-20 21:11:00" "2021-06-02 16:40:00"
    ## [349] "2021-07-15 10:41:00" "2021-08-13 09:29:00" "2021-08-23 22:40:00"
    ## [352] "2021-08-24 20:45:00" "2021-08-29 22:30:00" "2021-09-01 16:46:00"
    ## [355] "2021-09-01 16:50:00" "2021-09-05 22:45:00" "2021-09-05 23:36:00"
    ## [358] "2021-09-06 15:46:00" "2021-09-08 11:28:00" "2021-09-10 19:05:00"
    ## [361] "2021-09-12 21:12:00" "2021-09-13 12:42:00" "2021-09-29 15:09:00"

``` r
unique(disasters_by_states$incident_type)     #No abnormal value
```

    ##  [1] "Severe Storm(s)"  "Volcano"          "Flood"            "Snow"            
    ##  [5] "Tornado"          "Severe Ice Storm" "Typhoon"          "Fire"            
    ##  [9] "Coastal Storm"    "Hurricane"        "Mud/Landslide"    "Earthquake"      
    ## [13] "Dam/Levee Break"  "Other"

``` r
unique(disasters_by_states$declaration_type)  #No abnormal value
```

    ## [1] "DR"

``` r
unique(state_pop_selected$state)              #No abnormal value
```

    ##  [1] "Idaho"                "Arizona"              "Nevada"              
    ##  [4] "Utah"                 "Texas"                "South Carolina"      
    ##  [7] "Florida"              "Washington"           "Delaware"            
    ## [10] "Montana"              "North Carolina"       "Colorado"            
    ## [13] "Tennessee"            "Georgia"              "District of Columbia"
    ## [16] "South Dakota"         "Oregon"               "Oklahoma"            
    ## [19] "New Hampshire"        "Virginia"             "Wyoming"             
    ## [22] "Indiana"              "Maine"                "New Mexico"          
    ## [25] "Arkansas"             "Minnesota"            "Alabama"             
    ## [28] "Nebraska"             "North Dakota"         "Missouri"            
    ## [31] "Wisconsin"            "Iowa"                 "Kentucky"            
    ## [34] "Kansas"               "Maryland"             "Massachusetts"       
    ## [37] "Ohio"                 "Rhode Island"         "New Jersey"          
    ## [40] "Vermont"              "Pennsylvania"         "California"          
    ## [43] "Michigan"             "Connecticut"          "Louisiana"           
    ## [46] "Alaska"               "Mississippi"          "West Virginia"       
    ## [49] "Hawaii"               "Illinois"             "New York"            
    ## [52] "Puerto Rico"

``` r
unique(state_pop_selected$popest_2020)        #No abnormal value
```

    ##  [1]  1826913  7421401  3138259  3249879 29360759  5218040 21733312  7693612
    ##  [9]   986809  1080577 10600823  5807719  6886834 10710017   712816   892717
    ## [17]  4241507  3980783  1366275  8590563   582328  6754953  1350141  2106319
    ## [25]  3030522  5657342  4921532  1937552   765309  6151548  5832655  3163561
    ## [33]  4477251  2913805  6055802  6893574 11693217  1057125  8882371   623347
    ## [41] 12783254 39368078  9966555  3557006  4645318   731158  2966786  1784787
    ## [49]  1407006 12587530 19336776  3159343

``` r
unique(state_pop_selected$popchg_2020)        #No abnormal value
```

    ##  [1]  2.11580383  1.77675246  1.53644511  1.45146553  1.29012198  1.16986208
    ##  [7]  1.12253569  1.04528171  1.03832623  0.97689705  0.94691328  0.85496431
    ## [13]  0.82732520  0.77151718  0.64426130  0.63012398  0.60223675  0.50766586
    ## [19]  0.40359117  0.39642888  0.38130305  0.35571185  0.32479547  0.31838882
    ## [25]  0.31569174  0.30653967  0.27642821  0.25773956  0.20753571  0.18032807
    ## [31]  0.13861941  0.12549073  0.10969637  0.04016981  0.01400506 -0.01898509
    ## [37] -0.02812806 -0.09762247 -0.09995211 -0.11201097 -0.12211222 -0.17630886
    ## [43] -0.18267776 -0.25283075 -0.27836425 -0.33328653 -0.38415473 -0.58353567
    ## [49] -0.60814558 -0.62751159 -0.64920182 -1.07122067

Check for duplicates and remove duplicate rows

``` r
str(disasters_by_states)  
```

    ## 'data.frame':    6370 obs. of  5 variables:
    ##  $ event_year      : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ state           : chr  "NM" "NM" "NM" "NM" ...
    ##  $ declaration_type: chr  "DR" "DR" "DR" "DR" ...
    ##  $ declaration_date: chr  "2014-10-06 19:20:00" "2014-10-06 19:20:00" "2014-10-06 19:20:00" "2014-10-06 19:20:00" ...
    ##  $ incident_type   : chr  "Severe Storm(s)" "Severe Storm(s)" "Severe Storm(s)" "Severe Storm(s)" ...

``` r
sum(duplicated(disasters_by_states))  #6,004 duplicate due to same incident recorded on different counties of a state
```

    ## [1] 6004

``` r
sum(duplicated(state_pop_selected))  #0 duplicate for population dataset
```

    ## [1] 0

Remove duplicate for ‘disasters_by_states’ dataset

``` r
clean_disasters_by_states <- disasters_by_states %>% 
  distinct() %>% 
  drop_na()
str(clean_disasters_by_states)    #366 rows remaining
```

    ## 'data.frame':    366 obs. of  5 variables:
    ##  $ event_year      : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ state           : chr  "NM" "MT" "NM" "MO" ...
    ##  $ declaration_type: chr  "DR" "DR" "DR" "DR" ...
    ##  $ declaration_date: chr  "2014-10-06 19:20:00" "2014-10-09 21:35:00" "2014-10-29 09:00:00" "2014-10-31 13:10:00" ...
    ##  $ incident_type   : chr  "Severe Storm(s)" "Severe Storm(s)" "Severe Storm(s)" "Severe Storm(s)" ...

## Data analyzing

#### See how many disasters happened for recent years for each state

``` r
disaster_yearly_by_state <- clean_disasters_by_states %>%
  select(state,event_year,incident_type) %>%
  arrange(state,desc(event_year)) %>%
  pivot_wider(names_from = event_year,values_from =   #Making year as column header 
              incident_type,values_fn =   
                list(incident_type=length))          
print(disaster_yearly_by_state)
```

    ## # A tibble: 56 × 8
    ##    state `2021` `2019` `2018` `2016` `2020` `2017` `2015`
    ##    <chr>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
    ##  1 AK         1      1      3      2     NA     NA     NA
    ##  2 AL         2      3      2      1      4     NA     NA
    ##  3 AR        NA      2     NA      2      2      1      1
    ##  4 AS        NA     NA      1     NA     NA     NA     NA
    ##  5 AZ         1      2      1     NA     NA     NA      1
    ##  6 CA         3      6      3     NA      1      5      2
    ##  7 CO         1     NA     NA     NA     NA     NA      1
    ##  8 CT         1      1      1     NA     NA     NA      1
    ##  9 DC        NA     NA     NA      1     NA     NA     NA
    ## 10 DE         1     NA     NA      1     NA     NA     NA
    ## # … with 46 more rows

#### Figure out which states have the highest disaster count

``` r
highest_disaster_count_by_state <- clean_disasters_by_states %>% 
  group_by(state) %>% 
  summarise(disasters_count = n()) %>%  
  arrange(desc(disasters_count))   
highest_disaster_count_by_state
```

    ## # A tibble: 56 × 2
    ##    state disasters_count
    ##    <chr>           <int>
    ##  1 CA                 20
    ##  2 MS                 16
    ##  3 LA                 14
    ##  4 OK                 13
    ##  5 TX                 13
    ##  6 AL                 12
    ##  7 WV                 12
    ##  8 KY                 11
    ##  9 TN                 11
    ## 10 IA                 10
    ## # … with 46 more rows

#### Common disasters types for each state

``` r
frequent_disaster_type_by_states <- clean_disasters_by_states %>% 
  group_by(state,incident_type) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = incident_type,values_from = n)   #Making disaster type as header
```

    ## `summarise()` has grouped output by 'state'. You can override using the `.groups` argument.

``` r
frequent_disaster_type_by_states
```

    ## # A tibble: 56 × 15
    ## # Groups:   state [56]
    ##    state `Coastal Storm` Earthquake Flood `Mud/Landslide` `Severe Storm(s)`
    ##    <chr>           <int>      <int> <int>           <int>             <int>
    ##  1 AK                  2          1     1               1                 2
    ##  2 AL                 NA         NA    NA              NA                 7
    ##  3 AR                 NA         NA     2              NA                 4
    ##  4 AS                 NA         NA    NA              NA                NA
    ##  5 AZ                 NA         NA     3              NA                 2
    ##  6 CA                 NA         NA     3              NA                 8
    ##  7 CO                 NA         NA     1              NA                NA
    ##  8 CT                 NA         NA    NA              NA                 2
    ##  9 DC                 NA         NA    NA              NA                NA
    ## 10 DE                 NA         NA    NA              NA                NA
    ## # … with 46 more rows, and 9 more variables: Hurricane <int>, Tornado <int>,
    ## #   Fire <int>, Snow <int>, Severe Ice Storm <int>, Typhoon <int>,
    ## #   Volcano <int>, Dam/Levee Break <int>, Other <int>

**What we learn from these tables:**

1.  According to the “disaster_yearly_by_state” table, we did see that
    natural disasters are happening more frequently for past 2 or 3
    years.

2.  The “highest_disaster_count_by_state” table showed us that the most
    dangerous states are California and Southern region states.

3.  The “frequent_disaster_type_by_states” table can help us to avoid
    states with more devastating disaster type, like: fire in
    California, tornado in Texas…etc.

## Visualization

#### Overall disasters type and counting of states for 2015-2021

``` r
ggplot(clean_disasters_by_states,aes(x = fct_infreq(state),      #fct_infreq for descending order 
  fill=incident_type)) + geom_bar(stat = 'count')+
  theme_classic()+
   theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Major Disasters for States 2015-2021", x = "State", y =  
               "Count",fill= "Disaster Type")
```

![](Capstone-Case-Study-v2-for-Github_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

#### See the distribution of disaster types for each state

``` r
clean_disasters_by_states %>% 
ggplot(aes(x = event_year, fill=incident_type))+
  geom_bar()+
  facet_wrap(~state)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Disasters Types for States 2015-2021", x = "Event Year", 
       y ="Count",fill= "Disaster Type")
```

![](Capstone-Case-Study-v2-for-Github_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Export dataset for map making in Tableau

``` r
write.csv(clean_disasters_by_states,file = "disasters_by_states.csv")
write.csv(state_pop_selected,file = "state_population.csv")
```

#### Disaster intensity by states 2015-2021

![](images/disaster_intensity_map.png)

#### U.S. population distribution and change 2010-2020

![](images/population.png)

**What we learn from these visualizations:**

1.  Top 10 states that with the most disasters accounts for about 40% of
    total disasters. They are: California, Mississippi, Louisiana,
    Oklahoma, Texas, Alabama, West Virginia, Kentucky, Tennessee, Iowa.
    Means

2.  Top 10 states that with the most disasters accounts for 31%
    population in the U.S., more than 100 million people. (See note 1
    below)

3.  The most dangerous states and the most populated states are highly
    overlapping,

4.  These findings indicating that there will be a significant needs for
    relocation to safer states in the near future and developers will be
    playing a crucial role when this happening.

note 1:Calculation for top 10 disaster prone states population
percentage in whole nation

``` r
 us_total_pop <- 329484123     #2020 US total population from Census Bureau

top_10_pop <- state_pop_selected %>% 
  filter(state %in% c("California", "Mississippi", "Louisiana", 
   "Oklahoma", "Texas", "Alabama", "West Virginia", "Kentucky", "Tennessee", 
   "Iowa")) %>% 
  summarise(top_10_pop_total=sum(popest_2020)) 

top_10_pop <-101555689
top_10_pop_percentage <- (top_10_pop / us_total_pop)*100
top_10_pop_percentage
```

    ## [1] 30.82264

## Conclusion

Since 2015, we’ve seen more and more natural disasters due to climate
change happening in the United States, undoubtedly it has become the new
normal. It’s no longer just something that we see in the news only, it
indeed impacts every single aspect of our life now, especially when it
comes to where we live, because the effect of climate change is highly
geographical. Therefore considering the effect of climate change is
absolutely necessary for developers today.

After the detailed analysis for natural disaster location history and
compare that with the population dataset, I would recommend these couple
states have the greatest potential to be developed in the future, they
are:

-   Midwest region states:

1.  Michigan
2.  Ohio
3.  Indiana
4.  Illinois

-   West region states:

1.  Colorado
2.  Utah
3.  Arizona
4.  New Mexico

![U.S. Population Distribution and
Growth](images/disaster_intensity_map_select.png)

-   Why these 8 states:

1.  They are among the least impacted states by natural disasters.

2.  Even there are disasters coming, they are usually less devastating
    one, extremely  
    dangerous disaster like fire or tornado are less likely happening.

3.  The population of 4 states in the West region is growing and more
    corporations is establishing.

4.  While the population of those states in the Midwest region is mostly
    decreasing, the present population is still more than 41 millions in
    the total of 4 states. (See note 2 below)

note 2: Calculation of population of the 4 states in the Midwest

``` r
midwest_pop <- state_pop_selected %>% 
   filter(state %in% c("Michigan","Ohio","Indiana", "Illinois")) %>% 
   summarise(pop_total=sum(popest_2020))
 midwest_pop
```

    ##   pop_total
    ## 1  41002255

*This is the end of the project, hope you enjoy my analysis! Any
question please let me know!* *Thank you!*

*Yi-Chien Tung*

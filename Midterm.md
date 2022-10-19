Midterm
================
Yiping Li
2022-10-19

Requirement: formulate a clear and concise question to answer and
conduct data wrangling, exploratory data analysis, and data
visualization to explore/answer this question.

``` r
library(readxl)
library(plyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Introduction: Interested in the association between UPFs consumption and
type 2 diabetes among young adults (related variables)

Method (put down step by step, plots):

``` r
metaair <- readRDS("/Users/yiping/Desktop/Epi research/OneDrive - University of Southern California/MetaChem/metaair.rds")
UPFMean_grams <- read_excel("/Users/yiping/Desktop/Epi research/USC-Liz/independent research - UPF/UPF% calculation2/UPFgrams_baseline.xlsx")
```

``` r
myvars <- c("ID", "cage", "male", "race2", "diabetesbinary",
            "gluc_fasting", "gluc_120min", "hba1c", "HEI", "MDS", "DII", "DASH")

subsetmetachem <- metaair[myvars]
```

``` r
subsetmetachem$race2 <- revalue(subsetmetachem$race2, c("White"="1", "Hispanic"="2", "Other"="3"))

subsetmetachem$diabetesbinary <- revalue(subsetmetachem$diabetesbinary, c("no diabetes"="0", "T2D or prediabetes"="1"))
```

``` r
vars <- merge(subsetmetachem, UPFMean_grams)
```

``` r
vars$UPFcat <- cut(vars$UPFpercentage_mean,
                        breaks=c(0, 75, 100),
                        labels=c("0", "1"))

vars$UPFquart <- cut(vars$UPFpercentage_mean,
                          breaks=c(0, 25, 50, 75, 100),
                          labels=c("1", "2", "3", "4"))
```

``` r
names(vars)[2] <- "Age"
names(vars)[3] <- "Male"
names(vars)[4] <- "Race"
names(vars)[5] <- "DiabetesCat"
names(vars)[6] <- "Gluc_Fasting"
names(vars)[7] <- "Gluc_120min"
names(vars)[8] <- "HbA1C"
```

EDA:

``` r
#skimr package

vars <- vars %>%
  filter(!is.na(DiabetesCat))

vars <- vars %>%
  filter(Age > 18)
```

Data Visualization:

Conclusion:

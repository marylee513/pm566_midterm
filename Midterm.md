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

``` r
library(skimr)
library(ggplot2)
```

Introduction: Interested in the association between UPFs consumption and
type 2 diabetes among young adults (related variables)

Method (put down step by step, plots):

``` r
metaair <- readRDS("/Users/yiping/Desktop/Epi research/OneDrive - University of Southern California/MetaChem/metaair.rds")
UPFMean_grams <- read_excel("/Users/yiping/Desktop/Epi research/USC-Liz/independent research - UPF/UPF% calculation2/UPFgrams_baseline.xlsx")
```

``` r
myvars <- c("ID", "cage", "male", "race2", "bmiclass", "diabetesbinary", "gluc_fasting", "gluc_120min", "hba1c")
subsetmetachem <- metaair[myvars]
```

``` r
subsetmetachem$race2 <- revalue(subsetmetachem$race2, c("White"="1", "Hispanic"="2", "Other"="3"))
subsetmetachem$diabetesbinary <- revalue(subsetmetachem$diabetesbinary, c("no diabetes"="0", "T2D or prediabetes"="1"))
subsetmetachem$bmiclass <- revalue(subsetmetachem$bmiclass, c("Normal"="1", "Overweight"="2", "Obese"="3"))
```

``` r
vars <- merge(subsetmetachem, UPFMean_grams)
```

``` r
vars$UPF_quart <- cut(vars$UPFpercentage_mean,
                          breaks=c(0, 25, 50, 75, 100),
                          labels=c("1", "2", "3", "4"))

vars$UPF_cat <- cut(vars$UPFpercentage_mean,
                             breaks=c(0, 75, 100),
                             labels=c("0", "1"))
```

``` r
names(vars)[2] <- "Age"
names(vars)[3] <- "Male"
names(vars)[4] <- "Race"
names(vars)[5] <- "BMI"
names(vars)[6] <-  "Diabetes_cat"
names(vars)[7] <- "Gluc_Fasting"
names(vars)[8] <- "Gluc_120min"
names(vars)[9] <- "HbA1C"
```

EDA:

``` r
vars <- vars %>%
  filter(!is.na(Diabetes_cat)) %>%
  filter(Gluc_Fasting < 200) %>%
  filter(Gluc_120min < 400)
```

``` r
table1 <- vars %>%
  group_by(UPF_quart) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table1)
```

| UPF_quart | Gluc_Fasting_min | Gluc_Fasting_max | Gluc_120min_min | GLuc_120min_max | Gluc_num |
|:----------|-----------------:|-----------------:|----------------:|----------------:|---------:|
| 1         |               76 |              119 |              55 |             208 |      105 |
| 2         |               74 |              110 |              70 |             200 |       43 |
| 3         |               76 |               95 |              99 |             131 |        4 |
| 4         |               91 |               91 |             135 |             135 |        1 |

``` r
table2 <- vars %>%
  group_by(BMI) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table2)
```

| BMI | Gluc_Fasting_min | Gluc_Fasting_max | Gluc_120min_min | GLuc_120min_max | Gluc_num |
|:----|-----------------:|-----------------:|----------------:|----------------:|---------:|
| 1   |               74 |              107 |              55 |             176 |       24 |
| 2   |               76 |              107 |              55 |             200 |       73 |
| 3   |               77 |              119 |              83 |             208 |       56 |

``` r
vars <- vars %>%
  mutate(UPF_BMI = factor(
    case_when(UPF_cat == 0 & BMI == 1 ~ "Low UPF Normal Weight",
              UPF_cat == 0 & BMI == 2 ~ "Low UPF Overweight",
              UPF_cat == 0 & BMI == 3 ~ "Low UPF Obese",
              UPF_cat == 1 & BMI == 1 ~ "Low UPF Normal Weight",
              UPF_cat == 1 & BMI == 2 ~ "Low UPF Overweight",
              UPF_cat == 1 & BMI == 3 ~ "Low UPF Obese")
  ))

vars<- vars %>%
  filter(!is.na(UPF_BMI))
```

Data Visualization:

``` r
vars %>%
  ggplot(aes(x = Gluc_Fasting, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fasting Glucose by UPF Quartiles", x = "Fasting Glucose", fill = "UPF")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Midterm_files/figure-gfm/histogram,%20glu-fasting/120mins%20by%20UPF_quart-1.png)<!-- -->

``` r
vars %>%
  ggplot(aes(x = Gluc_120min, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Glucose after 120mins by UPF Quartiles", x = "Glucose after 120mins", fill = "UPF")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Midterm_files/figure-gfm/histogram,%20glu-fasting/120mins%20by%20UPF_quart-2.png)<!-- -->

``` r
vars %>%
  ggplot(aes(x = Gluc_Fasting, fill = BMI)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fasting Glucose by BMI", x = "Fasting Glucose", fill = "BMI")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Midterm_files/figure-gfm/histogram,%20glu_fasting/120mins%20by%20BMI-1.png)<!-- -->

``` r
vars %>%
  ggplot(aes(x = Gluc_120min, fill = BMI)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Glucose after 120mins by BMI", x = "Glucose after 120mins", fill = "BMI")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Midterm_files/figure-gfm/histogram,%20glu_fasting/120mins%20by%20BMI-2.png)<!-- -->

``` r
vars %>%
  ggplot(aes(x = UPF_BMI, fill = UPF_quart)) +
  geom_bar() +
  labs(title = "UPF binary and BMI combined by UPF Quartile", x = "UPF_BMI", fill = "UPF Quartile")
```

![](Midterm_files/figure-gfm/barchart,%20UPF_BMI%20by%20UPF_quart,%20cont-1.png)<!-- -->

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = UPF_quart, y = Gluc_Fasting),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Statistical Summary of Fasting GLucose by UPF Quartiles", x = "UPF Quartiles", y = "Fasting Glucose")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-1.png)<!-- -->

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = UPF_quart, y = Gluc_120min),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Statistical Summary of Fasting GLucose by UPF Quartiles", x = "UPF Quartiles", y = "Glucose after 120mins")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-2.png)<!-- -->

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = BMI, y = Gluc_Fasting),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Statistical Summary of Fasting GLucose by UPF Quartiles", x = "BMI", y = "Fasting Glucose")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-3.png)<!-- -->

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = BMI, y = Gluc_120min),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Statistical Summary of Fasting GLucose by UPF Quartiles", x = "BMI", y = "Glucose after 120mins")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-4.png)<!-- -->

Conclusion:

Midterm
================
Yiping Li
2022-10-23

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

Introduction: In the United States, more than half of the total dietary
energy consumption has been composed by ultra-processed foods (UPFs).
Most UPFs are calorie-dense and high in sugar, salt, and unhealthy fats,
while low in protein, vitamins, and minerals. Studies have demonstrated
that the increased consumption of UPFs could result in a poor
nutritional quality of the overall diet and high chances of developing
chronic diseases. Therefore, the aim of the study focuses on exploring
the association between UPF consumption and type 2 diabetes (T2DM) and
obesity among youth.

Method: The data used were obtained from the baseline visit (between
2014 and 2018) of the Children’s Health Study, which contained 158
participants aging from 17 to 22 years old. The 24-hour recall was used
to obtain the food items that participants consumed and to further
calculate the UPF consumption percentage (UPF%), which equaled to the
UPF consumption amount in grams divided by total food consumption amount
in grams. A total of 155 participants had appropriate UPF% were included
in the study. After exclude participants who did not have T2DM related
variables and implausible blood glucose levels, a total of 153
participants were being studied. Demographic variables, BMI, UPF%, and
T2DM related variables (fasting glucose, blood glucose after 120
minutes, and HbA1c) were included in this particular study.

\*Ultra-Processed Food Definition and Classification Ultra-processed
foods are defined as food items that go through multiple manipulated
process before people purchase or eat them. A total of 1167 food items
(including ingredients) were collected at baseline visit and a total of
807 food items were collected were collected at follow-up visit using
24-hour dietary recalls (24HRs). Two reviewers conducted following two
processes independently. Firstly, reviewers used NOVA classification to
group food items into 4 groups: unprocessed and minimally processed
foods, processed culinary ingredients, processed foods, and
ultra-processed foods (UPFs). Secondly, reviewers grouped the first 3
groups into non-UPFs group, so only two groups were resulted. After
grouping, reviewers had disagreements on food item classification,
reviewers share own perspectives to seek an agreement. In general, UPF
classification criteria were as following: the major ingredient of a
food item; how the food is made in the U.S. normally; the way most
people get the food – purchase from a grocery store or homemade.

``` r
metaair <- readRDS("/Users/yiping/Desktop/Epi research/OneDrive - University of Southern California/MetaChem/metaair.rds")
UPFMean_grams <- read_excel("/Users/yiping/Desktop/Epi research/USC-Liz/independent research - UPF/UPF% calculation2/UPFgrams_baseline.xlsx")
```

``` r
myvars <- c("ID", "cage", "male", "race2", "bmi_avg", "bmiclass", "diabetesbinary", "gluc_fasting", "gluc_120min", "hba1c")
subsetmetachem <- metaair[myvars]
```

``` r
vars <- merge(subsetmetachem, UPFMean_grams)
```

To better apply the numeric variable, the UPF%, was used to create 2
categorical variables. The first one was UPF quartiles, which was
created by evenly cut the UPF% into 4 groups. The second one was a
binary UPF variable, which was created by assuming those who consumed
less than 75% of UPFs as low UPF consumption group and the rest as high
UPF consumption group.

``` r
vars$UPF_quart <- cut(vars$UPFpercentage_mean,
                          breaks=c(0, 25, 50, 75, 100),
                          labels=c("1", "2", "3", "4"))

vars$UPF_cat <- cut(vars$UPFpercentage_mean,
                             breaks=c(0, 75, 100),
                             labels=c("0", "1"))
```

Appropriate names of variables were given to better view the dataset.

``` r
names(vars)[2] <- "Age"
names(vars)[3] <- "Male"
names(vars)[4] <- "Race"
names(vars)[5] <- "BMI_cont"
names(vars)[6] <- "BMI_cat"
names(vars)[7] <-  "Diabetes_cat"
names(vars)[8] <- "Gluc_Fasting"
names(vars)[9] <- "Gluc_120min"
names(vars)[10] <- "HbA1C"
```

Participants with missing values in T2DM or fasting glucose or blood
glucose after 120 minutes were excluded. Participants with implausible
fasting glucose of greater than 200 mg/dL and glucose after 120 minutes
of greater than 400 mg/dL were also excluded.

``` r
vars <- vars %>%
  filter(!is.na(Diabetes_cat)) %>%
  filter(!is.na(Gluc_Fasting)) %>%
  filter(!is.na(Gluc_120min)) %>%
  filter(Gluc_Fasting < 200) %>%
  filter(Gluc_120min < 400)
```

A categorical variable, UPF and BMI, that contained all cases of the UPF
binary variable and BMI categories conbination was created to explore
the association between UPF consumption and BMI.

``` r
vars <- vars %>%
  mutate(UPF_BMI = factor(
    case_when(UPF_cat == 0 & BMI_cat == "Normal" ~ "Low UPF Normal Weight",
              UPF_cat == 0 & BMI_cat == "Overweight" ~ "Low UPF Overweight",
              UPF_cat == 0 & BMI_cat == "Obese" ~ "Low UPF Obese",
              UPF_cat == 1 & BMI_cat == "Normal" ~ "High UPF Normal Weight",
              UPF_cat == 1 & BMI_cat == "Overweight" ~ "High UPF Overweight",
              UPF_cat == 1 & BMI_cat == "Obese" ~ "High UPF Obese")
  ))

vars<- vars %>%
  filter(!is.na(UPF_BMI))
```

Scatterplots of UPF% on BMI, fasting glucose, and blood glucose after
120 minutes were created. Histograms of fasting glucose by UPF quartiles
and blood glucose after 120 minutes by UPF quartiles were created. In
addition, histograms of fasting glucose by UPF quartiles and blood
glucose after 120 minutes by BMI were created.

Results: Table 1 shows the minimum and the maximum fasting glucose and
blood glucose after 120 minutes, and the number of subjects by UPF
quartiles. Most subjects consomed 0-25% of UPFs, and the number of
subjects decreases as the percentage of UPF consumption increases.

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

Table 2 shows the minimum and the maximum fasting glucose and blood
glucose after 120 minutes, and the number of subjects by UPF consumption
in low or high groups. Only 1 subject falls into the high UPF
consumption category.

``` r
table2 <- vars %>%
  group_by(UPF_cat) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table2)
```

| UPF_cat | Gluc_Fasting_min | Gluc_Fasting_max | Gluc_120min_min | GLuc_120min_max | Gluc_num |
|:--------|-----------------:|-----------------:|----------------:|----------------:|---------:|
| 0       |               74 |              119 |              55 |             208 |      152 |
| 1       |               91 |               91 |             135 |             135 |        1 |

Table 3 shows the minimum and the maximum fasting glucose and blood
glucose after 120 minutes, and the number of subjects by BMI.

``` r
table3 <- vars %>%
  group_by(BMI_cat) %>%
  summarise(
    Gluc_Fasting_min = min(Gluc_Fasting),
    Gluc_Fasting_max = max(Gluc_Fasting),
    Gluc_120min_min = min(Gluc_120min),
    GLuc_120min_max = max(Gluc_120min),
    Gluc_num = n(),
  )
knitr::kable(table3)
```

| BMI_cat    | Gluc_Fasting_min | Gluc_Fasting_max | Gluc_120min_min | GLuc_120min_max | Gluc_num |
|:-----------|-----------------:|-----------------:|----------------:|----------------:|---------:|
| Normal     |               74 |              107 |              55 |             176 |       24 |
| Obese      |               77 |              119 |              83 |             208 |       56 |
| Overweight |               76 |              107 |              55 |             200 |       73 |

Table 4 shows the number of subjects in each subgroup of the variable
UPF and BMI. Only 1 subject who was overweight falls into the category
of high UPF consumption.

``` r
table4 <- vars %>%
  group_by(UPF_BMI) %>%
  summarise(
    Group_num = n(),
  )
knitr::kable(table4)
```

| UPF_BMI               | Group_num |
|:----------------------|----------:|
| High UPF Overweight   |         1 |
| Low UPF Normal Weight |        24 |
| Low UPF Obese         |        56 |
| Low UPF Overweight    |        72 |

Figure 1 to 3 shows UPF% vs BMI, fasting glucose, and blood glucose
after 120 minutes correspondingly. None of the plots show a trend of
UPF% on those variables interested in.

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = BMI_cont)) + 
  geom_point() +
  labs(title = "Fig1: Scatterplot of UPF vs. BMI", x = "UPF%", y = "BMI")
```

![](Midterm_files/figure-gfm/scatterplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-1.png)<!-- -->

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_Fasting)) + 
  geom_point() +
  labs(title = "Fig2: Scatterplot of UPF vs. Fasting Glucose", x = "UPF%", y = "Fasting Glucose")
```

![](Midterm_files/figure-gfm/scatterplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-2.png)<!-- -->

``` r
ggplot(vars, aes(x = UPFpercentage_mean, y = Gluc_120min)) + 
  geom_point() +
  labs(title = "Fig3: Scatterplot of UPF vs. Blood Glucose after 120 minutes", x = "UPF%", y = "Blood Glucose after 120 minutes")
```

![](Midterm_files/figure-gfm/scatterplots%20of%20UPF_cont%20vs%20BMI_cont,%20Gluc_Fasting,%20Gluc_120min-3.png)<!-- -->

Figure 4 fasting glucose by UPF quartiles shows that most subjects
consumed 0-25% UPF and the second most subjects consumed 25-50% UPF.
However, both UPF amounts consumption show no pattern for fasting
flucose, which is also not normally distributed. This figure does not
show any association between fasting glucose and UPF consumption.

Figure 5 Blood Glucose after 120 minutes shows that subjects consumed
0-25% and 25-50% UPF are almost normally distributed for their blood
glucose level after 120 minutes. However, this figure also shows no
association between blood glucose after 120 minutes and UPF consumption.

``` r
vars %>%
  ggplot(aes(x = Gluc_Fasting, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig4: Fasting Glucose by UPF Quartiles", x = "Fasting Glucose", fill = "UPF")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Midterm_files/figure-gfm/histogram,%20glu-fasting/120mins%20by%20UPF_quart-1.png)<!-- -->

``` r
vars %>%
  ggplot(aes(x = Gluc_120min, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig5: Blood Glucose after 120mins by UPF Quartiles", x = "Blood Glucose after 120mins", fill = "UPF")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Midterm_files/figure-gfm/histogram,%20glu-fasting/120mins%20by%20UPF_quart-2.png)<!-- -->

Figure 6 BMI by UPF consumption shows that subjects who consumed 0-25%
UPF had a pattern with right skewed, while subjects who comsumed greater
than 25% UPF showed no specific pattern.

``` r
vars %>%
  ggplot(aes(x = BMI_cont, fill = UPF_quart)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(title = "Fig6: BMI by UPF Quartiles", x = "BMI", fill = "UPF")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Midterm_files/figure-gfm/histogram,%20BMI%20by%20UPF%20quartiles-1.png)<!-- -->

The barchart Figure 7 shows in another way that most subjects consumed
low amount of UPFs were either overweight or obese.

``` r
vars %>%
  ggplot(aes(x = UPF_BMI, fill = UPF_quart)) +
  geom_bar() +
  labs(title = "Fig7: UPF binary and BMI combined by UPF Quartile", x = "UPF_BMI", fill = "UPF Quartile")
```

![](Midterm_files/figure-gfm/barchart,%20UPF_BMI%20by%20UPF_quart,%20cont-1.png)<!-- -->

Figure 8 to Figure 11 are the statistical summary graphs of fasting
glucose or blood glucose after 120 minutes by UPF quartiles of BMI.

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = UPF_quart, y = Gluc_Fasting),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig8: Statistical Summary of Fasting GLucose by UPF Quartiles", x = "UPF Quartiles", y = "Fasting Glucose")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-1.png)<!-- -->

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = UPF_quart, y = Gluc_120min),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig9: Statistical Summary of Fasting GLucose by UPF Quartiles", x = "UPF Quartiles", y = "Glucose after 120mins")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-2.png)<!-- -->

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = BMI_cat, y = Gluc_Fasting),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig10: Statistical Summary of Fasting GLucose by UPF Quartiles", x = "BMI", y = "Fasting Glucose")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-3.png)<!-- -->

``` r
vars %>%
  ggplot() +
  stat_summary(mapping = aes(x = BMI_cat, y = Gluc_120min),
               fun.min = min,
               fun.max = max, 
               fun = median) + 
  labs(title = "Fig11: Statistical Summary of Fasting Glucose by UPF Quartiles", x = "BMI", y = "Glucose after 120mins")
```

![](Midterm_files/figure-gfm/statistical%20summary%20graph,%20glu-fasting/120mins%20by%20UPF_quart%20and%20BMI-4.png)<!-- -->

Conclusion: Based on the above data exploration and graphs, no clear
pattern shos that UPF consumption is associated with obesity or T2DM.
This might because most participants recruited that much UPF.

Data processing
================
Yan Wang
10/01/2022

-   [1 Data set description](#data-set-description)
-   [2 Data preparation](#data-preparation)
    -   [2.1 Reshape Df1 because we want one patient has multiple
        entries for symptoms, baseline contrallability scores, and
        controllability
        scores](#reshape-df1-because-we-want-one-patient-has-multiple-entries-for-symptoms-baseline-contrallability-scores-and-controllability-scores)
    -   [2.2 Merge with OEQ (text data) by participant ID and
        Symptom](#merge-with-oeq-text-data-by-participant-id-and-symptom)
    -   [2.3 Save the output for the linguisic feature
        extraction](#save-the-output-for-the-linguisic-feature-extraction)

# 1 Data set description

Data set 1 SQR: participant ID, time of controllability assessment,
symptom sequence, and 3 mean controllability scores

Data set 2 OEQ: participant ID, symptom and answers for the 5 questions:

1.  What does the symptom make you feel like?

2.  What do you think is causing your symptom?

3.  When did you first notice it, and does it follow any sort of
    pattern?

4.  How does the symptom affect you?

5.  Have you tried anything? Is it helpful?

Data set 3 Metadata: participant ID and sociodemographics, including
age, marrige status, employment, education, race, ethnicity.

# 2 Data preparation

``` r
names(SQR)
```

    ## [1] "Participant ID"                             
    ## [2] "GOGID"                                      
    ## [3] "Administration Number"                      
    ## [4] "SRQ: Mean Controllability Score (Symptom 1)"
    ## [5] "SRQ: Mean Controllability Score (Symptom 2)"
    ## [6] "SRQ: Mean Controllability Score (Symptom 3)"
    ## [7] "Symptom #1 (noticed most)"                  
    ## [8] "Symptom #2 (noticed 2nd most)"              
    ## [9] "Symptom #3 (noticed 3rd most)"

``` r
SQR_8BL<- SQR %>% filter(`Administration Number`=="Baseline (week 0)"|`Administration Number`== "8 week f/u")
names(SQR_8BL)[3:9]<-c("Time", "S1Cont", "S2Cont", "S3Cont" , "S1", "S2", "S3")
names(SQR_8BL)
```

    ## [1] "Participant ID" "GOGID"          "Time"           "S1Cont"        
    ## [5] "S2Cont"         "S3Cont"         "S1"             "S2"            
    ## [9] "S3"

## 2.1 Reshape Df1 because we want one patient has multiple entries for symptoms, baseline contrallability scores, and controllability scores

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ tibble  3.1.4     ✓ purrr   0.3.4
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
SQR_8BL1<-
  SQR_8BL %>%
  gather("S1","S2","S3",key="SymptomSequence",value="SymptomName") %>% select(-c(4:6))
SQR_8BL2<-
  SQR_8BL %>%
  gather("S1Cont","S2Cont","S3Cont",key="SymptomSequence",value="Controllability") %>% select(-c(4:6))
SQR_8BL2$SymptomSequence<-as.factor(SQR_8BL2$SymptomSequence)
SQR_8BL1$SymptomSequence<-as.factor(SQR_8BL1$SymptomSequence)
levels(SQR_8BL2$SymptomSequence)[1]<-"S1"
levels(SQR_8BL2$SymptomSequence)[2]<-"S2"
levels(SQR_8BL2$SymptomSequence)[3]<-"S3"
levels(SQR_8BL1$SymptomSequence)
```

    ## [1] "S1" "S2" "S3"

``` r
head(SQR_8BL1)[3:5]
```

    ## # A tibble: 6 × 3
    ##   Time              SymptomSequence SymptomName          
    ##   <chr>             <fct>           <chr>                
    ## 1 Baseline (week 0) S1              Abdominal Bloating   
    ## 2 8 week f/u        S1              Abdominal Bloating   
    ## 3 Baseline (week 0) S1              Skin Rash            
    ## 4 8 week f/u        S1              Skin Rash            
    ## 5 Baseline (week 0) S1              Peripheral Neuropathy
    ## 6 8 week f/u        S1              Peripheral Neuropathy

``` r
head(SQR_8BL2)[3:5]
```

    ## # A tibble: 6 × 3
    ##   Time              SymptomSequence Controllability
    ##   <chr>             <fct>                     <dbl>
    ## 1 Baseline (week 0) S1                          2.2
    ## 2 8 week f/u        S1                          3  
    ## 3 Baseline (week 0) S1                          2  
    ## 4 8 week f/u        S1                          2.2
    ## 5 Baseline (week 0) S1                          2.6
    ## 6 8 week f/u        S1                          1.8

``` r
Df<- inner_join(SQR_8BL1, SQR_8BL2, by= c("Participant ID", "GOGID", "Time", "SymptomSequence")) %>% na.omit()
names(Df)[1]<-"ID"
```

## 2.2 Merge with OEQ (text data) by participant ID and Symptom

``` r
names(OEQ)[1:2]<-c("ID", "SymptomName")
Df8BL<-left_join(Df ,OEQ, by=c("ID", "SymptomName")) %>% na.omit()
names(Df8BL)[7:9]<-c("FeelingCause", "Effect", "Strategy")
```

## 2.3 Save the output for the linguisic feature extraction

``` r
write.csv(Df8BL, "~/Desktop/Df8BL for text analysis.csv")
```

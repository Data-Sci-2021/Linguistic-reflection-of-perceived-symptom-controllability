final\_report
================
Yan Wang
12/10/2021

-   [1 Data](#data)
    -   [1.1 Parent study](#parent-study)
    -   [1.2 Design and sample](#design-and-sample)
    -   [1.3 Dependent vairable measure](#dependent-vairable-measure)
    -   [1.4 Data preparation](#data-preparation)
    -   [1.5 Data sharing](#data-sharing)
-   [2 Analysis](#analysis)
    -   [2.1 Multiple data sets are reshaped and merged in
        R](#multiple-data-sets-are-reshaped-and-merged-in-r)
    -   [2.2 Feature extraction in LIWC2015 and
        LightSide](#feature-extraction-in-liwc2015-and-lightside)
    -   [2.3 Data analysis in R](#data-analysis-in-r)
    -   [2.4 Results](#results)
        -   [2.4.1 Sample characteristics](#sample-characteristics)
        -   [2.4.2 Final model 1](#final-model-1)
        -   [2.4.3 Final model 2](#final-model-2)
        -   [2.4.4 Compare final model 1 and
            2](#compare-final-model-1-and-2)
        -   [2.4.5 Explanation of final model
            2](#explanation-of-final-model-2)
-   [3 Conclusions and implications](#conclusions-and-implications)
-   [4 Limitations](#limitations)

[Images
folder](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/tree/main/images)

# 1 Data

## 1.1 Parent study

WRITE Symptoms was a 3-arm RCT funded by the National Institutes of
Health/National Institute of Nursing Research NIH/NINR R01NR010735/NRG
GOG-259 as well as NIH/NCI grants to NRG Oncology (U10CA180822), NRG
Operations (U10CA180868), and UG1CA189867 (NCORP). Two versions of the
WRITE Symptoms intervention are based on Representational Approach to
patient education to improve symptom management, were compared to
enhanced care-as-usual. In the Nurse-guided WRITE Symptoms intervention
(Nurse WRITE), participants were guided by nurses through intervention
components via private online message boards. The SD WRITE follows an
identical process to Nurse WRITE, but participants engage in an
interactive web-based intervention without guidance from a nurse.

## 1.2 Design and sample

This is an secondary analysis focus on the association between
linguistic features and

Data included was from the 166 participants randomized to the
self-directed arm, including sociodemographic factors, perceived symptom
controllability score changes from baseline to 8 weeks, baseline
controllability scores, and their posts recorded verbatim in the message
board. For each participant who worked on each of the selected symptoms,
they posts reflections on the following questions: 1. What does the
symptom make you feel like? 2. What do you think is causing your
symptom? 3. When did you first notice it and does it follow any sort of
pattern? 4. How does the symptom affect you? 5. Have you tried anything?
Is it helpful?

Eligible participants of parent study were 18+ years; and had recurrent
or persistent ovarian, fallopian or primary peritoneal cancer,
experiencing 3 or more bothersome symptoms with a gynecologic oncology
group (GOG) performance status of less than 3; at least 3+ symptoms
(e.g., pain, fatigue, neuropathy) associated with cancer or treatment;
and able to read and write in English. The RCT is a national study, and
the sample is very likely representative of women with recurrent ovarian
cancer nationwide.

## 1.3 Dependent vairable measure

Perceived Symptom controllability was measured by 3 items for each
symptom addressing the extent to which the person believes that they can
control the symptom on a 5- point Likert-type scale of 0 (strongly
disagree) to 4 (strongly agree).

## 1.4 Data preparation

Qualitative data (participant posts) had been manually screened after
the the trial was over.

## 1.5 Data sharing

I used [NLM-Scrubber](https://scrubber.nlm.nih.gov/) designed and
developed at the National Library of Medicine. The goal is to produce
HIPAA compliant deidentified health information for scientific use;
however, the success rate of this goal depends on the input data. For
our dataset, it is still possible to reidentify individuals from
deidenitfied data with the use of additional information linked to the
same individuals. Data set can be shared upon request with data use
agreement.

# 2 Analysis

## 2.1 Multiple data sets are reshaped and merged in R

Data set 1 SQR: ID, time of controllability assessment, up to 3 selected
symptoms \[S1, S2, S3\] and 3 controllability scores \[S1Cont, S2Cont,
S3Cont\]; Data set 2 OEQ : ID, symptom, and patient posts on message
board Data set 3 Metadata: ID and sociodemographics, including age,
marriage status, employment, education, race, ethnicity

## 2.2 Feature extraction in LIWC2015 and LightSide

[LIWC2015](http://liwc.wpengine.com/) is the gold standard in
computerized text analysis. Learn how the words we use in everyday
language reveal our thoughts, feelings, personality, and motivations.I
used LIWC2015 to extract linguistic features as potential predictors
that might reflect the underlying psychological process of participant’s
sense of control. The features extracted include 7 self-designed
categories ( percentage of words in the text that are cancer and
treatment related symptoms, effort, impact on life, positive adjs
describing symptom, negative adjs describing symptom, symptom
controlled, symptom uncontrolled), 4 summary language variables
(analytical thinking, clout \[confidence\], authenticity, and emotional
tone), 3 general descriptor categories (words per sentence, percent of
target words captured by the dictionary, and percent of words in the
text that are longer than six letters), 21 standard linguistic
dimensions (e.g., percentage of words in the text that are pronouns,
articles, auxiliary verbs, etc.), 41 word categories tapping
psychological constructs (e.g., affect, cognition, biological processes,
drives), 6 personal concern categories (e.g., percentage of words in the
text that are work, home, leisure activities), 5 informal language
markers (percentage of words in the text that are assents, fillers,
swear words, netspeak, etc), and 12 punctuation categories (periods,
commas, etc).

The open-source [LightSide
platform](http://ankara.lti.cs.cmu.edu/side/), including the
machine-learning and feature-extraction core as well as the researcher’s
workbench UI, has been and continues to be funded in part through
Carnegie Mellon University, in particular by grants from the National
Science Foundation and the Office of Naval Research. I used LightSide to
tag and calculate frequency of the word “control” as verb and noun,
respectively.

## 2.3 Data analysis in R

Linear mixed effects model was used explore the relationship between
linguistic features and controllability score at 8 weeks. Participant ID
was used as random effect. Potential covariates are age, education,
employment, marriage status, and race. Didn’t include race or ethinicity
due to lack of variations.

Potential Fixed effects are baseline controllability score, total number
of words in the post, words per sentence, and the percentage of word or
punctuation categories of the total number of words in the posts for one
symptom, including 4 summary language variables analytical thinking,
clout \[confidence\], authenticity, and emotional tone), 3 general
descriptor categories (words per sentence, percent of target words
captured by the dictionary, and percent of words in the text that are
longer than six letters), 21 standard linguistic dimensions (e.g.,
percentage of words in the text that are pronouns, articles, auxiliary
verbs, etc.), 41 word categories tapping psychological constructs (e.g.,
affect, cognition, biological processes, drives), 6 personal concern
categories (e.g., work, home, leisure activities), 5 informal language
markers (assents, fillers, swear words, netspeak), and 12 punctuation
categories (periods, commas, etc)

Based on the [preliminary bivariate correlation
test](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/Bivariate%20correlation-1.png),
we have candidate predictors, including baseline controllability score,
total number of words, symptom word category, “controlled” words
category, “controlNN” word category (where patient used word control as
noun),anx (“anxiety” word category),“feel” word category, “body” word
category, “ingest” word category, “focuspresent” word category, “money”
word category, “informal” word category, “nonflu”(nonfluency) word
category, “negate” word category, “discrep”(discrepancy) word category,
“certain” word category, “Analytic”, Sixltr (words more than six
letters) word category, prep (prepositions) word category. I used full
opinionated approach with iterative forward selection on candidate
linguistic features to obtain final model 1.

I also used principle components analysis on all the linguistic features
to extract the underlying constructs and use linear mixed effect model
to structure the relationship between principle components and
controllability at 8 weeks. Principle componnets are chosen if their
eigen values are more than 1. I used iterative backward elimination to
select principle components to be included in the final model 2.

I compared final model 1 and 2 based on AIC and BIC.

## 2.4 Results

### 2.4.1 Sample characteristics

Participants were predominantly married or Living with
partner/significant other(75.16%), white (93%), non-hispanic (96.18%).
Over half of the participants were unemployed (59.24%). The mean of
[age](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/sociodemographic-1.png)
is 58.18 (SD = 9.72), ranging from [25 to
81](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/sociodemographic-4.png).
The average of [formal years of
education](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/sociodemographic-5.png)
is 14.4 (SD=2.72), ranging from
[10-22](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/sociodemographic-8.png)

### 2.4.2 Final model 1

After controlling for years of formal years of education and baseline
controllability score,in the [final
model](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/toy11%20model.png),
total number of words (b= 0.00042,95%CI (8.63e-05, 0.000753)), and
symptom word category (b= 0.061,95%CI (2.46e-02, 0.098)) is significant
positive predictor; anxiety (b= 0.056,95%CI (-4.5e-03, 0.12)) and
informal word categories (b= 0.11,95%CI (-3.91e-03,0.23)) are marginal
positive predictor on better symptom controllability at 8 weeks. Feel
(b= -0.038, 95%CI (-6.79e-02, -0.0074)) and focuson present word
categories (b= -0.018, 95%CI (-3.66e-02, -0.00028)) are significant
negative predictors; money (b=-0.18,95%CI (-0.37, 0.0025)) and informal
word categories (b= 0.11,95%CI (-3.91e-03, 0.23)) are marginal negative
predictors on symptom controllability at 8 weeks. The AIC of the final
model 1 is 551.16 and the BIC is 595.61.

### 2.4.3 Final model 2

There are 33 principle components that has eigen values more than 1,
explaining 76.8% of the variance. Here is the
[screeplot](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/screeplot.png).

Based on variable lodings on each principle components, I found some
meaningful underlying constructs. Here are the constructs included in
the final model and their loadings: [immerse writing of traumatic
experience](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-1.png);[confidence
and
motivation](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-2.png);
[emotionanlly
distant](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-3.png);[venting](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-4.png);[Effort
to fight
cancer](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-5.png);
[pressure and effort to
work](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-6.png);
[detailed symptom
description](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-7.png);
[financial
stress](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/variable%20loadings-8.png).

After controlling for years of formal years of education and baseline
controllability score, in the [final model
2](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/images/toy13%20model.png),
“immerse writing of traumatic experience” (b= -0.021,95%CI (-0.045,
0.0033)) and “pressure and effort to work” (b=-0.041,95%CI (-0.091,
0.0084)) are marginal negative predictors; “emotionanlly distant from
symptom experience” (b= -0.046,95%CI (-0.086,-0.0069)) and “financial
stress” (b= -0.061,95%CI (-0.11,-0.01)) are significant negative
predictors on symptom controllability at 8 weeks. “Confidence and
motivation” (b= 0.046,95%CI (0.014,0.079)), “venting”(b= 0.081,95%CI
(0.038, 0.12)), “effort to fight cancer”(b= 0.072,95%CI (0.027, 0.11)),
and “detailed symptom description”(b= 0.079,95%CI (0.027, 0.13)) are
significant positive predictors on better symptom controllability at 8
weeks. The AIC of the final model 2 is 540.2 and the BIC is 588.35.

### 2.4.4 Compare final model 1 and 2

Final model 2 is better at capturing the underlying meaningful
constructs that can predict patient symptom controllability at 8 weeks.
The AIC and BIC are lower in final model 2 than final model 1.
Therefore, final model 2 was chosen as the model to explain the
linguistic reflections of patient perceived symptom controllability.

### 2.4.5 Explanation of final model 2

After controlled for participant ID, baseline controllability, and
participant education, “immerse writing of experience” is a marginal
negative predictor for controllability at 8 weeks. In the immersive
writing of a traumatic event, the more authentic and relevant
participants write their symptom experience (cancer), the more likely
they relive the event and thus might feel worse and loss of control over
the related traumatic event (dealing with multiple ans severe symptoms)
they are experiencing now.

After controlled for participant ID, baseline controllability, and
participant education, “confidence and motivation” is a significant
positive predictor for controllability at 8 weeks. When cancer survivors
are confident and motivated fight against cancer, they are more likely
to manage the symptoms better.

After controlled for participant ID, baseline controllability, and
participant education, “emotionanlly distant” is a significant negative
predictor for controllability at 8 weeks. In expressive writing, some
participants wrote rich narratives of their experience and emotionally
invested while others were simply informative and emotionally distant
from the topic. When participants were emotionally distant from the
event they are dealing with, they are less likely to deal with the
issues.

After controlled for participant ID, baseline controllability, and
participant education, “venting” is a significant positive predictor for
controllability at 8 weeks. Studies on expressive writing have shown
that venting help patients release negative emotions and maintain
healthy mental state to manage their symptoms better

After controlled for participant ID, baseline controllability, and
participant education, “effort to fight cancer” is a significant
positive predictor for controllability at 8 weeks. When participants try
hard to fight cancer and survive, they are more motivated to manage
their symptoms to have a life with better quality.

After controlled for participant ID, baseline controllability, and
participant education, “pressure and effort to work” is a marginally
negative predictor for controllability at 8 weeks. When patients talked
about their work and job, most of them describe how cancer symptoms
affect their employment or work routine and how their job make their
symptom worse. On the other hand, some types of works aggravates
patients’ symptoms, such as pain, fatigue, abdominal bloating. Here is
an example “I have to push myself all day to keep up with my job duties.
Shortly after I eat lunch, I usually have that ‘sugar’ drop and want to
crawl in a hole and nap.” “I have to lay in bed for a few days before
going back to work.”

After controlled for participant ID, baseline controllability, and
participant education, “detailed symptom description” is a significant
positive predictor for controllability at 8 weeks. Reflecting and
describing how symptom affects daily life helps patients think about
their symptoms in a different way to create condition for behavior
change. Patients might be more aware of the limitations of current
belief and behavior.The more participants reflect on their symptoms and
how they manage it, the more likely they are willing to try new
strategies to better manage their symptoms.

After controlled for participant ID, baseline controllability, and
participant education, “financial stress” is a significant negative
predictor for controllability at 8 weeks. Many participants talked about
how financial struggling affects their emotional and social
participationIt is not surprising that general loss of control influence
their perceived sense of control over symptoms, such as depression,
anxiety.

# 3 Conclusions and implications

Based on the controllability linguistic markers, it is important for the
patients to reflect their symptoms in a personal and meaningful way
although there might be risk that immerse writing can evoke negative
emotions. When controlled for participants and their baseline
controllability scores and education, patients who are confident and
motivated, tend to release negative emotions, and make great efforts to
fight cancer in general are more likely to have better control of their
symptoms. Those who are experiencing stress from employment and finance
are more likely to do poorly in symptom control.

In contrast to f2f intervention where patients engage with materials in
a structured and monitored way, and clinicians directly observe patients
behavior and provide feedback, in digital intervention, it is important
to target the limited clinician’s attention to patients who need it
most. The linguistic features identified in the message board can help
clinicians monitor patient sense of control. By identifying patients who
are likely to end up not improve their ability to control symptoms
before it is too late, the clinicians can provide emotional support and
tailor the content in time to help those patients control their symptom
better.

# 4 Limitations

There are a few limitations of the study. Some models can’t be compared
directly because of different data sets. The validity of the study can
be further enhanced by (1) confirmation of self-designed word categories
from another content expert; (2) confirmation of a few potential
outliers in the data set from the primary investigator of the clinical
trial.

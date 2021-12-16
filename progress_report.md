Progress Report
================
Yan Wang
10/26/2021

-   [1st Progress Report](#1st-progress-report)
    -   [Sharing plan of my data](#sharing-plan-of-my-data)
-   [2nd Progress Report](#2nd-progress-report)
-   [licensing for the project](#licensing-for-the-project)
-   [3nd Progress Report](#3nd-progress-report)

# 1st Progress Report

I gathered some metadata and merged different data sets. But I found a
fair amount of missing data that looks weird to me. So, I will talk to
the primary investigator and confirm with her.

My [data
processing](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/Progress%20report/SD-WRITE-analysis.md)
can be found here.

## Sharing plan of my data

I am working with a librarian in Health Sciences Library. Our goal is to
safely put the datasets in to a repository. We haven’t decided which
repository to use.

# 2nd Progress Report

I have confirmed with the primary investigator of the clinical trial and
resolved the issue of missing data. I deleted the rows containing NAs in
the column of 8BLChange. I have also tried different automatic tools to
de-identify the patient dataset. In the end, I used
[NLM-scrubber](https://scrubber.nlm.nih.gov/). After the scrubber masked
the identifiable entries, I dived into the patient’s post and manually
checked if there is any sensitive information. Unfortunately, still,
information that might identify the patient if others put all the
information together. For example, the scrubber didn’t mask the patient
relative’s age, the nursing facility’s name. Example of de-identified
post. I need to confirm this issue and my de-identification scheme with
the primary investigator and Pitt IRB consultant. In the meantime, I
created a dictionary that contains multiple categories based on my
qualitative analysis of the patients’ posts and used
[LIWC](http://liwc.wpengine.com/) to extract words and count the
frequency of those categories. Here is the link to the
[dictionary](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/Dictionary/SDWRITE.dic).
I also used [LIWC2015
dictionary](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/Dictionary/LIWC2015Dictionary.pdf)
to extract other linguistic features (e.g., the total number of words).

# licensing for the project

I am not entirely sure which license I should choose for the project.
I’d like to use the MIT license because I want other researchers to use
the data set and contribute to the scholarship of patient
self-expression on symptom experiences. The data set is different than
other message boards or forums on the cancer support website. Patients
were led to reflect on their symptoms in a systematic manner and express
their feelings. But I think I need to consult the librarian to know the
norms in this kind of data set.

# 3nd Progress Report

Progress: I have finished almost all the [data
analysis](https://github.com/Data-Sci-2021/Linguistic-reflection-of-perceived-symptom-controllability/blob/main/Progress%20report/SD-WRITE-analysis-progress3.md)for
details.

Data: I have de-idenitfied the dataset using NLM-scrubber. But I am not
sure if it meet the standard of sharing the dataset publicly. The
primary investigator prefers to put the dataset in a designated
repository for research community instead of open access. So I need to
figure it out with the librarian.

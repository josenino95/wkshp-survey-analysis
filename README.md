# Workshop Survey Analysis

This repo includes de-identified responses to pre/post surveys completed by learners who attended UCSB Library Carpentry workshops. Results are presented through a [Quarto-based website](https://ucsbcarpentry.github.io/wkshp-survey-analysis/) that is served via GitHub pages.

Note that the 'raw' survey results (as downloaded from Qualtrics) are not part of this repo because they include IP addresses and potentially other forms of personally identifiable information. The file, `data-joined/all_workshops.csv`, is derived from csv files downloaded from Qualtrics and processed using the `datajoin.R` script; it should not include any PII.

# Workshop Survey Analysis

This repo includes de-identified responses to pre/post surveys completed by learners who attended UCSB Library Carpentry workshops. Results are presented through a [Quarto-based website](https://ucsbcarpentry.github.io/wkshp-survey-analysis/) that is served via GitHub pages.

Note that the 'raw' survey results (as downloaded from Qualtrics) are not part of this repo because they include IP addresses and potentially other forms of personally identifiable information. The file, `data-joined/all_workshops.csv`, is derived from csv files downloaded from Qualtrics and processed using the `datajoin.R` script; it should not include any PII.

## Adding New Data

- Download the pre/post survey results as `.csv` from Qualtrics. Make sure "use choice text" is selected.
- Move the files to `data` directory and rename them with the format `yyyy-mm-dd-workshopName-pre.csv` and `yyyy-mm-dd-workshopName-post.csv`. If you are following the carpentry convention for workshop repository names, you just need to append `-pre` and `post` to the repository name. You should have two files for each workshop, such as:
  - `2022-08-16-ucsb-machlearn-r-pre.csv`
  - `2022-08-16-ucsb-machlearn-r-post.csv`
- Run `datajoin.R` to build a new file: `data-joine/all_workshops.csv`
The point of this document is to record all steps taken over the course of this project so that producing the appendix for the final report is easier.

## Data cleaning/pre-processing

*7 November*
- Left joined listings.csv onto reviews.csv. This adds locational data and other fields to the reviews for use in clustering. (list_rev.csv, merge.R)

*8 November*
- edited merge.R so that list_rev.csv only includes listings with more than three reviews (as per Wheeler's example)
- deleted list_rev.csv from github and added to .gitignore (so the file exists locally but should be ignored by git)


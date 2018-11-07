library(readr)
library(dplyr)

list <- read_csv("data/listings.csv")
rev <- read_csv("data/reviews.csv")

list_rev <- left_join(rev, list, by = c("listing_id" = "id"))
write_csv(list_rev, "data/list_rev.csv")
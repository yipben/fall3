library(readr)
library(dplyr)

# Only need to run once

list <- read_csv("data/listings.csv")
rev <- read_csv("data/reviews.csv")

# keep listings with more than three reviews
three_plus <- rev %>%
  count(listing_id) %>%
  filter(n > 3) %>%
  pull(listing_id)
rev <- rev %>%
  filter(listing_id %in% three_plus)

# join and output complete dataset
list_rev <- left_join(rev, list, by = c("listing_id" = "id"))
write_csv(list_rev, "data/list_rev.csv")
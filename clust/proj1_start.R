library(tm)
library(readr)
library(dplyr)
library(tidytext)
library(text2vec)

list <- read_csv("data/listings.csv")
rev <- read_csv("data/reviews.csv")

# keep listings with more than three reviews
three_plus <- rev %>%
  count(listing_id) %>%
  filter(n > 3) %>%
  pull(listing_id)
rev <- rev %>%
  filter(listing_id %in% three_plus)


# df w/ one word per row
word_per_row <- rev %>%
  unnest_tokens(word, comments) %>%
  anti_join(stop_words) %>%
  filter(!is.na(word))


# create dtm
dtm <- word_per_row %>%
  count(listing_id, word) %>%
  cast_dtm(listing_id, word, n)
# inspect(dtm) # uncomment line to view contents of dtm


# calculate distances
cos <- 1 - sim2(as.matrix(dtm), method = "cosine", norm = "l2")
# jac <- 1 - sim2(as.matrix(dtm), method = "jaccard", norm= "none")


# OPTIONAL: join listing info before clustering

# dm = distance matrix (e.g. cos)
join_list <- function(dm) {
  df <- as_data_frame(dm)
  col_nms <- colnames(df)
  df <- df %>%
    mutate(listing_id = parse_integer(col_nms)) %>%
    rename_at(col_nms, funs(paste0("dist2_", .))) %>%
    select(listing_id, contains("dist2_"))
  left_join(df, list, by = c("listing_id" = "id"))
}

cos_list <- join_list(cos)


# cluster away!
test <- hclust(as.dist(cos), method = "complete")
plot(test)

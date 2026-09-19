# finds the best 80/20 match level train/test data split by trying multiple seeds
# selects seed with best balance across: match result, match venue, how far into season (thirds),and mean fatigue distribution

df <- readRDS("model_data.rds")
df <- as.data.frame(df)
matches <- read.csv("matches.csv")

df$match_id <- as.integer(df$match_id)
matches$match_id <- as.integer(matches$match_id)
matches$match_date <- as.Date(matches$match_date)
matches <- matches[order(matches$match_date), ]
matches <- matches[matches$match_id %in% unique(df$match_id), ]
matches$match_idx <- seq_len(nrow(matches))

# match characteristics
last_min <- aggregate(match_minute ~ match_id, data = df, FUN = max)
names(last_min)[2] <- "max_minute"
final_rows <- merge(df, last_min, by = "match_id")
final_rows <- final_rows[final_rows$match_minute == final_rows$max_minute, ]
final_state <- final_rows[!duplicated(final_rows$match_id), c("match_id","game_state")]
names(final_state)[2] <- "result"

#mean fatigue
match_mean <- aggregate(cbind(roll_hia, roll_hid) ~ match_id, data = df[!is.na(df$roll_hia), ], FUN = mean)
match_mean$mean_fatigue <- match_mean$roll_hia + match_mean$roll_hid

match_info <- merge(matches, final_state, by = "match_id")
match_info <- merge(match_info, match_mean[, c("match_id","mean_fatigue")], by = "match_id")

#split season into thirds (early season, mid season, late season) to account for potential accumulated season fatigue 
match_info$third <- cut(
  match_info$match_idx,
  breaks = quantile(match_info$match_idx, probs = c(0, 1/3, 2/3, 1)),
  labels = c("Early","Mid","Late"),
  include.lowest = TRUE
)

#specify test matches, for 80/20 split we need 7/35 games to test
n_test <- 7
n_matches <- nrow(match_info)
match_ids <- match_info$match_id

#distributions of match characteristics of focus to balance
print(table(match_info$result))
print(table(match_info$match_venue))
print(table(match_info$third))


# search through seeds
# for each seed randomly sample n test matches and "score" the split on criteria of match characteristics
#select seed with lowest "score"

seeds_to_try <- 1:500 
balance_results <- data.frame()

for (seed in seeds_to_try) {
  set.seed(seed)
  test_ids <- sample(match_ids, n_test, replace = FALSE)
  train_ids <- match_ids[!match_ids %in% test_ids]
  train_info <- match_info[match_info$match_id %in% train_ids, ]
  test_info <- match_info[match_info$match_id %in% test_ids,  ]
  
  # difference in proportion of match results
  train_result <- prop.table(table(train_info$result))
  test_result <- prop.table(table(test_info$result))
  all_results <- union(names(train_result), names(test_result))
  result_diff <- sum(abs(sapply(all_results, function(r)(train_result[r] - test_result[r]) %||% 0)), na.rm = TRUE)
  
  # difference in home and away proportion
  venue_diff <- abs(mean(train_info$match_venue == "home") - mean(test_info$match_venue == "home"))
  
  # difference in spread of matches through season (early, mid, late thirds)
  train_third <- prop.table(table(train_info$third))
  test_third <- prop.table(table(test_info$third))
  all_thirds <- union(names(train_third), names(test_third))
  third_diff <- sum(abs(sapply(all_thirds, function(t) (train_third[t] - test_third[t]) %||% 0)), na.rm = TRUE)
  
  # difference in mean fatigue score
  fatigue_diff <- abs(mean(train_info$mean_fatigue) - mean(test_info$mean_fatigue))
  
  # score (want lower)
  # note that fatigue_diff is normalised to similar scale as other proportions
  balance_score <- result_diff + venue_diff + third_diff + (fatigue_diff / sd(match_info$mean_fatigue)) 

  balance_results <- rbind(balance_results, data.frame(
    seed = seed,
    result_diff = round(result_diff, 4),
    venue_diff = round(venue_diff, 4),
    third_diff = round(third_diff, 4),
    fatigue_diff  = round(fatigue_diff, 4),
    balance_score = round(balance_score, 4),
    test_W = sum(test_info$result == "Winning"),
    test_D = sum(test_info$result == "Drawing"),
    test_L = sum(test_info$result == "Losing"),
    test_home = sum(test_info$match_venue == "home"),
    test_away = sum(test_info$match_venue == "away"),
    test_early = sum(test_info$third == "Early"),
    test_mid = sum(test_info$third == "Mid"),
    test_late = sum(test_info$third == "Late")
  ))
}

# results
balance_results <- balance_results[order(balance_results$balance_score), ]
best_seed <- balance_results$seed[1]
best_seed

#apply best seed to split data into train/test
set.seed(best_seed)
test_match_ids <- sample(match_ids, n_test, replace = FALSE)
train_match_ids <- match_ids[!match_ids %in% test_match_ids]
train_df <- df[df$match_id %in% train_match_ids, ]
test_df <- df[df$match_id %in% test_match_ids, ]
train_info <- match_info[match_info$match_id %in% train_match_ids, ]
test_info <- match_info[match_info$match_id %in% test_match_ids, ]


#final train/test split info
print(match_info[match_info$match_id %in% test_match_ids, c("match_id","match_date","match_venue", "result","third","mean_fatigue")], row.names = FALSE)

#save separate training and testing match data files
saveRDS(train_df, "train_data.rds")
saveRDS(test_df, "test_data.rds")
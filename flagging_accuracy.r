# Accuracy of flagging fatigue

library(lme4)

# Load chosen models
lmer_lag1 <- readRDS("lmer_lin_lag1.rds")
lmer_lag5 <- readRDS("lmer_lin_lag5.rds")
lmer_lag10 <- readRDS("lmer_lin_lag10.rds")

# Load test data with lag already computed for each lag value
test_lag1 <- readRDS("test_h2_lag1.rds")
test_lag5 <- readRDS("test_h2_lag5.rds")
test_lag10 <- readRDS("test_h2_lag10.rds")


# flagging function, predict deterioration at t+k for all players, find predicted top 4, 
# find actual top 4 at t+k from observed data, count overlap

flag_top4 <- function(model, test_data, horizon) {
  results <- data.frame()
  match_ids <- unique(as.character(test_data$match_id))

  for (mid in match_ids) {
    match_data <- test_data[as.character(test_data$match_id) == mid, ]
    minutes <- sort(unique(match_data$match_minute[ match_data$match_minute > 45 & match_data$match_minute <= (90 - horizon)]))
    for (min_t in minutes) {
      live <- match_data[match_data$match_minute == min_t & !is.na(match_data$deterioration_lag) & !is.na(match_data$deterioration_prop), ]
      if (nrow(live) < 4) next
      
      # Predict deterioration at t+k
      preds <- tryCatch(as.numeric(predict(model, newdata = live, re.form = ~(1|athlete_id), allow.new.levels = TRUE)), error = function(e) NULL)
      if (is.null(preds)) next
      
      # Predicted top 4
      live$pred <- preds
      pred_top4 <- as.character(live$athlete_id[order(live$pred, decreasing = TRUE)][1:4])
      
      # Actual top 4
      future <- test_data[as.character(test_data$match_id) == mid & test_data$match_minute == (min_t + horizon) & !is.na(test_data$deterioration_prop), ]
      if (nrow(future) < 4) next
      actual_top4 <- as.character(future$athlete_id[order(future$deterioration_prop, decreasing = TRUE)][1:4])
      
      # Overlap
      overlap <- length(intersect(pred_top4, actual_top4))
      results <- rbind(results, data.frame(match_id = mid, minute_t = min_t, overlap = overlap))}}
  results
}



#run flagging
res1 <- flag_top4(lmer_lag1, test_lag1, horizon = 1)
res5 <- flag_top4(lmer_lag5, test_lag5, horizon = 5)
res10 <- flag_top4(lmer_lag10, test_lag10, horizon = 10)


#results
print_results <- function(results, label) {
  cat(sprintf("All 4 correct: %.1f%%\n"))
  cat(sprintf("At least 3 of 4 correct: %.1f%%\n"))
  cat(sprintf("At least 2 of 4 correct: %.1f%%\n"))
}

print_results(res1, "Lag 1")
print_results(res5, "Lag 5")
print_results(res10, "Lag 10")
library(lme4)
library(lmerTest)
library(mgcv)
library(ggplot2)
library(tidyverse)

lag_minutes <- 10L

#load and prep data
train_df <- readRDS("train_data.rds")
test_df <- readRDS("test_data.rds")
train_df <- as.data.frame(train_df)
test_df <- as.data.frame(test_df)

pos_levels <- c("CB","FB","DM","CM","AM","CF","W")
gs_levels <- c("Drawing","Winning","Losing")

prep_df <- function(df) {
  df$fatigue_score <- df$roll_hia + df$roll_hid
  df$goal_diff <- df$goals_for - df$goals_against
  df$position_group <- factor(df$position_group, levels = pos_levels)
  df$game_state <- factor(df$game_state, levels = gs_levels)
  df$match_venue <- factor(df$match_venue)
  df$athlete_id <- factor(df$athlete_id)
  df$match_id <- factor(df$match_id)
  df
}

train_df <- prep_df(train_df)
test_df <- prep_df(test_df)

# setting up deterioration 
h1_peak_train <- aggregate(
  fatigue_score ~ athlete_id + match_id,
  data = train_df[train_df$period_id == 1 & !is.na(train_df$fatigue_score), ],
  FUN = function(x) quantile(x, 0.90, na.rm = TRUE)
)
names(h1_peak_train)[3] <- "h1_peak_fatigue"

h1_peak_test <- aggregate(
  fatigue_score ~ athlete_id + match_id,
  data = test_df[test_df$period_id == 1 & !is.na(test_df$fatigue_score), ],
  FUN = function(x) quantile(x, 0.90, na.rm = TRUE)
)
names(h1_peak_test)[3] <- "h1_peak_fatigue"

train_df <- merge(train_df, h1_peak_train, by = c("athlete_id","match_id"), all.x = TRUE)
test_df <- merge(test_df, h1_peak_test, by = c("athlete_id","match_id"), all.x = TRUE)

train_df$deterioration_prop <- (train_df$h1_peak_fatigue - train_df$fatigue_score) / (train_df$h1_peak_fatigue + 0.01)
test_df$deterioration_prop <- (test_df$h1_peak_fatigue - test_df$fatigue_score) / (test_df$h1_peak_fatigue  + 0.01)

train_df$match_minute2 <- train_df$match_minute^2
train_df$match_minute3 <- train_df$match_minute^3
test_df$match_minute2 <- test_df$match_minute^2
test_df$match_minute3 <- test_df$match_minute^3

add_lag_lead <- function(df, lag_val) {
  df <- df %>%
    arrange(athlete_id, match_id, match_minute) %>%
    group_by(athlete_id, match_id) %>%
    mutate(
      deterioration_lag = lag(deterioration_prop, n = lag_val, default = NA),
      deterioration_next = lead(deterioration_prop, n = lag_val, default = NA)
    ) %>%
    ungroup()
  as.data.frame(df)
}

train_df <- add_lag_lead(train_df, lag_minutes)
test_df <- add_lag_lead(test_df, lag_minutes)

model_vars <- c("deterioration_next", "deterioration_lag", "match_minute", "match_minute2", "match_minute3",
                "player_days_since_last", "days_into_season", "cumulative_dist", "cumulative_hsd",
                "position_group", "game_state", "match_venue", "h1_peak_fatigue", "athlete_id", "match_id")

df_model <- train_df[train_df$match_minute > 45, ]
df_model <- df_model[complete.cases(df_model[, model_vars]), ]

test_h2 <- test_df[test_df$match_minute > 45, ]
test_h2 <- test_h2[complete.cases(test_h2[, model_vars]), ]
saveRDS(test_h2, "test_h2_lag10.rds")


# Training metrics for lmer modesl
train_metrics_lmer <- function(model) {
  actual <- model.frame(model)$deterioration_next
  preds  <- predict(model, re.form = ~(1|athlete_id))
  resids <- actual - preds
  ok <- !is.na(resids)
  data.frame(
    AIC = round(AIC(model), 2),
    Train_RMSE = round(sqrt(mean(resids[ok]^2)), 4),
    Train_MAE  = round(mean(abs(resids[ok])),    4)
  )
}


# Test metrics for lmer
test_metrics_lmer <- function(model, test_data) {
  preds <- tryCatch(predict(model, newdata = test_data, re.form = ~(1|athlete_id), allow.new.levels = TRUE), error = function(e) NULL)
  if (is.null(preds)) return(data.frame(Test_RMSE=NA, Test_MAE=NA))
  actual <- as.numeric(test_data$deterioration_next)
  preds <- as.numeric(preds)
  ok <- !is.na(actual) & !is.na(preds) & is.finite(preds)
  data.frame(
    Test_RMSE = round(sqrt(mean((actual[ok] - preds[ok])^2)), 4),
    Test_MAE = round(mean(abs(actual[ok] - preds[ok])), 4)
  )
}

# Training metrics for GAM
train_metrics_gam <- function(model, sm) {
  resids <- residuals(model, type = "response")
  data.frame(
    AIC = round(AIC(model), 2),
    Dev_Expl = round(as.numeric(sm$dev.expl) * 100, 1),
    Train_RMSE = round(sqrt(mean(resids^2, na.rm = TRUE)), 4),
    Train_MAE = round(mean(abs(resids),   na.rm = TRUE), 4),
    Train_R2 = round(as.numeric(sm$r.sq), 4)
  )
}


# Test metrics for GAM
test_metrics_gam <- function(model, test_data) {
  preds <- tryCatch(predict(model, newdata = test_data, type = "response", exclude = "s(match_id)", newdata.guaranteed = TRUE), error = function(e) NULL)
  if (is.null(preds)) return(data.frame(Test_RMSE=NA, Test_MAE=NA, Test_R2=NA))
  actual <- as.numeric(test_data$deterioration_next)
  preds <- as.numeric(preds)
  ok <- !is.na(actual) & !is.na(preds) & is.finite(preds)
  data.frame(
    Test_RMSE = round(sqrt(mean((actual[ok] - preds[ok])^2)), 4),
    Test_MAE = round(mean(abs(actual[ok] - preds[ok])), 4),
    Test_R2 = round(cor(actual[ok], preds[ok])^2, 4)
  )
}




#Linear mixed effects model
lmer_full_10 <- lmer(
  deterioration_next ~
    deterioration_lag +
    match_minute +
    player_days_since_last +
    days_into_season +
    cumulative_dist +
    cumulative_hsd +
    h1_peak_fatigue +
    position_group +
    match_venue +
    match_minute:position_group +
    match_minute:match_venue +
    match_minute:player_days_since_last +
    match_minute:h1_peak_fatigue +
    cumulative_dist:position_group +
    cumulative_hsd:position_group +
    (1 | athlete_id) +
    (1 | match_id),
  data = df_model,
  REML = FALSE,
)

lmer_step_10 <- step(lmer_full_10, reduce.random = FALSE)
lmer_best_10 <- get_model(lmer_step_10)
print(lmer_step_10)

lmer_train_10 <- train_metrics_lmer(lmer_best_10)
lmer_test_10 <- test_metrics_lmer(lmer_best_10, test_h2)

cat(sprintf("AIC: %.2f",  lmer_train_10$AIC))
cat(sprintf("Train RMSE:%.4f", lmer_train_10$Train_RMSE))
cat(sprintf("Train MAE:%.4f", lmer_train_10$Train_MAE))
cat(sprintf("Test RMSE:%.4f", lmer_test_10$Test_RMSE))
cat(sprintf("Test MAE:%.4f", lmer_test_10$Test_MAE))
cat(sprintf("Test R²:%.4f", lmer_test_10$Test_R2))




#Quadratic mixed effects
lmer_quad_full_10 <- lmer(
  deterioration_next ~
    deterioration_lag +
    match_minute +
    match_minute2 +
    player_days_since_last +
    days_into_season +
    cumulative_dist +
    cumulative_hsd +
    h1_peak_fatigue +
    position_group +
    match_venue +
    match_minute:position_group +
    match_minute:match_venue +
    match_minute:player_days_since_last +
    match_minute:h1_peak_fatigue +
    cumulative_dist:position_group +
    cumulative_hsd:position_group +
    match_minute2:position_group +
    match_minute2:match_venue +
    (1 | athlete_id) +
    (1 | match_id),
  data = df_model,
  REML = FALSE,
)

lmer_quad_step_10 <- step(lmer_quad_full_10, reduce.random = FALSE)
lmer_quad_best_10 <- get_model(lmer_quad_step_10)
print(lmer_quad_step_10)

lmer_quad_train_10 <- train_metrics_lmer(lmer_quad_best_10)
lmer_quad_test_10 <- test_metrics_lmer(lmer_quad_best_10, test_h2)

cat(sprintf("AIC:%.2f",  lmer_quad_train_10$AIC))
cat(sprintf("Train RMSE:%.4f", lmer_quad_train_10$Train_RMSE))
cat(sprintf("Train MAE:%.4f", lmer_quad_train_10$Train_MAE))
cat(sprintf("Test RMSE:%.4f", lmer_quad_test_10$Test_RMSE))
cat(sprintf("Test MAE:%.4f", lmer_quad_test_10$Test_MAE))
cat(sprintf("Test R²:%.4f", lmer_quad_test_10$Test_R2))


#Cubic mixed effects
lmer_cub_full_10 <- lmer(
  deterioration_next ~
    deterioration_lag +
    match_minute +
    match_minute2 +
    match_minute3 +
    player_days_since_last +
    days_into_season +
    cumulative_dist +
    cumulative_hsd +
    h1_peak_fatigue +
    position_group +
    match_venue +
    match_minute:position_group +
    match_minute:match_venue +
    match_minute:player_days_since_last +
    match_minute:h1_peak_fatigue +
    cumulative_dist:position_group +
    cumulative_hsd:position_group +
    match_minute2:position_group +
    match_minute2:match_venue +
    match_minute3:position_group +
    match_minute3:match_venue +
    (1 | athlete_id) +
    (1 | match_id),
  data = df_model,
  REML = FALSE,
)

lmer_cub_step_10 <- step(lmer_cub_full_10, reduce.random = FALSE)
lmer_cub_best_10 <- get_model(lmer_cub_step_10)
print(lmer_cub_step_10)

lmer_cub_train_10 <- train_metrics_lmer(lmer_cub_best_10)
lmer_cub_test_10 <- test_metrics_lmer(lmer_cub_best_10, test_h2) 

cat(sprintf("AIC:%.2f", lmer_cub_train_10$AIC))
cat(sprintf("Train RMSE:%.4f", lmer_cub_train_10$Train_RMSE))
cat(sprintf("Train MAE:%.4f", lmer_cub_train_10$Train_MAE))
cat(sprintf("Test RMSE:%.4f", lmer_cub_test_10$Test_RMSE))
cat(sprintf("Test MAE:%.4f", lmer_cub_test_10$Test_MAE))
cat(sprintf("Test R²: %.4f", lmer_cub_test_10$Test_R2))






#GAM Gaussian
gam_full_gauss_10 <- gam(
  deterioration_next ~
    s(deterioration_lag, k = 15) +
    s(match_minute, by = position_group) +
    s(match_minute, by = match_venue) +
    s(match_minute, by = player_days_since_last) +
    s(match_minute, by = h1_peak_fatigue) +
    s(cumulative_hsd, by = position_group, k = 20) +
    s(cumulative_hsd, k = 20) +
    s(cumulative_dist, by = position_group, k = 15) +
    s(cumulative_dist, k = 15) +
    s(player_days_since_last) +
    s(days_into_season) +
    s(h1_peak_fatigue, k = 15) +
    position_group +
    match_venue +
    s(athlete_id, bs = "re") +
    s(match_id, bs = "re"),
  data = df_model,
  family = gaussian(),
  method = "ML",
  select = TRUE
)

sm_gauss <- summary(gam_full_gauss_10)
gauss_train_10 <- train_metrics_gam(gam_full_gauss_10, sm_gauss)
gauss_test_10 <- test_metrics_gam(gam_full_gauss_10, test_h2)

print(sm_gauss)
cat(sprintf("AIC: %.2f", gauss_train_10$AIC))
cat(sprintf("Deviance expl: %.1f%%", gauss_train_10$Dev_Expl))
cat(sprintf("Train RMSE: %.4f", gauss_train_10$Train_RMSE))
cat(sprintf("Train MAE: %.4f", gauss_train_10$Train_MAE))
cat(sprintf("Test RMSE: %.4f", gauss_test_10$Test_RMSE))
cat(sprintf("Test MAE: %.4f", gauss_test_10$Test_MAE))
cat(sprintf("Test R²: %.4f", gauss_test_10$Test_R2))

gam.check(gam_full_gauss_10)
plot(gam_full_gauss_10, pages=1, shade=TRUE, shade.col="#1565C020", seWithMean=TRUE, scale=0,rug=TRUE)




#GAM scaled t
gam_full_scat_10 <- gam(
  deterioration_next ~
    s(deterioration_lag, k = 15) +
    s(match_minute, by = position_group) +
    s(match_minute, by = match_venue) +
    s(match_minute, by = player_days_since_last) +
    s(match_minute, by = h1_peak_fatigue) +
    #s(days_into_season, by = player_days_since_last) +
    s(cumulative_hsd, by = position_group, k = 20) +
    s(cumulative_hsd, k = 20) +
    s(cumulative_dist, by = position_group, k = 15) +
    s(cumulative_dist, k = 15) +
    s(player_days_since_last) +
    s(days_into_season) +
    s(h1_peak_fatigue, k = 15) +
    position_group +
    match_venue +
    s(athlete_id, bs = "re") +
    s(match_id, bs = "re"),
  data = df_model,
  family = scat(),
  method = "ML",
  select = TRUE
)

sm_scat <- summary(gam_full_scat_10)
scat_train_10 <- train_metrics_gam(gam_full_scat_10, sm_scat)
scat_test_10 <- test_metrics_gam(gam_full_scat_10, test_h2)

print(sm_scat)
cat(sprintf("AIC: %.2f", scat_train_10$AIC))
cat(sprintf("Deviance expl: %.1f%%", scat_train_10$Dev_Expl))
cat(sprintf("Train RMSE: %.4f", scat_train_10$Train_RMSE))
cat(sprintf("Train MAE: %.4f", scat_train_10$Train_MAE))
cat(sprintf("Test RMSE: %.4f", scat_test_10$Test_RMSE))
cat(sprintf("Test MAE: %.4f", scat_test_10$Test_MAE))
cat(sprintf("Test R²: %.4f", scat_test_10$Test_R2))


plot(gam_full_scat_10, pages=1, shade=TRUE, shade.col="#1565C020", seWithMean=TRUE, rug=TRUE, scale=0)






#comparison table
comparison <- data.frame(
  Model = c("Linear","Quadratic","Cubic","GAM Gaussian","GAM Scaled T"),
  AIC = c(lmer_train_10$AIC, lmer_quad_train_10$AIC, lmer_cub_train_10$AIC, gauss_train_10$AIC, scat_train_10$AIC),
  Train_RMSE = c(lmer_train_10$Train_RMSE, lmer_quad_train_10$Train_RMSE, lmer_cub_train_10$Train_RMSE, 
                 gauss_train_10$Train_RMSE, scat_train_10$Train_RMSE),
  Train_MAE = c(lmer_train_10$Train_MAE, lmer_quad_train_10$Train_MAE, lmer_cub_train_10$Train_MAE, 
                gauss_train_10$Train_MAE, scat_train_10$Train_MAE),
  Dev_Expl = c(NA, NA, NA, gauss_train_10$Dev_Expl, scat_train_10$Dev_Expl),
  Test_RMSE = c(lmer_test_10$Test_RMSE, lmer_quad_test_10$Test_RMSE, lmer_cub_test_10$Test_RMSE, 
                 gauss_test_10$Test_RMSE, scat_test_10$Test_RMSE),
  Test_MAE = c(lmer_test_10$Test_MAE, lmer_quad_test_10$Test_MAE, lmer_cub_test_10$Test_MAE, 
                 gauss_test_10$Test_MAE, scat_test_10$Test_MAE))

comparison$delta_AIC <- round(comparison$AIC - min(comparison$AIC[1:4]), 2)
print(comparison, row.names = FALSE)

# diagnostic plot function
plot_diagnostics <- function(pred_list, model_name, lag_val) {
  actual <- pred_list$actual
  preds <- pred_list$preds
  resids <- pred_list$resids
  minutes <- pred_list$minutes
  rmse <- round(sqrt(mean(resids^2)), 4)
  mae <- round(mean(abs(resids)), 4)
  r2 <- round(cor(actual, preds)^2, 4)
  bias <- round(mean(resids), 4)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # observed vs fitted
  plot(actual, preds, pch = 16, cex = 0.4, col = adjustcolor(HIBS_GREEN, 0.8), 
       xlab = "Observed Deterioration", ylab = "Fitted Deterioration", main = "Observed vs Fitted")
  abline(a = 0, b = 1, col = "blue", lty = 2, lwd = 1.5)
  
  # residuals vs fitted
  plot(preds, resids, pch = 16, cex = 0.4, col = adjustcolor(HIBS_GREEN, 0.8),
       xlab = "Fitted Deterioration", ylab = "Residual", main = "Residuals vs Fitted")
  abline(h = 0, col = "blue", lty = 2, lwd = 1.5)
  
  # residual histogram
  hist(resids, breaks = 40, col = adjustcolor(HIBS_GREEN, 0.8), border = "white",
       xlab = "Residuals", main = "Residual Distribution")
  abline(v = 0, col = "blue", lty = 2, lwd = 1.5)
  
  # residuals vs match minute
  plot(minutes, resids, pch = 16, cex = 0.4, col = adjustcolor(HIBS_GREEN, 0.8),
       xlab = "Match Minute", ylab = "Residuals ", main = "Residuals vs Match Minute", xaxt = "n")
  axis(1, at = seq(46, 90, by = 4))
  abline(h = 0, col = "blue", lty = 2, lwd = 1.5)
  
  par(mfrow = c(1,1), mar = c(5,4,4,2))
}



#run diagnostics for all models
# Linear
preds_linear <- get_preds_lmer(lmer_best_10, test_h2)
plot_diagnostics(preds_linear, "Linear Mixed Effects", lag_minutes)

# Quadratic
preds_quad <- get_preds_lmer(lmer_quad_best_10, test_h2)
plot_diagnostics(preds_quad, "Quadratic Mixed Effects", lag_minutes)

# Cubic
preds_cub <- get_preds_lmer(lmer_cub_best_10, test_h2)
plot_diagnostics(preds_cub, "Cubic Mixed Effects", lag_minutes)

# GAM Gaussian
preds_gauss <- get_preds_gam(gam_full_gauss_10, test_h2)
plot_diagnostics(preds_gauss, "GAM Gaussian", lag_minutes)

# GAM Scaled T
preds_scat <- get_preds_gam(gam_full_scat_10, test_h2)
plot_diagnostics(preds_scat, "GAM Scaled T", lag_minutes)


saveRDS(lmer_best_10, "lmer_lin_lag10.rds")
saveRDS(lmer_quad_best_10, "lmer_quad_lag10.rds")
saveRDS(lmer_cub_best_10, "lmer_cub_lag10.rds")
saveRDS(gam_full_gauss_10, "gam_gauss_lag10.rds")
saveRDS(gam_full_scat_10, "gam_scat_lag10.rds")

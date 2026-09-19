# verification of deterioration as a measure

library(tidyverse)
library(ggplot2)
library(tidyverse)

df_full <- readRDS("model_data.rds")
df_full <- as.data.frame(df_full)
df_full$fatigue_score <- df_full$roll_hia + df_full$roll_hid
df_full$match_minute <- df_full$period_minutes + (df_full$period_id - 1) * 45

# First-half 90th percentile peak
h1_peak <- aggregate(
  fatigue_score ~ athlete_id + match_id,
  data = df_full[df_full$period_id == 1 & !is.na(df_full$fatigue_score), ],
  FUN  = function(x) quantile(x, 0.90, na.rm = TRUE)
)
names(h1_peak)[3] <- "h1_peak_fatigue"
df_full <- merge(df_full, h1_peak, by = c("athlete_id","match_id"), all.x = TRUE)

# deterioration definition (positive = below first-half peak aka how much have they deteriorated)
df_full$deterioration_prop <- (df_full$h1_peak_fatigue - df_full$fatigue_score) / (df_full$h1_peak_fatigue + 0.01)

# find players subbed out in second half
player_summary <- aggregate(
  cbind(first_minute = match_minute, last_minute  = match_minute) ~ athlete_id + match_id + athlete_name + position_group,
  data = df_full,
  FUN = function(x) c(min=min(x), max=max(x))
)

ps <- data.frame(athlete_id = df_full$athlete_id, match_id = df_full$match_id, athlete_name = df_full$athlete_name, position_group = df_full$position_group)
ps <- unique(ps)

first_min <- aggregate(match_minute ~ athlete_id + match_id, data = df_full, FUN = min)
last_min <- aggregate(match_minute ~ athlete_id + match_id, data = df_full, FUN = max)
names(first_min)[3] <- "first_minute"
names(last_min)[3] <- "last_minute"

ps <- merge(ps, first_min, by = c("athlete_id","match_id"))
ps <- merge(ps, last_min, by = c("athlete_id","match_id"))

subbed <- ps[ps$first_minute == 0 & ps$last_minute > 45 & ps$last_minute < 88, ]
not_subbed <- ps[ps$first_minute == 0 & ps$last_minute >= 88, ]

cat(sprintf("Players subbed off in second half: %d", nrow(subbed)))
cat(sprintf("Full-game players: %d", nrow(not_subbed)))

#deterioration at time of sub
sub_records <- do.call(rbind, lapply(seq_len(nrow(subbed)), function(i) {
  pid <- subbed$athlete_id[i]
  mid <- subbed$match_id[i]
  sub_min <- subbed$last_minute[i]
  pdata <- df_full[df_full$athlete_id == pid & df_full$match_id == mid & df_full$match_minute == sub_min, ]
  if (nrow(pdata) == 0 || is.na(pdata$deterioration_prop[1])) return(NULL)
  data.frame(
    Name = subbed$athlete_name[i],
    Position = subbed$position_group[i],
    Sub_Minute = sub_min,
    Deterioration = round(pdata$deterioration_prop[1], 3),
    stringsAsFactors = FALSE)}))

sub_records <- sub_records[order(sub_records$Sub_Minute), ]
print(sub_records, row.names = FALSE)




# Control, second-half minutes for non-substituted players (full time)
control_det <- df_full$deterioration_prop[df_full$athlete_id %in% not_subbed$athlete_id & df_full$match_minute > 45 & !is.na(df_full$deterioration_prop)]
thresholds <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)


#threshold, percent subbed, percent in control, difference
for (tau in thresholds) {
  pct_sub <- mean(sub_records$Deterioration > tau, na.rm=TRUE) * 100
  pct_ctrl <- mean(control_det > tau, na.rm=TRUE) * 100
  cat(sprintf("> %.0f%%    %-5s    %-5s    +%.1f pp\n", tau*100, sprintf("%.1f%%", pct_sub), sprintf("%.1f%%", pct_ctrl), pct_sub - pct_ctrl))
}

# Histogram of deterioration density
hist_df <- rbind(data.frame(deterioration = sub_records$Deterioration, group = "Substituted"),
                 data.frame(deterioration = control_det, group = "Non-substituted"))

ggplot(hist_df, aes(x = deterioration, fill = group, after_stat(density))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity", colour = "white") +
  scale_fill_manual(values = c("Substituted" = "#DC3220", "Non-substituted" = "#1A85FF")) +
  labs(x = "Deterioration", y = "Density", fill = NULL) +
  theme_bw() + theme(panel.grid = element_blank(), legend.position = "bottom")














# Substituted players — deterioration at minute of substitution
sub_final <- sub_records[, c("Deterioration")]
sub_final_df <- data.frame(deterioration = sub_records$Deterioration, group = "Substituted\n(at sub minute)")

# Non-substituted — deterioration at their last recorded minute
not_sub_final <- do.call(rbind, lapply(seq_len(nrow(not_subbed)), function(i) {
  pid <- not_subbed$athlete_id[i]
  mid <- not_subbed$match_id[i]
  last_m <- not_subbed$last_minute[i]
  pdata <- df_full[df_full$athlete_id == pid &
                     df_full$match_id == mid &
                     df_full$match_minute == last_m &
                     !is.na(df_full$deterioration_prop), ]
  if (nrow(pdata) == 0) return(NULL)
  data.frame(deterioration = pdata$deterioration_prop[1], group = "Completed\nFull Match")
}))
plot_df <- rbind(sub_final_df, not_sub_final)

# Deterioration at Point of Substitution vs End of Match
ggplot(plot_df, aes(x = deterioration, fill = group, after_stat(density))) +
  geom_histogram(bins = 25, alpha = 0.6, position = "identity", colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.6) +
  scale_fill_manual(
    values = c("Substituted\n(at sub minute)" = "#DC3220",
               "Completed\nFull Match"         = "#1A85FF")
  ) +  
  annotate("text", x = -0.05, y = Inf, label = "At first-half\npeak", vjust = 1.5, hjust = 1, size = 4, colour = "grey40") +
  annotate("text", x = 0.05, y = Inf, label = "Below first-half\npeak", vjust = 1.5, hjust = 0, size = 4, colour = "grey40") +
  labs(x = "Proportional Deterioration\n(0 = at first-half peak, positive = below peak)",
       y = "Density", fill  = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "right", )

# plots for predictor EDA, what should be included in my models?
# does the predictor vary with deterioration?
# does the predictor effect the deterioration trajectory? 

library(ggplot2)
library(tidyverse)
library(patchwork)


df_full <- readRDS("model_data.rds")
df_full <- as.data.frame(df_full)

pos_levels <- c("CB","FB","DM","CM","AM","CF","W")
gs_levels <- c("Drawing","Winning","Losing")

df_full$fatigue_score <- df_full$roll_hia + df_full$roll_hid
df_full$position_group <- factor(df_full$position_group, levels = pos_levels)
df_full$game_state <- factor(df_full$game_state, levels = gs_levels)
df_full$match_venue <- factor(df_full$match_venue)
df_full$athlete_id <- factor(df_full$athlete_id)
df_full$match_id <- factor(df_full$match_id)

# first-half 90th percentile peak
h1_peak <- aggregate(
  fatigue_score ~ athlete_id + match_id,
  data = df_full[df_full$period_id == 1 & !is.na(df_full$fatigue_score), ],
  FUN = function(x) quantile(x, 0.90, na.rm = TRUE)
)
names(h1_peak)[3] <- "h1_peak_fatigue"
df_full <- merge(df_full, h1_peak, by = c("athlete_id","match_id"), all.x = TRUE)

# deterioration
df_full$deterioration_prop <- (df_full$h1_peak_fatigue - df_full$fatigue_score) / (df_full$h1_peak_fatigue + 0.01)

# Second half only
df_model <- df_full[df_full$match_minute > 45 & !is.na(df_full$deterioration_prop), ]

HIBS_GREEN <- "#00753B"

theme_eda <- theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, colour = "grey40"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)
        )



#Match minute, mean deterioration trajectory across second half
ggplot(df_model, aes(x = match_minute, y = deterioration_prop)) +
  # geom_point(alpha = 0.05, size = 0.4, colour = HIBS_GREEN) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, colour = HIBS_GREEN, fill = HIBS_GREEN, alpha = 0.15, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(45, 90, by = 4)) +
  labs(x = "Match Minute", y = "Deterioration") +
  theme_eda


# match minute interaction w/ position, separate trajectories per position
p_pos_traj<-ggplot(df_model, aes(x = match_minute, y = deterioration_prop, colour = position_group)) +
  #geom_point(alpha = 0.05, size = 0.4, colour = HIBS_GREEN) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE,linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(45, 90, by = 5)) +
  labs(x = "Match Minute", y = "Deterioration", colour = "Position") +
  theme_eda +
  theme(legend.position = "left")
p_pos_traj


#match minute interaction w/ game state, separate trajectories by game state
ggplot(df_model, aes(x = match_minute, y = deterioration_prop, colour = game_state)) +
  #geom_point(alpha = 0.05, size = 0.4, colour = HIBS_GREEN) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_manual(values = c("Drawing" = "blue", "Winning" = HIBS_GREEN, "Losing" = "red")) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(45, 90, by = 5)) +
  labs(x = "Match Minute", y = "Deterioration", colour = "Game State") +
  theme_eda +
  theme(legend.position = "left")


#match minute interaction w/ venue, separate trajectories by venue
p_venue_traj<-ggplot(df_model, aes(x = match_minute, y = deterioration_prop, colour = match_venue, linetype = match_venue)) +
  #geom_point(alpha = 0.05, size = 0.4, colour = HIBS_GREEN) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, alpha = 0.1, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_manual(values = c("home" = "blue", "away" = "red")) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(45, 90, by = 5)) +
  labs(x = "Match Minute", y = "Deterioration", colour = "Venue", linetype = "Venue") +
  theme_eda +
  theme(legend.position = "left")
p_venue_traj


#match minute interaction w/ days since last match
df_model$days_group <- cut(df_model$player_days_since_last, breaks = c(0, 5, 7, 10, Inf),
                           labels = c("<=5 days","6-7 days", "8-10 days",">10 days"), include.lowest = TRUE)

p_days_traj<-ggplot(df_model, aes(x = match_minute, y = deterioration_prop, colour = days_group)) +
  #geom_point(alpha = 0.05, size = 0.4, colour = HIBS_GREEN) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(45, 90, by = 5)) +
  labs(x = "Match Minute", y = "Deterioration", colour = "Recovery Days") +
  theme_eda +
  theme(legend.position = "left")
p_days_traj



#match minute interaction w/ first half peak fatigue
df_model$peak_quartile <- cut(df_model$h1_peak_fatigue,
                              breaks = quantile(df_model$h1_peak_fatigue, probs = 0:4/4, na.rm = TRUE),
                              labels = c("Q1 Low","Q2","Q3","Q4 High"),
                              include.lowest = TRUE)

p_peak_traj<-ggplot(df_model, aes(x = match_minute, y = deterioration_prop, colour = peak_quartile)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = seq(45, 90, by = 5)) +
  labs(x = "Match Minute", y = "Deterioration", colour = "H1 Peak Quartile") +
  theme_eda +
  theme(legend.position = "left")
p_peak_traj

combined_interactions <- (p_pos_traj + labs(title = "Position Group")) +
  (p_venue_traj + labs(title = "Match Venue")) +
  (p_days_traj + labs(title = "Days Since Last Match")) +
  (p_peak_traj + labs(title = "First-Half Peak")) +
  plot_layout(nrow = 2, ncol = 2)
combined_interactions



#first half peak fatigue
p_peak<-ggplot(df_model, aes(x = h1_peak_fatigue, y = deterioration_prop)) +
  #geom_point(alpha = 0.4, size = 1.5, colour = HIBS_GREEN) +
  coord_cartesian(ylim = c(-0.25, NA)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, colour = "red", fill = "red", alpha = 0.15, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  labs(x = "First-Half 90th Percentile Peak", y = "Deterioration") +
  theme_eda
p_peak





#position group
# pos_summary <- aggregate(deterioration_prop ~ position_group, data = df_model, FUN = mean, na.rm = TRUE)
# pos_summary$se <- aggregate(deterioration_prop ~ position_group, data = df_model, FUN = function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))$deterioration_prop
# 
# ggplot(pos_summary, aes(x = position_group, y = deterioration_prop, colour = position_group, group = 1)) +
#   geom_line(colour = "grey60", linewidth = 0.6) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = deterioration_prop - 2*se, ymax = deterioration_prop + 2*se), width = 0.2, linewidth = 0.6) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
#   scale_colour_brewer(palette = "Dark2") +
#   labs(x = "Position Group", y = "Mean Deterioration") +
#   theme_eda +
#   theme(legend.position = "none")
# 


#game state
# ggplot(df_model, aes(x = game_state, y = deterioration_prop, fill = game_state)) +
#   geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.3) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
#   scale_fill_manual(values = c("Drawing" = "blue", "Winning" = HIBS_GREEN, "Losing" = "red")) +
#   labs(x = "Game State", y = "Deterioration") +
#   theme_eda +
#   theme(legend.position = "none")


# 
# gs_summary <- aggregate(deterioration_prop ~ game_state, data = df_model, FUN = mean, na.rm = TRUE)
# gs_summary$se <- aggregate(deterioration_prop ~ game_state, data = df_model, FUN = function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))$deterioration_prop
# 
# ggplot(gs_summary, aes(x = game_state, y = deterioration_prop, colour = game_state, group = 1)) +
#   geom_line(colour = "grey60", linewidth = 0.6) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = deterioration_prop - 2*se, ymax = deterioration_prop + 2*se), width = 0.2, linewidth = 0.6) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
#   scale_colour_manual(values = c("Drawing" = "blue", "Winning" = HIBS_GREEN, "Losing"  = "red")) +
#   labs(x = "Game State", y = "Mean Deterioration") +
#   theme_eda +
#   theme(legend.position = "none")






#cumulative HSD
ggplot(df_model, aes(x = cumulative_hsd, y = deterioration_prop)) +
  #geom_point(alpha = 0.1, size = 0.6, colour = HIBS_GREEN) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, colour = "red", fill = "red", alpha = 0.15, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Cumulative High-Speed Distance (m)", y = "Deterioration") +
  theme_eda


#cumulative HSD interaction w/ position
df_model$hsd_quartile <- cut(
  df_model$cumulative_hsd,
  breaks = quantile(df_model$cumulative_hsd, probs = 0:4/4, na.rm = TRUE),
  labels = c("Q1","Q2","Q3","Q4"),
  include.lowest = TRUE
)

hsd_pos <- aggregate(
  deterioration_prop ~ hsd_quartile + position_group,
  data = df_model[!is.na(df_model$hsd_quartile), ],
  FUN = mean, na.rm = TRUE
)

p_cumHSD<-ggplot(hsd_pos, aes(x = hsd_quartile, y = deterioration_prop, colour = position_group, group = position_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Cumulative HSD Quartile", y = "Mean Deterioration", colour = "Position") +
  theme_eda +
  theme(legend.position = "bottom")
p_cumHSD




#cumulative distance covered
ggplot(df_model, aes(x = cumulative_dist, y = deterioration_prop)) +
  #geom_point(alpha = 0.1, size = 0.6, colour = HIBS_GREEN) +
  coord_cartesian(ylim = c(0, NA)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, colour = "red", fill = "red", alpha = 0.15, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  labs(x = "Cumulative Distance Covered (m)", y = "Deterioration") +
  theme_eda


#cumulative distance interaction w/ position
df_model$dist_quartile <- cut(
  df_model$cumulative_dist,
  breaks = quantile(df_model$cumulative_dist, probs = 0:4/4, na.rm = TRUE),
  labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE
)
dist_pos <- aggregate(
  deterioration_prop ~ dist_quartile + position_group,
  data = df_model[!is.na(df_model$dist_quartile), ],
  FUN = mean, na.rm = TRUE
)

p_cumD<-ggplot(dist_pos, aes(x = dist_quartile, y = deterioration_prop, colour = position_group, group = position_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Cumulative Distance Quartile", y = "Mean Deterioration", colour = "Position") +
  theme_eda +
  theme(legend.position = "bottom")
p_cumD


#days since last match
# ggplot(df_model[!is.na(df_model$days_group), ], aes(x = days_group, y = deterioration_prop, fill = days_group)) +
#   geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.3) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
#   scale_fill_brewer(palette = "Dark2") +
#   labs(x = "Days Since Last Match", y = "Deterioration") +
#   theme_eda +
#   theme(legend.position = "none")


# days_summary <- aggregate(deterioration_prop ~ days_group, data = df_model[!is.na(df_model$days_group), ], FUN = mean, na.rm = TRUE)
# days_summary$se <- aggregate(deterioration_prop ~ days_group, data = df_model[!is.na(df_model$days_group), ],
#                              FUN = function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))$deterioration_prop
# 
# ggplot(days_summary, aes(x = days_group, y = deterioration_prop, group = 1)) +
#   geom_line(colour = HIBS_GREEN, linewidth = 0.8) +
#   geom_point(colour = HIBS_GREEN, size = 4) +
#   geom_errorbar(aes(ymin = deterioration_prop - 2*se, ymax = deterioration_prop + 2*se), colour = HIBS_GREEN, width = 0.2, linewidth = 0.6) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
#   labs(x = "Days Since Last Match", y = "Mean Deterioration") +
#   theme_eda






#match venue
# ggplot(df_model, aes(x = match_venue, y = deterioration_prop, fill = match_venue)) +
#   geom_boxplot(alpha = 0.7, outlier.size = 0.8, outlier.alpha = 0.3) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
#   scale_fill_manual(values = c("home" = "blue", "away" = "red")) +
#   labs(x = "Match Venue", y = "Deterioration") +
#   theme_eda +
#   theme(legend.position = "none")
# 
# venue_summary <- aggregate(deterioration_prop ~ match_venue, data = df_model, FUN = mean, na.rm = TRUE)
# venue_summary$se <- aggregate(deterioration_prop ~ match_venue, data = df_model,
#                               FUN = function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))$deterioration_prop
# 
# ggplot(venue_summary, aes(x = match_venue, y = deterioration_prop, colour = match_venue, group = 1)) +
#   geom_line(colour = "grey60", linewidth = 0.6) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = deterioration_prop - 2*se, ymax = deterioration_prop + 2*se), width = 0.2, linewidth = 0.6) +
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
#   scale_colour_manual(values = c("home" = "blue", "away" = "red")) +
#   labs(x = "Match Venue", y = "Mean Deterioration") +
#   theme_eda +
#   theme(legend.position = "none")









#days into season
p_season<-ggplot(df_model, aes(x = days_into_season, y = deterioration_prop)) +
  #geom_point(alpha = 0.5, size = 1.2, colour = HIBS_GREEN) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = FALSE, colour = "red", fill = "red", alpha = 0.15, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = "Days Into Season", y = "Deterioration") +
  theme_eda
p_season

#interaction with days since last match
# df_model$season_third <- cut(
#   df_model$days_into_season,
#   breaks = quantile(df_model$days_into_season, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
#   labels = c("Early","Mid","Late"),
#   include.lowest = TRUE
# )
# 
# season_days <- aggregate(
#   deterioration_prop ~ days_group + season_third,
#   data = df_model[!is.na(df_model$days_group) &
#                     !is.na(df_model$season_third), ],
#   FUN = mean, na.rm = TRUE
# )
# 
# ggplot(season_days, aes(x = days_group, y = deterioration_prop, colour = season_third, group = season_third)) +
#   geom_line(linewidth = 0.9) +
#   geom_point(size = 2.5) +
#   geom_hline(yintercept = 0, linetype = "dashed",
#              colour = "grey40", linewidth = 0.5) +
#   scale_colour_manual(values = c("Early"  = "#2E7D32", "Mid"  = "blue", "Late"   = "red")) +
#   labs(x = "Days Since Last Match", y = "Mean Deterioration", colour = "Season Stage") +
#   theme_eda +
#   theme(legend.position = "bottom")


combines_others1 <- (p_peak + labs(title = "First Half Peak")) +
  (p_season + labs(title = "Days into Season")) +
  plot_layout(nrow = 1, ncol = 2)
combines_others1

combines_others2 <- (p_cumHSD + labs(title = "Cumulative HSD x Position")) +
  (p_cumD + labs(title = "Cumulative Distance x Position")) +
  plot_layout(nrow = 1, ncol = 2)
combines_others2



p_hsd_pos <- ggplot(hsd_pos, aes(x = hsd_quartile, y = deterioration_prop, colour = position_group, group = position_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Cumulative HSD x Position", x = "Cumulative HSD Quartile", y = "Mean Deterioration", colour = "Position") +
  theme_eda

p_dist_pos <- ggplot(dist_pos, aes(x = dist_quartile, y = deterioration_prop, colour = position_group, group = position_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_colour_brewer(palette = "Set1") +
  labs(title  = "Cumulative Distance x Position", x = "Cumulative Distance Quartile", y = "Mean Deterioration", colour = "Position") +
  theme_eda

(p_hsd_pos + p_dist_pos) + plot_layout(guides = "collect") & theme(legend.position = "bottom")












#athlete random
player_traj <- aggregate( deterioration_prop ~ match_minute + athlete_id + athlete_name, data = df_model, FUN = mean, na.rm = TRUE)

player_rand<-ggplot(player_traj, aes(x = match_minute, y = deterioration_prop, group = athlete_id)) +
  geom_line(alpha = 0.5, linewidth = 0.5, colour = HIBS_GREEN) +
  geom_smooth(aes(group = 1), method = "gam", formula = y ~ s(x), se = TRUE, colour = "red", fill = "red", alpha = 0.15, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(45, 90, by = 5)) +
  labs(x = "Match Minute", y = "Mean Deterioration") +
  theme_eda

#match random
match_traj <- aggregate(deterioration_prop ~ match_minute + match_id, data = df_model, FUN = mean, na.rm = TRUE)

match_rand<-ggplot(match_traj, aes(x = match_minute, y = deterioration_prop, group = match_id)) +
  geom_line(alpha = 0.5, linewidth = 0.5, colour = HIBS_GREEN) +
  geom_smooth(aes(group = 1), method = "gam", formula = y ~ s(x), se = TRUE, colour = "red", fill = "red", alpha = 0.15, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  scale_x_continuous(breaks = seq(45, 90, by = 5)) +
  labs(x = "Match Minute", y = "Mean Deterioration") +
  theme_eda

combines_random <- (player_rand + labs(title = "Individual Player Deterioration")) +
  (match_rand + labs(title = "Individual Match Deterioration")) +
  plot_layout(nrow = 1, ncol = 2)
combines_random


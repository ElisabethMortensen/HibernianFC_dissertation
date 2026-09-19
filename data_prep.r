# Loads and cleans the Hibernian FC GPS/accelerometer data
# Run this script first to produce model_data.rds used by all other scripts
# Ensure that the minute by minute and match data files are in the working directory

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)

# Load data
mbm <- read_csv("minute_by_minute.csv", show_col_types = FALSE)
matches <- read_csv("matches.csv", show_col_types = FALSE)
mbm <- as.data.frame(mbm)
matches <- as.data.frame(matches)

# Order by match date
matches$match_date <- as.Date(matches$match_date)
matches <- matches[order(matches$match_date), ]

# set match minutes to be 0-90 minutes
mbm$match_minute <- ifelse(mbm$period_id == 1L, mbm$period_minutes, mbm$period_minutes + 45L)


# classify appearances as starters and full match
# starter defined as a player appearing from minute 0
# full match defined as starting in mins 0 AND played until minute 90.
appearance_first <- aggregate(match_minute ~ athlete_id + match_id, data = mbm, FUN = min)
names(appearance_first)[3] <- "first_minute"
appearance_last <- aggregate(match_minute ~ athlete_id + match_id, data = mbm, FUN = max)
names(appearance_last)[3] <- "last_minute"
appearance_info <- merge(appearance_first, appearance_last, by = c("athlete_id", "match_id"))
appearance_info$is_starter <- appearance_info$first_minute == 0
appearance_info$is_full_game <- appearance_info$first_minute == 0 & appearance_info$last_minute >= 90

cat("Total appearances:", nrow(appearance_info))
cat("Starters:", sum(appearance_info$is_starter))
cat("Full-game appearances:", sum(appearance_info$is_full_game))

#create data frame that restricts to just starters
mbm <- merge(mbm, appearance_info, by = c("athlete_id", "match_id"), all.x = TRUE)
df <- mbm[!is.na(mbm$is_starter) & mbm$is_starter == TRUE, ]
df <- as.data.frame(df)

# other derived metrics and match data
df$hiad <- df$high_intensity_accelerations + df$high_intensity_decelerations
df$goal_diff <- df$goals_for - df$goals_against

# cumulative within-match load
# lag included so that model only sees past load
df <- df[order(df$athlete_id, df$match_id, df$match_minute), ]
lag_cumsum <- function(x) {
  cs <- cumsum(x)
  c(0, cs[-length(cs)])
}
df$cumulative_hia <- ave(df$high_intensity_accelerations, df$athlete_id, df$match_id,FUN = lag_cumsum)
df$cumulative_hid <- ave(df$high_intensity_decelerations, df$athlete_id, df$match_id,FUN = lag_cumsum)
df$cumulative_hiad <- ave(df$hiad, df$athlete_id, df$match_id,FUN = lag_cumsum)
df$cumulative_hsd <- ave(df$high_speed_distance_covered, df$athlete_id, df$match_id,FUN = lag_cumsum)
df$cumulative_dist <- ave(df$distance_covered, df$athlete_id, df$match_id, FUN = lag_cumsum)



# match-level days since last fixture (team level)
matches$days_since_last <- c(NA, as.numeric(diff(matches$match_date)))

# days into the season at match-level, relative to the first match of the season
season_start <- min(matches$match_date)
matches$days_into_season <- as.numeric(matches$match_date - season_start)

# rest period for the team (3 days, from the 31 July 2025 pre-season fixture to the season opener on 3 August 2025)
matches$days_since_last[is.na(matches$days_since_last)] <- 3

df <- merge(df,matches[, c("match_id", "match_venue", "match_date", "days_since_last", "days_into_season")], by = "match_id", all.x = TRUE)

# for each player, the number of days since their individual previous appearance
player_match_dates <- df[!duplicated(df[, c("athlete_id", "match_id")]), c("athlete_id", "match_id", "match_date")]
player_match_dates <- player_match_dates[order(player_match_dates$athlete_id, player_match_dates$match_date), ]

player_match_dates$player_days_since_last <- ave(as.numeric(player_match_dates$match_date),
                                                 player_match_dates$athlete_id,
                                                 FUN = function(x) c(NA, diff(x)))

df <- merge(df,player_match_dates[, c("athlete_id", "match_id", "player_days_since_last")],
            by = c("athlete_id", "match_id"), all.x = TRUE)

# assume the same rest period as the team 
# (3 days, from the 31 July 2025 pre-season fixture to the season opener on 3 August 2025)
df$player_days_since_last[is.na(df$player_days_since_last)] <- 3

pos_levels <- c("CB", "FB", "DM", "CM", "AM", "CF", "W")
df$position_group <- factor(df$position_group, levels = pos_levels)
df$game_state <- factor(df$game_state, levels = c("Drawing", "Winning", "Losing"))


#add rolling windows to data frame
rolling_window <- 10L
df <- df %>%
  arrange(athlete_id, match_id, match_minute) %>%
  group_by(athlete_id, match_id) %>%
  mutate(
    roll_hia= rollapply(high_intensity_accelerations, rolling_window, sum,  fill = NA, align = "right", partial = TRUE),
    roll_hid= rollapply(high_intensity_decelerations, rolling_window, sum,  fill = NA, align = "right", partial = TRUE),
    roll_hsd_mean = rollapply(high_speed_distance_covered, rolling_window, mean, fill = NA, align = "right", partial = TRUE)
  ) %>%
  ungroup()
df <- as.data.frame(df)


# save data file
saveRDS(df, "model_data.rds")
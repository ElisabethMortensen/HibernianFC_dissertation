# Real-Time Deterioration Insight

library(ggplot2)
library(gridExtra)


HIBS_GREEN <- "#00753B"

# load and prep data 
test_df <- readRDS("test_data.rds")
test_df <- as.data.frame(test_df)
pos_levels <- c("CB","FB","DM","CM","AM","CF","W")
gs_levels <- c("Drawing","Winning","Losing")
test_df$position_group <- factor(test_df$position_group, levels = pos_levels)
test_df$game_state <- factor(test_df$game_state, levels = gs_levels)
test_df$match_venue <- factor(test_df$match_venue)
test_df$athlete_id <- factor(test_df$athlete_id)
test_df$match_id <- factor(test_df$match_id)
 
# Match minute
test_df$match_minute <- ifelse(test_df$period_id == 1, test_df$period_minutes, test_df$period_minutes + 45)

# Fatigue score
test_df$fatigue_score <- test_df$roll_hia + test_df$roll_hid

# First-half 90th percentile peak per player-match
h1_peak <- aggregate(
  fatigue_score ~ athlete_id + match_id,
  data = test_df[test_df$period_id == 1 & !is.na(test_df$fatigue_score), ],
  FUN  = function(x) quantile(x, 0.90, na.rm = TRUE)
)
names(h1_peak)[3] <- "h1_peak_fatigue"
test_df <- merge(test_df, h1_peak, by = c("athlete_id","match_id"), all.x = TRUE)

test_df$deterioration_prop <- (test_df$h1_peak_fatigue - test_df$fatigue_score) / (test_df$h1_peak_fatigue + 0.01)

#select example from test set
set.seed(50)
example_match <- as.character(sample(unique(test_df$match_id), 1))
example_minute <- 68L

#build deterioration "dashboard"
deterioration_dashboard <- function(data, match_id_val, minute_t) {
  match_data <- data[as.character(data$match_id) == match_id_val, ]
  live <- match_data[match_data$match_minute == minute_t & !is.na(match_data$deterioration_prop), ]
  live <- live[order(live$deterioration_prop, decreasing = TRUE), ]
  live <- head(live, 5) 
  gs <- as.character(live$game_state[1])
  gf <- live$goals_for[1]
  ga <- live$goals_against[1]
  venue <- toupper(as.character(live$match_venue[1]))
  invisible(live[, c("athlete_name", "position_group", "deterioration_prop", "game_state", "goals_for", "goals_against", "match_venue")])
}

#Run example match
result <- deterioration_dashboard(data = test_df, match_id_val = example_match, minute_t = example_minute)


#build  table
table_df <- data.frame(
  Rank = seq_len(nrow(result)),
  Player = substr(as.character(result$athlete_name), 1, 20),
  Position = as.character(result$position_group),
  Deterioration = sprintf("%.1f%%", result$deterioration_prop * 100),
  stringsAsFactors = FALSE
)


# match context used in title
gs <- as.character(result$game_state[1])
gf <- result$goals_for[1]
ga <- result$goals_against[1]
venue <- toupper(as.character(result$match_venue[1]))

#table visual
tt <- gridExtra::tableGrob(
  table_df, rows = NULL, theme = gridExtra::ttheme_minimal(colhead = list(fg_params = list(fontface = "bold", fontsize = 10, col = "white"),
  bg_params = list(fill = "#00753B", col = NA)),
  core = list(fg_params = list(fontsize = 9),bg_params = list(fill = c("white","grey95"), col = "white"))))


gridExtra::grid.arrange(tt,
  top = grid::textGrob(sprintf("Hibernian FC Fatigue Monitor\nMinute %s    Match %s    Score %d-%d (%s)",
                               example_minute, example_match, gf, ga, gs),
  gp = grid::gpar(fontsize = 13,fontface = "bold",col = HIBS_GREEN)),padding = unit(2, "mm"))
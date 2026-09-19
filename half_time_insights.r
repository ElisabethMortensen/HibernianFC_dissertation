# Half-Time Deterioration Insight

library(ggplot2)
library(gridExtra)
library(patchwork)
library(grid)

HIBS_GREEN <- "#00753B"

#oad and prep data
test_df <- readRDS("test_data.rds")
test_df <- as.data.frame(test_df)
pos_levels <- c("CB","FB","DM","CM","AM","CF","W")
gs_levels <- c("Drawing","Winning","Losing")
test_df$position_group <- factor(test_df$position_group, levels = pos_levels)
test_df$game_state <- factor(test_df$game_state,     levels = gs_levels)
test_df$match_venue <- factor(test_df$match_venue)
test_df$athlete_id <- factor(test_df$athlete_id)
test_df$match_id <- factor(test_df$match_id)

test_df$match_minute  <- ifelse(test_df$period_id == 1, test_df$period_minutes, test_df$period_minutes + 45)
test_df$fatigue_score <- test_df$roll_hia + test_df$roll_hid

h1_peak <- aggregate(
  fatigue_score ~ athlete_id + match_id,
  data = test_df[test_df$period_id == 1 & !is.na(test_df$fatigue_score), ],
  FUN = function(x) quantile(x, 0.90, na.rm = TRUE)
)
names(h1_peak)[3] <- "h1_peak_fatigue"
test_df <- merge(test_df, h1_peak, by = c("athlete_id","match_id"), all.x = TRUE)

test_df$deterioration_prop <- (test_df$h1_peak_fatigue - test_df$fatigue_score) / (test_df$h1_peak_fatigue + 0.01)


# seelect sample match from test data set
set.seed(42)
example_match <- as.character(sample(unique(test_df$match_id), 1))


#build half-time report
halftime_report <- function(data, match_id_val, top_n = 5) {
  match_data <- data[as.character(data$match_id) == match_id_val, ]
  
  # First half with valid fatigue score
  h1 <- match_data[match_data$period_id == 1 & !is.na(match_data$fatigue_score), ]
  if (nrow(h1) == 0) return(invisible(NULL))
  
  # Last minute per player in first half
  last_h1 <- aggregate(match_minute ~ athlete_id, data = h1, FUN = max)
  names(last_h1)[2] <- "last_minute"
  
  # Deterioration at last first-half minute
  end_h1 <- do.call(rbind, lapply(seq_len(nrow(last_h1)), function(i) {
    pid <- as.character(last_h1$athlete_id[i])
    min <- last_h1$last_minute[i]
    row <- match_data[as.character(match_data$athlete_id) == pid & match_data$match_minute == min & !is.na(match_data$deterioration_prop), ]
    if (nrow(row) == 0) return(NULL) row[1, ]}))
  
  if (is.null(end_h1) || nrow(end_h1) == 0) return(invisible(NULL))
  end_h1 <- end_h1[order(end_h1$deterioration_prop, decreasing = TRUE), ]
  end_h1 <- head(end_h1, 5)  
  invisible(head(end_h1, top_n))
}


# run half time report
result_ht <- halftime_report(data= test_df, match_id_val = example_match, top_n= 5)

# Match context for title
gs <- as.character(result_ht$game_state[1])
gf <- result_ht$goals_for[1]
ga <- result_ht$goals_against[1]
venue <- toupper(as.character(result_ht$match_venue[1]))

# Build table dataframe
table_df <- data.frame(
  Rank = seq_len(nrow(result_ht)),
  Player = substr(as.character(result_ht$athlete_name), 1, 20),
  Position = as.character(result_ht$position_group),
  Deterioration = sprintf("%.1f%%", result_ht$deterioration_prop * 100),
  stringsAsFactors = FALSE
)

# Build table visual
tt <- gridExtra::tableGrob(
  table_df, rows = NULL,
  theme = gridExtra::ttheme_minimal(
    colhead = list(fg_params = list(fontface = "bold",fontsize = 10, col= "white"),
      bg_params = list(fill = HIBS_GREEN, col = NA)),
    core = list(fg_params = list(fontsize = 9), bg_params = list(fill = c("white","grey96"), col= "white"))))

gridExtra::grid.arrange(
  tt, top = grid::textGrob(sprintf("Hibernian FC Half Time Fatigue Report\n Match %s     Score %d-%d (%s) ",
                                   example_match,gf, ga, gs),
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = HIBS_GREEN)), padding = unit(2, "mm"))
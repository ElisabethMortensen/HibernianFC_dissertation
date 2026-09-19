# Hibernian FC, Football Player Fatigue Monitoring

# Consultancy Project

This project uses GPS and accelerator data from Hibernian FC's 25/26 Scottish Premiership season to define and monitor fatigue.

## Data

Two data files provided by Hibernian FC are required and should be added to the working directory where you will be running the files, `minute_by_minute.csv`, GPS and accelerometer data and `matches.csv`, match level data.

## R Script Descriptions

Scripts must be run in the following order:

1\. `data_prep.R`

2\. `split_train_test.R`

3\. `param_eda.R`

4\. `deterioration_verify.R`

5\. `lag1_models.R`

6\. `lag5_models.R`

7\. `lag10_models.R`

8\. `flagging_accuracy.R`

9\. `half_time_insights.R`

10\. `deterioration_insights.R`

### `data_prep.R`

Loads GPS and match data, computes rolling 10-minute window metrics for all five external load variables (HIA, HID, HSD, distance, max velocity), calculates match minute as a continuous 0–90 scale, and merges player and match data. Outputs `model_data.rds`, the full pre-processed dataset used by all subsequent scripts.

### `final_split_train_test.R`

Splits the 35 available matches into a training set (28 matches) and a test set (7 matches). Tries 500 random seeds and selects the seed producing the most balanced split across match result distribution, venue balance, season thirds, and mean fatigue.

### `param_eda.R`

Exploratory analysis of all candidate model predictors against deterioration. Used to justify predictor inclusion before modelling.

### `deterioration_verify.R`

Exploratory analysis of deterioration metric against observed substitutions to verify as a meanigful and accurate metric for fatigue.

### `lag1_models.R`

Fits all five model specifications at lag $\ell = 1$ (one-minute-ahead prediction).

### `lag5_models.R`

Fits all five model specifications at lag $\ell = 5$ (five-minute-ahead prediction).

### `lag10_models.R`

Fits all five model specifications at lag $\ell = 10$ (ten-minute-ahead prediction).

### `flagging_accuracy.R`

Evaluates flagging accuracy of the selected linear mixed effects models at each lag.

### `half_time_insights.R`

Half-time deterioration monitoring insight visual. At half-time, ranks all players by their observed deterioration and displays the top 5 most fatigued players.

### `deterioration_insights.R`

Real-time deterioration monitoring insight visual. For a given match and minute, ranks all starting players by their current observed deterioration and displays the top 5 most fatigued players.

# step_metrics
Function to calculate step metrics from epochSummary actigraph files

At the moment, the function generates 2 databases, at day level and person level (daily average). Averages are obtained as plain and weighted means. The metrics provided are:
  - Steps per day
  - Peak 60, 30 and 1 min cadences
  - Time spent in cadence bands from 0 to 120 steps/min in 20 steps/min increments
  - Time spent at user-specified moderate and vigorous PA thresholds based on steps/min
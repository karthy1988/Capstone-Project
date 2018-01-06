library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)

team_statistics <- read.csv("team_statistics.csv")

team_statistics <- team_statistics[,2:10]


# No linear relation
team_statistics %>% ggplot(aes(x = three_pt_Percentage_mean, y = opp_three_pt_Percentage_mean)) +
  geom_point() + 
  geom_smooth(method = "lm")

# No linear relation
team_statistics %>% ggplot(aes(x = three_pt_Made_mean, y = opp_three_pt_Made_mean)) +
  geom_point() + 
  geom_smooth(method = "lm")

# No linear relation
team_statistics %>% ggplot(aes(x = Percent_of_Points_mean, y = opp_Percent_of_Points_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")


# Positive linear relation, high residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = three_pt_Percentage_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

wins_v_tpp <- lm(three_pt_Percentage_mean ~ win_team_sum, data = team_statistics)


# Positive linear relation, high residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = three_pt_Made_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

wins_v_tpm <- lm(three_pt_Made_mean ~ win_team_sum, data = team_statistics)


# Positive linear relation, high residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = Percent_of_Points_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

wins_v_pop <- lm(Percent_of_Points_mean ~ win_team_sum, data = team_statistics)


# Negative linear relation, high residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = opp_three_pt_Made_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

wins_v_otpm <- lm(opp_three_pt_Made_mean ~ win_team_sum, data = team_statistics)


# Negative linear relation, small residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = opp_three_pt_Percentage_mean)) + 
  geom_point() +
  geom_smooth(method = "lm")

wins_v_otpp <- lm(opp_three_pt_Percentage_mean ~ win_team_sum, data = team_statistics)


# No linear relation
team_statistics %>% ggplot(aes(x = win_team_sum, y = opp_Percent_of_Points_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Positive linear relation, high residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = diff_tpm)) +
  geom_point() +
  geom_smooth(method = "lm")

# Positive linear relation, small residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = diff_tpp)) +
  geom_point() +
  geom_smooth(method = "lm")

# Positive linear relation, high residuals
team_statistics %>% ggplot(aes(x = win_team_sum, y = diff_pop)) +
  geom_point() +
  geom_smooth(method = "lm")
library(dplyr)
library(tidyverse)

team_gamelogs <- read.csv("complete_gamelogs.csv")


b <- team_gamelogs[,c(2,4:11,15:17)]

team_statistics <- team_gamelogs %>%
                    group_by(team) %>%
                    summarise_each(funs(mean, sum), c(three_pt_Made,three_pt_Percentage,opp_three_pt_Made,opp_three_pt_Percentage,Percent_of_Points,opp_Percent_of_Points,win_team))

team_statistics$three_pt_Percentage_sum = NULL
team_statistics$opp_three_pt_Percentage_sum = NULL
team_statistics$Percent_of_Points_sum = NULL
team_statistics$opp_Percent_of_Points_sum = NULL
team_statistics$three_pt_Made_sum = NULL
team_statistics$opp_three_pt_Made_sum = NULL

team_statistics <- team_statistics %>%
                    mutate(diff_tpm = three_pt_Made_mean - opp_three_pt_Made_mean,
                           diff_tpp = three_pt_Percentage_mean - opp_three_pt_Percentage_mean,
                           diff_pop = Percent_of_Points_mean - opp_Percent_of_Points_mean)

write.csv(team_statistics,"team_statistics.csv")
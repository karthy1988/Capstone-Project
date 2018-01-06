library(dplyr)
library(tidyverse)

all_gamelogs <- read.csv("team_gamelogs_3s.csv")


a <- all_gamelogs[,2:7]

# Find Opponents three point attempts
with_opp_att = a %>%
  group_by(game.id) %>%
  mutate(opp_three_pt_Attempts = rev(three_pt_Attempts))

# Find Opponents three point made
with_opp_made = with_opp_att %>%
  group_by(game.id) %>%
  mutate(opp_three_pt_Made = rev(three_pt_Made))

# Find Opponents three point percentage
with_opp_tpp = with_opp_made %>%
  group_by(game.id) %>%
  mutate(opp_three_pt_Percentage = rev(three_pt_Percentage))


# Find max points for each game
with_win_col = with_opp_tpp %>% 
  group_by(game.id) %>% 
  mutate(win_c = max(total_pts.points))

# add 1 for max row
with_win_col$win_team = ifelse(with_win_col$total_pts.points == with_win_col$win_c,1,0)

# drop win_c column
with_win_col$win_c = NULL

# Add difference column for each 3-point stat per team per game
# Add percentage of point are contributed by 3-pointers
with_win_col <- with_win_col %>%
                  mutate(diff_tpa = three_pt_Attempts - opp_three_pt_Attempts,
                         diff_tpm = three_pt_Made - opp_three_pt_Made,
                         diff_tpp = three_pt_Percentage - opp_three_pt_Percentage,
                         Percent_of_Points = (three_pt_Made*3)/total_pts.points)

# Find opponents Percent_of_Points
with_win_col <- with_win_col %>%
                  group_by(game.id) %>%
                  mutate(opp_Percent_of_Points = rev(Percent_of_Points))

# Find difference of Percent_of_Points for each game
with_win_col <- with_win_col %>%
                  mutate(diff_pop = Percent_of_Points - opp_Percent_of_Points)
    


write.csv(with_win_col,file="complete_gamelogs.csv")
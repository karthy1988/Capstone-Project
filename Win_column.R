library(dplyr)
library(tidyverse)

all_gamelogs <- read.csv("team_gamelogs_3s.csv")
all_gamelogs$win <- 0

a <- all_gamelogs[,2:8]

outcome_gamelogs <- data.frame(team = character(),
                           game.id = numeric(),
                           three_pt_Attempts = numeric(),
                           three_pt_Made = numeric(),
                           three_pt_Percentage = numeric(),
                           win = numeric())

# winner <- function(x,y){
#   if(x > y) {
#     return(game_score$win[1] <- 1)
#   } else {
#     return(game_score$win[2] <- 1)
#   }
# }


# Add Win columns
for(i in a$game.id){
  game_score <- a %>% 
    filter(game.id == i)

  if(game_score$total_pts.points[1] > game_score$total_pts.points[2]) {
    game_score$win[1] <- 1
  } else {
    game_score$win[2] <- 1
  }
  
  outcome_gamelogs <- merge(outcome_gamelogs,game_score,all=TRUE)
}

write.csv(outcome_gamelogs,file="outcome_gamelogs.csv")
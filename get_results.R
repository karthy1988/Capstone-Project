library(mysportsfeedsR)
library(httr)
library(dplyr)
library(tidyverse)


# Log into MySportsFeed
.MySportsFeedsEnv <- new.env()
.MySportsFeedsEnv$data <- list( v1_0_username <- "USERNAME", v1_0_password <- "PASSWORD")
authenticate_v1_0('USERNAME', 'PASSWORD')

authenticate_v1_0('chiga','abc123')

# List of all NBA Teams
teams <- c("bos", "bro", "nyk", "phi", "tor",
           "gsw", "lac", "lal", "phx", "sac",
           "chi", "cle", "det", "ind", "mil",
           "dal", "hou", "mem", "nop", "sas",
           "atl", "cha", "mia", "orl", "was",
           "den", "min", "okl", "por", "uta")

# Function to get game logs for nba_team
nba_gamelog <- function (nba_team) {
  team_gamelog <- msf_get_results(league='nba',
                                  season='2016-2017-regular',
                                  feed='team_gamelogs',
                                  params=list(team=nba_team))
  return(team_gamelog)
}

# Initialize a data frame
all_gamelogs <- data.frame(team = character(),
                           game.id = numeric(),
                           three_pt_Attempts = numeric(),
                           three_pt_Made = numeric(),
                           three_pt_Percentage = numeric())

# Make for loop to go through each team, grab gamelog data, select relevent columns, and store them in their own dataframe
for (t in teams){
  t_gamelog <- nba_gamelog(t)
  
  # Pull the dataframe
  t_gamelog_df <- t_gamelog$api_json$teamgamelogs$gamelogs
  
  # Wrangle the pulled data sets
  t_gamelog_df[,c(25,34,40,46,70)] <- sapply(t_gamelog_df[,c(25,34,40,46,70)],as.numeric)
  
  names(t_gamelog_df)[c(25,34,40,46,70)] <- c("stats.Fg2PtMade.#text"="two_pt_Made",
                                              "stats.Fg3PtAtt.#text"="three_pt_Attempts",
                                              "stats.Fg3PtMade.#text"="three_pt_Made",
                                              "stats.Fg3PtPct.#text"="three_pt_Percentage",
                                              "stats.FtMade.#text"="ft_Made")
  
  total_pts <- t_gamelog_df %>% select(two_pt_Made,three_pt_Made,ft_Made) %>% mutate(points = two_pt_Made*2 + three_pt_Made*3 + ft_Made)
  
  # Create a column of dummy text for the selected team
  team <- rep(t,time=82)
  
  relevant <- data.frame(team,t_gamelog_df[,c(1,34,40,46)],total_pts$points)
  
  ## Select relevent columns within the pulled api dataframe
  # assign(paste(t,"_relevant_data",sep=""),relevant)
  
  all_gamelogs <- merge(all_gamelogs,relevant,all=TRUE)
} 


write.csv(all_gamelogs,file="team_gamelogs_3s.csv")
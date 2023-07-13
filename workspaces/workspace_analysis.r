library(tidyverse)   # loads amongst others dplyr, tidyr and ggplot2
library(tidymodels)  # loads several packages useful for model testing
library(vtable)      # provides summary statistics table function
library(rpart.plot)  # provides plotting helper for decision trees

source("other/bb_court.R") # loads basketball court plot object

# load nba shots dataset
shots <- read_csv("dataset/nbashots0423/NBA_2004_2023_Shots.csv")

# preprocess the data in the shots dataset
shots %<>%
  # remove a few unneeded variables from the dataset
  select(-SEASON_1, -EVENT_TYPE, -GAME_DATE,
         -POSITION, -POSITION_GROUP, -ZONE_ABB,
         -PLAYER_ID, -PLAYER_NAME) %>%
  # join mins left and secs left inside the same variable
  unite(TIME_LEFT, c(MINS_LEFT,SECS_LEFT), sep = ":") %>%
  # make some ids shorter
  mutate(TEAM_ID    = as.numeric(str_sub(TEAM_ID, -2)),
         SHOT_TYPE  = as.numeric(str_sub(SHOT_TYPE, 1, 1)),
         # make sure no outdated names/abbreviations are used
         TEAM_NAME  = case_when(
         TEAM_NAME %in% c("New Orleans Hornets",
                          "New Orleans/Oklahoma City Hornets")
                          ~ "New Orleans Pelicans",
         TEAM_NAME == "New Jersey Nets" ~ "Brooklyn Nets",
         TEAM_NAME == "Seattle SuperSonics" ~ "Oklahoma City Thunder",
         TEAM_NAME == "LA Clippers" ~ "Los Angeles Clippers",
         TEAM_NAME == "Charlotte Bobcats" ~ "Charlotte Hornets",
         TRUE ~ TEAM_NAME),
         HOME_TEAM  = case_when(
         HOME_TEAM %in% c("NOH", "NOK") ~ "NOP",
         HOME_TEAM == "NJN" ~ "BKN",
         HOME_TEAM == "SEA" ~ "OKC",
         TRUE ~ HOME_TEAM),
         AWAY_TEAM  = case_when(
         AWAY_TEAM %in% c("NOH", "NOK") ~ "NOP",
         AWAY_TEAM == "NJN" ~ "BKN",
         AWAY_TEAM == "SEA" ~ "OKC",
         TRUE ~ AWAY_TEAM))

# take a look at the structure of the shots dataset
glimpse(shots)

shots_desc <- tibble(
  Variable = names(shots),
  Type = sapply(shots,class),
  Description = c(
  "Season indicator",
  "NBA's unique ID variable of that specific team in their API",
  "Name of the team the player who took the shot plays in",
  "NBA's unique ID variable of that specific game in their API",
  "Name of the home team in the game the shot took place in",
  "Name of the away team in the game the shot took place in",
  "Logical variable denoting a shot outcome (`TRUE` or `FALSE`)",
  "Description of shot type (dunk, jump shot, etc.)",
  "Type of shot (2 or 3-pointer)",
  "Name of the court zone the shot took place in",
  "Name of the side of court the shot took place in",
  "Distance range of shot by zones",
  "X coordinate of the shot in the x,y plane of the court (0,50)",
  "Y coordinate of the shot in the x,y plane of the court (0,50)",
  "Distance of the shot with respect to the center of the hoop, in feet",
  "Quarter of the game",
  "Minutes and seconds remaining in the quarter"
))

# load datasets pertaining to nba details on a per-game basis
games           <- read_csv("dataset/nbagames/games.csv")
games_details   <- read_csv("dataset/nbagames/games_details.csv")
# players       <- read_csv("dataset/nbagames/players.csv")
# ranking       <- read_csv("dataset/nbagames/ranking.csv")
# teams         <- read_csv("dataset/nbagames/teams.csv")

# preprocess the data in the games dataset
games %<>%
  # remove a few unneeded variables from the dataset
  select(-GAME_STATUS_TEXT, -HOME_TEAM_ID, -VISITOR_TEAM_ID) %>%
  # make some ids shorter
  mutate(TEAM_ID_home = as.numeric(str_sub(TEAM_ID_home, -2)),
         TEAM_ID_away = as.numeric(str_sub(TEAM_ID_away, -2)),
         # turn the home team win variable into a factor for later use
         HOME_TEAM_WINS = factor(HOME_TEAM_WINS,labels=c("No","Yes"))) %>%
  # rename the variable for consistency's sake
  rename(WIN_home = HOME_TEAM_WINS)

vt(games, out="latex") %>% kableExtra::kable_styling(position = "float_right")

# preprocess the data in the games details dataset
games_details %<>%
  # remove a few unneeded variables from the dataset
  select(-(PLAYER_ID:MIN), -OREB, -DREB,
         -FG_PCT, -FG3_PCT, -FT_PCT) %>%
  # make some ids shorter
  mutate(TEAM_ID = as.numeric(str_sub(TEAM_ID, -2)),
         # turn all NAs to 0s
         across(everything(),~replace_na(.,0)),
         # make sure no outdated abbreviations are used
         TEAM_ABBREVIATION = case_when(
         TEAM_ABBREVIATION %in% c("NOH", "NOK") ~ "NOP",
         TEAM_ABBREVIATION == "NJN" ~ "BKN",
         TEAM_ABBREVIATION == "SEA" ~ "OKC",
         TRUE ~ TEAM_ABBREVIATION)) %>%
  # add the season column to the dataset for later use
  left_join(games %>% select(GAME_ID,SEASON),
            by="GAME_ID",multiple="first")

# take a look at the structure of the games dataset
glimpse(games)

games_desc <- tibble(
  Variable = names(games),
  Type = sapply(games,class),
  Description = c(
  "Date in which the game take place (Year-Month-Day)",
  "NBA's unique ID variable of that specific game in their API",
  "Season indicator",
  "ID of the home team in the game the shot took place in",
  "Total points scored by the home team",
  "Field Goal percentage for the home team",
  "Free Throw percentage for the home team",
  "3 Point Field Goal percentage for the home team",
  "Number of assists for the home team",
  "Number of rebounds for the home team",
  "ID of the away team in the game the shot took place in",
  "Total points scored by the away team",
  "Field Goal percentage for the away team",
  "Free Throw percentage for the away team",
  "3 Point Field Goal percentage for the away team",
  "Number of assists for the away team",
  "Number of rebounds for the away team",
  "Factor variable indicating whether the home team won the game"
))

# take a look at the structure of the games details dataset
glimpse(games_details)

gd_desc <- tibble(
  Variable = names(games_details),
  Type = sapply(games_details,class),
  Description = c(
  "NBA's unique ID variable of that specific game in their API",
  "ID of the player's team",
  "3-letter abbreviation of the team name",
  "City in which the team plays",
  "Field Goals made by the player",
  "Field Goals attempted by the player",
  "3 Point Field Goals made by the player",
  "3 Point Field Goals attempted by the player",
  "Free Throws made by the player",
  "Free Throws attempted by the player",
  "Rebounds taken by the player",
  "Assists given by the player",
  "Steals made by the player",
  "Blocks made by the player",
  "Turnovers made by the player",
  "Personal fouls made by the player",
  "Total points scored by the player",
  "Team's points - opponent's points while player is in the game",
  "Season indicator"
))

# Calculate total shots made and attempted in each BASIC_ZONE
# and calculate the probability of a shot being made in each BASIC_ZONE.
# Finally, calculate the expected value of a shot in each BASIC_ZONE.
EV_info <- shots %>%
  filter(SHOT_MADE) %>%
  count(BASIC_ZONE, SHOT_TYPE, name = "TOT_MADE") %>%
  left_join(count(shots, BASIC_ZONE, SHOT_TYPE, name = "ATTEMPTS")) %>%
  mutate(PROB = TOT_MADE/ATTEMPTS, EV = PROB*SHOT_TYPE) %>%
  group_by(BASIC_ZONE) %>%
  filter(ATTEMPTS == max(ATTEMPTS)) %>%
  select(BASIC_ZONE,EV) %>%
  arrange(desc(EV))

# Determine the expected and actual win percentages for teams based on the EV statistic.
# The resulting teams_percentages dataset summarizes the predictor-based win percentages.
teams_percentages <- shots %>%
  left_join(EV_info) %>%
  group_by(GAME_ID,SEASON_2,TEAM_ID) %>%
  summarize(TOTAL_EV = sum(EV)) %>%
  mutate(EXP_RESULT=if_else(TOTAL_EV==max(TOTAL_EV),"Win","Loss")) %>%
  inner_join(games_details %>%
    group_by(GAME_ID,TEAM_ID) %>%
    summarize(PTS=sum(PTS)) %>%
    mutate(WIN_REAL=if_else(PTS==max(PTS),"Win","Loss"))) %>%
  mutate(MOD_FLAG=if_else(EXP_RESULT==WIN_REAL,TRUE,FALSE)) %>%
  left_join(shots %>% select(TEAM_ID, TEAM_NAME) %>% distinct()) %>%
  group_by(SEASON_2,TEAM_NAME) %>%
  summarize(MODEL_PCT = mean(EXP_RESULT == "Win"),
            REAL_PCT  = mean(WIN_REAL   == "Win")) %>%
  arrange(desc(REAL_PCT))

# Determine which teams took the most statistically efficient shots
# on a season-per-season basis.
analytical_teams <- shots %>%
  left_join(EV_info) %>%
  group_by(SEASON_2,TEAM_NAME) %>%
  summarize(EV_AVG = mean(EV)) %>%
  arrange(desc(EV_AVG))
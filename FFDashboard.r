library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(gt)
library(highcharter)
library(forcats)
library(htmltools)
library(ffscrapr)
library(tidyverse)
library(data.table)
library(DT)
library(scales)
library(broom)
library(plotly)
library(reactable)
library(reactablefmtr)
library(nflreadr)
library(magrittr)
library(xml2)
library(httr)
library(gtExtras)
library(ggiraph)
library(ggimage)
library(nflplotR)


# ==== functions (percent, commas, etc.) ====
percent1 <- function(num, digits = 1, ...) paste0(formatC(
  num*100, format="f", digits=digits, ...), "%")
percent3 <- function(num, digits = 3, ...) paste0(formatC(
  num*100, format="f", digits=digits, ...), "%")
commadec1 <- function(x) format(x, digits=1, big.mark=",", scientific=FALSE)


# --- THEMES
## GGPLOT
theme_lamernfl <- theme_classic() +
  theme(# TITLES
    plot.title = element_text(color = "ivory2", face = "bold",
                              size = 20, vjust = 1, hjust = 0),
    plot.subtitle = element_text(color = "gold2",
                                 size = 14, vjust = 1, hjust = 0),
    plot.caption = element_text(color = "lightskyblue2",
                                size = 10, hjust = 1),
    # LEGEND
    legend.position = "top",
    legend.text = element_text(hjust = 0, size = 12, color = "ivory"),
    legend.background = element_rect(fill = 'black',
                                     color = 'deepskyblue3'),
    legend.box.background = element_rect(fill = 'black',
                                         color = 'deepskyblue3'),
    legend.title = element_text(color = "ivory3", face = "bold", size = 12),
    legend.key = element_blank(),
    # AXIS TITLES
    axis.title.x = element_text(size = 14, angle = 0,
                                color = "deepskyblue1"),
    axis.title.y = element_text(size = 14, angle = 90,
                                color = "deepskyblue1"),
    # AXIS TEXT
    axis.text.x = element_text(color = "ivory2", size = 14),
    axis.text.y = element_text(color = "ivory2", size = 14),
    # LINES
    axis.line.y = element_line(color = "#011638", linewidth = 0.7),
    axis.line.x = element_line(color = "ivory2", linewidth = 0.7),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "ivory2", linewidth = 0.2),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    # PANEL BACKGROUND
    panel.background = element_rect(fill = "#0f6e04", color = "ivory2", linewidth = 1),
    plot.background = element_rect(fill = ("#011638")),
    # FACETS
    strip.text = element_text(face = "bold", color = "gray2", hjust = 0, size = 18),
    strip.background = element_rect(color = "ivory", fill="firebrick", linewidth = 1.1),
    strip.text.x = element_text(angle = 0, hjust = 0.5, color = "ivory"))

# Hicharter Theme
nfl_hc_theme <- hc_theme(
  chart = list(
    backgroundColor = "#0f6e04",
    borderColor = '#011638', borderRadius = 5, borderWidth = 3,
    style = list(fontFamily = "Arial")),
  title = list(style = list(color = "#edf1f2", fontFamily = "Arial")),
  legend = list(itemStyle = list(fontFamily = "Arial", color = "#edf1f2", fontSize = "24px")),
  tooltip = list(style = list(fontSize = "24px")))
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# LOAD DATA ====
league_id <- "1641276"
season    <- 2025
weeks     <- 1:16

# !Get ESPN codes from .Renviron file ====
ESPN_S2  <- Sys.getenv("ESPN_S2_CD")
ESPN_SWID <- Sys.getenv("ESPN_SWID_CD")

ESPNLEAGUE <- espn_connect(
  season    = season,
  league_id = league_id,
  espn_s2   = ESPN_S2,
  swid      = ESPN_SWID)

fantasy_schedule  <- ff_schedule(ESPNLEAGUE)
fantasy_franchise <- ff_franchises(ESPNLEAGUE)
potential_points  <- espn_potentialpoints(ESPNLEAGUE, weeks = weeks)


# *** Data Management *** ====
# Franchise information
fantasy_franchise <- fantasy_franchise %>%
  mutate(
    logo_url  = as.character(logo),
    logo_html = paste0("<img src='", logo_url, "' height='38'/>")) %>%
  select(franchise_id, franchise_name, logo_url, logo_html)
fantasy_franchise <- fantasy_franchise %>%
  mutate(logo_url = ifelse(franchise_name == "Njibas in paris ", paste0(
    "https://live.staticflickr.com/65535/53173431574_69296aa99f.jpg"),
    ifelse(franchise_name == "🏆 RyanAIR TIME", paste0(
      "https://www.thesun.co.uk/wp-content/uploads/2025/01/NINTCHDBPICT000960241098_57f2c1.jpg"),
      logo_url)))

# Add schedule / scores
fantasy_schedule <- fantasy_schedule %>%
  select(week, franchise_id, opponent_id, franchise_score, opponent_score)

TEAMSCHEDULE <- merge(fantasy_schedule, fantasy_franchise, by="franchise_id", all.x=TRUE) %>%
  mutate(gameid = paste0(week, franchise_name))

# Add potential points
potential_points <- potential_points %>%
  mutate(gameid = paste0(week, franchise_name)) %>%
  select(gameid, player_name, actual_slot, player_pos, team, player_score)

FANTASYTEAMS <- merge(TEAMSCHEDULE, potential_points, by="gameid")

# Add NFL team logos
TEAMS <- nflreadr::load_teams(current = TRUE)
FANTASYTEAMS <- FANTASYTEAMS %>%
  mutate(team = case_when(
    team == "TBB"~"TB",
    team == "SFO"~"SF",
    team == "OAK"~"LV",
    team == "NOS"~"NO",
    team == "NEP"~"NE",
    team == "LAR"~"LA",
    team == "KCC"~"KC",
    team == "GBP"~"GB",
    team == "JAC"~"JAX",
    TRUE ~ team))
FANTASYTEAMS$teamlogo <- with(FANTASYTEAMS, TEAMS$team_logo_espn[
  match(FANTASYTEAMS$team, TEAMS$team_abbr)])


FANTASYTEAMS <- FANTASYTEAMS %>%
  dplyr::arrange(week) %>%
  group_by(player_name) %>%
  mutate(cumulative = cumsum(player_score)) %>%
  ungroup()

### data FANTASYTEAMSACT ====
# Provide franchise, it's players, and their scores
# week, franchise_name, player_name, player_pos, teamlogo,
# player_score, meanweek, maxweek, cumulative, meancum
FANTASYTEAMSACT <- FANTASYTEAMS %>%
  filter(actual_slot != "BE") %>%
  select(week, franchise_name, player_name, team, player_pos, teamlogo, player_score,
         cumulative, logo_url, logo_html) %>%
  group_by(player_pos, week) %>%
  mutate(meanweek = mean(player_score),
         meancum = mean(cumulative),
         maxweek = max(player_score)) %>% ungroup() %>%
  mutate(meanweek = commadec1(meanweek),
         meancum = commadec1(meancum)) %>%
  dplyr::select(c(week, franchise_name, player_name, player_pos, teamlogo, player_score,
                  meanweek, maxweek, cumulative, meancum))
# !Set order to factor levels ====
FANTASYTEAMSACT$player_pos <- factor(FANTASYTEAMSACT$player_pos,
                                     levels=c("QB","RB","WR","TE","K","DST"))

### data PLAYERSTATS ====
# Create stat pages based on player positions from NFLREADR
PLAYERSTATS <- nflreadr::load_player_stats(seasons = season)
# colnames(PLAYERSTATS)
FFPOSITIONS = c("QB", "WR", "TE", "RB", "K")
PLAYERSTATS <- PLAYERSTATS %>%
  filter(position %in% FFPOSITIONS)

FANTASYTEAMS <- FANTASYTEAMS %>%
  mutate(franchisematch = paste0(week,player_name))
PLAYERSTATS <- PLAYERSTATS %>%
  mutate(franchisematch = paste0(week,player_display_name))

PLAYERSTATS$franchise <- with(PLAYERSTATS, FANTASYTEAMS$franchise_name[
  match(franchisematch, FANTASYTEAMS$franchisematch)])

PLAYERSTATS$opp_logo <- with(PLAYERSTATS, TEAMS$team_logo_squared[
  match(opponent_team, TEAMS$team_abbr)])

PLAYERSTATS$fantasypts <- with(PLAYERSTATS, FANTASYTEAMS$player_score[
  match(franchisematch, FANTASYTEAMS$franchisematch)])

QB_PLAYERSTATS <- PLAYERSTATS %>% filter(position == "QB")
RB_PLAYERSTATS <- PLAYERSTATS %>% filter(position == "RB")
WR_PLAYERSTATS <- PLAYERSTATS %>% filter(position == "WR")
TE_PLAYERSTATS <- PLAYERSTATS %>% filter(position == "TE")
K_PLAYERSTATS <- PLAYERSTATS %>% filter(position == "K")

#### data QB ====
# fantasy_points, fantasy_points_ppr, attempts, completions, passing_yards, rushing_tds,
# passing_tds, rushing_yards, passing_air_yards, mean_passing_air_yards, passing_first_downs,
# mean_passing_first_downs, passing_epa, dakota, pacr, Claimed_flag
QB_PLAYERSTATSparent <- QB_PLAYERSTATS %>%
  group_by(player_display_name, headshot_url) %>%
  summarise(
    fantasy_points = mean(fantasy_points, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    completions = sum(completions, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    mean_passing_yards = mean(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_tds, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    passing_air_yards = sum(passing_air_yards, na.rm = TRUE),
    mean_passing_air_yards = mean(passing_air_yards, na.rm = TRUE),
    passing_first_downs = sum(passing_first_downs, na.rm = TRUE),
    mean_passing_first_downs = mean(passing_first_downs, na.rm = TRUE),
    passing_epa = mean(passing_epa, na.rm = TRUE),
    pacr = mean(pacr, na.rm = TRUE),
    passing_interceptions = sum(passing_interceptions, na.rm = TRUE),
    claimed_flag = any(!is.na(franchise) & nzchar(franchise)),
    .groups = "drop") %>%
  mutate(
    fantasy_points = round(fantasy_points),
    passing_epa = round(passing_epa, 2),
    mean_passing_yards = round(mean_passing_yards, 2),
    mean_passing_air_yards = round(mean_passing_air_yards, 2),
    mean_passing_first_downs = round(mean_passing_first_downs, 2),
    pacr = round(pacr, 2),
    Claimed = if_else(claimed_flag, "Yes", "No")) %>%
  select(Claimed, everything(), -claimed_flag) %>%
  arrange(desc(fantasy_points))


#### data RB ====
# fantasy_points, fantasy_points_ppr, carries, rushing_yards, rushing_tds,
# rushing_fumbles, rushing_epa, Claimed_flag
RB_PLAYERSTATSparent <- RB_PLAYERSTATS %>%
  group_by(player_display_name, headshot_url) %>%
  summarise( 
    fantasy_points = mean(fantasy_points, na.rm = TRUE),
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    rushing_fumbles = sum(rushing_fumbles, na.rm = TRUE),
    rushing_epa = mean(rushing_epa, na.rm = TRUE),
    claimed_flag = any(!is.na(franchise) & nzchar(franchise)),
    .groups = "drop") %>% # !better way to ungroup after summarise ====
  mutate(
    fantasy_points = round(fantasy_points, digits = 0),
    rushing_epa = round(rushing_epa, digits = 2),
    Claimed = if_else(claimed_flag, "Yes", "No")) %>%
  select(Claimed, everything(), -claimed_flag) %>%
  arrange(desc(fantasy_points))

#### data WR ====
# fantasy_points, targets, attempts, receptions, target_share, receiving_yards,
# receiving_air_yards, receiving_tds, receiving_yards_after_catch, receiving_epa,
# Claimed_flag
WR_PLAYERSTATSparent <- WR_PLAYERSTATS %>%
  group_by(player_display_name, headshot_url) %>%
  summarise( 
    fantasy_points = mean(fantasy_points, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    target_share = mean(target_share, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_air_yards = mean(receiving_air_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    receiving_yards_after_catch = mean(receiving_yards_after_catch, na.rm = TRUE),
    receiving_epa = mean(receiving_epa, na.rm = TRUE),
    claimed_flag = any(!is.na(franchise) & nzchar(franchise)),
    .groups = "drop") %>%
  mutate(
    fantasy_points = round(fantasy_points, digits = 0),
    target_share = round(target_share, digits = 1),
    receiving_yards_after_catch = round(receiving_yards_after_catch, digits = 1),
    receiving_epa = round(receiving_epa, digits = 2),
    Claimed = if_else(claimed_flag, "Yes", "No")) %>%
  select(Claimed, everything(), -claimed_flag) %>%
  arrange(desc(fantasy_points))

#### data TE ====
# fantasy_points, targets, attempts, receptions, target_share, receiving_yards,
# receiving_air_yards, receiving_tds, receiving_yards_after_catch, receiving_epa,
# rushing_yards, rushing_tds, Claimed_flag
TE_PLAYERSTATSparent <- TE_PLAYERSTATS %>%
  group_by(player_display_name, headshot_url) %>%
  summarise( 
    fantasy_points = mean(fantasy_points, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    target_share = mean(target_share, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_air_yards = mean(receiving_air_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    receiving_yards_after_catch = mean(receiving_yards_after_catch, na.rm = TRUE),
    receiving_epa = mean(receiving_epa, na.rm = TRUE),
    claimed_flag = any(!is.na(franchise) & nzchar(franchise)),
    .groups = "drop") %>%
  mutate(
    fantasy_points = round(fantasy_points, digits = 0),
    target_share = round(target_share, digits = 1),
    receiving_yards_after_catch = round(receiving_yards_after_catch, digits = 1),
    receiving_epa = round(receiving_epa, digits = 2),
    Claimed = if_else(claimed_flag, "Yes", "No")) %>%
  select(Claimed, everything(), -claimed_flag) %>%
  arrange(desc(fantasy_points))

#### data K ====
# fantasy_points, fg_att, fg_made, 
# fg_blocked, Claimed_flag
K_PLAYERSTATSparent <- K_PLAYERSTATS %>%
  group_by(player_display_name, headshot_url) %>%
  summarise(
    fantasy_points = mean(fantasypts, na.rm = TRUE),
    fg_att = sum(fg_att, na.rm = TRUE),
    fg_made = sum(fg_made, na.rm = TRUE),
    fg_blocked = sum(fg_blocked, na.rm = TRUE),
    claimed_flag = any(!is.na(franchise) & nzchar(franchise)),
    .groups = "drop") %>%
  mutate(
    fantasy_points = round(fantasy_points, 0),
    success = round((fg_made/fg_att) * 100, 2),
    Claimed = if_else(claimed_flag, "Yes", "No")) %>%
  select(Claimed, everything(), -claimed_flag) %>%
  arrange(desc(fantasy_points))


#### data DST ====
# Show defense team, opponent and summary of allowed passes/runs and defensive actions
## import GAME DATA (pbp) from NFLREADR
### ---- DEFENSE (from play-by-play) ----
GAMEDATA <- nflreadr::load_pbp(seasons = season)

DEFENSE <- GAMEDATA %>%
  dplyr::select(
    week, defteam, posteam, play_type, qb_spike, qb_kneel,
    air_yards, yards_after_catch, complete_pass, pass_touchdown,
    passer_player_name, receiver_player_name, passing_yards, receiving_yards,
    kick_distance, field_goal_result, extra_point_result,
    interception, fumble, sack, return_touchdown,
    rush_touchdown, rusher_player_name, rushing_yards) %>%
  dplyr::filter(play_type %in% c("pass","run"), qb_spike != 1, qb_kneel != 1) %>%
  tidyr::drop_na(posteam) %>%
  dplyr::mutate(
    field_goal_result  = ifelse(field_goal_result == "Made", 1L, 0L),
    extra_point_result = ifelse(extra_point_result == "Made", 1L, 0L))

# Logos
DEFENSE$def_logo     <- TEAMS$team_logo_squared[ match(DEFENSE$defteam, TEAMS$team_abbr) ]
DEFENSE$posteam_logo <- TEAMS$team_logo_squared[ match(DEFENSE$posteam, TEAMS$team_abbr) ]

### ---- data DEFENSEPASS ----
DEFENSEPASS <- DEFENSE %>%
  dplyr::filter(play_type == "pass") %>%
  tidyr::drop_na(receiver_player_name) %>%
  dplyr::group_by(week, defteam, posteam, posteam_logo, def_logo, receiver_player_name) %>%
  dplyr::summarise(
    air_yards = sum(air_yards, na.rm = TRUE),
    yards_after_catch = sum(yards_after_catch, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    pass_touchdown = sum(pass_touchdown, na.rm = TRUE),
    complete_pass = sum(complete_pass, na.rm = TRUE),
    interception  = sum(interception,  na.rm = TRUE),
    .groups = "drop")

DEFENSEPASSparent <- DEFENSE %>%
  dplyr::filter(play_type == "pass") %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(
    tot_receiving_yards = sum(receiving_yards, na.rm = TRUE),
    tot_pass_touchdown  = sum(pass_touchdown,  na.rm = TRUE),
    tot_interception    = sum(interception,    na.rm = TRUE),
    def_logo            = first(TEAMS$team_logo_squared[ match(defteam, TEAMS$team_abbr) ]),
    .groups = "drop")

PLAYERS_LOAD <- nflreadr::load_players()

DEFENSEPASS_receiver <- DEFENSE %>%
  dplyr::filter(play_type == "pass") %>%
  dplyr::group_by(week, defteam, posteam, receiver_player_name) %>%
  dplyr::summarise(
    air_yards = sum(air_yards, na.rm = TRUE),
    yards_after_catch = sum(yards_after_catch, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    complete_pass = sum(complete_pass, na.rm = TRUE),
    pass_touchdown = sum(pass_touchdown, na.rm = TRUE),
    interception = sum(interception, na.rm = TRUE),
    .groups = "drop") %>%
  dplyr::left_join(
    PLAYERS_LOAD %>% dplyr::select(display_name, headshot),
    by = c("receiver_player_name" = "display_name")) %>%
  dplyr::rename(receiver_headshot = headshot)

# --- PASS defense by opponent (first child level)
DEFENSEPASS_opp <- DEFENSE %>%
  dplyr::filter(play_type == "pass") %>%
  dplyr::group_by(week, defteam, posteam) %>%
  dplyr::summarise(
    air_yards = sum(air_yards, na.rm = TRUE),
    yards_after_catch = sum(yards_after_catch, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    pass_touchdown = sum(pass_touchdown, na.rm = TRUE),
    complete_pass = sum(complete_pass, na.rm = TRUE),
    interception  = sum(interception,  na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(
    posteam_logo = TEAMS$team_logo_squared[ match(posteam, TEAMS$team_abbr) ],
    def_logo     = TEAMS$team_logo_squared[ match(defteam, TEAMS$team_abbr) ])


### ---- data DEFENSERUN ----
DEFENSERUN <- DEFENSE %>%
  dplyr::filter(play_type == "run") %>%
  tidyr::drop_na(rusher_player_name) %>%
  dplyr::group_by(week, defteam, posteam, posteam_logo, def_logo, rusher_player_name) %>%
  dplyr::summarise(
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rush_touchdown = sum(rush_touchdown, na.rm = TRUE),
    fumble = sum(fumble, na.rm = TRUE),
    .groups = "drop")

DEFENSERUNparent <- DEFENSE %>%
  dplyr::filter(play_type == "run") %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(
    tot_rushing_yards = sum(rushing_yards, na.rm = TRUE),
    tot_rush_touchdown= sum(rush_touchdown, na.rm = TRUE),
    tot_fumble        = sum(fumble, na.rm = TRUE),
    def_logo          = first(TEAMS$team_logo_squared[ match(defteam, TEAMS$team_abbr) ]),
    .groups = "drop")

DEFENSERUN_rusher <- DEFENSE %>%
  dplyr::filter(play_type == "run") %>%
  dplyr::group_by(week, defteam, posteam, rusher_player_name) %>%
  dplyr::summarise(
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rush_touchdown = sum(rush_touchdown, na.rm = TRUE),
    fumble = sum(fumble, na.rm = TRUE),
    .groups = "drop") %>%
  dplyr::left_join(
    PLAYERS_LOAD %>% dplyr::select(display_name, headshot),
    by = c("rusher_player_name" = "display_name")) %>%
  dplyr::rename(rusher_headshot = headshot)

# --- RUN defense by opponent (first child level)
DEFENSERUN_opp <- DEFENSE %>%
  dplyr::filter(play_type == "run") %>%
  dplyr::group_by(week, defteam, posteam) %>%
  dplyr::summarise(
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rush_touchdown = sum(rush_touchdown, na.rm = TRUE),
    fumble = sum(fumble, na.rm = TRUE),
    .groups = "drop") %>%
  dplyr::mutate(
    posteam_logo = TEAMS$team_logo_squared[ match(posteam, TEAMS$team_abbr) ],
    def_logo     = TEAMS$team_logo_squared[ match(defteam, TEAMS$team_abbr) ])


### data DEFENSECOMPARE plots ====
DEFENSECOMPARE <- DEFENSE %>%
  dplyr::select(c(defteam, passing_yards, rushing_yards)) %>% 
  dplyr::group_by(defteam) %>%
  dplyr::summarise(
    Passing = round(mean(passing_yards, na.rm = TRUE), digits = 1),
    Rushing = round(mean(rushing_yards, na.rm = TRUE), digits = 1),
    .groups = "drop") 

# ! Convert nflcolors to a dataframe ====
nflcolors = c("ARI" = "#97233f", "ARZ" = "#97233f", "ATL" = "#a71930", "BAL" = "#241773",
              "BLT" = "#241773", "BUF" = "#00338d", "CAR" = "#0085ca", "CHI" = "#0b162a",
              "CIN" = "#000000", "CLE" = "#fb4f14", "CLV" = "#fb4f14", "DAL" = "#002238",
              "DEN" = "#002238", "DET" = "#005a8b", "GB" = "#203731", "HOU" = "#03202f", 
              "HST" = "#03202f", "IND" = "#002c5f", "JAX" = "#000000", "KC" = "#e31837", 
              "LA" = "#002238", "LAC" = "#002238", "LAR" = "#002238", "LV" = "#a5acaf", 
              "MIA" = "#008e97", "MIN" = "#4f2683", "NE" = "#002238", "NO" = "#9f8958", 
              "NYG" = "#0b2265", "NYJ" = "#203731", "OAK" = "#a5acaf", "PHI" = "#004953",
              "PIT" = "#000000", "SD" = "#002238", "SF" = "#aa0000", "SEA" = "#002238", 
              "STL" = "#002238", "TB" = "#d50a0a", "TEN" = "#002238", "WAS" = "#773141")
nflcolorsdf <- data.frame(nflcolors)
# ! move index to the first column ====
nflcolorsdf <- cbind(newColName = rownames(nflcolorsdf), nflcolorsdf)
rownames(nflcolorsdf) <- 1:nrow(nflcolorsdf)
DEFENSECOMPARE$color1 <- with(DEFENSECOMPARE, nflcolorsdf$nflcolors[
  match(defteam, nflcolorsdf$newColName)])

nflcolors2 = c("ARI" = "#000000","ARZ" = "#000000","ATL" = "#000000","BAL" = "#000000",
               "BLT" = "#000000","BUF" = "#c60c30","CAR" = "#000000","CHI" = "#c83803",
               "CIN" = "#fb4f14","CLE" = "#22150c","CLV" = "#22150c","DAL" = "#b0b7bc",
               "DEN" = "#fb4f14","DET" = "#b0b7bc","GB" = "#ffb612","HOU" = "#a71930",
               "HST" = "#a71930","IND" = "#a5acaf","JAX" = "#006778","KC" = "#ffb612",
               "LA" = "#b3995d","LAC" = "#0073cf","LAR" = "#b3995d","LV" = "#000000",
               "MIA" = "#f58220","MIN" = "#ffc62f","NE" = "#c60c30","NO" = "#000000",
               "NYG" = "#a71930","NYJ" = "#1c2d25","OAK" = "#000000","PHI" = "#a5acaf",
               "PIT" = "#ffb612","SD" = "#0073cf","SF" = "#b3995d","SEA" = "#69be28",
               "STL" = "#b3995d","TB" = "#34302b","TEN" = "#4b92db","WAS" = "#ffb612")
nflcolors2df <- data.frame(nflcolors2)
nflcolors2df <- cbind(newColName = rownames(nflcolors2df), nflcolors2df)
rownames(nflcolors2df) <- 1:nrow(nflcolors2df)
DEFENSECOMPARE$color2 <- with(DEFENSECOMPARE, nflcolors2df$nflcolors2[
  match(defteam, nflcolors2df$newColName)])

DEFENSECOMPARE$nick <- with(DEFENSECOMPARE, TEAMS$team_nick[
  match(defteam, TEAMS$team_abbr)])

# Create tooltip text for plot (in server output)
TEAMDEF_HC_text <- c(" ", "Passing Yards:    ", "Rushing Yards:    ")
TEAMDEF_HC_values <- c("{point.nick}", "{point.Passing}", "{point.Rushing}")
TEAMDEF_HC_tooltip <- tooltip_table(TEAMDEF_HC_text,
                                    TEAMDEF_HC_values)

### data DEFENSECOUNTS plot ====
DEFENSECOUNTS <- DEFENSE %>%
  dplyr::group_by(defteam) %>%
  dplyr::summarise(
    Interceptions = sum(interception, na.rm = TRUE),
    Fumbles       = sum(fumble,       na.rm = TRUE),
    Sacks         = sum(sack,         na.rm = TRUE),
    Return_TD     = sum(return_touchdown, na.rm = TRUE),
    Touchdowns    = sum(pass_touchdown + rush_touchdown, na.rm = TRUE),
    .groups = "drop") %>% 
  tidyr::pivot_longer(c(Interceptions, Fumbles, Sacks, Return_TD, Touchdowns),
                      names_to = "Events", values_to = "Count")

DEFENSECOUNTS$color1 <- with(DEFENSECOUNTS, nflcolorsdf$nflcolors[
  match(defteam, nflcolorsdf$newColName)])
DEFENSECOUNTS$color2 <- with(DEFENSECOUNTS, nflcolors2df$nflcolors2[
  match(defteam, nflcolors2df$newColName)])
DEFENSECOUNTS$nick <- with(DEFENSECOUNTS, TEAMS$team_nick[
  match(defteam, TEAMS$team_abbr)])
DEFENSECOUNTS <- DEFENSECOUNTS %>% mutate(Events = as.factor(Events))  %>% 
  mutate(label = paste0(nick, ": ", Count))

# ggiraph tooltip defaults
set_girafe_defaults(
  opts_hover = opts_hover(css = "stroke: black; stroke-width: 1px; fill: #a4ff08; opacity: 1;"),
  opts_zoom = opts_zoom(min = 1, max = 4),
  opts_tooltip = opts_tooltip(css = "padding:3px;background-color:#333333;color:gold;"),
  opts_sizing = opts_sizing(rescale = TRUE),
  opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 2000))

DEFENSECOUNTSplot <- DEFENSECOUNTS %>%
  ggplot(aes(x = Count, y = Events, group = defteam, 
             fill = color2, color = color1)) +
  geom_hline(yintercept = 1.5, color = "ivory2", linewidth = 1, alpha = 0.8) +
  geom_hline(yintercept = 2.5, color = "ivory2", linewidth = 1, alpha = 0.8) +
  geom_hline(yintercept = 3.5, color = "ivory2", linewidth = 1, alpha = 0.8) +
  geom_hline(yintercept = 4.5, color = "ivory2", linewidth = 1, alpha = 0.8) +
  geom_point_interactive(aes(tooltip = label, data_id = label), shape = 21, size = 2, 
                         position = position_jitter(height = 0.3), stroke = 1.5) + 
  scale_fill_identity() +
  scale_color_identity() +
  theme_set(theme_lamernfl) +
  theme_set(theme_lamernfl) +
  scale_x_continuous(breaks = seq(0, 200, by = 5)) +
  scale_y_discrete(labels = c("Touchdowns" = "Allowed TDs", "Sacks" = "Sacks", 
                              "Return_TD" = "Returned TDs", "Interceptions" = "Interceptions", 
                              "Fumbles" = "Fumbles")) +
  labs(title = "Defensive Actions",
       caption = "Data source: NFLREADR",
       x = "Number", y = "") +
  theme(legend.position = "") 


### data FANTASY_CURRENT ====
# Players by franchise used for creating cards
headshots <- PLAYERSTATS %>%
  dplyr::distinct(player_display_name, headshot_url)
FANTASY_CURRENT <- FANTASYTEAMSACT %>%
  left_join(headshots, by = c("player_name" = "player_display_name"))

### data FANTASYOPPONENT ====
# ! distinct() similar to unique() but only selects one var. to keep others add .keep_all = TRUE ====
OPPONENT <- FANTASYTEAMS %>% select(franchise_id, franchise_name, logo_url) %>% distinct()
FANTASYTEAMS$opponent_name <- OPPONENT$franchise_name[match(FANTASYTEAMS$opponent_id, OPPONENT$franchise_id)]
FANTASYTEAMS$opplogo_url <- OPPONENT$logo_url[match(FANTASYTEAMS$opponent_id, OPPONENT$franchise_id)]

FANTASYOPPONENT <- FANTASYTEAMS %>%
  select(week, franchise_name, opplogo_url, franchise_score, opponent_name, opponent_score) %>%
  group_by(week, opplogo_url, franchise_name, opponent_name) %>%
  mutate(WINLOSE = ifelse(franchise_score>opponent_score, "Won!!",
                          ifelse(franchise_score<opponent_score, "Lost", "Unplayed"))) %>%
  ungroup() %>% distinct()

### data TEAMLOGODATA ====
TEAMLOGODATA <- FANTASYTEAMS %>%
  select(franchise_name, logo_url)
TEAMLOGODATA <- TEAMLOGODATA[!duplicated(TEAMLOGODATA), ]

RECENTWEEK = max(FANTASYTEAMSACT$week)

### data SCHEDULEGRID ====
SCHEDULE <- nflreadr::load_schedules()
SCHEDULE <- SCHEDULE %>%
  filter(season == 2025)

# ! NA_character_ excludes any value if it is NA ====
SCHEDULEGRID <- SCHEDULE %>%
  mutate(
    game_odds = dplyr::case_when(
      is.na(home_moneyline) | is.na(away_moneyline) ~ NA_character_,
      home_moneyline > away_moneyline ~ paste0(
        away_team, " favored to win at ", away_moneyline,
        " versus ", home_team, " at ", home_moneyline, ", total:", total_line),
      TRUE ~ paste0(
        home_team, " favored to win at ", home_moneyline,
        " versus ", away_team, " at ", away_moneyline, ", total:", total_line))) %>%
  dplyr::select(week, away_team, away_score, home_team, home_score, game_odds)

SCHEDULEGRID$home_logo <- with(SCHEDULEGRID, TEAMS$team_logo_squared[
  match(home_team, TEAMS$team_abbr)])
SCHEDULEGRID$away_logo <- with(SCHEDULEGRID, TEAMS$team_logo_squared[
  match(away_team, TEAMS$team_abbr)])

#### data WLD ====
GAMEWLD <- GAMEDATA %>%
  group_by(game_id, week, home_team, away_team) %>%
  summarize(
    home_score = last(home_score),
    away_score = last(away_score),
    .groups = 'drop') 
GAMEWLD <- GAMEWLD %>% 
  mutate(
    home_result = case_when(
      home_score > away_score ~ "W",
      home_score < away_score ~ "L",
      TRUE ~ "D"),
    away_result = case_when(
      away_score > home_score ~ "W",
      away_score < home_score ~ "L",
      TRUE ~ "D"))
GAMEWLDH <- GAMEWLD %>% 
  dplyr::select(c(home_team, home_result)) %>% 
  rename("team" = "home_team",
         "result" = "home_result")
GAMEWLDA <- GAMEWLD %>% 
  dplyr::select(c(away_team, away_result)) %>% 
  rename("team" = "away_team",
         "result" = "away_result")
GAMEWLDALL <- rbind(GAMEWLDH, GAMEWLDA)

GAMEWLDALL <- GAMEWLDALL %>% 
  mutate(W = ifelse(result == "W", 1, 0),
         L = ifelse(result == "L", 1, 0),
         D = ifelse(result == "D", 1, 0))
GAMEWLDALL <- GAMEWLDALL %>% 
  group_by(team) %>% 
  summarise(W = sum(W),
            L = sum(L),
            D = sum(D),
            .groups = 'drop') %>% 
  mutate(WLD = paste0(" W: ", W, "  L: :", L, "  D: ", D))

SCHEDULEGRID$WLDH <- with(SCHEDULEGRID, GAMEWLDALL$WLD[
  match(home_team, GAMEWLDALL$team)])
SCHEDULEGRID$WLDA <- with(SCHEDULEGRID, GAMEWLDALL$WLD[
  match(away_team, GAMEWLDALL$team)])

SCHEDULEGRID <- SCHEDULEGRID %>%
  dplyr::select(c(week, home_score, home_team, WLDH, home_logo, away_logo, away_team, WLDA,
                  away_score, game_odds))

# *** FUNCTIONS *** ====
player_card <- function(
    name,
    position,
    week,
    points,
    meanweek,    
    maxweek,
    cumulative,
    meancum,
    headshot_url,
    teamlogo,
    bg_color = "#f5f7fb") 
{
  # Fallback: use team logo when headshot missing
  use_img <- headshot_url
  if (is.null(use_img) || is.na(use_img) || identical(use_img, "") ) 
    {
    use_img <- teamlogo
    }
  if (is.null(use_img) || is.na(use_img) || identical(use_img, "") ) 
    {
    use_img <- "https://via.placeholder.com/96?text=No+Image"
    }
  
  tags$div(
    class = "player-card",
    style = paste0("--card-bg:", bg_color, ";"),
    # LEFT: headshot (or fallback) with team logo underneath
    tags$div(
      class = "player-left",
      tags$img(class = "player-photo", src = use_img, alt = paste(name, "photo")),
      if (!is.null(teamlogo) && !is.na(teamlogo) && teamlogo != "") {
        tags$img(class = "player-logo", src = teamlogo, alt = "team logo")
      }),
    # RIGHT: meta
    tags$div(
      class = "player-meta",
      tags$div(class = "player-name", name),
      tags$div(class = "player-stat",
               tags$span(class = "label", "Position:"),
               tags$span(class = "value", as.character(position))),
      tags$div(class = "player-stat",
               tags$span(class = "label", "Week #:"),
               tags$span(class = "value", as.character(week))),
      tags$div(class = "player-stat",
               tags$span(class = "label", "Fantasy Points:"),
               tags$span(class = "value", as.character(points))),
      tags$div(class = "player-stat",
               tags$span(class = "label", "Avg Points per Position:"),
               tags$span(class = "value", as.character(meanweek))),
      tags$div(class = "player-stat",
               tags$span(class = "label", "Max Points per Position:"),
               tags$span(class = "value", as.character(maxweek))),
      tags$div(class = "player-stat",
               tags$span(class = "label", "Total Points:"),
               tags$span(class = "value", as.character(cumulative))),
      tags$div(class = "player-stat",
               tags$span(class = "label", "Avg Total per Position:"),
               tags$span(class = "value", as.character(meancum)))
    ))}

# For newsfeed
parse_rfc822 <- function(x) 
  {
  # typical RSS pubDate format: "Wed, 02 Oct 2024 12:34:56 +0000"
  out <- try(as.POSIXct(x, format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC"), silent = TRUE)
  if (inherits(out, "try-error") || is.na(out)) return(NA)
  out
  }

fetch_rss <- function(url, n = 30, fantasy_only = TRUE, timeout_sec = 10) 
  {
  if (is.null(url) || !nzchar(url)) return(data.frame())
  
  # 1) Fetch with a friendly UA (many feeds block default libcurl UA)
  resp <- try(
    GET(url, user_agent("R (xml2 + httr) – shiny dashboard feed loader"), timeout(timeout_sec)),
    silent = TRUE)
  if (inherits(resp, "try-error") || inherits(resp, "httr_error") || is.null(resp) ||
      http_error(resp)) 
    {
    return(data.frame())  # network/HTTP issue
    }
  
  # 2) Parse XML from response text, recover on minor errors
  txt <- content(resp, as = "text", encoding = "UTF-8")
  doc <- try(read_xml(txt, options = c("RECOVER", "NOERROR", "NOBLANKS")), silent = TRUE)
  if (inherits(doc, "try-error")) return(data.frame())
  
  # 3) Items: support both RSS <item> and Atom <entry>
  items <- xml_find_all(doc, ".//item | .//entry")
  if (length(items) == 0) return(data.frame())
  
  # Helper to extract first non-empty child text/attr
  first_nonempty <- function(xs) 
    {
    xs <- xs[nzchar(xs)]
    if (length(xs)) xs[[1]] else ""
    }
  
  # 4) Build rows item-by-item (avoids vector-length mismatches)
  rows <- lapply(items, function(it) 
    {
    title <- first_nonempty(c(
      xml_text(xml_find_first(it, "./title")),
      xml_text(xml_find_first(it, ".//title"))))
    
    # RSS <link>text</link>, Atom <link href="..."> (prefer rel="alternate")
    link <- first_nonempty(c(
      xml_text(xml_find_first(it, "./link")),
      xml_attr(xml_find_first(it, './link[@rel="alternate"]'), "href"),
      xml_attr(xml_find_first(it, "./link"), "href")))
    
    # Categories/tags (RSS: <category>, Atom: <category term="...">)
    cats <- unique(c(
      xml_text(xml_find_all(it, ".//category")),
      xml_attr(xml_find_all(it, ".//category"), "term"),
      xml_text(xml_find_all(it, ".//dc:subject"))))
    cats_join <- paste(cats[nzchar(cats)], collapse = ", ")
    
    # Date (many formats)
    dates_raw <- c(
      xml_text(xml_find_first(it, "./pubDate")),
      xml_text(xml_find_first(it, "./updated")),
      xml_text(xml_find_first(it, "./published")),
      xml_text(xml_find_first(it, ".//dc:date")))
    dates_raw <- dates_raw[nzchar(dates_raw)]
    date_parsed <- suppressWarnings(as.POSIXct(dates_raw, 
                                               format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC"))
    # Try ISO formats if RFC822 failed
    if (all(is.na(date_parsed)) && length(dates_raw)) 
      {
      date_parsed <- suppressWarnings(as.POSIXct(dates_raw, tz = "UTC"))
      }
    date_fmt <- if (length(date_parsed) && !is.na(date_parsed[1])) 
      {
      format(date_parsed[1], "%b %d, %Y %I:%M %p %Z")
      } else 
        {
      first_nonempty(dates_raw)
        }
    
    data.frame(
      title = title,
      link = link,
      categories = cats_join,
      date = date_fmt,
      stringsAsFactors = FALSE)
    })
  
  df <- do.call(rbind, rows)
  df <- df[nzchar(df$title) & nzchar(df$link), , drop = FALSE]
  
  if (isTRUE(fantasy_only) && nrow(df)) 
    {
    keep <- grepl("\\bfantasy\\b", df$categories, ignore.case = TRUE) |
      grepl("\\bfantasy\\b", df$title, ignore.case = TRUE)
    df <- df[keep, , drop = FALSE]
    }
  
  head(df, n)
}


# --- GLOBAL FILTER CHOICES ====
franchise_choices <- sort(unique(as.character(FANTASYTEAMS$franchise_name)))
week_choices <- sort(unique(FANTASYTEAMS$week))

# *** UI *** ====
ui <- dashboardPage(
  header = dashboardHeader(
    title = "🏈 2025 Fantasy Football",
    
    ### MENUS ====
    dropdownMenu(
      type = "messages",
      messageItem(from = "Last Updated:", message = Sys.time(), icon = icon("calendar")),
      messageItem(from = "Okie Wan Kenokie", message = "Click here to Message Me",
                  icon = icon("jedi"),
                  href = "mailto:chris.lamer@gmail.com")),
    
    dropdownMenu(
      type = "notifications",
      notificationItem(
        text = "Switched to shinydashboard format.",
        icon = icon("info-circle"),
        status = "warning"),
      notificationItem(
        text = "Check out the news section",
        icon = icon("newspaper"),
        status = "warning"),
      notificationItem(
        text = "Added Data on Positions",
        icon = icon("football"),
        status = "danger"))),
  
  ## *** SIDEBAR ***====
  sidebar = dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("My Team", tabName = "player_cards", icon = icon("address-card")),
      menuItem("My Games", tabName = "fantasy_games", icon = icon("football")),
      menuItem("Positions", icon = icon("users"), startExpanded = FALSE,
               menuSubItem("Quarter Back", tabName = "qb_tab", icon = icon("person-walking")),
               menuSubItem("Running Back", tabName = "rb_tab", icon = icon("person-running")),
               menuSubItem("Wide Receiver", tabName = "wr_tab", icon = icon("child-reaching")),
               menuSubItem("Tight End", tabName = "te_tab", icon = icon("person-praying")),
               menuSubItem("Kicker", tabName = "k_tab", icon = icon("shoe-prints")),
               menuSubItem("Defense", tabName = "Defense", icon = icon("shield"))),
      menuItem("NFL Schedule", tabName = "schedule", icon = icon("calendar-days")),
      menuItem("ESPN Fantasy", tabName = "espnwebsite", icon = icon("arrow-up-right-from-square")),
      menuItem("Fantasy News (Draft Sharks)", tabName = "home", icon = icon("newspaper")),
      menuItem("Injury Report (Yahoo)", tabName = "injurywebsite", icon = icon("user-injured"))),
    br(), # space
    uiOutput("franchise_filter"),
    uiOutput("week_filter"),
    br(),
    uiOutput("franchiselogo", style = "text-align: center;")),
  
  ## *** BODY ***====
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
       
/* === NEWS CSS === */
.news-panel {
  max-height: calc(100vh - 240px);
  overflow-y: auto;
 border: 2px solid rgba(0,0,0,.15);
  border-radius: 8px;
  padding: 8px 10px;
  background: rgba(255,255,255,.03);
}
.news-item a { font-size: 150%; font-weight: bold; color: #4d2c19; text-decoration: none; }
    .news-item a:hover { text-decoration: underline; color: #ffaa00; }
        .news-item .muted.small { font-size: 110%; color: #4a4845; }
 
 
/* === PLAYER CARDS CSS === */
.cards-wrap { display: grid !important; gap: 18px; grid-template-columns: 1fr 1fr; justify-content: center; align-content: start; }
@media (max-width: 1000px){ .cards-wrap { grid-template-columns: 1fr; } }
.player-card {
  display: grid; grid-template-columns: 120px 1fr; align-items: center; gap: 16px;
  padding: 18px 20px; border-radius: 16px; background: var(--card-bg, #e8e1c1);
  min-height: 140px; width: 100%; margin: 0 auto; border: 3px solid #4d2c19;
  box-shadow: 0 1px 2px rgba(0,0,0,.08), 0 6px 18px rgba(0,0,0,.06);
}
.player-name { font-weight: 800; font-size: 2.2rem; margin-bottom: 10px; color: #070452 !important; }
.player-stat { font-size: 1.5rem; color: #000 !important; margin: 6px 0; }
.player-stat .label { opacity: .8; margin-right: 12px; color: #3b0207 !important; font-weight: 600; font-size: 1.5rem; }
.player-stat .value { font-variant-numeric: tabular-nums; font-weight: 800; color: #000 !important; }
.player-left { display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 8px; }
.player-photo {
  display:block; width:120px; height:120px; border-radius:14px; object-fit:cover; background:#c2bb97;
  border:2px solid #4d2c19; box-shadow: 0 0 0 0.02px rgba(255,255,255,.7) inset;
}
.player-logo {
  display:block; width:64px; height:64px; border-radius:10px; border:2px solid #081b40;
  object-fit:contain; background:#f5f7fb;
}"))),
    
    
    tabItems(
      ### --- My Team ====
      tabItem(tabName = "player_cards",
              fluidRow(
                box(width = 12, title = "My Team",
                    solidHeader = TRUE, status = "primary",
                    uiOutput("player_cards", container = div, class = "cards-wrap")))),
      
      ### --- My Games ====
      tabItem(tabName = "fantasy_games",
              fluidRow(
                box(width = 12, title = "My Games",
                    solidHeader = TRUE, status = "primary",
                    uiOutput("opponent_table")))),
      
      ### --- Position Tables ====
      tabItem(tabName = "qb_tab",  reactableOutput("qb_player")),
      tabItem(tabName = "rb_tab",  reactableOutput("rb_player")),
      tabItem(tabName = "wr_tab",  reactableOutput("wr_player")),
      tabItem(tabName = "te_tab",  reactableOutput("te_player")),
      tabItem(tabName = "k_tab",   reactableOutput("k_player")),
      tabItem(
        tabName = "Defense",
        tabBox(title = "Defense and Special Teams", width = 12, id = "def_tabs",
               tabPanel(
                 "Yards Allowed",
                 fluidRow(box(width = 12, highchartOutput("def_yards", height = "1000px")))),
               tabPanel(
                 "Defense Plays",
                 fluidRow(box(width = 12, girafeOutput("def_counts", height = "1000px")))),
               tabPanel("Passing Yards", reactableOutput("dstpass_player")),
               tabPanel("Rushing Yards",  reactableOutput("dstrun_player")))),

      
      ### --- NFL Schedule Table ====
      tabItem(tabName = "schedule", reactableOutput("schedule_table")),
      
      ### --- ESPN Fantasy Website ====
      tabItem(tabName = "espnwebsite",
              fluidRow(
                box(title = "ESPN Fantasy (opens in new tab)", width = 12,
                    HTML(
                      "<p>ESPN blocks embedding in iframes. Use this link instead:</p>
         <p><a href='https://fantasy.espn.com/football/league?leagueId=1641276' 
         target='_blank' rel='noopener noreferrer'>
            Open my ESPN league </a></p>")))),
      
      ### --- Yahoo Injury Website =====
      tabItem(tabName = "injurywebsite",
              fluidRow(
                box(title = "Embedded Website", width = 12,
                    tags$iframe(
                      src = "https://sports.yahoo.com/nfl/injuries/",
                      height = "900px", width = "100%", frameborder = "0")))),
      
      ### --- Fantasy News ====
      tabItem(tabName = "home",
              h3("Fantasy Football News"),
              div(class = "news-panel", uiOutput("news"))))),
  
  skin = "blue")



## *** SERVER ***====
server <- function(input, output, session) 
  {
  
  # NEWS FEED ====
  output$news <- renderUI({
    # refresh every 30 minutes *even if nothing else changes*
    invalidateLater(30 * 60 * 1000, session)
    
    # Try your DraftSharks feed; swap to PFF if you prefer:
    # url <- "https://www.pff.com/feed"
    url <- "https://www.draftsharks.com/rss/latest-news"
    
    df <- fetch_rss(url, n = 12, fantasy_only = FALSE)
    
    if (!nrow(df)) 
      {
      div(
        style = "opacity:.8;font-size:13px;",
        HTML(paste0(
          "No news found (yet). If this persists, the feed may be blocking requests. ",
          "Try another RSS like <code>https://www.pff.com/feed</code>.")))
    } else 
      {
      tagList(lapply(seq_len(nrow(df)), function(i) 
        {
        tags$div(class = "news-item", style = "margin:6px 0 10px;",
                 tags$a(href = df$link[i], target = "_blank", df$title[i]),
                 if (nzchar(df$date[i])) tags$div(class = "muted small", df$date[i]))
        }))
     }
    })
  
  
  
  # INPUT FILTERS ====
  output$franchise_filter <- renderUI({
    selectInput("fran", "Franchise", choices = franchise_choices, selected = franchise_choices[1])
    })
  
  output$week_filter <- renderUI({
    selectInput("wk", "Week", choices = week_choices, selected = max(week_choices))
    })
  
# FRANCHISE LOGO ====
  output$franchiselogo <- renderUI({
    req(input$fran)
    selected_row <- TEAMLOGODATA[TEAMLOGODATA$franchise_name == input$fran, ]
    if (nrow(selected_row) == 0) return(NULL)
    tags$img(src = selected_row$logo_url, width = "200px", height = "200px")
  })
  
  # OPPONENT TABLE ====
  output$opponent_table <- renderUI({
    matchup <- FANTASYOPPONENT %>%
      filter(franchise_name == input$fran) %>%
      distinct(week, opplogo_url, opponent_name, franchise_score, opponent_score, WINLOSE)
    tbl <- matchup %>%
      transmute(`Week #` = week,
                ` ` = opplogo_url, # rename headers
                Opponent = opponent_name,
                `My Score` = franchise_score,
                `Their Score` = opponent_score,
                Result = WINLOSE) %>%
      gt() %>% # Call GT table
      cols_align( 
        align = "right",
        columns = Result) %>%
      cols_align(
        align = "center",
        columns = 'Their Score') %>%
      cols_align(
        align = "center",
        columns = 'My Score') %>%
      opt_horizontal_padding(scale = 3) %>% # add space between columns
      text_transform(
        locations = cells_body(columns = " "),
        fn = function(urls) web_image(url = urls, height = 40))
    HTML(as_raw_html(tbl))
  })
  
  # MY TEAM ====
  output$player_cards <- renderUI({
    df <- FANTASY_CURRENT %>% filter(franchise_name == input$fran, week == input$wk)
    if (nrow(df) == 0) return(tags$div("No players match this franchise."))
    
    cards <- lapply(seq_len(nrow(df)), function(i) 
      {
      player_card(
        name         = df$player_name[i],
        position     = df$player_pos[i],
        week         = df$week[i],
        points       = df$player_score[i],
        meanweek     = df$meanweek[i],
        maxweek      = df$maxweek[i],
        cumulative   = df$cumulative[i],
        meancum      = df$meancum[i],
        headshot_url = df$headshot_url[i],
        teamlogo     = df$teamlogo[i],
        bg_color     = "#faf7e8"
        )
      })
    tagList(cards)
  })
  
  # POSITIONS ====
  ### QB Table ====
  output$qb_player <- renderReactable({
    req(QB_PLAYERSTATSparent)
    # fantasy_points, fantasy_points_ppr, attempts, completions, passing_yards, rushing_tds,
    # passing_tds, rushing_yards, passing_air_yards, mean_passing_air_yards, passing_first_downs,
    # mean_passing_first_downs, passing_epa, dakota, pacr, Claimed_flag
    reactable(
      QB_PLAYERSTATSparent,
      fullWidth = TRUE,
      theme = fivethirtyeight(centered = TRUE),
      searchable = TRUE,
      defaultPageSize = 20,
      filterable = TRUE,
      columns = list(
        Claimed = colDef(name = "Claimed", align = "center", width = 90),
        headshot_url = colDef(
          name = "", align = "center", filterable = FALSE,
          cell = function(url) 
            {
            if (is.na(url) || url == "") return(NULL)
            htmltools::img(src = url, height = 40)
            }, width = 60),
        player_display_name = colDef(width = 180, name = "Player", align = "left"),
        fantasy_points = colDef(name = "Avg Points", align = "center", filterable = FALSE),
        attempts = colDef(name = "Pass Attempts", align = "center",
                          filterable = FALSE),
        completions = colDef(name = "Completions", align = "center",
                             filterable = FALSE),
        passing_yards = colDef(name = "Avg Pass Yards", align = "center",
                                        filterable = FALSE),
        mean_passing_yards = colDef(show = FALSE),
        passing_tds = colDef(name = "Passing TDs", align = "center",
                             filterable = FALSE),
        rushing_yards = colDef(name = "Rushing Yards", align = "center",
                               filterable = FALSE),
        rushing_tds = colDef(name = "Rushing TDs", align = "center",
                             filterable = FALSE),
        passing_interceptions = colDef(name = "Interceptions", align = "center",
                                       filterable = FALSE),
        passing_yards = colDef(show = FALSE),
        mean_passing_air_yards = colDef(show = FALSE),
        passing_air_yards  = colDef(show = FALSE),
        passing_first_downs = colDef(show = FALSE),
        mean_passing_first_downs = colDef(show = FALSE),
        pacr = colDef(show = FALSE),
        passing_epa = colDef(name = "Passing EPA", align = "center",
                             filterable = FALSE)),
      details = function(index) 
        {
        Player <- QB_PLAYERSTATSparent$player_display_name[index]
        qb_sub <- QB_PLAYERSTATS %>%
          dplyr::filter(player_display_name == Player) %>%
          dplyr::arrange(week) %>%
          dplyr::select(
            week, franchise, team, opp_logo, fantasy_points, attempts, completions, 
            passing_yards, passing_tds, rushing_yards, rushing_tds, passing_interceptions)
        
        reactable(
          qb_sub,
          compact = TRUE,
          defaultSorted = list(week = "asc"),
          columns = list(
            week = colDef(maxWidth = 60, align = "center"),
            franchise = colDef(aggregate = "unique", width = 120,
                               name = "Franchise", align = "left"),
            team = colDef(maxWidth = 80, align = "right", name = "Opponent"),
            opp_logo = colDef(width = 75, cell = embed_img(height = 50, width = 60),
                              name = ""),
            fantasy_points = colDef(aggregate = "max", width = 80,
                                    name = "Points", align = "center"),
            attempts    = colDef(aggregate = "sum", width = 100,
                                 name = "Attempts", align = "center"),
            completions = colDef(aggregate = "sum", width = 100,
                                 name = "Completions", align = "center"),
            passing_yards = colDef(aggregate = "sum", width = 80,
                                   name = "Pass Yards", align = "center"),
            passing_tds = colDef(aggregate = "sum", width = 80,
                                 name = "Pass TDs", align = "center"),
            rushing_yards = colDef(aggregate = "sum", width = 80,
                                   name = "Rush Yards", align = "center"),
            rushing_tds = colDef(aggregate = "sum", width = 80,
                                 name = "Rush TDs", align = "center"),
            passing_interceptions = colDef(aggregate = "sum", width = 100,
                                  name = "Interceptions", align = "center")))
        })
    })
  
  ### RB Table ====
  output$rb_player <- renderReactable({
    req(RB_PLAYERSTATSparent)
    # fantasy_points, fantasy_points_ppr, carries, rushing_yards, rushing_tds,
    # rushing_fumbles, rushing_epa, Claimed_flag
    reactable(
      RB_PLAYERSTATSparent,
      fullWidth = TRUE,
      theme = fivethirtyeight(centered = TRUE),
      searchable = TRUE,
      defaultPageSize = 20,
      filterable = TRUE,
      columns = list(
        Claimed = colDef(name = "Claimed", align = "center", width = 90),
        headshot_url = colDef(
          name = "", align = "center", filterable = FALSE,
          cell = function(url) 
            {
            if (is.na(url) || url == "") return(NULL)
            htmltools::img(src = url, height = 40)
            }, width = 60),
        player_display_name = colDef(width = 180, name = "Player", align = "left"),
        fantasy_points = colDef(name = "Avg Points", align = "center", filterable = FALSE),
        carries = colDef(name = "Carries", align = "center", filterable = FALSE),
        rushing_yards = colDef(name = "Rush Yards", align = "center", filterable = FALSE),
        rushing_tds = colDef(name = "TDs", align = "center", filterable = FALSE),
        rushing_fumbles = colDef(name = "Fumbles", align = "center", filterable = FALSE),
        rushing_epa = colDef(name = "Rushing EPA", align = "center", filterable = FALSE)),
      details = function(index) 
        {
        Player <- RB_PLAYERSTATSparent$player_display_name[index]
        rb_sub <- RB_PLAYERSTATS %>%
          filter(player_display_name == Player) %>%
          arrange(week) %>%
          select(week, franchise, team, opp_logo, fantasy_points,
                 carries, rushing_yards, rushing_tds, rushing_fumbles, rushing_epa)
        reactable(
          rb_sub,
          compact = TRUE,
          defaultSorted = list(week = "asc"),
          columns = list(
            week = colDef(maxWidth = 60, align = "center"),
            franchise = colDef(aggregate = "unique", width = 120,
                               name = "Franchise", align = "left"),
            team = colDef(maxWidth = 80, align = "right", name = "Opponent"),
            opp_logo = colDef(width = 75, cell = embed_img(height = 50, width = 60),
                              name = ""),
            fantasy_points = colDef(aggregate = "max", width = 80,
                                    name = "Points", align = "center"),
            carries = colDef(aggregate = "sum", width = 80,
                             name = "Carries", align = "center"),
            rushing_yards = colDef(aggregate = "sum", width = 100,
                                   name = "Rush Yards", align = "center"),
            rushing_tds = colDef(aggregate = "sum", width = 80,
                                 name = "Rush TDs", align = "center"),
            rushing_fumbles = colDef(aggregate = "sum", width = 100,
                                     name = "Fumbles", align = "center"),
            rushing_epa = colDef(aggregate = "mean", format = colFormat(digits = 2),
                                 width = 100, name = "Rush EPA", align = "center")))
        })
    })
  
  ### WR TABLE ====
  output$wr_player <- renderReactable({
    req(WR_PLAYERSTATSparent)
    # fantasy_points, targets, attempts, receptions, target_share, receiving_yards,
    # receiving_air_yards, receiving_tds, receiving_yards_after_catch, receiving_epa,
    # Claimed_flag
    reactable(
      WR_PLAYERSTATSparent,
      fullWidth = TRUE,
      theme = fivethirtyeight(centered = TRUE),
      searchable = TRUE,
      defaultPageSize = 20,
      filterable = TRUE,
      columns = list(
        Claimed = colDef(name = "Claimed", align = "center", width = 90),
        headshot_url = colDef(
          name = "", align = "center", filterable = FALSE,
          cell = function(url) 
            {
            if (is.na(url) || url == "") return(NULL)
            htmltools::img(src = url, height = 40)
            }, width = 60),
        player_display_name = colDef(width = 180, name = "Player", align = "left"),
        fantasy_points = colDef(name = "Avg Points", align = "center", filterable = FALSE),
        receptions = colDef(name = "Receptions", align = "center", filterable = FALSE),
        targets = colDef(name = "Targets", align = "center", filterable = FALSE),
        target_share = colDef(name = "Target %", align = "center", filterable = FALSE),
        receiving_yards = colDef(name = "Rec Yards", align = "center", filterable = FALSE),
        receiving_air_yards = colDef(name = "Avg Air Yards", align = "center", filterable = FALSE),
        receiving_tds = colDef(name = "TDs", align = "center", filterable = FALSE),
        receiving_yards_after_catch = colDef(name = "Avg YAC", align = "center", filterable = FALSE),
        receiving_epa = colDef(name = "Receiving EPA", align = "center", filterable = FALSE)),
      details = function(index) 
        {
        Player <- WR_PLAYERSTATSparent$player_display_name[index]
        wr_sub <- WR_PLAYERSTATS %>%
          filter(player_display_name == Player) %>%
          arrange(week) %>%
          select(week, franchise, team, opp_logo, fantasy_points,
                 receptions, targets, target_share, receiving_yards, receiving_tds,
                 receiving_air_yards, receiving_yards_after_catch, receiving_epa)
        reactable(
          wr_sub,
          compact = TRUE,
          defaultSorted = list(week = "asc"),
          columns = list(
            week = colDef(maxWidth = 60, align = "center"),
            franchise = colDef(aggregate = "unique", width = 120,
                               name = "Franchise", align = "left"),
            team = colDef(maxWidth = 80, align = "right", name = "Opponent"),
            opp_logo = colDef(width = 75, cell = embed_img(height = 50, width = 60),
                              name = ""),
            fantasy_points = colDef(aggregate = "max", width = 80,
                                    name = "Points", align = "center"),
            receptions = colDef(aggregate = "sum", width = 100,
                                name = "Receptions", align = "center"),
            targets = colDef(aggregate = "sum", width = 100,
                             name = "Targets", align = "center"),
            target_share = colDef(aggregate = "mean", width = 100,
                                  name = "Target %", align = "center"),
            receiving_yards = colDef(aggregate = "sum", width = 120,
                                     name = "Rec Yards", align = "center"),
            receiving_tds = colDef(aggregate = "sum", width = 100,
                                   name = "Rec TDs", align = "center"),
            receiving_air_yards = colDef(aggregate = "sum", width = 120,
                                         name = "Air Yards", align = "center"),
            receiving_yards_after_catch = colDef(aggregate = "sum", width = 120,
                                                 name = "YAC", align = "center"),
            receiving_epa = colDef(aggregate = "mean", format = colFormat(digits = 2),
                                   width = 120, name = "Receiving EPA", align = "center")))
        })
    })
  
  ### TE TABLE ====
  output$te_player <- renderReactable({
    req(TE_PLAYERSTATSparent)
    # fantasy_points, targets, attempts, receptions, target_share, receiving_yards,
    # receiving_air_yards, receiving_tds, receiving_yards_after_catch, receiving_epa,
    # rushing_yards, rushing_tds, Claimed_flag
    reactable(
      TE_PLAYERSTATSparent,
      fullWidth = TRUE,
      theme = fivethirtyeight(centered = TRUE),
      searchable = TRUE,
      defaultPageSize = 20,
      filterable = TRUE,
      columns = list(
        Claimed = colDef(name = "Claimed", align = "center", width = 90),
        headshot_url = colDef(
          name = "", align = "center", filterable = FALSE,
          cell = function(url) 
            {
            if (is.na(url) || url == "") return(NULL)
            htmltools::img(src = url, height = 40)
            }, width = 60),
        player_display_name = colDef(width = 180, name = "Player", align = "left"),
        fantasy_points = colDef(name = "Avg Points", align = "center", filterable = FALSE),
        receptions = colDef(name = "Receptions", align = "center", filterable = FALSE),
        targets = colDef(name = "Targets", align = "center", filterable = FALSE),
        target_share = colDef(name = "Target %", align = "center", filterable = FALSE),
        rushing_yards = colDef(name = "Rush Yards", align = "center", filterable = FALSE),
        rushing_tds = colDef(name = "TDs", align = "center", filterable = FALSE),
        receiving_yards = colDef(name = "Rec Yards", align = "center", filterable = FALSE),
        receiving_air_yards = colDef(name = "Air Yards", align = "center", filterable = FALSE),
        receiving_tds = colDef(name = "TDs", align = "center", filterable = FALSE),
        receiving_yards_after_catch = colDef(name = "YAC", align = "center", filterable = FALSE),
        receiving_epa = colDef(name = "Receiving EPA", align = "center", filterable = FALSE)),
      details = function(index) 
        {
        Player <- TE_PLAYERSTATSparent$player_display_name[index]
        te_sub <- TE_PLAYERSTATS %>%
          filter(player_display_name == Player) %>%
          arrange(week) %>%
          select(week, franchise, team, opp_logo, fantasy_points, rushing_tds,
                 receptions, targets, target_share, receiving_yards, receiving_tds,
                 receiving_air_yards, receiving_yards_after_catch, receiving_epa,
                 rushing_yards)
        reactable(
          te_sub,
          compact = TRUE,
          defaultSorted = list(week = "asc"),
          columns = list(
            week = colDef(maxWidth = 60, align = "center"),
            franchise = colDef(aggregate = "unique", width = 120,
                               name = "Franchise", align = "left"),
            team = colDef(maxWidth = 80, align = "right", name = "Opponent"),
            opp_logo = colDef(width = 75, cell = embed_img(height = 50, width = 60),
                              name = ""),
            fantasy_points = colDef(aggregate = "max", width = 80,
                                    name = "Points", align = "center"),
            receptions = colDef(aggregate = "sum", width = 100,
                                name = "Receptions", align = "center"),
            targets = colDef(aggregate = "sum", width = 100,
                             name = "Targets", align = "center"),
            target_share = colDef(aggregate = "mean", width = 100,
                                  name = "Target %", align = "center"),
            rushing_yards = colDef(aggregate = "sum", width = 100,
                                   name = "Rush Yards", align = "center"),
            rushing_tds = colDef(aggregate = "sum", width = 80,
                                 name = "Rush TDs", align = "center"),
            receiving_yards = colDef(aggregate = "sum", width = 120,
                                     name = "Rec Yards", align = "center"),
            receiving_tds = colDef(aggregate = "sum", width = 100,
                                   name = "Rec TDs", align = "center"),
            receiving_air_yards = colDef(aggregate = "sum", width = 120,
                                         name = "Air Yards", align = "center"),
            receiving_yards_after_catch = colDef(aggregate = "sum", width = 120,
                                                 name = "YAC", align = "center"),
            receiving_epa = colDef(aggregate = "mean", format = colFormat(digits = 2),
                                   width = 120, name = "Receiving EPA", align = "center")))
        })
    })
  
  ### K Table ====
  output$k_player <- renderReactable({
    req(K_PLAYERSTATSparent)
    # fantasy_points, fgs_att, fgs_made, avg_distance, max_distance,
    # fgs_blocked, fgs_pct, Claimed_flag
    reactable(
      K_PLAYERSTATSparent,
      fullWidth = TRUE,
      theme = fivethirtyeight(centered = TRUE),
      searchable = TRUE,
      defaultPageSize = 20,
      filterable = TRUE,
      columns = list(
        Claimed = colDef(name = "Claimed", align = "center", width = 90),
        headshot_url = colDef(
          name = "", align = "center", filterable = FALSE,
          cell = function(url) 
            {
            if (is.na(url) || url == "") return(NULL)
            htmltools::img(src = url, height = 40)
            }, width = 60),
        player_display_name = colDef(width = 180, name = "Player", align = "left"),
        fantasy_points = colDef(name = "Avg Points", align = "center", filterable = FALSE),
        fg_att = colDef(name = "Attempts", align = "center", filterable = FALSE),
        fg_made = colDef(name = "Field Goals", align = "center", filterable = FALSE),
        success = colDef(name = "% FG Made", align = "center", filterable = FALSE),
        fg_blocked = colDef(name = "Blocked FG", align = "center", filterable = FALSE)),
      details = function(index) 
        {
        Player <- K_PLAYERSTATSparent$player_display_name[index]
        k_sub <- K_PLAYERSTATS %>%
          filter(player_display_name == Player) %>%
          arrange(week) %>%
          select(week, franchise, team, opp_logo, fantasy_points,
                 fg_att, fg_made, fg_made_list,
                 fg_blocked) #fgs_pct
        reactable(
          k_sub,
          compact = TRUE,
          defaultSorted = list(week = "asc"),
          columns = list(
            week = colDef(maxWidth = 60, align = "center"),
            franchise = colDef(aggregate = "unique", width = 120,
                               name = "Franchise", align = "left"),
            team = colDef(maxWidth = 80, align = "right", name = "Opponent"),
            opp_logo = colDef(width = 75, cell = embed_img(height = 50, width = 60),
                              name = ""),
            fantasy_points = colDef(aggregate = "max", width = 80,
                                    name = "Points", align = "center"),
            fg_att = colDef(aggregate = "sum", width = 80,
                            name = "Attempts", align = "center"),
            fg_made = colDef(aggregate = "sum", width = 100,
                             name = "Field Goals", align = "center"),
            fg_made_list = colDef(aggregate = "unique", width = 200,
                                  name = "FG Distance(s)", align = "center"),
            fg_blocked = colDef(aggregate = "sum", format = colFormat(digits = 2),
                                width = 80, name = "Blocked", align = "center")))
        })
    })
  
  ### DEF PASS TABLE ====
  # --- PASS defense table (with nested details) ---
  output$dstpass_player <- renderReactable({
    req(DEFENSEPASSparent)
    
    reactable(
      DEFENSEPASSparent,
      fullWidth = TRUE,
      theme = fivethirtyeight(centered = TRUE),
      defaultPageSize = 20,
      filterable = TRUE,
      columns = list(
        def_logo = colDef(
          name = "", align = "center", filterable = FALSE, width = 60,
          cell = function(url) if (!is.na(url) && nzchar(url)) 
            htmltools::img(src = url, height = 40)),
        defteam = colDef(width = 80, name = "Defense", align = "left"),
        tot_receiving_yards = colDef(name = "Passing Yards Allowed", align = "center", 
                                     filterable = FALSE),
        tot_pass_touchdown  = colDef(name = "Passing TDs", align = "center",
                                     filterable = FALSE),
        tot_interception    = colDef(name = "INTs", align = "center",
                                     filterable = FALSE)),
        details = function(index) 
          {
        team <- DEFENSEPASSparent$defteam[index]
        def_sub <- DEFENSEPASS_opp %>%
          dplyr::filter(defteam == team) %>%
          dplyr::arrange(week, posteam)
        
        reactable(
          def_sub,
          compact = TRUE,
          defaultSorted = list(week = "asc"),
          columns = list(
            week = colDef(maxWidth = 60, align = "center"),
            defteam = colDef(name = "Defense"),
            posteam_logo = colDef(
              width = 60, name = "",
              cell = function(url) if (!is.na(url) && nzchar(url)) 
                htmltools::img(src = url, height = 40)),
            posteam = colDef(name = "Opponent", maxWidth = 80, align = "center"),
            complete_pass = colDef(name = "Comp", align = "center"),
            pass_touchdown = colDef(name = "TDs", align = "center"),
            receiving_yards = colDef(name = "Rec Yards", align = "center"),  
            yards_after_catch = colDef(name = "YAC", align = "center"),
            interception = colDef(name = "INTs", align = "center"),
            def_logo = colDef(show = FALSE)),
          details = function(child_index) 
            {
            opp <- def_sub$posteam[child_index]
            wk  <- def_sub$week[child_index]
            
            def_sub2 <- DEFENSEPASS_receiver %>%
              dplyr::filter(defteam == team, posteam == opp, week == wk) %>%
              dplyr::arrange(dplyr::desc(receiving_yards))
            
            reactable(
              def_sub2,
              compact = TRUE,
              columns = list(
                receiver_headshot = colDef(
                  name = "", align = "center", filterable = FALSE, width = 60,
                  cell = function(url) if (!is.na(url) && nzchar(url)) 
                    htmltools::img(src = url, height = 40)),
                defteam = colDef(name = "Defense"),
                posteam = colDef(name = "Opponent"),
                receiver_player_name = colDef(name = "Receiver", align = "left"),
                complete_pass = colDef(name = "Comp", align = "center"),
                yards_after_catch = colDef(name = "YAC", align = "center"),
                pass_touchdown = colDef(name = "TDs", align = "center"),
                interception = colDef(name = "INTs", align = "center")))
            })
        })
    })
  
  ### DEF RUN TABLE ====
  # --- RUN defense table (with nested details) ---
  output$dstrun_player <- renderReactable({
    req(DEFENSERUNparent)
    
    reactable(
      DEFENSERUNparent,
      fullWidth = TRUE,
      theme = fivethirtyeight(centered = TRUE),
      defaultPageSize = 20,
      filterable = TRUE,
      columns = list(
        def_logo = colDef(
          name = "", align = "center", filterable = FALSE, width = 60,
          cell = function(url) if (!is.na(url) && nzchar(url)) 
            htmltools::img(src = url, height = 40)),
        defteam = colDef(width = 80, name = "Defense", align = "left"),
        tot_rushing_yards = colDef(name = "Rushing Yards Allowed", align = "center",
                                   filterable = FALSE),
        tot_rush_touchdown  = colDef(name = "Rushing TDs", align = "center",
                                     filterable = FALSE),
        tot_fumble    = colDef(name = "Fumbles", align = "center",
                               filterable = FALSE)),
      details = function(index) 
        {
        team <- DEFENSERUNparent$defteam[index]
        def_sub <- DEFENSERUN_opp %>%
          dplyr::filter(defteam == team) %>%
          dplyr::arrange(week, posteam)
        
        reactable(
          def_sub,
          compact = TRUE,
          defaultSorted = list(week = "asc"),
          columns = list(
            week = colDef(maxWidth = 60, align = "center"),
            defteam = colDef(name = "Defense"),
            posteam_logo = colDef(
              width = 60, name = "",
              cell = function(url) if (!is.na(url) && nzchar(url)) 
                htmltools::img(src = url, height = 40)),
            posteam = colDef(name = "Opponent", maxWidth = 80, align = "center"),
            rushing_yards = colDef(name = "Rushing Yards", align = "center"),
            rush_touchdown = colDef(name = "TDs", align = "center"),
            fumble = colDef(name = "Fumbles", align = "center"),
            def_logo = colDef(show = FALSE)),
          details = function(child_index) 
            {
            opp <- def_sub$posteam[child_index]
            wk  <- def_sub$week[child_index]
            
            def_sub2 <- DEFENSERUN_rusher %>%
              dplyr::filter(defteam == team, posteam == opp, week == wk) %>%
              dplyr::arrange(dplyr::desc(rushing_yards))
            
            reactable(
              def_sub2,
              compact = TRUE,
              columns = list(
                rusher_headshot = colDef(
                  name = "", align = "center", filterable = FALSE, width = 60,
                  cell = function(url) if (!is.na(url) && nzchar(url)) 
                    htmltools::img(src = url, height = 40)),
                defteam = colDef(name = "Defense"),
                posteam = colDef(name = "Opponent"),
                rusher_player_name = colDef(name = "Rusher", align = "left"),
                rushing_yards = colDef(name = "Rushing Yards", align = "center"),
                rush_touchdown = colDef(name = "TDs", align = "center"),
                fumble = colDef(name = "Fumbles", align = "center")))
            })
        })
    })
  
  
  
  ## DEFENSE COMPARE ====
  output$def_yards <- renderHighchart({

    hchart(DEFENSECOMPARE, 'scatter', hcaes(x = Passing, y = Rushing, 
                                            group = defteam, size = 0.5)) %>% 
      hc_add_theme(nfl_hc_theme) %>% 
      hc_colors(DEFENSECOMPARE$color2) %>% 
      hc_credits(text = "Source: NFLREADR", enabled = TRUE,
                 style = list(fontSize = "12px")) %>%  
      hc_tooltip(pointFormat = TEAMDEF_HC_tooltip, useHTML = TRUE,
                 crosshairs = TRUE, shared = FALSE, borderWidth = 3, 
                 style = list(fontSize = "20px")) %>% 
      hc_xAxis(
        title = list(text = "Passing Yards Allowed by Defense", 
                     style = list(color = "#edf1f2")), 
        labels = list(style = list(color = "#edf1f2", fontSize = "24px")), 
        lineColor = "#edf1f2") %>% 
      hc_yAxis(
        title = list(text = "Rushing Yards Allowed by Defense", 
                     style = list(color = "#edf1f2")), 
        labels = list(style = list(color = "#edf1f2", fontSize = "24px")), 
        lineColor = "#edf1f2") %>% 
      hc_plotOptions(series = list(states = list(inactive = list(opacity = 0.1))))
  })
  
## DEFENSE COUNTS ====
  output$def_counts <- renderGirafe({
    
    girafe(ggobj = DEFENSECOUNTSplot,
           options = list(
             opts_tooltip(
               css = "font-size: 36px; padding: 3pt; color: white; background-color: #333;")))
  })
  
  
  ## NFL SCHEDULE ====
  output$schedule_table <- renderReactable({
    reactable(SCHEDULEGRID, fullWidth = TRUE,
              theme = fivethirtyeight(centered = TRUE),
              searchable = TRUE, defaultPageSize = 16, filterable = TRUE,
              columns=list(
                week = colDef(maxWidth = 100, align = "right"),
                home_score = colDef(width = 100, name = "Score", align = "center",
                                    filterable = FALSE),
                home_team = colDef(width = 100, name = "Home", align = "right"),
                WLDH = colDef(width = 120, name = "WLD", align = "center",
                              filterable = FALSE),
                home_logo = colDef(width = 75, cell = embed_img(height = 40, width = 40),
                                   filterable = FALSE, name = ""),
                away_logo = colDef(width = 75, cell = embed_img(height = 40, width = 40),
                                   filterable = FALSE, name = ""),
                WLDA = colDef(width = 120, name = "WLD", align = "center",
                              filterable = FALSE),
                away_team = colDef(width = 100, name = "Away", align = "left"),
                away_score = colDef(width = 100, name = "Score", align = "center",
                                    filterable = FALSE),
                game_odds = colDef(name = "Vegas Odds", align = "left", filterable = FALSE)))
    })
}

shinyApp(ui, server)
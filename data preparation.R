library(tidyverse)

## Loading Datasets

games <- read_csv("games.csv")
player <- read_csv("players.csv")
pbp <- read_csv("plays.csv")
player_pbp <- read_csv("player_play.csv")
track <- read_csv("tracking_cleaned.csv")
motion_pbp <- read.csv("motion pbp.csv")

## Merging + Adding Binary + Run/Pass

motion_pbp_grouped <- motion_pbp |>
  group_by(gameId, playId) |>
  summarise(anyMotion = max(motionSinceLineset))

epa_merge <- pbp |>
  left_join(track, by = c("gameId", "playId")) |>
  mutate(isMotion = !is.na(`speed at snap`),
         isPass = !is.na(passLength),
         success = expectedPointsAdded > 0)

epa_merge <- epa_merge |>
  left_join(motion_pbp_grouped, by = c("gameId", "playId"))

defining_motion <- function(mt, ismot) {
  for(idx in 1:length(mt)) {
    if (ismot[idx] == 'True' & is.na(mt[idx])) {
      mt[idx] = 'other'
    }
  }
  return(mt)
}

epa_merge <- epa_merge |>
  mutate(motion_type = defining_motion(epa_merge$`motion type`, epa_merge$anyMotion))

epa_merge <- epa_merge |>
  left_join(games, by= "gameId")

## Feature engineering new columns

find_time_to_pressure <- function(col) {
  for (idx in 1:length(col)) {
    if (col[idx] > 20) {
      col[idx] = 10.5
    }
  }
  return(col)
}

pbp_grouped <- player_pbp |>
  group_by(gameId,playId) |>
  summarise(num_playersRushing = sum(wasInitialPassRusher, na.rm = TRUE),
            num_playersBlocking = sum(!is.na(pressureAllowedAsBlocker)),
            time_toPressure = min(timeToPressureAsPassRusher, na.rm=TRUE))

pbp_grouped$time_toPressure = find_time_to_pressure(pbp_grouped$time_toPressure)

epa_merge <- epa_merge |>
  left_join(pbp_grouped, by=c("gameId", "playId"))

epa_merge$man_coverage = epa_merge$pff_manZone == 'Man'
epa_merge$fly = epa_merge$motion_type == 'fly'
epa_merge$glide_in = epa_merge$motion_type == 'glide in'
epa_merge$glide_out = epa_merge$motion_type == 'glide out'
epa_merge$jet = epa_merge$motion_type == 'jet'
epa_merge$other_motion = epa_merge$motion_type == 'other'
epa_merge$down_two = epa_merge$down == 2
epa_merge$down_three = epa_merge$down == 3
epa_merge$down_four = epa_merge$down == 4
epa_merge$yards_to_go = epa_merge$yardsToGo 
epa_merge$yard_line = epa_merge$absoluteYardlineNumber
epa_merge$homeTeamOffense = epa_merge$homeTeamAbbr == epa_merge$possessionTeam
epa_merge$awayTeamOffense = epa_merge$visitorTeamAbbr == epa_merge$possessionTeam
epa_merge$posteam_win_probability = epa_merge$homeTeamOffense * epa_merge$preSnapHomeTeamWinProbability
epa_merge$posteam_win_probability = epa_merge$posteam_win_probability + (epa_merge$awayTeamOffense * epa_merge$preSnapVisitorTeamWinProbability)

epa_merge_pass <- epa_merge |>
  filter(isDropback)
epa_merge_pass <- epa_merge_pass |>
  filter(!is.na(pff_passCoverage))
y = data.frame(gameId = epa_merge_pass$gameId,  playId = epa_merge_pass$playId,
               time_to_pressure = epa_merge_pass$time_toPressure, is_pressure = epa_merge_pass$time_toPressure < 10.5)
X = epa_merge_pass[, (ncol(epa_merge)-15):ncol(epa_merge)]
X = X[, -3]
X = X[, -15]
X[is.na(X)] <- FALSE
model = glm(y~., X, family = binomial)
model
summary(model)
X$gameId = epa_merge_pass$gameId
X$playId = epa_merge_pass$playId
write.csv(X, 'pressure_model.csv')
write.csv(epa_merge, 'epa_merge.csv')

pff_passCoverage_ <- epa_merge_pass$pff_passCoverage
pff_passCoverage_ <- data.frame(pff_passCoverage_ = pff_passCoverage_)
mod_matrix <- model.matrix(~ pff_passCoverage_, data = pff_passCoverage_)
X <- bind_cols(X, mod_matrix[,-1])
y$EPA <- epa_merge_pass$expectedPointsAdded
y$time_to_throw <- epa_merge_pass$timeToThrow
y$time_to_sack <- epa_merge_pass$timeToSack
y$isSack <- !is.na(epa_merge_pass$timeToSack)
X$play_clock_remaining <- epa_merge_pass$playClockAtSnap
pbpg <- player_pbp |>
  group_by(gameId, playId) |>
  summarise(yards_lossed_sack = max(sackYardsOffense, na.rm = TRUE),
            had_pass_reception = max(hadPassReception, na.rm = TRUE),
            YAC = max(yardageGainedAfterTheCatch, na.rm = TRUE),
            pass_defended = max(passDefensed, na.rm = TRUE),
            quarterback_hit = max(quarterbackHit, na.rm = TRUE))
y <- y |>
  left_join(pbpg, by = c("gameId", "playId"))
y$pass_length <- epa_merge_pass$passLength
y$interception <- epa_merge_pass$passResult == 'IN'
y[is.na(y)] <- 0
y$time_to_sack <- y$time_to_sack + (as.integer(!y$isSack) * 12)
y$time_to_throw <- y$time_to_throw + (as.integer(y$isSack) * y$time_to_sack)
y_attempts_only <- y |>
  filter(!y$isSack & y$time_to_throw > 0)
y_completions <- y |> 
  filter(y$had_pass_reception == 1)
write.csv(y, 'targets_dropback.csv')
write.csv(y_attempts_only, 'targets_attempts.csv')
write.csv(y_completions, 'targets_completions.csv')
write.csv(X, 'features_dropbacks.csv')
write.csv(epa_merge_pass, 'epa_merge_pass_dropbacks.csv')
write.csv(X_attempts, 'features_attempts.csv')
write.csv(X_completions, 'features_completions.csv')

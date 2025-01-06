library(tidyverse)

## Loading Datasets

games <- read_csv("games.csv")
player <- read_csv("players.csv")
pbp <- read_csv("plays.csv")
player_pbp <- read_csv("player_play.csv")
track <- read_csv("tracking_cleaned.csv")

## Merging + Adding Binary + Run/Pass

epa_merge <- pbp |>
  left_join(track, by = c("gameId", "playId")) |>
  mutate(isMotion = !is.na(`speed at snap`),
         isPass = !is.na(passLength),
         success = expectedPointsAdded > 0)

## Cover ones with only pass plays

epa_merge |>
  filter(pff_passCoverage == "Cover-1" & isPass & isMotion) |>
  ggplot(aes(x = `motion type`, y = expectedPointsAdded)) + 
  geom_boxplot() +
  theme_bw() + 
  labs(title = "Cover One EPA with different Motion Type")

## Cover ones run vs pass

epa_merge |>
  filter(pff_passCoverage == "Cover-1" & isMotion) |>
  ggplot(aes(x = `motion type`, y = expectedPointsAdded)) + 
  geom_boxplot() +
  facet_wrap(~isPass) +
  theme_bw() + 
  labs(title = "Cover One EPA comparing Run/Pass",
       subtitle = "TRUE = Passing Play")

## Cover ones motion vs no motion

epa_merge |>
  filter(pff_passCoverage == "Cover-1") |>
  ggplot(aes(x = `motion type`, y = expectedPointsAdded)) + 
  geom_boxplot() +
  theme_bw() + 
  labs(title = "Cover One EPA comparing Run/Pass",
       subtitle = "TRUE = Passing Play")

## Success of PA motion vs no motion

epa_merge |>
  filter(playAction) |>
  ggplot(aes(y = expectedPointsAdded)) + 
  geom_boxplot() +
  facet_wrap(~isMotion) +
  theme_bw() + 
  labs(title = "PA motion vs no motion",
       subtitle = "TRUE = PA")

epa_merge |>
  filter(playAction) |>
  group_by(isMotion) |>
  summarize(success_rate = mean(success))

## Motion short vs. long frequency & success

epa_merge |>
  filter(isMotion) |>
  group_by(`motion direction`) |>
  summarize(count = n(),
            success_rate = mean(success))

## Motion by team and quarters, success analysis

epa_merge |>
  group_by(possessionTeam) |>
  summarize(motion_rate = mean(isMotion),
            success_rate = mean(success)) |>
  arrange(desc(motion_rate))

epa_merge |>
  group_by(quarter) |>
  summarize(motion_rate = mean(isMotion))

## Offensive Formation Analysis

epa_merge |>
  group_by(offenseFormation) |>
  summarize(motion = sum(isMotion),
            motion_rate = mean(isMotion))


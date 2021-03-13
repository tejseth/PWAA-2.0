devtools::install_github("tylermorganwall/rayshader")

games <- read_csv("http://www.habitatring.com/games.csv")

scores <- games %>%
  mutate(winning_score = case_when(
    home_score > away_score ~ home_score,
    away_score > home_score ~ away_score,
    home_score == away_score ~ home_score
  )) %>%
  mutate(losing_score = case_when(
    home_score > away_score ~ away_score,
    away_score > home_score ~ home_score,
    home_score == away_score ~ away_score
  )) %>%
  select(winning_score, losing_score)

seasons <- 2017:2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>%
  mutate(season = substr(old_game_id, 1, 4))

pbp_rp <- pbp_rp %>%
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

pbp_rp <- pbp_rp %>%
  mutate(
    defteam = case_when(
      defteam == 'OAK' ~ 'LV',
      defteam == 'SD' ~ 'LAC',
      defteam == 'STL' ~ 'LA',
      TRUE ~ defteam
    )
  )

garbage_pbp <- pbp_rp %>%
  filter(wp < .05 | wp > .95)

garbage_stats <- garbage_pbp %>%
  filter(!is.na(epa)) %>%
  filter(!is.na(cpoe)) %>%
  filter(qb_dropback == 1) %>%
  group_by(passer) %>% 
  summarize(garbage_plays = n(), 
            garbage_epa = mean(epa),
            garbage_cpoe = mean(cpoe)) %>%
  filter(garbage_plays >= 50) %>% 
  arrange(desc(garbage_epa))



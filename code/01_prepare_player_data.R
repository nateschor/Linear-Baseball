
pacman::p_load(
  tidyverse,
  fst
)

df_2021 <- read_fst("data/raw/all_data.fst") %>% 
  filter(year_ID == 2021) %>% 
  glimpse()


# Salary ------------------------------------------------------------------

df_salary <- df_2021 %>% 
  group_by(player_name) %>% 
  summarize(
    salary = sum(salary),
    bwar = sum(bwar162)
  ) %>% 
  filter(
    if_all(c(salary, bwar), ~ !is.na(.))
  ) %>% 
  print()

# Position ---------------------------------------------------------------

df_pitcher_position <- df_2021 %>% 
  select(player_name, starts, g_pitch) %>% 
  filter(
    if_all(c(starts, g_pitch), ~ !is.na(.))
  ) %>%
  group_by(player_name) %>% 
  summarize(
    across(everything(), ~ sum(.))
  ) %>% 
  transmute(
    player_name,
    position = if_else(starts / g_pitch >= .5, "SP", "RP")
  ) %>% 
  print()

df_fielder_position <- df_2021 %>% 
  select(player_name, starts_with("gms")) %>% 
  group_by(player_name) %>% 
  summarize(
    across(everything(), ~ sum(.))
  ) %>% 
  ungroup() %>% 
  pivot_longer(., -player_name, 
               names_to = "position", 
               values_to = "games_played", 
               names_prefix = "gms_") %>%
  group_by(player_name) %>% 
  slice_max(games_played, with_ties = FALSE, n = 1) %>% 
  ungroup() %>% 
  filter(position %in% c("C", "1B", "2B", "3B", "SS", "OF", "DH")) %>% 
  select(player_name, position) %>% 
  anti_join(df_pitcher_position, by = "player_name") %>% 
  print()

df_position <- bind_rows(df_fielder_position, df_pitcher_position)

check_unique_players <- df_position %>% 
  count(player_name) %>% 
  slice_max(n, with_ties = FALSE) %>% 
  pull(n)

stopifnot("Each player should only occur once in dataset" = check_unique_players == 1)

# Team --------------------------------------------------------------------

df_team <- df_2021 %>% 
  select(player_name, team_ID, stint_ID, starts_with("gms")) %>% 
  pivot_longer(., starts_with("gms"), 
               names_to = "position", values_to = "games", names_prefix = "gms_") %>% 
  select(-position) %>% 
  group_by(player_name, team_ID, stint_ID) %>% 
  summarize(
    games = sum(games)
  ) %>% 
  group_by(player_name) %>% 
  slice_max(games, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(player_name, team = team_ID)


# Merged Dataset ----------------------------------------------------------

df <- list(df_salary, df_position, df_team) %>% 
  reduce(full_join, by = "player_name") %>% 
  filter(
    if_all(everything(), ~ !is.na(.))
  )

write_csv(df, "data/derived/merged_players.csv")

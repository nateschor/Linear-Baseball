
pacman::p_load(
  tidyverse,
  lpSolve
)

df_players <- read_csv("data/derived/merged_players.csv") %>% 
  mutate(
    grouped_position = case_when(
      position %in% c("1B", "2B", "SS", "3B") ~ "IF",
      position %in% c("LF", "CF", "RF") ~ "OF",
      TRUE ~ position
    ),
    position = if_else(player_name == "Anthony Rizzo", "1B", position),
    grouped_position = if_else(player_name == "Anthony Rizzo", "IF", grouped_position)
  ) %>% 
  filter(salary > 0)

df_payroll <- read_csv("data/derived/payroll_2021.csv")

num_players <- nrow(df_players)

Create_Individual_Position_Indicator <- function(pos) {
  
  if_else(df_players %>% pull(position) == pos, 1, 0)
}

Create_Grouped_Position_Indicator <- function(pos) {
  
  if_else(df_players %>% pull(grouped_position) == pos, 1, 0)
}


Solve_Team_LP <- function(team) {
  
  team_salary <- df_payroll %>% 
    filter(Team == team) %>% 
    pull(Payroll)
  
  constraints <- rbind(
    rep(1, num_players),
    Create_Individual_Position_Indicator("SP"),
    Create_Individual_Position_Indicator("RP"),
    Create_Individual_Position_Indicator("CF"),
    Create_Individual_Position_Indicator("RF"),
    Create_Individual_Position_Indicator("LF"),
    Create_Individual_Position_Indicator("2B"),
    Create_Individual_Position_Indicator("3B"),
    Create_Individual_Position_Indicator("1B"),
    Create_Individual_Position_Indicator("SS"),
    Create_Individual_Position_Indicator("C"),
    Create_Individual_Position_Indicator("DH"),
    Create_Grouped_Position_Indicator("IF"),
    Create_Grouped_Position_Indicator("OF"),
    df_players$salary
    
  )
  
    objective <- df_players$bwar
    signs <- c(
      rep("==", 3),
      rep(">=", 7),
      rep("==", 4),
      "<="
    )
  
  constraint_rhs <- c(25, 
                      5,
                      7,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      1,
                      2,
                      1,
                      5,
                      5,
                      team_salary)
  
  model <- lp("max", objective, constraints, signs, constraint_rhs, 
              all.bin = TRUE,
              compute.sens = 1)
  
  solution <- model$solution
  
  df_solution <- df_players %>% 
    slice(which(solution == 1)) %>% 
    arrange(desc(bwar))
  
  return(df_solution)
}

df_payroll <- df_payroll %>% 
  mutate(
    team_id = row_number() %>% as.character()
  )

df_lp_solved <- map_dfr(df_payroll$Team, ~ Solve_Team_LP(.), .id = "team_id") %>% 
  left_join(., df_payroll, by = "team_id") %>% 
  select(-team_id) %>% 
  mutate(
    optimal_team = case_when(
      Team == "A's" ~ "OAK", 
      Team == "Angels" ~ "LAA", 
      Team == "Astros" ~ "HOU", 
      Team == "B. Jays" ~ "TOR", 
      Team == "Braves" ~ "ATL", 
      Team == "Brewers" ~ "MIL", 
      Team == "Cardinals" ~ "STL", 
      Team == "Cubs" ~ "CHC", 
      Team == "D-Backs" ~ "ARI", 
      Team == "Dodgers" ~ "LAD", 
      Team == "Giants" ~ "SFG", 
      Team == "Guardians" ~ "CLE", 
      Team == "Mariners" ~ "SEA", 
      Team == "Marlins" ~ "MIA", 
      Team == "Mets" ~ "NYM", 
      Team == "Nationals" ~ "WSN", 
      Team == "Orioles" ~ "BAL", 
      Team == "Padres" ~ "SDP", 
      Team == "Phillies" ~ "PHI", 
      Team == "Pirates" ~ "PIT", 
      Team == "R. Sox" ~ "BOS", 
      Team == "Rangers" ~ "TEX", 
      Team == "Rays" ~ "TBR", 
      Team == "Reds" ~ "CIN", 
      Team == "Rockies" ~ "COL", 
      Team == "Royals" ~ "KCR", 
      Team == "Tigers" ~ "DET", 
      Team == "Twins" ~ "MIN", 
      Team == "W. Sox" ~ "CHW", 
      Team == "Yankees" ~ "NYY"
    )
) %>% 
  rename(actual_team = team) %>% 
  select(-Team) %>% 
  print()

write_csv(df_lp_solved, "data/derived/lp_solved.csv")

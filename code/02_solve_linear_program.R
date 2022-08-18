
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
    )
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

Solve_Team_LP("Yankees")
Solve_Team_LP("Astros")
Solve_Team_LP("Pirates")

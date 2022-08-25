
pacman::p_load(
  tidyverse,
  cowplot,
  kableExtra
)

# Data --------------------------------------------------------------------

df_players <- read_csv("data/derived/merged_players.csv") %>% 
  filter(salary > 0) %>% 
  print()

df_lp <- read_csv("data/derived/lp_solved.csv") %>% 
  print()

# Plots -------------------------------------------------------------------

p_salary_hist <- ggplot(df_players, aes(x = salary)) +
  geom_histogram(fill = "blue", color = "black", bins = 50) +
  scale_x_continuous(label = scales::dollar, name = "Salary") +
  scale_y_continuous(expand = expansion(0, 0), name = "Count") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
  )

p_war <- ggplot(df_players, aes(x = war_value)) +
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  scale_x_continuous(name = "JEFFBAGWELL") +
  scale_y_continuous(expand = expansion(0, 0), name = "Count") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
  )

Reorder_By_Median <- function(salary_or_war) {
  
  df_players %>% 
    group_by(position) %>% 
    summarize(
      metric_median = median({{salary_or_war}})
    ) %>% 
    arrange(metric_median) %>% 
    pull(position) %>% 
    as_factor(., )
}

factor_salary <- Reorder_By_Median(salary) 
factor_war <- Reorder_By_Median(war_value)

p_salary_position <- ggplot(df_players, aes(x = salary, y = factor(position, levels = factor_salary))) +
  geom_boxplot(fill = "red") +
  scale_x_continuous(label = scales::dollar, name = "Salary") +
  scale_y_discrete(expand = expansion(0, 0), name = "Position") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
  ) 

p_war_position <- ggplot(df_players, aes(x = war_value, y = factor(position, levels = factor_war))) +
  geom_boxplot(fill = "red") +
  scale_x_continuous(name = "JEFFBAGWELL") +
  scale_y_discrete(expand = expansion(0, 0), name = "Position") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank()
  ) 

df_actual_salary_war <- df_players %>% 
  group_by(team) %>% 
  summarize(
    total_salary = sum(salary),
    total_war = sum(war_value)
  )

df_optimal_salary_war <- df_lp %>% 
  group_by(optimal_team) %>% 
  summarize(
    total_salary = sum(salary),
    total_war = sum(war_value)
  )

Plot_WAR_VS_SALARY <- function(actual_or_optimal) {
  
  line_color <- switch (actual_or_optimal,
    "actual" = "blue",
    "optimal" = "red"
  )
  
  plotting_data <- switch (actual_or_optimal,
    "actual" = df_actual_salary_war,
    "optimal" = df_optimal_salary_war
  )
  
  ggplot(data = plotting_data) +
    geom_smooth(aes(x = total_salary, y = total_war), color = line_color, se = FALSE) +
    geom_point(aes(x = total_salary, y = total_war)) +
    theme_minimal() +
    scale_x_continuous(label = scales::dollar, breaks = seq(0, 300E6, 50E6)) +
    labs(
      x = "Team Total Dollars Spent",
      y = "Team Total JEFFBAGWELL"
    ) +
    theme(
      panel.grid = element_blank()
    )
}

p_actual_war_salary <- Plot_WAR_VS_SALARY("actual")
p_optimal_war_salary <- Plot_WAR_VS_SALARY("optimal")

p_war_salary_cowplot <- plot_grid(p_actual_war_salary, p_optimal_war_salary, 
          labels = c("Actual Team Values", "Optimal Team Values"))

# Save Plots --------------------------------------------------------------

path_plots <- "paper/figures/"

ggsave(paste0(path_plots, "salary_hist.png"), p_salary_hist)
ggsave(paste0(path_plots, "war_hist.png"), p_war)
ggsave(paste0(path_plots, "salary_position_boxplots.png"), p_salary_position)
ggsave(paste0(path_plots, "war_position_boxplots.png"), p_war_position)
ggsave(paste0(path_plots, "bwar_salary_scatter_cowplot.png"), p_bwar_salary_cowplot)

# Tables ------------------------------------------------------------------

path_tables <- "paper/tables/"

df_lp %>% 
  count(player_name, sort = TRUE) %>% 
  rename(
    "Player" = player_name,
    "Teams Picked" = n
  ) %>% 
  kable(., format = "latex", caption = "Number of teams (max of 30) selecting a given player for their optimal roster", 
        escape = TRUE, align = c("cc")) %>% 
  kable_styling(., latex_options = "striped") %>% 
  column_spec(., 1, border_left = TRUE) %>% 
  column_spec(., 2, border_right = TRUE) %>% 
  save_kable(., paste0(path_tables, "teams_picked.tex"))

v_teams <- df_players %>% 
  pull(team) %>% 
  unique()

df_optimal_equal_actual <- df_lp %>% 
  filter(optimal_team == actual_team) %>% 
  count(optimal_team, sort = TRUE)

v_teams_with_0 <- setdiff(v_teams, df_optimal_equal_actual$optimal_team)

df_optimal_equal_actual <- df_optimal_equal_actual %>% 
  add_row(
    optimal_team = v_teams_with_0,
    n = 0
  ) %>% 
  rename(
    Team = optimal_team,
    "Number of Players" = n
  )
        
kable(df_optimal_equal_actual, format = "latex", align = c("lc"),
      caption = "For each team, the number of players selected for their optimal team who are also on their actual team") %>% 
  kable_styling(., latex_options = "striped") %>% 
  column_spec(., 1, border_left = TRUE) %>% 
  column_spec(., 2, width = "10em", border_right = TRUE) %>% 
  save_kable(., paste0(path_tables, "num_players_actual_and_optimal.tex"))        
  
Save_Optimal_Roster <- function(team) {
  
  df_lp %>% 
    filter(optimal_team == team) %>% 
    select(
      `Actual Team` = actual_team,
      Player = player_name,
      Position = position, 
      JEFFBAGWELL = bwar,
      Salary = salary
    ) %>% 
    kable(., format = "latex", caption = paste("Optimal team for", team), escape = TRUE) %>% 
    kable_styling(., bootstrap_options = "striped") %>% 
    save_kable(., paste0(path_tables, team, "_optimal_team.tex"))  
}

walk(v_teams, ~ Save_Optimal_Roster(.))



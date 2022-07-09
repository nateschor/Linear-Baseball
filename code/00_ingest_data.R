
pacman::p_load(
  tidyverse,
  fst
)

df_raw <- read_csv("https://raw.githubusercontent.com/NeilPaine538/MLB-WAR-data-historical/master/jeffbagwell_war_historical.csv") %>% 
  glimpse()

write_fst(df_raw, "data/raw/all_data.fst")

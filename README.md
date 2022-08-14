# Linear-Baseball

Use Linear Programming to solve the following MLB optimization problem:

Objective: Based on the 2021 season, maximize each team's total [Jeff Bagwell War](https://github.com/NeilPaine538/MLB-WAR-data-historical)

Constraints:
- only 25 players allowed per team
  - 5 starting pitchers
  - 7 relief pitchers
  - 2 catchers 
  - 6 infielders (at least 1 1B, 1 2B, 1 3B, and 1 SS)
  - 5 outfielders (at least 1 LF, 1 CF, and 1 RF)
- each team can only spend as much as they actually spent in 2021
    - [Forbes](https://www.forbes.com/sites/maurybrown/2021/12/22/2021-mlb-final-player-payrolls-show-168m-drop-from-last-full-season-heres-every-team/?sh=674b3e663999)
    
The output will be the 25 players for each of the 30 teams (750 players in total, likely repeats) as well as their total bwar and salary

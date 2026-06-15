# 12_data_preprocess.R

# __ Harmonize team names ______________________________________________________
team_codes <- c(
  "JL Bourg" = "BEB",
  "Blois" = "BLO",
  "Boulazac Dordogne" = "BOU",
  "Champagne Châlons Reims" = "CHR",
  "Cholet" = "CHO",
  "Cholet Basket" = "CHO",
  "Élan Chalon" = "CSS",
  "Élan Sportif Chalonnais" = "CSS",
  "JDA Dijon" = "DIJ",               
  "Jeanne d'Arc Dijon Basket" = "DIJ",
  "Fos Provence Basket" = "FOS",
  "BCM Gravelines" = "GRA",
  "BCM Gravelines-Dunkerque" = "GRA",
  "Le Mans Sarthe" = "LEM",
  "Le Mans Sarthe Basket" = "LEM",
  "ESSM Le Portel" = "LEP",
  "Metropolitans 92" = "LEV",
  "Limoges CSP" = "LIM",
  "ASVEL" = "LYO",
  "LDLC ASVEL" = "LYO",
  "AS Monaco" = "MON",
  "Nancy" = "NCY",
  "SLUC Nancy Basket" = "NCY",
  "Nanterre 92" = "NTR",
  "Orléans Loiret" = "ORL",
  "Paris Basketball" = "PAR",
  "ÉB Pau-Lacq-Orthez" = "PAU",
  "Roanne" = "ROA",
  "Chorale Roanne Basket" = "ROA",
  "Stade Rochelais" = "ROC",
  "Saint-Quentin Basketball" = "SQT",
  "SIG Strasbourg" = "STR",
  "xSIG Strasbourg" = "STR"
)
all_teams <- unique(unlist(team_codes))

# __ Preprocess data ___________________________________________________________
clean_games <- function(df, phase, season) {
  
  Sys.setlocale("LC_TIME", "C")
  
  df_clean <- df |>
    
    # Rename existing columns
    rename(
      date = Date,
      team_home = Opp,
      pts_home = `PTS...5`,
      team_away = Team,
      pts_away = `PTS...3`
    ) |>
    
    # Delete postponed & canceled games
    filter(!is.na(pts_home)) |>
    
    # Update existing variables and create new ones
    mutate(
      
      date = as.Date(date, "%a %b %d %Y"),
      team_home = recode(team_home, !!!team_codes),
      team_away = recode(team_away, !!!team_codes),
      
      score_diff = pts_home - pts_away,
      playoff = phase == "playoffs",
      season = season,
      serie = NA_character_,
      wins_A = 0,
      wins_B = 0,
    )
  
  # Rearrange columns
  df_clean <- df_clean[c(
    "date", "season", "playoff", "serie",
    "team_home", "pts_home", "team_away", "pts_away",
    "score_diff",
    "wins_A", "wins_B"
  )]
  
  return(df_clean)
}
games <- bind_rows(
  lapply(names(raw_games), function(phase) {
    lapply(names(raw_games[[phase]]), function(season) {
      clean_games(raw_games[[phase]][[season]], phase, season)
    })
  })
)

compute_po_series <- function (df) {
  
  df <- df |>
    mutate(
      serie = paste(
        pmin(team_home, team_away),
        pmax(team_home, team_away),
        sep = "-"
      )
    )
  
  df <- df |>
    group_by(season, serie) |>
    mutate(
      wins_A = lag(
        cumsum(
          team_home == pmin(team_home, team_away) & score_diff > 0 |
            team_away == pmin(team_home, team_away) & score_diff < 0
        ),
        default = 0
      ),
      wins_B = lag(
        cumsum(
          team_home == pmax(team_home, team_away) & score_diff > 0 |
            team_away == pmax(team_home, team_away) & score_diff < 0
        ),
        default = 0
      )
    ) |>
    ungroup()
  
  return(df)
}
games <- bind_rows(
  games |> filter(!playoff),
  compute_po_series(games |> filter(playoff))
)

# __ Set up random framework ___________________________________________________
N <- 10000
Seed <- 1807

mon2par <- games |>
  filter(
    season %in% c("24", "25"),
    team_home %in% c("PAR", "MON") & team_away %in% c("PAR", "MON")
  )

# games |> 
#   filter(season == "26", !playoff) |>
#   group_by(team_home) |>
#   summarise(wins = sum(score_diff > 0)) |>
#   arrange(desc(wins))
# games |> 
#   filter(season == "26", !playoff) |>
#   group_by(team_away) |>
#   summarise(wins = sum(score_diff < 0)) |>
#   arrange(desc(wins))
# games |>
#   filter(season == "26", !playoff) |>
#   mutate(winner = ifelse(score_diff > 0, team_home, team_away)) |>
#   count(winner, name = "wins") |>
#   arrange(desc(wins))
# 
# games |>
#   filter(season == "26", !playoff) |>
#   filter(team_home == "MON" | team_away == "MON") |>
#   summarise(n_games = n())

# 12_data_preprocess.R

# __ Harmonize team names ______________________________________________________
team_codes <- c(
  "AS Monaco" = "ASM",
  "ASVEL" = "ASV",
  "BCM Gravelines" = "BCM",
  "BCM Gravelines-Dunkerque" = "BCM",
  "Blois" = "BLO",
  "Boulazac Dordogne" = "BOU",
  "Champagne Châlons Reims" = "CHR",
  "Cholet" = "CHO",
  "Cholet Basket" = "CHO",
  "Chorale Roanne Basket" = "ROA",
  "ÉB Pau-Lacq-Orthez" = "PLO",
  "Élan Chalon" = "CSS",
  "Élan Sportif Chalonnais" = "CSS",
  "ESSM Le Portel" = "LPO",
  "ESPE Basket Châlons-en-Champagne" = "CHR",
  "Fos Provence Basket" = "FOS",
  "JDA Dijon" = "JDA",               
  "Jeanne d'Arc Dijon Basket" = "JDA",
  "JL Bourg" = "BEB",
  "LDLC ASVEL" = "ASV",
  "Le Mans Sarthe" = "LMS",
  "Le Mans Sarthe Basket" = "LMS",
  "Limoges CSP" = "LIM",
  "Metropolitans 92" = "M92",
  "Nancy" = "NCY",
  "Nanterre 92" = "NTR",
  "Orléans Loiret" = "ORL",
  "Paris Basketball" = "PAR",
  "Roanne" = "ROA",
  "Saint-Quentin Basketball" = "SQT",
  "SIG Strasbourg" = "STR",
  "SLUC Nancy Basket" = "NCY",
  "Stade Rochelais" = "ROC",
  "xSIG Strasbourg" = "STR"
)
all_teams <- unique(unlist(team_codes))
teams26 <- c(
  "PAR", "ASM", "NTR", "ASV", "CHO", "LMS", "BEB", "STR",
  "CSS", "NCY", "DIJ", "BOU", "LIM", "BCM", "SQT", "LPO"
)

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

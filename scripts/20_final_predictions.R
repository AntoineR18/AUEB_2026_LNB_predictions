# 20_final_predictions.R

# __ Prepare data ______________________________________________________________
make_design <- function(data, teams, ref = "PAR") {
  
  n <- nrow(data)
  X <- matrix(0, nrow = n, ncol = length(teams))
  colnames(X) <- teams
  
  data = data |>
    mutate(
      team_home = relevel(factor(team_home, levels = teams), ref = ref),
      team_away = relevel(factor(team_away, levels = teams), ref = ref)
    )
  
  for (i in 1:n) {
    X[i, as.character(data$team_home[i])] <- 1
    X[i, as.character(data$team_away[i])] <- -1
  }
  
  X <- X[, colnames(X) != ref]
  
  cbind(home = 1, as.data.frame(X))
}

games26 <- games |> filter(season == "26")
teams26 <- unique(games26 |> pull(team_home))
X_train26 <- cbind(
  make_design(data = games26, teams = teams26),
  score_diff = games26$score_diff
)

games26_history <- bind_rows(mon2par, games26)
X_train26_history <- cbind(
  make_design(data = games26_history, teams = teams26),
  score_diff = games26_history$score_diff
)

games2526 <- games |> filter(season >= "25")
teams2526 <- unique(games2526 |> pull(team_home))
X_train2526 <- cbind(
  make_design(data = games2526, teams = teams2526),
  score_diff = games2526$score_diff
)
  
# __ Train models ______________________________________________________________
fit26 <- lm(
  score_diff ~ . - 1,
  data = X_train26
)

fit26_history <- lm(
  score_diff ~ . - 1,
  data = X_train26_history
)

fit2526 <- lm(
  score_diff ~ . - 1,
  data = X_train2526
)

# __ Simulation BO5 ____________________________________________________________
make_newgame <- function(team_home, team_away, teams, ref = "PAR") {
  row <- setNames(rep(0, length(teams) - 1), teams[teams != ref])
  if (team_home != ref) row[team_home] <- 1
  if (team_away != ref) row[team_away] <- -1
  data.frame(home = 1, t(row))
}

simulate_bo5 <- function(
    n = N, seed = Seed,
    train, model,
    known_results = NULL,
    team_A = "PAR", team_B = "MON", teams,
    home = c(1, 2, 5)
) {
  
  set.seed(seed)
  
  all_states     <- vector("list", n)
  all_wins       <- vector("list", n)
  all_pred_scores <- matrix(NA_real_, nrow = n, ncol = 5)
  all_sim_scores  <- matrix(NA_real_, nrow = n, ncol = 5)
  all_exact_hit   <- matrix(NA_real_, nrow = n, ncol = 5)
  
  n_known <- length(known_results)
  
  for (i in 1:n) {
    
    train_rolling  <- train
    model_rolling  <- model
    wins  <- c(0, 0)
    states <- character(5)
    
    for (g in 1:5) {
      
      if (max(wins) == 3) next
      
      home_team <- ifelse(g %in% home, team_A, team_B)
      away_team <- ifelse(g %in% home, team_B, team_A)
      
      newgame <- make_newgame(home_team, away_team, teams)
      
      sigma <- summary(model_rolling)$sigma
      pred_score_diff <- as.numeric(predict(model_rolling, newdata = newgame))
      
      if (g <= n_known) {
        sim_score_diff <- known_results[g]
      } else {
        sim_score_diff <- rnorm(1, mean = pred_score_diff, sd = sigma)
      }
      
      win_A <- (g %in% home) == (sim_score_diff > 0)
      wins[ifelse(win_A, 1, 2)] <- wins[ifelse(win_A, 1, 2)] + 1
      states[g] <- paste0("G", g, ":", wins[1], "-", wins[2])
      
      train_rolling <- rbind(
        train_rolling,
        cbind(newgame, score_diff = sim_score_diff)
      )
      model_rolling <- lm(
        score_diff ~ . - 1,
        data = train_rolling
      )
      
      all_exact_hit[i, g]  <- (round(sim_score_diff) == round(pred_score_diff))
      all_pred_scores[i, g] <- pred_score_diff
      all_sim_scores[i, g]  <- sim_score_diff
    }
    
    all_states[[i]] <- states
    all_wins[[i]]   <- wins
  }
  
  return(list(
    wins        = all_wins,
    states      = all_states,
    pred_scores = all_pred_scores,
    sim_scores  = all_sim_scores,
    exact_hit   = all_exact_hit
  ))
}

summarise_bo5 <- function(
    sim,
    known_results = NULL,
    team_A = "PAR", team_B = "MON",
    home = c(1, 2, 5)
) {
  
  n_known <- length(known_results)
  
  all_wins       <- sim$wins
  all_states     <- sim$states
  all_pred_scores <- sim$pred_scores
  all_sim_scores  <- sim$sim_scores
  all_exact_hit   <- sim$exact_hit
  
  summary_games <- tibble(
    Game = 1:5,
    Home = ifelse(1:5 %in% home, team_A, team_B),
    P_home_win = round(colSums(
      all_sim_scores > 0, na.rm = TRUE) / 
        colSums(!is.na(all_sim_scores)),
      3
    ),
    P_away_win = 1 - P_home_win,
    Actual_score_diff = c(known_results, rep(NA, 5 - length(known_results))),
    Pred_score_diff = round(colMeans(all_pred_scores, na.rm = TRUE)),
    P_pred_score = round(colMeans(all_exact_hit, na.rm = TRUE), 3),
    P_played = round(colMeans(!is.na(all_sim_scores)), 3)
  )
  if (n_known == 0) {
    summary_games <- summary_games |> select(-Actual_score_diff)
  }
  
  summary_serie <- tibble(
    Team_A = team_A,
    Team_B = team_B,
    P_A_wins = round(mean(sapply(all_wins, function(g) g[1] == 3)), 3),
    P_B_wins = round(mean(sapply(all_wins, function(g) g[2] == 3)), 3),
    P_A_wins_after3 = round(mean(sapply(
      all_wins, function(g) g[1] == 3 & sum(g) == 3
    )), 3),
    P_B_wins_after3 = round(mean(sapply(
      all_wins, function(g) g[2] == 3 & sum(g) == 3
    )), 3),
    P_5games = round(mean(sapply(all_wins, function(g) sum(g) == 5)), 3)
  )
  
  return(list(
    summary_serie = summary_serie,
    summary_games = summary_games
  ))
}

# __ Probability tree __________________________________________________________
plot_tree <- function(
    all_states, all_wins,
    team_A = "PAR", team_B = "MON",
    title, caption, from_game) {
  
  n_sim <- length(all_states)
  
  transitions <- do.call(rbind, lapply(all_states, function(s) {
    s <- s[s != ""]
    if (length(s) < 2) return(NULL)
    do.call(rbind, lapply(1:(length(s)-1), function(i) {
      c(from = s[i], to = s[i+1])
    }))
  }))
  transitions <- as.data.frame(transitions)
  
  first_states <- do.call(rbind, lapply(all_states, function(s) {
    s <- s[s != ""]
    c(from = "0-0", to = s[1])
  }))
  transitions <- rbind(as.data.frame(first_states), transitions)
  
  trans_prob <- transitions |>
    group_by(from, to) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(from) |>
    mutate(p_transition = round(n / sum(n), 3)) |>
    ungroup()
  
  get_winner <- function(from, to) {
    parse_state <- function(s) {
      if (s == "0-0") return(list(mon = 0, par = 0))
      mon <- as.integer(sub("G\\d+:(\\d+)-.*", "\\1", s))
      par <- as.integer(sub("G\\d+:\\d+-(\\d+)", "\\1", s))
      list(mon = mon, par = par)
    }
    f <- parse_state(from); t <- parse_state(to)
    if (t$mon > f$mon) team_A else team_B
  }
  
  trans_prob <- trans_prob |>
    rowwise() |>
    mutate(winner = get_winner(from, to),
           edge_label = paste0(winner, " ", p_transition)) |>
    ungroup()
  
  states_vec <- unlist(lapply(all_states, function(s) s[s != ""]))
  p_states <- as.data.frame(table(states_vec) / n_sim)
  names(p_states) <- c("node", "p_marginal")
  p_states <- rbind(
    data.frame(node = "0-0", p_marginal = 1),
    p_states
  )
  
  keep_nodes <- p_states$node[
    p_states$node == "0-0" |
      grepl("^G", p_states$node)
  ]
    
  p_states  <- p_states  |> filter(node %in% keep_nodes)
  trans_prob <- trans_prob |> filter(from %in% keep_nodes, to %in% keep_nodes)
  
  g <- graph_from_data_frame(
    d = trans_prob |> select(from, to, p_transition, winner, edge_label),
    vertices = p_states |> rename(name = node),
    directed = TRUE
  )
  
  parse_state <- function(s) {
    if (s == "0-0") return(list(g = 0, mon = 0, par = 0))
    g   <- as.integer(sub("G(\\d+):.*", "\\1", s))
    mon <- as.integer(sub("G\\d+:(\\d+)-.*", "\\1", s))
    par <- as.integer(sub("G\\d+:\\d+-(\\d+)", "\\1", s))
    list(g = g, mon = mon, par = par)
  }
  
  coords <- do.call(rbind, lapply(V(g)$name, function(s) {
    p <- parse_state(s)
    c(x = p$g, y = p$mon - p$par)
  }))
  
  edge_df <- trans_prob |>
    mutate(
      from_coord = lapply(from, function(s) { p <- parse_state(s); c(p$g, p$mon - p$par) }),
      to_coord   = lapply(to,   function(s) { p <- parse_state(s); c(p$g, p$mon - p$par) }),
      x = (sapply(from_coord, `[`, 1) + sapply(to_coord, `[`, 1)) / 2,
      y = (sapply(from_coord, `[`, 2) + sapply(to_coord, `[`, 2)) / 2
    )
  
  ggraph(g, layout = coords) +
    geom_edge_link(
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      end_cap = circle(8, "mm"),
      color = "black"
    ) +
    geom_label(                          # labels manuels sur les midpoints
      data = edge_df,
      aes(x = x, y = y, label = edge_label, color = winner),
      size = 3,
      fill = "white",
      linewidth = 0,
      inherit.aes = FALSE
    ) +
    scale_color_manual(
      values = setNames(c("#E2001A", "#007A33"), c(team_A, team_B)),
      name   = "Vainqueur"
    ) +
    geom_node_label(
      aes(label = paste0(name, "\n", round(p_marginal, 3))),
      size = 2.5,
      fontface = "bold"
    ) +
    theme_graph(base_family = "Arial") +
    theme(plot.caption = element_text(size = 9)) +
    labs(
      title = title,
      caption = caption
    )
}

# __ Predictions with 26 _______________________________________________________
# sim26 <- simulate_bo5(
#   train = X_train26, model = fit26, teams = teams26
# )
# final26 <- summarise_bo5(sim26)
# tree26 <- plot_tree(
#   all_states = sim26$states,
#   all_wins = sim26$wins,
#   title = "Probability tree of the possible outcomes",
#   caption = paste0(
#     "Stage of the series: before the first game",
#     "\n",
#     "Model: trained on seasons 26"
#   ),
#   from_game = 0
# )

# __ Predictions with 26 & history _____________________________________________
# sim26_history <- simulate_bo5(
#   train = X_train26_history, model = fit26_history, teams = teams26
# )
# final26_history <- summarise_bo5(sim26_history)
# tree26_history <- plot_tree(
#   all_states = sim26_history$states,
#   all_wins = sim26_history$wins,
#   title = "Probability tree of the possible outcomes",
#   caption = paste0(
#     "Stage of the series: before the first game",
#     "\n",
#     "Model: trained on season 26 with games between MON and PAR since 24"
#   ),
#   from_game = 0
# )

# __ Predictions with 25 & 26 __________________________________________________
# sim2526 <- simulate_bo5(
#   train = X_train2526, model = fit2526, teams = teams2526
# )
# final2526 <- summarise_bo5(sim2526)
# tree2526 <- plot_tree(
#   all_states = sim2526$states,
#   all_wins = sim2526$wins,
#   title = "Probability tree of the possible outcomes",
#   caption = paste0(
#     "Stage of the series: before the first game",
#     "\n",
#     "Model: trained on seasons 25 and 26"
#   ),
#   from_game = 0
# )

# sim2526_after1 <- simulate_bo5(
#   train = X_train2526, model = fit2526, teams = teams2526,
#   known_results = c(+4)
# )
# final2526_after1 <- summarise_bo5(sim2526_after1, known_results = c(+4))
# tree2526_after1 <- plot_tree(
#   all_states = sim2526_after1$states,
#   all_wins = sim2526_after1$wins,
#   title = "Probability tree of the possible outcomes",
#   caption = paste0(
#     "Stage of the series: after game 1",
#     "\n",
#     "Model: trained on seasons 25 and 26"
#   ),
#   from_game = 1
# )

# sim2526_after2 <- simulate_bo5(
#   train = X_train2526, model = fit2526, teams = teams2526,
#   known_results = c(+4, -12)
# )
# final2526_after2 <- summarise_bo5(sim2526_after2, known_results = c(+4, -12))
# tree2526_after2 <- plot_tree(
#   all_states = sim2526_after2$states,
#   all_wins = sim2526_after2$wins,
#   title = "Probability tree of the possible outcomes",
#   caption = paste0(
#     "Stage of the series: after game 2",
#     "\n",
#     "Model: trained on seasons 25 and 26"
#   ),
#   from_game = 2
# )

sim2526_after3 <- simulate_bo5(
  train = X_train2526, model = fit2526, teams = teams2526,
  known_results = c(+4, -12, -11)
)
final2526_after3 <- summarise_bo5(
  sim2526_after3,
  known_results = c(+4, -12, -11)
)
tree2526_after3 <- plot_tree(
  all_states = sim2526_after3$states,
  all_wins = sim2526_after3$wins,
  title = "Probability tree of the possible outcomes",
  caption = paste0(
    "Stage of the series: after game 3",
    "\n",
    "Model: trained on seasons 25 and 26"
  ),
  from_game = 3
)

sim2526_after4 <- simulate_bo5(
  train = X_train2526, model = fit2526, teams = teams2526,
  known_results = c(+4, -12, -11, +12)
)
final2526_after4 <- summarise_bo5(
  sim2526_after4,
  known_results = c(+4, -12, -11, +12)
)
tree2526_after4 <- plot_tree(
  all_states = sim2526_after4$states,
  all_wins = sim2526_after4$wins,
  title = "Probability tree of the possible outcomes",
  caption = paste0(
    "Stage of the series: after game 4",
    "\n",
    "Model: trained on seasons 25 and 26"
  ),
  from_game = 4
)

# __ Show outputs ______________________________________________________________
mean_points <- function (train) {
  mean_points <- left_join(
    train |>
      group_by(team = team_home) |>
      summarise(mean_diff_home = mean(score_diff)),
    train |>
      group_by(team = team_away) |>
      summarise(mean_diff_away = mean(-score_diff)),
    by = "team"
  ) |>
    mutate(mean_diff = (mean_diff_home + mean_diff_away) / 2) |>
    arrange(desc(mean_diff))
  return(head(mean_points, 4))
}
mean_points(games26)
mean_points(games2526)

series_summary_bg0 <- as.data.frame(bind_rows(
  final26$summary_serie,
  final2526$summary_serie,
  final26_history$summary_serie
)) |>
  mutate(Train_data = c("25-26", "24-25 & 25-26", "25-26 & history")) |>
  select(
    Train_data, P_A_wins, P_B_wins, P_A_wins_after3, P_B_wins_after3,P_5games
  )

show_outputs <- function () {
  
  cat("=== Series summaries before game 1 ===\n\n")
  print(series_summary_bg0)
  
  cat("\n=== Games summaries before game 1 ===\n\n")
  
  cat("--- Train data: 25-26 season\n")
  print(as.data.frame(final26$summary_games))
  
  cat("\n--- Train data: 24-25 & 25-26 seasons\n")
  print(as.data.frame(final2526$summary_games))
  
  cat("\n--- Train data: 26 season & history ---\n")
  print(as.data.frame(final26_history$summary_games))
  
}
# show_outputs()

# __ Export outputs ____________________________________________________________
export_outputs <- function () {
  
  write_csv(
    final2526_after4$summary_games,
    file = "outputs/after_g4/final2526_after4_games.csv"
  )
  
  # write_csv(
  #   final2526_after3$summary_serie,
  #   file = "outputs/after_g3/final2526_after3_series.csv"
  # )
  
  # print(
  #   xtable(
  #     Final_after1_with25$summary_games,
  #     digits = c(0, 0, 0, 3, 3, 0, 0, 3, 3)
  #   ),
  #   include.rownames = FALSE,
  #   file = "outputs/Final_predictions/Final_after1_with25_games.tex"
  # )
  
  ggsave(
    filename = "outputs/after_g4/final2526_after4_tree.png",
    plot = tree2526_after4,
    width = 10,
    height = 6
  )
}
# export_outputs()

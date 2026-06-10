# 11_data_load.R

load_data <- function (phase, season) {
  
  path <-  file.path(
    "raw_data", phase,
    paste0("lnb", "_", season, "_", phase, ".csv")
  )
  read_csv(path)
}

load_games <- function () {
  
  reg <- list()
  po <- list()
  
  for (i in 21:26) {
    reg[[paste0(i)]] <- load_data("reg", paste0(i-1, "-", i))
    po[[paste0(i)]] <- load_data("po", paste0(i-1, "-", i))
  }
  return(list(
    regular = reg,
    playoffs = po
  ))
}
raw_games <- load_games()

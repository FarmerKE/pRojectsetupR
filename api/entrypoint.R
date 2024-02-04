library(plumber)

r <- plumb("api.R")$run(port = 8080, host = "0.0.0.0")$
  setDebug(debug = TRUE)
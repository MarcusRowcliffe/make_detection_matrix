source("https://raw.githubusercontent.com/MarcusRowcliffe/make_detection_matrix/refs/heads/main/make_detection_matrix.R")
library(lubridate)

deployments <- data.frame(deploymentID = c(1,2,3,4),
                  locationID = c(1,1,2,3),
                  start = ymd_hms(c("2000/01/01 00:00:00",
                                    "2000/02/20 00:00:00",
                                    "2000/01/01 00:00:00",
                                    "2000/01/01 00:00:00")),
                  end = ymd_hms(c("2000/02/01 00:00:00",
                                  "2000/03/01 00:00:00",
                                  "2000/03/01 00:00:00",
                                  "2000/03/01 00:00:00")))
observations <- data.frame(deploymentID = c(1,1,2,3),
                  scientificName = c("a", "b", "b", "c"),
                  timestamp = ymd_hms(c("2000/01/02 12:12:12",
                                        "2000/01/15 21:21:21",
                                        "2000/02/22 05:05:05",
                                        "2000/01/15 14:14:14")))
pk <- list(data=list(observations=observations, deployments=deployments))
res <- make_detection_matrix(pk, 
                            species = "c",
                            interval = 7, 
                            start_hour = 0)
res$matrix
make_emat(deployments, start_hour = 10, interval = 10)
res$matrix
res$effort
res$cuts

source("https://raw.githubusercontent.com/MarcusRowcliffe/make_detection_matrix/refs/heads/main/make_detection_matrix.R")
library(lubridate)

deployments <- data.frame(deploymentID = c("d1", "d2", "d3", "d4"),
                  locationID = c("l1", "l1", "l3", "l2"),
                  locationName = c("c", "c", "a", "b"),
                  start = ymd_hms(c("2000/01/01 00:00:00",
                                    "2000/02/20 00:00:00",
                                    "2000/01/01 00:00:00",
                                    "2000/01/01 00:00:00")),
                  end = ymd_hms(c("2000/02/01 00:00:00",
                                  "2000/03/01 00:00:00",
                                  "2000/02/01 00:00:00",
                                  "2000/03/01 00:00:00")))
observations <- data.frame(deploymentID = c("d1", "d1", "d2", "d3", "d4", "d4", "d4"),
                  scientificName = c("sp1", "sp2", "sp2", "sp3", "sp1", "sp2", "sp2"),
                  timestamp = ymd_hms(c("2000/01/02 12:12:12",
                                        "2000/01/15 21:21:21",
                                        "2000/02/22 05:05:05",
                                        "2000/01/15 14:14:14",
                                        "2000/02/28 14:14:14",
                                        "2000/01/17 14:14:14",
                                        "2000/01/18 14:14:14")))
pk <- list(data=list(observations=observations, deployments=deployments))

make_emat(deployments)
make_dmat(deployments, observations)
make_dmat(deployments, subset(observations, scientificName=="sp1"))

make_detection_matrix(pk, species = "sp2")
res <- make_detection_matrix(pk, species = c("sp1", "sp2", "sp3"))
res$matrix$sp1
res$matrix$sp2
res$matrix$sp3
res$effort
res$cuts

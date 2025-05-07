
dep <- data.frame(deploymentID = c(1,2,3,4),
                  locationID = c(1,1,2,3),
                  start = c("2000/01/01 00:00:00",
                            "2000/02/20 00:00:00",
                            "2000/01/01 00:00:00",
                            "2000/01/01 00:00:00"),
                  end = c("2000/02/01 00:00:00",
                          "2000/03/01 00:00:00",
                          "2000/03/01 00:00:00",
                          "2000/03/01 00:00:00"))
obs <- data.frame(deploymentID = c(1,1,2,3),
                  species = c("a", "b", "b", "c"),
                  timestamp = c("2000/01/02 12:12:12",
                                "2000/01/15 21:21:21",
                                "2000/02/22 05:05:05",
                                "2000/01/15 14:14:14"))

res <- get_detection_matrix(obs, dep, 
                            subset = species=="c",
                            interval = 7, 
                            start_hour = 12)
res$matrix

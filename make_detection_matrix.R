library(dplyr)
library(lubridate)

#' Function to make detection matrices for occupancy analysis using camtrapDP data
#' 
#' INPUT
#' obsdat: dataframe of observation data with (at least) columns:
#'   timestamp: POSIX or character date-times when observations occurred
#'   deploymentID: unique camera trap location identifier matchable with
#'       the same key in depdat
#'   Must also contain any columns named in the subset argument
#' depdat: dataframe of deployment data with one row per deployment and
#'   (at least) columns:
#'    deploymentdID: unique deployment identifier matchable with the same 
#'      key in obsdat
#'    locationID: location identifiers (can be repeated)
#'    start, end: POSIX or character deployment start/end data-times 
#'      (ymd_hms format if character)
#' subset: a logical expression defining which subset of obsdat to use, 
#'   with an obsdat column name  on the left e.g. scientificName == "Dama dama"
#' interval: length of occasion interval in days
#' start_hour: a number from 0 to 24 giving the time of day at which to start
#'  occasions
#' trim: if TRUE, detection records for all deployment occasions with less 
#'  than full interval effort are missing, otherwise only those with zero effort.
#'
#' OUTPUT
#' A list with elements:
#'  matrix: the detection matrix
#'  effort: a matrix of effort (days) for each deployment occasion
#'  cuts: a vector of the time cuts defining occasions
get_detection_matrix <- function(obsdat, depdat, 
                                 subset = TRUE,
                                 interval=7, 
                                 start_hour=0,
                                 trim=FALSE){
  fieldsOK <- all(c("deploymentID", "timestamp") %in% names(obsdat),
                  c("deploymentID", "locationID", "start", "end") %in% names(depdat))
  if(!fieldsOK) 
    stop("Can't find the necessary data: obsdat must contain columns named timestamp and locationID; depdat must contain columns named start, end and locationID")
  depdat <- depdat %>%
    mutate(start = if(grepl("POSIX", class(dep$start)[1])) start else lubridate::ymd_hms(start),
           end = if(grepl("POSIX", class(dep$end)[1])) end else lubridate::ymd_hms(end),
           deploymentID = as.character(deploymentID),
           locationID = as.character(locationID))
  
  obsdat <- obsdat %>%
    dplyr::filter(!!enquo(subset)) %>%
    mutate(timestamp = if(grepl("POSIX", class(dep$timestamp)[1])) timestamp else lubridate::ymd_hms(timestamp),
           deploymentID = as.character(deploymentID)) %>%
    dplyr::left_join(dplyr::select(depdat, deploymentID, locationID), 
                     join_by(deploymentID))
  missingDeps <- unique(obsdat$deploymentID[!obsdat$deploymentID %in% depdat$deploymentID])
  if(length(missingDeps)>0)
    stop(paste("These deploymentID values in obsdat are missing from depdat:",
               paste(missingDeps, collapse = " ")))
  
  checkdat <- dplyr::left_join(obsdat, 
                               dplyr::select(depdat, deploymentID, start, end),
                               by="deploymentID")
  bad <- with(checkdat, timestamp<start | timestamp>end)
  if(any(bad)){
    message("Error: some observations occur outside their deployment time: 
            returning problematic observations")
    return(checkdat[bad, ])
  } else{
    mn <- min(depdat$start) %>%
      as.POSIXlt()
    mx <- max(depdat$end)
    time <- mn$hour + mn$min/60 + mn$sec/3600
    mn <- if(time >= start_hour) 
      mn - 3600 * (time + start_hour) else
        mn - 3600 * (time + 24 - start_hour)
    cuts <- seq(mn, mx+interval*86400, interval*86400)
    ndep <- nrow(depdat)
    nocc <- length(cuts) - 1
    nobs <- nrow(obsdat)
    ij <- expand.grid(dep=1:ndep, occ=1:nocc)
    effort <- with(depdat, 
                   cbind(difftime(cuts[ij$occ], start[ij$dep], units="day"),
                         difftime(cuts[ij$occ+1], start[ij$dep], units="day"),
                         difftime(end[ij$dep], cuts[ij$occ], units="day"),
                         difftime(end[ij$dep], cuts[ij$occ+1], units="day"))
    ) %>%
      apply(1, function(x){
        signsum <- sum(sign(x))
        ifelse(signsum==0, 0, 
               ifelse(signsum==4, interval, 
                      ifelse(x[1]<=0, x[2], x[3])))
      }) %>%
      matrix(ncol=nocc) %>%
      apply(2, tapply, depdat$locationID, sum)
    
    ijk <- expand.grid(dep=1:ndep, occ=1:nocc, obs=1:nobs)
    loc <- depdat$locationID[ijk$dep]
    isin <- obsdat$timestamp[ijk$obs] >= cuts[ijk$occ] & 
      obsdat$timestamp[ijk$obs] <= cuts[ijk$occ+1] & 
      obsdat$locationID[ijk$obs] == loc
    mat <- isin %>%
      tapply(list(loc, ijk$occ), any) %>%
      as.numeric() %>%
      matrix(ncol=nocc)
    if(trim) mat[effort<interval] <- NA else
      mat[effort==0] <- NA
    return(list(matrix=mat, effort=effort, cuts=cuts))
  }
}

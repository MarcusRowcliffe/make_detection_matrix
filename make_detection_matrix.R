library(dplyr)

#' Make a sequence of occasion time cuts
#' 
#' INPUT
#' start / end: matching vectors of POSIX deployment start / end date-times
#' interval: occasion intervals in days
#' start_hour: numeric hour of day at which to start the sequence
#' 
#' OUTPUT
#' A regular sequence of POSIX data-times spanning the range of start / end
#' 
make_cutSeq <- function(start, end, interval=7, start_hour=0){
  mn <- min(start) %>%
    as.POSIXlt()
  mx <- max(end)
  time <- mn$hour + mn$min/60 + mn$sec/3600
  mn <- if(time >= start_hour) 
    mn - 3600 * (time + start_hour) else
      mn - 3600 * (time + 24 - start_hour)
  seq(mn, mx+interval*86400, interval*86400)
}

#' Make an effort matrix
#' 
#' INPUT
#' deployments: a dataframe of deployment data with columns
#'    locationID: location identifiers, typically globally unique
#'    locationName: alternative location identifiers, typically shorter, locally unique
#'    start / end: POSIX date-times at which deployments start and end
#' cuts: a sequence of POSIX date-times defining detection occasions
#'    Generated internally using make_cutSeq if NULL
#' interval / start_hour: passed to make_cutSeq if cuts is NULL
#' 
#' OUTPUT
#' A list with elements:
#'    effort: a sites x occasions numeric matrix of effort in days
#'    cuts: a POSIX vector of the occasion date-time cutpoints
#'    
make_emat <- function(deployments, cuts = NULL,
                      interval = 7, start_hour = 0){
  if(is.null(cuts)) cuts <- make_cutSeq(deployments$start, 
                                        deployments$end, 
                                        interval = interval,
                                        start_hour = start_hour)
  intervals <- as.numeric(diff(cuts))
  if(any(intervals <= 0))
    stop("cuts are not continually increasing")
  if(min(deployments$start) < min(cuts) | max(deployments$end) > max(cuts))
    stop("cuts do not span start/end times")
  
  nocc <- length(intervals)
  ndep <- nrow(deployments)
  emat <- data.frame(loc = rep(deployments$locationName, nocc),
                     occ = rep(1:nocc, each = ndep),
                     s = rep(deployments$start, nocc),
                     e = rep(deployments$end, nocc),
                     c1 = rep(head(cuts, -1), each=ndep),
                     c2 = rep(tail(cuts, -1), each=ndep),
                     i = rep(intervals, each=ndep),
                     z = 0) %>%
    dplyr::mutate(e_c1 = as.numeric(difftime(e, c1, units="day")),
                  c2_s = as.numeric(difftime(c2, s, units="day")),
                  e_s = as.numeric(difftime(e, s, units="day")),
                  sums = (c1<s) + (c2<=s) + (c1<e) + (c2<=e),
                  sel = case_when(sums==1 ~ "e_c1",
                                  sums==2 & c2>e ~ "e_s",
                                  sums==2 & c2<=e ~ "i",
                                  sums==3 ~ "c2_s",
                                  .default = "z")) %>%
    rowwise() %>%
    mutate(eff = get(sel)) %>%
    select(loc, occ, eff) %>%
    dplyr::group_by(loc, occ) %>%
    dplyr::summarise(eff = sum(eff), .groups = "drop") %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = occ, values_from = eff) %>%
    tibble::column_to_rownames("loc") %>%
    as.matrix()
  list(effort = emat, cuts = cuts)
}

#' Make a detection matrix from dataframes
#' 
#' INPUT
#' deployments: dataframe of deployment data with columns:
#'    deploymentID / locationID: deployment and location identifiers
#'    start / end: POSX deployment start / end date-times (required if effort is NULL)
#'    (locationName): location name, required if effort = NULL
#' observations: dataframe of observation data with columns:
#'    deploymentID: deployment identifiers
#'    timestamp: POSIX date-times of observation occurence
#' effort: sites x occasions matrix of effort values
#'    Generated internally using make_emat if NULL
#' trim: logical, whether incomplete cells (effort<interval) should be set to NA
#' interval / start_hour: passed to make_emat then make_cutSeq if effort is NULL
#' 
#' OUTPUT
#' A sites x occasions detection / non-detection matrix
#' 
make_dmat <- function(deployments, observations, 
                      effort = NULL,
                      cuts = NULL,
                      trim=FALSE, 
                      interval=7, 
                      start_hour=0){
  if(is.null(effort))
    effort <- make_emat(deployments, cuts=cuts, interval=interval, start_hour=start_hour)
  if("locationID" %in% names(observations))
    observations <- dplyr::select(observations, -locationID)
  observations <- deployments %>%
    dplyr::select(deploymentID, locationID) %>%
    dplyr::right_join(observations, by=join_by(deploymentID))
  locs <- unique(deployments$locationID)
  nobs <- nrow(observations)
  nloc <- length(locs)
  nocc <- length(effort$cuts) - 1
  ijk <- expand.grid(loc=1:nloc, occ=1:nocc, obs=1:nobs)
  deploc <- locs[ijk$loc]
  obsloc <- observations$locationID[ijk$obs]
  ts <- observations$timestamp[ijk$obs]
  cut1 <- effort$cuts[ijk$occ]
  cut2 <- effort$cuts[ijk$occ+1]
  isin <- ts >= cut1 & ts < cut2 & obsloc == deploc %>%
    array(c(nloc, nocc, nobs))
  mat <- apply(isin, 1:2, any) %>%
    {+.}
  emult <- if(trim) ifelse(effort$effort<interval, NA, 1) else 
    ifelse(effort$effort==0, NA, 1)
  mat * emult
}

#' Make a detection matrix from a camtrapDP datapackage
#' 
#' INPUT
#' pkg: a camtrapDP-like list of camera trap data containing
#'    package$data$deployments and package$data$observations, dataframes with
#'    required columns as for make_dmat, plus scientificName required in observations
#' species: a character vector giving one or more species to create matrices for
#' trim / interval / start_hour: arguments passed to make_dmat
#' 
#' OUTPUT
#' A list with elements:
#'    effort: a sites x occasions effort matrix
#'    cuts: POSIX occasion date-time cutpoints
#'    matrix: named a list of detection-nondetection matrices, one for each 
#'      species named in the species argument
#'      
make_detection_matrix <- function(pkg,
                                  species,
                                  trim=FALSE,
                                  interval=7,
                                  start_hour=0){
  obsReq <- c("deploymentID", "scientificName", "timestamp")
  depReq <- c("deploymentID", "locationID", "start", "end")
  fieldsOK <- all(obsReq %in% names(pkg$data$observations),
                  depReq %in% names(pkg$data$deployments))
  if(!fieldsOK) 
    stop("Can't find the necessary data: 
         obsdat must contain columns named timestamp and locationID; 
         depdat must contain columns named start, end and locationID")
  
  if(!all(species %in% pkg$data$observations$scientificName))
    stop("Can't find any observations for that/those species")
  
  depdat <- pkg$data$deployments %>%
    dplyr::mutate(deploymentID = as.character(deploymentID),
                  locationID = as.character(locationID))
  obsdat <- pkg$data$observations %>%
    dplyr::mutate(deploymentID = as.character(deploymentID))
  
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
    effort <- make_emat(depdat)
    dmats <- lapply(species, function(sp) 
      make_dmat(depdat, 
                subset(obsdat, scientificName==sp),
                effort,
                trim = trim, 
                interval = interval, 
                start_hour=start_hour))
    names(dmats) <- species
    return(c(effort, matrix=list(dmats)))
  }
}
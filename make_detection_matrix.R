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
make_cuts <- function(start, end, interval=7, start_hour=0){
  if(!all(inherits(deployments$deploymentStart, "POSIXt"),
          inherits(deployments$deploymentEnd, "POSIXt")))
    stop("start and end must be class POSIXt")
  
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
#'    locationName: location identifier
#'    deploymentStart / deploymentEnd: POSIX date-times at which deployments start and end
#' cuts: a sequence of POSIX date-times defining detection occasions
#'    Generated internally using make_cuts if NULL
#' interval / start_hour: passed to make_cuts if cuts is NULL
#' 
#' OUTPUT
#' A list with elements:
#'    effort: a sites x occasions numeric matrix of effort in days
#'    cuts: a POSIX vector of the occasion date-time cutpoints

make_emat <- function(deployments, 
                      cuts = NULL,
                      interval = 7, 
                      start_hour = 0){
  
  # Check required data columns present
  req <- c("locationName", "deploymentStart", "deploymentEnd")
  if(!all(req %in% names(deployments)))
    stop(paste(c("deployments must contain columns:", depReq), collapse=" "))
  
  # Make cuts if necessary, check they make sense
  if(is.null(cuts)) cuts <- make_cuts(deployments$deploymentStart, 
                                      deployments$deploymentEnd, 
                                      interval = interval,
                                      start_hour = start_hour)
  intervals <- as.numeric(diff(cuts))
  if(any(intervals <= 0))
    stop("cuts are not continually increasing")
  if(min(deployments$deploymentStart) < min(cuts) | max(deployments$deploymentEnd) > max(cuts))
    stop("cuts do not span start/end times")
  
  # Make effort matrix
  nocc <- length(intervals)
  ndep <- nrow(deployments)
  emat <- data.frame(loc = rep(deployments$locationName, nocc),
                     occ = rep(1:nocc, each = ndep),
                     s = rep(deployments$deploymentStart, nocc),
                     e = rep(deployments$deploymentEnd, nocc),
                     c1 = rep(head(cuts, -1), each=ndep),
                     c2 = rep(tail(cuts, -1), each=ndep),
                     i = rep(intervals, each=ndep),
                     z = 0) %>%
    dplyr::mutate(e_c1 = as.numeric(difftime(e, c1, units="day")),
                  c2_s = as.numeric(difftime(c2, s, units="day")),
                  e_s = as.numeric(difftime(e, s, units="day")),
                  sums = (c1<s) + (c2<=s) + (c1<e) + (c2<=e),
                  sel = dplyr::case_when(sums==1 ~ "e_c1",
                                         sums==2 & c2>e ~ "e_s",
                                         sums==2 & c2<=e ~ "i",
                                         sums==3 ~ "c2_s",
                                         .default = "z")) %>%
    rowwise() %>%
    dplyr::mutate(eff = get(sel)) %>%
    dplyr::select(loc, occ, eff) %>%
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
#' deployments: dataframe of deployment data with columns as for make_emat plus:
#'    deploymentID: deployment identifier matched with observations$deploymentID
#' observations: dataframe of observation data with columns:
#'    deploymentID: deployment identifiers, which must all be present in deployments
#'    eventStart: POSIX date-times of observation occurence
#' cuts / interval / start_hour: passed to make_emat
#' type: presence returns a binary presence/absence matrix; count returns an
#'    observation count matrix
#' trim: logical, whether incomplete cells (effort<interval) should be set to NA
#' fail_outliers: what to do if any observations lie outside their deployment;
#'    if TRUE, outlying observations are returned; 
#'    if FALSE, a matrix is returned with any outliers discarded
#' 
#' OUTPUT
#' A sites x occasions detection or count matrix

make_dmat <- function(deployments, observations, 
                      cuts = NULL,
                      interval = 7, 
                      start_hour = 0,
                      type = c("presence", "count"),
                      trim = FALSE, 
                      fail_outliers=FALSE){
  
  # Check necessary data fields are present
  if(!all("deploymentID" %in% names(deployments),
          c("deploymentID", "eventStart") %in% names(observations)))
    stop("deployments and observations must both contain a deploymentID column;
         observations must also contain an eventStart column")
  
  # Check all observation deploymentIDs are present in deployments
  missingDeps <- observations %>%
    dplyr::filter(!deploymentID %in% deployments$deploymentID) %>%
    dplyr::pull(deploymentID) %>%
    unique()
  if(length(missingDeps)>0)
    stop(paste(c("These observation deploymentIDs are missing from deployments:",
                 missingDeps), 
               collapse = "\n"))
  
  # Check observations all occur within their deployments (if fail_outliers == TRUE)
  checkdat <- dplyr::left_join(observations, deployments, by="deploymentID")
  badObs <- with(checkdat, eventStart<deploymentStart | eventStart>deploymentEnd)
  if(any(badObs) & fail_outliers){
    message("Error: some observations occur outside their deployment time - they have been returned")
    return(checkdat[bad, ])
  } else{
    # Make matrices  
    type <- match.arg(type)
    observations <- observations[!badObs, ]
    effort <- make_emat(deployments, 
                        cuts=cuts, 
                        interval=interval, 
                        start_hour=start_hour)
    if("locationName" %in% names(observations))
      observations <- dplyr::select(observations, -locationName)
    observations <- deployments %>%
      dplyr::select(deploymentID, locationName) %>%
      dplyr::right_join(observations, by=join_by(deploymentID))
    locs <- sort(unique(deployments$locationName))
    nobs <- nrow(observations)
    nloc <- length(locs)
    nocc <- length(effort$cuts) - 1
    ijk <- expand.grid(loc=1:nloc, occ=1:nocc, obs=1:nobs)
    deploc <- locs[ijk$loc]
    obsloc <- observations$locationName[ijk$obs]
    ts <- observations$eventStart[ijk$obs]
    cut1 <- effort$cuts[ijk$occ]
    cut2 <- effort$cuts[ijk$occ+1]
    isin <- ts >= cut1 & ts < cut2 & obsloc == deploc %>%
      array(c(nloc, nocc, nobs))
    mat <- apply(isin, 1:2, sum)
    if(type == "presence") mat[mat>1] <- 1
    emult <- if(trim) ifelse(effort$effort<interval, NA, 1) else 
      ifelse(effort$effort==0, NA, 1)
    mat * emult
  }
}

#' Make a detection matrix from a camtrapDP datapackage
#' 
#' INPUT
#' pkg: a camtrapDP-like list of camera trap data containing
#'    package$data$deployments and package$data$observations, dataframes with
#'    required columns as for make_dmat, plus scientificName required in observations
#' species: a character vector giving one or more species to create matrices for;
#'    uses scientific binomial names, matched in observations$scientificName
#' trim / interval / start_hour / type / fail_outliers:
#'    arguments passed to make_dmat
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
                                  start_hour=0,
                                  type = c("presence", "count"),
                                  fail_outliers=FALSE){
  
  if(!all(species %in% pkg$data$observations$scientificName))
    stop("Can't find any observations for that/those species")
  
  depdat <- pkg$data$deployments %>%
    dplyr::mutate(deploymentID = as.character(deploymentID),
                  locationName = as.character(locationName))
  obsdat <- pkg$data$observations %>%
    dplyr::mutate(deploymentID = as.character(deploymentID))
  
  emat <- make_emat(depdat,
                    interval=interval, 
                    start_hour=start_hour)
  dmats <- lapply(species, function(sp) 
    make_dmat(depdat, 
              subset(obsdat, scientificName==sp),
              trim = trim, 
              interval = interval, 
              start_hour = start_hour,
              type = type,
              fail_outliers = fail_outliers))
  names(dmats) <- species
  return(c(emat, matrix=list(dmats)))
}

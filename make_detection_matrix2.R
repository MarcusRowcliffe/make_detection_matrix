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
  ndep <- length(deployments)
  emat <- data.frame(loc = rep(deployments$locationID, nocc),
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

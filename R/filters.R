# filters.R
# filter raw track data

#' observe_thresh
#'
#' @param trackRaw A data frame from the trackRaw table. Must contain columns `expID`, `chanID`, `trackID`, and `y`.
#' @param thresh A numeric value for the threshold.
#'
#' @return The number of tracks that are observed crossing the threshold.
#' @export
#'
#' @importFrom dplyr %>% group_by summarize pull
observe_thresh <- function(trackRaw, thresh) {
  trackRaw |>
    group_by(expID, chanID, trackID) |>
    summarize(obs_thresh = any(y >= thresh) & any(y < thresh)) |>
    summarize(n_obs_thresh = sum(obs_thresh)) |>
    pull(n_obs_thresh)
}

#' filter_thresh
#'
#' @param trackRaw A data frame from the trackRaw table. Must contain the column `y`.
#' @param thresh A numeric value for the threshold.
#' @param invert A logical value. If FALSE (default), rows with y < thresh are flagged. If TRUE, rows with y > thresh are flagged.
#' @param drop A logical value. If FALSE (default), a new logical column `drop` is added to `trackRaw`. If TRUE, rows that are flagged are dropped.
#'
#' @return A data frame.
#' @export
#'
#' @importFrom dplyr %>% mutate filter
filter_thresh <- function(trackRaw, thresh, invert = FALSE, drop = FALSE) {
  if (invert) {
    trackRaw <- trackRaw |> mutate(drop = y > thresh)
  } else {
    trackRaw <- trackRaw |> mutate(drop = y < thresh)
  }

  if (drop) {
    trackRaw <- trackRaw |>
      filter(!drop) |>
      select(-drop)
  }

  trackRaw
}

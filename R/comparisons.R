# comparisons.R
# functions to run statistical comparisons between groups

#' compare_prop_finish
#' Compare the proportion of tracks that cross the lower ledge
#' 
#' @param dat data.frame of track data. Must contain the columns `treatment`, `sample`, `prop_finished`, `n_cells`, and the column to compare.
#' @param comp_grp character value of the column to use for comparison
#' @param trt character vector of the treatments to include in the comparison
#' @param samp character vector of the samples to include in the comparison
#' 
#' @details This function uses the prop.test function to compare the proportion of tracks that cross the lower ledge between two groups. If `trt` or `samp` are `NULL` all values in dat will be included.
#' 
#' @return A list of the fold change and p-value for each comparison
#' 
#' @export
#' @importFrom dplyr group_by summarize n filter across all_of
#' @importFrom stats prop.test
compare_prop_finish <- function(dat, comp_grp, trt = NULL, samp = NULL)
{
  # take care of those pesky "no visible binding" notes
  if(FALSE)
    prop_finished <- n_cells <- n_finished <- treatment <- sample <- NULL
  

  # verify that we have the correct columns in dat
  stopifnot(all(c("treatment", "sample", "prop_finished", "n_cells", comp_grp) %in% colnames(dat)))


  # take care of NULL arguments
  if(is.null(trt))
    trt <- unique(dat$treatment)
  
  if(is.null(samp))
    samp <- unique(dat$sample)

  
  # run prop.test  
  filter(dat, treatment %in% trt, sample %in% samp) |>
    
    group_by(across(all_of(comp_grp))) |>
    summarize(n_finished = sum(prop_finished * n_cells),
              n_cells = sum(n_cells)) |>
    
    with(suppressWarnings(prop.test(n_finished, n_cells))) |>
    
    with(list(fc = unname(estimate[1] / estimate[2]),
              prop_test_p = p.value))
}
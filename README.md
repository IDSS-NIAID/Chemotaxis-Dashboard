# Chemotaxis-Dashboard

Welcome to the Chemotaxis-Dashboard repository on GitHub!

Migration of neutrophils to a nidus of infection is a critical component of the host innate immune response; defects in neutrophil chemotaxis can have a severe impact on immune function. Timelapse videos of neutrophil chemotaxis in response to a chemoattractant can be a valuable tool for diagnosing rare defects, but manual collection of quantitative data from these videos can be tedious and time-consuming. We have developed an AI-based workflow to automatically segment and track the paths of individual migrating cells and provide some measures of cell shape.

The chemotaxis dashboard facilitates exploration of summary statistics for each cell (cell velocity over time and direction of movement) and for each sample (average velocity over time, average direction of movement, and the proportion of cells successfully migrating across the area of observation). Cells from different samples can be clustered, compared and contrasted based on the distribution of these summary statistics (e.g. cells obtained from healthy donors vs patients or cells responding to different chemoattractants).

The rest of this README contains lower-level notes and information on the repository itself. For additional information, checkout [this vignette](https://abcsfrederick.github.io/Chemotaxis-Dashboard/poster.html), or you can try out the shiny app with some simulated data on [shinyapps.io](https://mckalliprn.shinyapps.io/demo_app/?_ga=2.247871367.1296701277.1659379204-1106696516.1659379204).

## Utilities

We have a number of R scripts included in the `utils/` directory, and we are working on documenting them here. Check back soon for additional details.

* Preprocessing raw tracks and calculation of summary statistics
  * `preprocess.R` 
  * `one_experiment.R`
* Preprocess data in batches on our HPC cluster
  * `make_swarm.R`
  * `mergeData.R`
* Identifying specific images to use for each cell for cell shape analysis
  * `track_plots.R`
   * Contains functions to generate frame-by-frame plots of cell position, with options to highlight particular tracks of interest. Was used to calibrate the threshold for functions in `track_automated.R`
  * `track_automated.R`
   * Contains the function find_frames(), which identifies all the 'good frames' for each track -- those where the cell is at least a certain (threshold) distance away from the next closest cell.
   * find_frames() generates a .csv file of the 'good frames' for each track based on the selected threshold and if they user wants to filter all frames where the track position is before a certain y-coordinate
  * `findGoodFrames.R`
   * Calls find_frames() from `track_automated.R` for all channels within a certain experiment (date)
  * `shapeAnalysis1.py`
   * Pulls the pixel segmentation mask for a particular experiment and finds the area and perimeter for each cell at frames designated 'good frames' (not overlapping another cell)
   * Returns a .csv of shape data for the selected file
  * `findShape.R`
   * Calls `shapeAnalysis1.py` for all channels within a certain experiment (date)
  * `shapePlots.R`
   * Makes various plots of shape data for the selected experiment, including line plots area and perimeter over time at the channel and track level and violin plots of area and perimeter at the channel level.
* Clustering of cell tracks
  * `clustering_code.R`

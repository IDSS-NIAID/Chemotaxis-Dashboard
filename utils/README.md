# Chemotaxis Dashboard: Utilities

In this folder you will find R scripts and python scripts which are called and run to process different parts of the data in various ways. This README file is designed to make the functions and usage of these files clear.

## Simulated Data

`simulated_data.R` is used to generate the test data that is automatically loaded into the app if no other data sources are required.

* Run from within R: `source(simulated_data.R)`
* Inputs: none
* Outputs: 
    * `utils/results_csv/19000101_*_.csv`: six simulated raw tracks files.
    * `data/19000101.RData`: A processed experiment summary file (this is ignored by other scripts in the utils directory, unless explicitly included as input).
    * `R/sysdata.rda`: Package data to be loaded if no other data are available in the app. This includes three R objects: `all_experiments`, `track_summ_select`, and `exp_summ`.

## Pre-Processing of Data

### Preprocess individual experiments

`preprocess.R` is used to read raw cell tracks from the convolutional model and prepare them to be used in the dashboard.

* Run from the terminal: `Rscript preprocess.R experiment=20070308`
* Inputs:
    * experiment: Date of the experiment to run. Argument should be of the form: `experiment=%Y%m%d`.
    * `utils/results_csv/<experiment><meta_data>.csv`: Expects csv files (one for each channel in the experiment - typically six of them) with the experiment date and some meta data to be located in the `utils/results_csv` directory.
* Output:
    * `data/<experiment>.RData`: A processed experiment summary file.
* Depends:
    * `dplyr`, `purrr`
    * `ChemotaxisDashboard::one_experiment()`

`make_swarm.R` will create a swarm file to be run on biowulf that will call `preprocess.R` for each experiment that needs to be processed. (Run this on biowulf)
* Run from the terminal:
    * `Rscript make_swarm.R`
    * `Rscript make_swarm.R --args csvPath=/data/IDSS_projects/chemotaxis_results/results_csv datPath=/data/IDSS_projects/chemotaxis_results/data`
    * `Rscript make_swarm.R --args root=~/Chemotaxis-Dashboard`
* Inputs:
    * csvPath: Path to csv files from cell tracking model (default is `root/utils/results_csv`).
    * datPath: Path to preprocessed data (default is `root/data`).
    * root: Path to git repository (default assumes this script is run from within the repo and that `git rev-parse` will return the proper path).
* Output:
    * A file, `swarmfile`, to be run on biowulf.
* Depends:
    * `dplyr`, `purrr`, `tools`

### Merge preprocessed data and create a summary of all experiments

`mergeData.R` is then used to combine information from processed experiment summary files. This does not need to be run on all experiments in `data/`. Rather, it will add any new experiments that aren't already in `data/historical.RData`.

* Run from either the terminal or within R.
* Inputs:
    * `data/<experiment>.RData`: processed experiment summary files created in `preprocess.R`.
    * (optional) `data/historical.RData`: If this already exists, new experimental data in `data/` will be added to `all_experiments` and `track_summ_select`.
* Outputs:
    * `data/historical.RData`: Summary of all experiments in `data/`, including the data.frame, `all_experiments`, and a list of all experiments in the data set, `track_summ_select`. The latter is used to subset / select experiments for display in the channel summary tab of the shiny app.
* Depends:
    * `dplyr`

## Remaining scripts to incorporate into the R pacakge

* clustering_code.R
* findGoodFrames.R
* findShape.R
* make_swarm.R
* shapeAnalysis1.py
* shapePlots.R
* track_automated.R
* track_plot.R


#### 1. Process Cell Shape Data
Since the cells often overlap as they travel across the slide, obtaining accurate cell shape data from the segmented pixel mask requires selecting only the frames in which the cells do not overlap. That is the function of `findGoodFrames.R`, which calls `track_automated.R` and automatically selects the frames for each track where the cell is not within a certain threshold of distance.
At present, this threshold is set to 35 pixels. That threshold was decided through testing various thresholds and seeing which tracks were eliminated in the cell migration videos. The details of this process will be included later in the README.
After running `findGoodFrames.R` for the experiment(s) of interest, `findShape.R` can be used to process the cell shape data for the 'good frames' of each track. This script calls `shapeAnalysis1.py` to process the shape data from the cell migration videos.
Finally, `shapePlots.R` creates plots of cell shape, including cell shape statistics (area and perimeter) over time, and channel-level cell shape distribution.

##### Workflow for one experiment
1. In terminal, run `Rscript <filepath>/findGoodFrames.R <experiment>` where filepath is the path to where findGoodFrames is saved and experiment is the experiment of interest
    * This will create a csv of the frames for each track where the nearest neighbor of the cell is outside of the threshold distance
    * If you want to adjust the threshold, you will need to edit `findGoodFrames.R` as the threshold is currently hard-coded in
    * `findGoodFrames.R` will save the file to a folder called 'good_frames' in the working directory -- it is important to move the files from that folder into this directory: '/data/IDSS_projects/good_frames1'
        * `shapeAnalysis1.py` will look for files in the shared folder '/data/IDSS_projects/good_frames1'
        * If someone wants to change `track_automated.R` to save the 'good frames' files directly to '/data/IDSS_projects/good_frames1', that could be helpful
2. Make sure you have a Mamba project set up with the necessary packages for `shapeAnalysis1.py` (notes on how to do this are included at the top of `shapeAnalysis1.py`)
3. Once the Mamba project is properly set up, run `Rscript <filepath>/findShape.R <experiment>` in terminal. 
    * This will create a csv of shape data for each channel in the experiment, including the area and perimeter of each cell at each frame included in the 'good frames'.
    * The csv file will be saved to a folder called 'shape_data' in the chemotaxis dashboard folder -- it is important to move files from that folder into this directory: '/data/IDSS_projects/shape_data'
        * `shapePlots.R` will look for files in the shared folder '/data/IDSS_projects/shape_data'
        * If someone wants to change `shapeAnalysis1.py` to save the 'shape data' files directly to '/data/IDSS_projects/shape_data', that could be helpful
4. Use the functions contained within `shapePlots.R` to make plots visualizing the cell shape data of your experiment of interest
    * First run `preprocess(experiment)` with your experiment of interest, save the result as dat_sub
    * If you are interested in track-level plots of perimeter or area over time, you can run `graph_area_byTrack(dat_sub, joint_channel_select)` or `graph_perim_byTrack(dat_sub, joint_channel_select)` where dat_sub is the result from preprocess(experiment) and joint_channel_select is the name of the channel you are interested in. Examples of format for joint_channel_select can be found in the comments around these functions.
    * Finally, for experiment-level graphs, you can run `channel_shape(dat_sub)` where dat_sub is the results of preprocess(experiment). This will generate several interesting graphs on area & perimeter over time, as well as area and perimeter distribution for the whole experiment. Save the result and use $ to access the plots within the data.frame result.

##### Work left to do
* Find a proper method for handling outliers. 
    * There still appear to be outliers in the cell shape data which are extreme enought that they likely represent the merging of two cells. Since some of these are untracked cells, the automated track selection does not detect a merge of cells.
     Because they aren't caught by the automated track selection process, we may need to eliminate these outliers after the fact. I have started to do that in `shapePlots.R`, by eliminating any cells greater than 3Q + 1.5(IQR). However, I'm not sure if that is the best way to handle the outliers or if some outliers still exist in the data.
     Single cells appear to have an upper bound on perimeter of around 130-150 pixels; cells with perimeter greater than this are likely outliers. However, I have not checked this extensively against the raw data.
    * It is also possible that the threshold for `track_automated.R` needs to be raised, especially if tracked cells are getting close enough together to form merged cells. I will include my process for finding the 35 pixel threshold below, in case someone wants to duplicate this work or adjust the process to find a new threshold / confirm the 35 pixel threshold.
* Include plots from `shapePlots.R` in the ShinyApp.

##### Process for finding 35 pixel threshold
Functions used below can be found in `track_plot.R` and `track_automated.R`
1.	Choose file and create `colorplot()` to get an idea of which tracks do not overlap
2.	Run `find_tracks()` with thresholds of 50, 40, 35, 30, 25, and 20
3.	Starting at 20, visually inspect the tracks that are not included in the next threshold up (i.e., if a threshold of 25 returns 1,3,5 and a threshold of 20 returns 1,2,3,5,7, check 2 and 7 to see if they intersect another tracked cell)
4.	If one or more of the “new” tracks at this threshold do intersect another tracked cell, move up to the next threshold level and repeat this process.
5.	Once a threshold is found where all “new” tracks do not intersect, this threshold is chosen.
Repeat this process on 5-10 files and then test the threshold on an additional 10 files. (The larger the sample size the better -- the process is very time intensive though. It took me about a week)

#### 2. Process migration statistics
The migration statistics include cell velocity over time, angle of migration for each cell, chemotactic efficiency for each cell (how much of the cell's movement was in the direction of the chemoattractant),
and the proportion of cells in each channel completing migration. To calculate these statistics and generate plots displaying them for each experiment, you will need to run `Rscript <filepath>/preprocess.R <experiment>` for your experiment of interest.
`preprocess.R` calls `one_experiment.R`, which does all the work of calculating the statistics and generating plots and saves these stats and plots in an .RData file named after the experiment. These files live in the 'data' folder in the Chemotaxis Dashboard directory
Once you have called `preprocess.R` on all of your experiments of interest, you can run `mergeData.R`, which will merge all the .RData files into one .RData file, called 'historical.RData'. This will be the data file primarily used in the ShinyApp.
The other .RData files are important for the app too, so do not delete them.
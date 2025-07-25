% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_experiments.R
\name{process_experiments}
\alias{process_experiments}
\title{preprocess_experiments 
This will read in results from the convolutional model and preprocess for the dashboard}
\usage{
process_experiments(
  experiment,
  source_dir,
  results_dir,
  ledge_dist = 260,
  ledge_upper = 100,
  ledge_lower = 500
)
}
\arguments{
\item{experiment}{Date of the experiment to run. Argument should be of the form: `experiment="\%Y\%m\%d"`.}

\item{source_dir}{Path to directory containing raw csv tracks files}

\item{results_dir}{Path to processed data directory}

\item{ledge_dist}{Numeric, distance between top and bottom ledges of the microscope image in micrometers (default is 260)}

\item{ledge_upper}{Numeric, location of the upper ledge in the raw Y coordinate system (default is 100)}

\item{ledge_lower}{Numeric, location of the lower ledge in the raw Y coordinate system (default is 500)}
}
\value{
A list of data.frames...
}
\description{
preprocess_experiments 
This will read in results from the convolutional model and preprocess for the dashboard
}

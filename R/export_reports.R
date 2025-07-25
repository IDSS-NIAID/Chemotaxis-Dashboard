#' Export reports based on data from a single experiment
#' export_ses
#' 
#' @param experiment character string, experiment ID
#' @param db character string, path to SQLite database
#' @param format character string, format for the report file (must be a format supported by quarto)
#' @param file character string, name for output files (not including the file extension)
#' @param file_ext character string, file extension for the report file
#' @param outdir character string, path to output directory
#' @param seed integer, random seed
#' @param render logical, whether to render the report. When false an unrendered qmd file is saved to the output directory.
#' @param export_csv logical, whether to export csv files for each channel in the experiment
#' @param qmd character string, path to qmd file for the report (see Details)
#' 
#' @details This function exports reports based on data from a single experiment.
#' The report files are saved in the output directory and include csv files for each channel in the experiment and a report file of the specified file type.
#' The report file is generated using the `quarto` package and must be a format supported by `quarto` (see https://quarto.org/docs/reference/).
#' The location, `outdir`, must exist and the user must have write permissions to the directory.
#' 
#' If `render` is FALSE, then the report file is saved as an unrendered qmd file, and dependency checks for the rendering step will not be run.
#' 
#' Using an alternate qmd file may cause some of the other parameters to stop working (i.e. `format`, `db`, `file`, `file_ext`, `outdir`, `seed`, `export_csv`).
#' See the provided file, `ses_report.qmd` and/or the function definition for `export_ses` for additional details.
#' 
#' @return NULL
#' @export
#' @importFrom quarto quarto_render
#' @importFrom stringr str_replace_all
export_ses <- function(experiment, db, format = "docx", file = experiment,
                       file_ext = "docx", outdir = getwd(), seed = NULL, render = TRUE,
                       export_csv = TRUE, qmd = system.file("extdata", "ses_report.qmd", package = "ChemotaxisDashboard"))
{
  # if not rendering, these dependencies are not needed
  if(render)
  {
    # make sure we have the necessary quarto version
    check_quarto()
    
    # make sure we have all packages needed by the quarto document
    if(!requireNamespace("dbplyr", quietly = TRUE))
      stop("Please install the 'dbplyr' package to run this report.")
  }

  # read in the sample report and fill in variables
  report <- qmd |>
    readLines() |> # these variables are assumed to be in the file - if they aren't this should still run,
    str_replace_all("<<experiment>>", experiment) |> # but these parameters will be ignored
    str_replace_all("<<db>>", db) |>
    str_replace_all("<<format>>", format) |>
    str_replace_all("<<file>>", file) |>
    str_replace_all("<<file_ext>>", file_ext) |>
    str_replace_all("<<outdir>>", outdir) |>
    str_replace_all("<<seed>>", ifelse(is.null(seed), "NULL", as.character(seed))) |>
    str_replace_all("<<export_csv>>", ifelse(export_csv, "TRUE", "FALSE"))
  
  # write the report to a file
  cat(report,
      file = file.path(outdir, paste0(file, ".qmd")),
      sep = '\n')
  
  # render the report file
  if(render)
  {
    file.path(outdir, paste0(file, ".qmd")) |>
      quarto_render(quiet = TRUE)
  }

  invisible(NULL)
}

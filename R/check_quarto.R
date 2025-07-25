#' check_quarto
#' Check that the desired version of quarto is installed
#' 
#' @param version character, desired version of quarto (use "" to check for any version of quarto)
#' @param version_warn logical, should a warning be issued if the installed version is less than the desired version? This is ignored if `version_error` is TRUE.
#' @param version_error logical, should an error be issued if the installed version is less than the desired version?
#' 
#' @return TRUE if the desired version of quarto is installed, FALSE otherwise
#' @importFrom utils compareVersion
#' @export
check_quarto <- function(version = '1.5.57', version_warn = TRUE, version_error = !version_warn)
{
  # check that any version of quarto is installed
  if(Sys.which("quarto") == "")
    stop("The quarto package is required (see https://quarto.org/docs/get-started/).")
  
  if(compareVersion(system2("quarto", "--version", stdout = TRUE),
                    version) < 0)
  {
    if(version_error)
      stop(paste("Version", version, "of quarto is required.",
                 "Consider upgrading (see https://quarto.org/docs/get-started/)."))
    
    if(version_warn)
    warning(paste("Version", version, "of quarto is recommended.",
                  "Consider upgrading (see https://quarto.org/docs/get-started/)."))
  }

  invisible(TRUE)
}

## ---- Example for QProjekt Report -------------------------------------------
#
#
#' @title Create Example for German Version of Project Report of Qualitas AG
#'
#' @description
#' A prepared example report consisting of an RMarkdown source file, a logo and
#' a diagram is copied to a target directory given by the parameter ps_path.
#'
#'
#' @param ps_path target path where example report should be positioned
#' @param pb_open should the example report source file be opened
#'
#' @export create_example_qprojektreport
#'
#' @examples
#' tmprepdir <- tempdir()
#' create_example_qprojektreport(ps_path = tmprepdir, pb_open = FALSE)
#' unlink(file.path(tmprepdir, "qprojekt_report"), recursive = TRUE, force = TRUE)
create_example_qprojektreport <- function(ps_path, pb_open = rlang::is_interactive()){
  # check whether ps_path exists
  if (!dir.exists(ps_path)) {
    cat("[create_example_qprojektreport] * Create target directory: ", ps_path, "\n")
    dir.create(ps_path, recursive = TRUE)
  }
  # copy the example from the systems directory
  s_example_dir <- system.file("example_data", "qprojekt_report", package = "qrmdtmpl")
  if (dir.exists(s_example_dir)) {
    cat("[create_example_qprojektreport] * Copy ", s_example_dir, " to: ", ps_path, "\n")
    fs::dir_copy(path = s_example_dir, new_path = ps_path)
    # open report source file, if specified
    if (pb_open){
      s_bn_report <- basename(s_example_dir)
      s_report_rmd <- file.path(ps_path, s_bn_report, paste(s_bn_report, ".Rmd", sep = ""))
      if (file.exists(s_report_rmd)){
        usethis::edit_file(s_report_rmd)
      } else {
        cat("[create_example_qprojektreport] * CANNOT FIND target report Rmd file: ", s_report_rmd, "\n")
      }
    }
  } else {
    cat("[create_example_qprojektreport] * CANNOT FIND example directory: ", s_example_dir, "\n")
  }
  # return nothing
  return(invisible(NULL))

}




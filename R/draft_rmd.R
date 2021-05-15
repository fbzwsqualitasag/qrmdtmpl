## ---- Create Empty Generic Rmarkdown Document -------------------------------
#'
#'
#' @title Create Empty Generic Rmarkdown Document
#'
#' @description
#' Wrapper function for rmarkdown::draft to create an empty generic
#' Rmarkdown document with reasonable default values.
#'
#' @details
#' The template 'qemptydoc' is taken from this package 'qrmdtmpl'.
#'
#' @param ps_path path to the document to be created
#' @param ps_template name of the tempalte
#' @param ps_package package which contains the template
#' @param ps_create_dir specify whether to create a new directory
#' @param pb_edit flag whether newly created file should be edited
#' @param pb_open open the created file in rstudio editor
#'
#' @return invisible(TRUE)
#'
#' @examples
#' \dontrun{
#' draft_qemptydoc(ps_path = 'empty_test_doc')
#' }
#' @export draft_qemptydoc
draft_qemptydoc <- function(ps_path,
                            ps_template   = 'qemptydoc',
                            ps_package    = 'qrmdtmpl',
                            ps_create_dir = "default",
                            pb_edit       = FALSE,
                            pb_open       = rlang::is_interactive()){
  # remove any file extensions
  s_path <- tools::file_path_sans_ext(ps_path)
  # use the draft function of rmarkdown
  rmarkdown::draft(file       = s_path,
                   template   = ps_template,
                   package    = ps_package,
                   create_dir = ps_create_dir,
                   edit       = pb_edit)
  # open the file, if specified
  if (pb_open) {
    if (ps_create_dir == 'default') {
      create_dir <- get_default_create_dir(ps_template = ps_template, ps_package = ps_package)
    } else {
      create_dir <- ps_create_dir
    }
    if (create_dir){
      s_base_path <- basename(s_path)
      s_path <- file.path(s_path, s_base_path)
    }
    if (!identical(tolower(tools::file_ext(s_path)), "rmd"))
      s_path <- paste(s_path, '.Rmd', sep = '')
    usethis::edit_file(path = s_path)
  }

  return(invisible(TRUE))

}


## ---- Generic Qualitas AG Document ------------------------------------------
#'
#' @title Create Generic Qualitas Document
#'
#' @description
#' Wrapper function for rmarkdown::draft to create an empty Qualitas AG
#' Rmarkdown document including the logo on the title page.
#'
#' @details
#' The template 'qemptydoc' is taken from this package 'qrmdtmpl'.
#'
#' @param ps_path path to the document to be created
#' @param ps_template name of the tempalte
#' @param ps_package package which contains the template
#' @param ps_create_dir specify whether to create a new directory
#' @param pb_edit flag whether newly created file should be edited
#' @param pb_open open the created file in rstudio editor
#'
#' @return invisible(TRUE)
#'
#' @examples
#' \dontrun{
#' draft_qgenericdoc(ps_path = 'empty_test_doc')
#' }
#' @export draft_qgenericdoc
draft_qgenericdoc <- function(ps_path,
                              ps_template   = 'qgenericdoc',
                              ps_package    = 'qrmdtmpl',
                              ps_create_dir = "default",
                              pb_edit       = FALSE,
                              pb_open       = rlang::is_interactive()){
  # remove any file extensions
  s_path <- tools::file_path_sans_ext(ps_path)
  # use the draft function of rmarkdown
  rmarkdown::draft(file       = s_path,
                   template   = ps_template,
                   package    = ps_package,
                   create_dir = ps_create_dir,
                   edit       = pb_edit)
  # open the file, if specified
  if (pb_open) {
    if (ps_create_dir == 'default') {
      create_dir <- get_default_create_dir(ps_template = ps_template, ps_package = ps_package)
    } else {
      create_dir <- ps_create_dir
    }
    if (create_dir){
      s_base_path <- basename(s_path)
      s_path <- file.path(s_path, s_base_path)
    }
    if (!identical(tolower(tools::file_ext(s_path)), "rmd"))
      s_path <- paste(s_path, '.Rmd', sep = '')
    usethis::edit_file(path = s_path)
  }

  return(invisible(TRUE))
}


## ---- Qualitas AG Report Document ------------------------------------------
#'
#' @title Create Qualitas Report Document
#'
#' @description
#' Wrapper function for rmarkdown::draft to create an empty Qualitas AG
#' report document including the logo on the title page.
#'
#' @details
#' The template 'quagprojectreport' is taken from this package 'qrmdtmpl'.
#'
#' @param ps_path path to the document to be created
#' @param ps_template name of the tempalte
#' @param ps_package package which contains the template
#' @param ps_create_dir specify whether to create a new directory
#' @param pb_edit flag whether newly created file should be edited
#' @param pb_open open the created file in rstudio editor
#'
#' @return invisible(TRUE)
#'
#' @examples
#' \dontrun{
#' draft_quagprojectreport(ps_path = 'empty_test_doc')
#' }
#' @export draft_quagprojectreport
draft_quagprojectreport <- function(ps_path,
                                 ps_template   = 'quagprojectreport',
                                 ps_package    = 'qrmdtmpl',
                                 ps_create_dir = "default",
                                 pb_edit       = FALSE,
                                 pb_open       = rlang::is_interactive()){
  # remove any file extensions
  s_path <- tools::file_path_sans_ext(ps_path)
  # use the draft function of rmarkdown
  rmarkdown::draft(file       = s_path,
                   template   = ps_template,
                   package    = ps_package,
                   create_dir = ps_create_dir,
                   edit       = pb_edit)
  # open the file, if specified
  if (pb_open) {
    if (ps_create_dir == 'default') {
      create_dir <- get_default_create_dir(ps_template = ps_template, ps_package = ps_package)
    } else {
      create_dir <- ps_create_dir
    }
    if (create_dir){
      s_base_path <- basename(s_path)
      s_path <- file.path(s_path, s_base_path)
    }
    if (!identical(tolower(tools::file_ext(s_path)), "rmd"))
      s_path <- paste(s_path, '.Rmd', sep = '')
    usethis::edit_file(path = s_path)
  }

  return(invisible(TRUE))
}


## ---- Qualitas AG FB ZWS Report Dokument ------------------------------------------
#'
#' @title Create Qualitas FB ZWS Report (German)
#'
#' @description
#' Wrapper function for rmarkdown::draft to create a skeleton for a Qualitas AG
#' report document in German. The document includes the logo on the title page.
#'
#' @details
#' The template 'qprojektreport' is taken from this package 'qrmdtmpl'.
#'
#' @param ps_path path to the document to be created
#' @param ps_template name of the tempalte
#' @param ps_package package which contains the template
#' @param ps_create_dir specify whether to create a new directory
#' @param pb_edit flag whether newly created file should be edited
#' @param pb_open open the created file in rstudio editor
#'
#' @return invisible(TRUE)
#'
#' @examples
#' \dontrun{
#' draft_qprojektreport(ps_path = 'empty_test_doc')
#' }
#' @export draft_qprojektreport
draft_qprojektreport <- function(ps_path,
                                    ps_template   = 'qprojektreport',
                                    ps_package    = 'qrmdtmpl',
                                    ps_create_dir = "default",
                                    pb_edit       = FALSE,
                                    pb_open       = rlang::is_interactive()){
  # remove any file extensions
  s_path <- tools::file_path_sans_ext(ps_path)
  # use the draft function of rmarkdown
  rmarkdown::draft(file       = s_path,
                   template   = ps_template,
                   package    = ps_package,
                   create_dir = ps_create_dir,
                   edit       = pb_edit)
  # open the file, if specified
  if (pb_open) {
    if (ps_create_dir == 'default') {
      create_dir <- get_default_create_dir(ps_template = ps_template, ps_package = ps_package)
    } else {
      create_dir <- ps_create_dir
    }
    if (create_dir){
      s_base_path <- basename(s_path)
      s_path <- file.path(s_path, s_base_path)
    }
    if (!identical(tolower(tools::file_ext(s_path)), "rmd"))
      s_path <- paste(s_path, '.Rmd', sep = '')
    usethis::edit_file(path = s_path)
  }

  return(invisible(TRUE))
}



## ---- Empty Beamer Slides ---------------------------------------------------
#'
#' @title Create Empty Beamer Slides
#'
#' @description
#' Wrapper function to create empty beamer slides document using
#' \code{rmarkdown::draft()}.
#'
#' @details
#' The template quagbeamer is taken from this package qrmdtmpl.
#'
#' @param ps_path path to the document to be created
#' @param ps_template name of the tempalte
#' @param ps_package package which contains the template
#' @param ps_create_dir specify whether to create a new directory
#' @param pb_edit flag whether newly created file should be edited
#' @param pb_open open the created file in rstudio editor
#'
#' @examples
#' \dontrun{
#' draft_quagbeamerslides(ps_path = 'qbeamer_slides')
#' }
#'
#' @export draft_qbeamerslides
draft_qbeamerslides <- function(ps_path,
                                   ps_template   = 'quagbeamer',
                                   ps_package    = 'qrmdtmpl',
                                   ps_create_dir = "default",
                                   pb_edit       = FALSE,
                                   pb_open       = rlang::is_interactive()){
  # remove any file extensions
  s_path <- tools::file_path_sans_ext(ps_path)
  # use the draft function of rmarkdown
  rmarkdown::draft(file       = s_path,
                   template   = ps_template,
                   package    = ps_package,
                   create_dir = ps_create_dir,
                   edit       = pb_edit)
  # open the file, if specified
  if (pb_open) {
    if (ps_create_dir == 'default') {
      create_dir <- get_default_create_dir(ps_template = ps_template, ps_package = ps_package)
    } else {
      create_dir <- ps_create_dir
    }
    if (create_dir){
      s_base_path <- basename(s_path)
      s_path <- file.path(s_path, s_base_path)
    }
    if (!identical(tolower(tools::file_ext(s_path)), "rmd"))
      s_path <- paste(s_path, '.Rmd', sep = '')
    usethis::edit_file(path = s_path)
  }
  return(invisible(TRUE))
}


## ----- Helper Functions ----------------------------------------------------
#'
#' @title Find Default Option for create_dir
#'
#' @description
#' Find the value of the option create_dir from the template.yaml in the
#' template directory
#'
#' @details
#' The template.yaml file is read using \code{rmarkdown:::yaml_load_file}
#'
#' @param ps_template name of the template
#' @param ps_package name of the package that contains the table
#'
get_default_create_dir <- function(ps_template,
                                   ps_package = NULL){
  # get template path
  if (!is.null(ps_package)){
    template_path <-  system.file("rmarkdown", "templates", ps_template, package = ps_package)
    if (!nzchar(template_path)) {
      stop("The template '", ps_template, "' was not found in package: ", ps_package)
    }
  } else {
    template_path <- ps_template
  }
  template_yaml <- file.path(template_path, "template.yaml")
  if (!file.exists(template_yaml)) {
    stop("No template.yaml file found for template '", ps_template,"'")
  }
  template_meta <- rmarkdown:::yaml_load_file(template_yaml)
  return(template_meta$create_dir)
}

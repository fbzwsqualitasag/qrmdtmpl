## ---- Generic Document Draft Wrapper Function -------------------------------
#
#
#' @title Generic Document Draft Wrapper Function
#'
#' @description
#' This function calls \code{rmarkdown::draft()} to create a new document.
#'
#'
#' @param ps_path path to the document to be created
#' @param ps_template name of the template
#' @param ps_package package containing the template
#' @param ps_create_dir flag whether document should be put into new directory
#' @param pb_open flag whether to open the newly created document in rstudio
#' @param pl_repl_values list of template placeholder replacement values
#'
generic_rmd_draft <- function(ps_path,
                              ps_template,
                              ps_package,
                              ps_create_dir,
                              pb_open,
                              pl_repl_values){
  # remove any file extensions
  s_path <- tools::file_path_sans_ext(ps_path)
  # use the draft function of rmarkdown
  rmarkdown::draft(file       = s_path,
                   template   = ps_template,
                   package    = ps_package,
                   create_dir = ps_create_dir,
                   edit       = FALSE)

  # create full path to new rmd file based on option and ending
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

  # substitute placeholers with values
  l_default_repl_values <- get_generic_replacement_values()
  if (is.null(pl_repl_values)){
    l_repl_values <- l_default_repl_values
  } else {
    l_repl_values <- merge_list_to_default(pl_base = pl_repl_values, pl_default = l_default_repl_values)
  }
  sub_pattern_replacement(ps_path = s_path, pl_repl_values = l_repl_values)

  # open the file, if specified
  if (pb_open) {
    usethis::edit_file(path = s_path)
  }

  return(invisible(NULL))

}


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
#' @param pb_open open the created file in rstudio editor
#' @param pl_repl_values list with values to replace placeholders
#'
#' @return invisible(TRUE)
#'
#' @examples
#' \dontrun{
#' draft_qemptydoc(ps_path = 'empty_test_doc')
#' }
#' @export draft_qemptydoc
draft_qemptydoc <- function(ps_path,
                            ps_template    = 'qemptydoc',
                            ps_package     = 'qrmdtmpl',
                            ps_create_dir  = "default",
                            pb_open        = rlang::is_interactive(),
                            pl_repl_values = NULL){
  # call generic draft wrapper
  generic_rmd_draft <- function(ps_path        = ps_path,
                                ps_template    = ps_template,
                                ps_package     = ps_package,
                                ps_create_dir  = ps_create_dir,
                                pb_open        = pb_open,
                                pl_repl_values = pl_repl_values)
    return(invisible(NULL))
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
#' @param pb_open open the created file in rstudio editor
#' @param pl_repl_values list with values to replace placeholders
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
                              pb_open       = rlang::is_interactive(),
                              pl_repl_values = NULL){
  # call generic draft wrapper
  generic_rmd_draft <- function(ps_path        = ps_path,
                                ps_template    = ps_template,
                                ps_package     = ps_package,
                                ps_create_dir  = ps_create_dir,
                                pb_open        = pb_open,
                                pl_repl_values = pl_repl_values)
    return(invisible(NULL))
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
#' @param pb_open open the created file in rstudio editor
#' @param pl_repl_values list with values to replace placeholders
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
                                 pb_open       = rlang::is_interactive(),
                                 pl_repl_values = NULL){
  # call generic draft wrapper
  generic_rmd_draft <- function(ps_path        = ps_path,
                                ps_template    = ps_template,
                                ps_package     = ps_package,
                                ps_create_dir  = ps_create_dir,
                                pb_open        = pb_open,
                                pl_repl_values = pl_repl_values)
    return(invisible(NULL))
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
#' @param pb_open open the created file in rstudio editor
#' @param pl_repl_values list with values to replace placeholders
#'
#' @return invisible(NULL)
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
                                 pb_open       = rlang::is_interactive(),
                                 pl_repl_values = NULL){
  # call generic draft wrapper
  generic_rmd_draft <- function(ps_path        = ps_path,
                                ps_template    = ps_template,
                                ps_package     = ps_package,
                                ps_create_dir  = ps_create_dir,
                                pb_open        = pb_open,
                                pl_repl_values = pl_repl_values)
    return(invisible(NULL))
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
#' @param pb_open open the created file in rstudio editor
#' @param pl_repl_values list with values to replace placeholders
#'
#' @examples
#' \dontrun{
#' draft_quagbeamerslides(ps_path = 'qbeamer_slides')
#' }
#'
#' @export draft_qbeamerslides
draft_qbeamerslides <- function(ps_path,
                                   ps_template    = 'quagbeamer',
                                   ps_package     = 'qrmdtmpl',
                                   ps_create_dir  = "default",
                                   pb_open        = rlang::is_interactive(),
                                   pl_repl_values = NULL){
  # call generic draft wrapper
  generic_rmd_draft <- function(ps_path        = ps_path,
                                ps_template    = ps_template,
                                ps_package     = ps_package,
                                ps_create_dir  = ps_create_dir,
                                pb_open        = pb_open,
                                pl_repl_values = pl_repl_values)
    return(invisible(NULL))
}


## ---- Tufte Report ----------------------------------------------------------

#' Create Draft Version of a Tufte Report
#'
#' @description
#' Wrapper function to create a draft of a Tufte Report using
#' \code{rmarkdown::draft()}.
#'
#' @details
#' The used RMarkdown template qtufte is taken from this package qrmdtmpl.
#'
#' @param ps_path path to the document to be created
#' @param ps_template name of the tempalte
#' @param ps_package package which contains the template
#' @param ps_create_dir specify whether to create a new directory
#' @param pb_open open the created file in rstudio editor
#' @param pl_repl_values list with values to replace placeholders
#'
#' @export draft_qtufte
#'
#' @examples
#' \dontrun{
#' draft_qtufte(ps_path = "qtufte_report")
#' }
draft_qtufte <- function(ps_path,
                          ps_template    = "qtufte",
                          ps_package     = "qrmdtmpl",
                          ps_create_dir  = "default",
                          pb_open        = rlang::is_interactive(),
                          pl_repl_values = NULL){
  # call generic draft wrapper
  generic_rmd_draft <- function(ps_path        = ps_path,
                                ps_template    = ps_template,
                                ps_package     = ps_package,
                                ps_create_dir  = ps_create_dir,
                                pb_open        = pb_open,
                                pl_repl_values = pl_repl_values)
    return(invisible(NULL))
}


## ---- Bookdown PDF Document -------------------------------------------------
#
#
#' @title Create Bookdown PDF Document
#'
#' @description
#' Wrapper to create bookdown pdf document using \code{rmarkdown::draft}.
#'
#' @param ps_path path to newly created document
#' @param ps_template template to be used
#' @param ps_package package where template is stored
#' @param ps_create_dir option whether to create new directory for new document
#' @param pb_open whether document should be opened
#' @param pl_repl_values list with pattern replacement values
#'
#' @export draft_qbdpdf2
#'
#' @examples
#' \donotrun{
#' draft_qbdpdf2(ps_path = "example_bdpdf2")
#' }
draft_qbdpdf2 <- function(ps_path,
                          ps_template    = "qbdpdf2",
                          ps_package     = "qrmdtmpl",
                          ps_create_dir  = "default",
                          pb_open        = rlang::is_interactive(),
                          pl_repl_values = NULL){
  # call generic draft wrapper
  generic_rmd_draft <- function(ps_path        = ps_path,
                                ps_template    = ps_template,
                                ps_package     = ps_package,
                                ps_create_dir  = ps_create_dir,
                                pb_open        = pb_open,
                                pl_repl_values = pl_repl_values)
    return(invisible(NULL))
}




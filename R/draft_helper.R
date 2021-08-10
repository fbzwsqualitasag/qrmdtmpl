## ----- Determination of Defaults ----------------------------------------------------
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


## ----- Pattern Replacement ---------------------------------------------------
#' @title Replace Template Placeholders with Values
#'
#' @description
#' The skeleton.Rmd template files contain placeholders which are to be
#' replaced by the values given in the list pl_repl_values.
#'
#' @param ps_path path to new Rmarkdown file
#' @param pl_repl_values list with replacement values
#'
#' @examples
#' \dontrun{
#' tmpdir <- tempdir()
#' s_skeleton_path <- system.file("rmarkdown", "templates", "qemptydoc", "skeleton", "skeleton.Rmd", package = "qrmdtmpl")
#' s_skeleton_file <- basename(s_skeleton_path)
#' file.copy(from = s_skeleton_path, to = tmpdir)
#' s_trg_path <- file.path(tmpdir, s_skeleton_file)
#' qrmdtmpl:::sub_pattern_replacement(ps_path = s_trg_path,
#'                         pl_repl_values = list(author = "Peter von Rohr",
#'                                               title  = "A New Rmarkdown Document",
#'                                               output_format = "html_document"))
#' vec_doc <- readLines(con = file(s_trg_path))
#' cat(paste0(vec_doc, collapse = "\n"), "\n")
#' file.remove(s_trg_path)
#' unlink(tmpdir)
#' }
sub_pattern_replacement <- function(ps_path, pl_repl_values){
  # check that file in ps_path exists
  if (!file.exists(ps_path)) stop (" *** ERROR in sub_pattern_replacement: CANNOT FIND FILE: ", ps_path)
  # read the template file
  vec_tmpl <- readLines(con = file(ps_path))
  s_tmpl <- paste0(vec_tmpl, collapse = "\n")
  # replace placeholders with values
  s_result <- glue::glue(s_tmpl,
                         title = pl_repl_values$title,
                         author = pl_repl_values$author,
                         output_format = pl_repl_values$output_format,
                         .open = "<ph>",
                         .close = "</ph>")
  # write result back to file
  cat(s_result, "\n", file = ps_path)

  return(invisible(TRUE))

}


## ---- Extension Of Replacement Value Lists -----------------------------------

#' Extend Replacement Value List
#'
#' @description
#' A given value replacement list is extended by a reasonable choice of defaults
#' for all missing list elements. The defaults are obtained by special getter
#' function.
#'
#' @param pl_repl_values given value replacement list
#'
#' @return l_repl_value_result extended value replacement list
#'
#' @examples
#' \dontrun{
#' extend_repl_values_qtufte(pl_repl_values = list(title = "Tufte Report Title"))
#' }
extend_repl_values_qtufte <- function(pl_repl_values){
  l_repl_value_result <- get_repl_value_defaults_tufte()
  vec_names_result <- names(l_repl_value_result)
  # extend l_repl_value_result with the values specified in pl_repl_values
  vec_names <- names(pl_repl_values)
  for (n_idx in seq_along(vec_names)){
    s_cur_name <- vec_names[n_idx]
    if (!is.element(s_cur_name, vec_names_result))
        stop(" *** ERROR in extend_repl_values_qtufte: Invalid current name: ", s_cur_name)
    l_repl_value_result[[s_cur_name]] <- pl_repl_values[[s_cur_name]]
  }
  return(l_repl_value_result)
}


## ---- Defaults for Replacement Value Lists -----------------------------------

#' Defaults For Tufte Report Value Replacement List
#'
#' @description
#' The placeholder replacement in a RMarkdown template skeleton expectes a
#' fixed number of replacement values. The default values for this value
#' replacement list is returned
#'
#' @return default for tufte report value replacement list
get_repl_value_defaults_tufte <- function(){
  return(list(author        = whoami::username(),
              title         = 'Default Title for Tufte Report',
              output_format = ''))
}

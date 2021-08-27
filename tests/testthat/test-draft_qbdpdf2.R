library(qrmdtmpl)

test_that("draft qbookdown pdf_document2", {
  s_file <- "example_qpdpdf2"
  s_path <- file.path(tempdir(), s_file)

  draft_qbdpdf2(ps_path = s_path,
                pb_open = FALSE,
                pl_repl_value = list(title = "Example Bookdown PDF2 Document",
                                     author = "Peter von Rohr",
                                     date   = "2021-08-27"))

  # read generated document
  s_qbdpdf2_path <- file.path(s_path, paste0(s_file, ".Rmd"))
  con_qbdpdf2 <- file(description = s_qbdpdf2_path)
  vec_qbdpdf2 <- readLines(con = con_qbdpdf2)
  close(con = con_qbdpdf2)
  # read expected result
  s_exp_result <- system.file("testdata","example_qpdpdf2.Rmd")
  con_exp_result <- file(description = s_exp_result)
  vec_exp_result <- readLines(con = con_exp_result)
  close(con = con_exp_result)
  # compare
  expect_equal(vec_qbdpdf2, vec_exp_result)
  # clean up
  fs::dir_delete(path = s_path)

})

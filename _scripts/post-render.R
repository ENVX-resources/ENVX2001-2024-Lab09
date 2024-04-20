qmd_file <- list.files(pattern = ".qmd")
knitr::purl(qmd_file)

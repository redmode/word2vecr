#' Converts words to phrases
#'
#' @export
word2phrase <- function(train_file,
                        output_file,
                        min_count = 5,
                        threshold = 100)
{
  if (!file.exists(train_file)) {
    stop("Can't find the train file!")
  }
  train_dir <- dirname(train_file)

  if (missing(output_file)) {
    output_file <- gsub(gsub("^.*\\.", "", basename(train_file)), "bin", basename(train_file))
    output_file <- file.path(train_dir, output_file)
  }

  outfile_dir <- dirname(output_file)
  if (!file.exists(outfile_dir)) {
    dir.create(outfile_dir, recursive = TRUE)
  }

  train_file <- normalizePath(train_file, winslash = "/", mustWork = FALSE)
  output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)

  # OUT <- .C("CWrapper_word2phrase",
  #           train_file = as.character(train_file),
  #           output_file = as.character(output_file),
  #           min_count = as.character(min_count),
  #           threshold = as.character(threshold))

  CWrapper_word2phrase(train_file,
                       output_file,
                       min_count,
                       threshold)

  output_file

  # class(OUT) <- "word2phrase"
  # names(OUT)[2] <- "model_file"
  # cat(paste("The model was generated in '", dirname(output_file), "'!\n", sep = ""))
  # return(OUT)
}

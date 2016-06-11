#' Main function to train the model
#'
#' @export
word2vec <- function(infile, outfile, size = 100) {

  CWrapper_word2vec(train_file_ = infile,
                    output_file_ = outfile,
                    binary_ = 1,
                    size_ = size)
  out <- read.binary.vectors(outfile)
  class(out) <- c("word2vec", "matrix")
  out
}

#' Custom printer
#'
#' @export
print.word2vec <- function(x) {
  cat(sprintf("%d words with %d vectors...\n\n", nrow(x), ncol(x)))
  print(head(x[, 1:min(10, nrow(x))], 10))
}


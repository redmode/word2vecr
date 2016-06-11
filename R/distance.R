#' Distance function
#'
#' @export
#'
distance <- function(file_name, word, size) {

  if (!file.exists(file_name)) {
    stop("Can't find the model file!")
  }

  OUT <- CWrapper_distance(file_name_ = file_name,
                           word_ = word,
                           # returnw_ = retw,
                           # returnd_ = as.double(rep(0, size)),
                           size_ = size)

  vword <- strsplit(gsub("^ *", "", OUT$returnw), split = " ")[[1]]
  vdist <- OUT$returnd
  if (length(vword) == 0) vdist <- numeric()

  data.frame(word = vword, dist = vdist)
}


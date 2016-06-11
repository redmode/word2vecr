
read.binary.vectors = function(filename, nrows=Inf) {
  a = file(filename,'rb')
  rows = ""
  mostRecent=""
  while(mostRecent!=" ") {
    mostRecent = readChar(a,1)
    rows = paste0(rows,mostRecent)
  }
  rows = as.integer(rows)

  cols = ""
  while(mostRecent!="\n") {
    mostRecent = readChar(a,1)
    cols = paste0(cols,mostRecent)
  }
  cols = as.integer(cols)

  if(nrows<rows) {
    rows = nrows
  }

  message(paste("Reading a word2vec binary file of",rows,"rows and",cols,"columns"))

  ## Read a row
  rownames = rep("",rows)

  # create progress bar
  pb <- utils::txtProgressBar(min = 0, max = rows, style = 3)


  matrix = t(
    vapply(1:rows,function(i) {
      utils::setTxtProgressBar(pb,i)

      rowname=""
      mostRecent=""
      while(TRUE) {
        mostRecent = readChar(a,1)
        if (mostRecent==" ") {break}
        if (mostRecent!="\n") {
          # Some versions end with newlines, some don't.
          rowname = paste0(rowname,mostRecent)
        }
      }
      rownames[i] <<- rowname
      row = readBin(a,numeric(),size=4,n=cols,endian="little")
      return(row)
    },as.array(rep(0,cols)))
  )
  close(pb)
  close(a)
  rownames(matrix) = rownames

  matrix
}


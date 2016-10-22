library(plyr)

compile <- function(dir, key, headername) {
  filenames <- list.files(dir, key, full.names=T)
  header <- read.table(paste0(dir,'/',headername), header=T, sep='\t')
  header <- names(header)
  ldply(filenames, read.table, sep='\t', header=F, col.names=header, row.names=NULL, stringsAsFactors=F)
}

check_merge <- function(a, b, col) {
  if(length(a[,col] > length(b[,col]))) {
    a <- unique(a[,col])
  } else {
    a <- a[,col]
  }
  b <- b[,col]
  a_not_in_b <- a[!(a %in% b)]
  b_not_in_a <- b[!(b %in% a)]
  if(length(a_not_in_b) != 0) {
    print(paste('In a, not in b:',a_not_in_b))
  }
  if(length(b_not_in_a) != 0) {
    print(paste('In b, not in a:',b_not_in_a))
  }
}
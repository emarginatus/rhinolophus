#'@importFrom digest digest
sha1 <- function(x){
  digest(x, algo = "sha1")
}

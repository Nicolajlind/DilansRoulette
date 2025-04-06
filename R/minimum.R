minimum <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  } else {
    return(min(x, na.rm = TRUE))
  }
}

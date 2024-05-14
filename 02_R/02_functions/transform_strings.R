transform_strings <- function(strings) {
  sapply(strings, function(x) {
    if(tolower(x) == "avia") {
      return("AVIA")
    } else {
      return(paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = ""))
    }
  }, USE.NAMES = FALSE)
}

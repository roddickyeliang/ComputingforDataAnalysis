count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if(is.null(cause))
    stop("Cause can't be null!")
  
  ## Check that specific "cause" is allowed; else throw error
  allowedCauses <- c("asphyxiation","blunt force","other","shooting","stabbing","unknown")
  if(!is.element(cause, allowedCauses))
    stop("This cause is not allowed!")
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract causes of death
  r <- regexec("<dd>[C|c]ause: (.*?)</dd>", homicides)
  m <- regmatches(homicides, r)
  causes <- sapply(m, function(x) x[2])
  result <- subset(causes, tolower(causes)==cause)
  
  ## Return integer containing count of homicides for that cause
  return(length(result))
}
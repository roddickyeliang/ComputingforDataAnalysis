agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if(is.null(age))
    stop("Age can't be null!")
  
  ## Read "homicides.txt" data file
  homicides <- readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is given
  r <- regexec("([0-9]+) years old", homicides)
  m <- regmatches(homicides, r)
  ages <- sapply(m, function(x) x[2])
  result <- subset(ages, as.numeric(ages)==age)
  
  ## Return integer containing count of homicides for that age
  return(length(result))
}
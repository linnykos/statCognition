.synthetic_arg_grabber <- function(func, keyword = "param"){
  txt <- utils::capture.output(args(func))

  #put all into one line
  txt <- paste0(txt, collapse = " ")

  #find all the parameters
  vec <- strsplit(txt, "param([[:digit:]])*?.*?=.*?c")[[1]][-1]

  #for each element, output the bounds
  lapply(vec, .snippet)
}

.snippet <- function(str){
  comma <- regexpr(",", str)
  rbrace <- regexpr(")", str)

  val1 <- as.numeric(substr(str, 2, comma-1))
  val2 <- as.numeric(substr(str, comma+1, rbrace-1))

  c(val1, val2)
}

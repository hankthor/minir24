# The string algebra
# See the chord formatting test, the string similarity test

#-----------------------------------------------
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x);
}

#-----------------------------------------------
left <- function(x, n) {
  substr(x, 1, n);
}

#-----------------------------------------------
starts_with <- function(big, small) {
  n <- nchar(small);
  substr(big, 1, n) == small;
}


#-----------------------------------------------
replace <- function(x, ost, nst) {
  gsub(ost, nst, x, fixed=TRUE);
}

#-----------------------------------------------
regex_replace <- function(x, ost, nst) {
  gsub(ost, nst, x, fixed=FALSE);
}

#-----------------------------------------------
#END

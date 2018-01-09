qual <- function(x){
	if(is.na(x)){
		x <- 0
	} else if ( x == "Po" ){
	  x <- 1
	} else if ( x == "Fa" ){
		x <- 2
	} else if ( x == "TA" ){
		x <- 3
	} else if ( x == "Gd" ){
		x <- 4
	} else if ( x == "Ex" ){
	  x <- 5
	} else x <- 0
x <- as.numeric(x)
}
quality <- Vectorize(qual)
rm(qual)
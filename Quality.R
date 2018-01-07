qual <- function(x){
	if(x == "Po"){
		x <- 1
	} else if ( x == "Po" ){
		x <- 1
	} else if ( x == "Fa" ){
		x <- 2
	} else if ( x == "TA" ){
		x <- 3
	} else if ( x == "Gd" ){
		x <- 4
	} else x <- 0
x <- as.factor(x)
x <- levels(x)[0:5] # This line need a revision.
}
quality <- Vectorize(qual)
rm(qual)
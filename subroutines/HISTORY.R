HISTORY <-
function(principle, unique=TRUE){
	principle=toupper(principle)
	if(unique==TRUE){print(unique(graveyard$history[graveyard$history$principle==principle,]))}
	if(unique==FALSE){print(graveyard$history[graveyard$history$principle==principle,])}
}

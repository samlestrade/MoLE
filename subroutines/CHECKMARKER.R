CHECKMARKER <-
function(ID, generation='max', steps=1){
	ID=1
	if(generation=='max'){generation=length(graveyard$brains)}
	for(i in seq(2, generation, by=(steps*2))){print(graveyard$brains[[i]]$generation); print(graveyard$brains[[i]]$nouns[graveyard$brains[[i]]$nouns$ID==ID,])}
}

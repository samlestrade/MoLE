MULTIRUN <-
function(nHours=48, nLineages=3, name='lineage', nAgents=2, nSaves=4, close=TRUE, ...){
	parameters=list(...)
	if(!'data'%in%dir()){dir.create('data')}
	if(length(parameters)!=0){for(i in 1:length(parameters)){world[[names(parameters)[i]]]=parameters[[i]]}}
	for(j in 1:nLineages){
		FOUND(nAgents)
		for(i in 1:nSaves){
			RUN(nHours/nSaves)
			save.image(paste('data/', name, '-', j, '.rdata', sep=''))
	}	}
	if(close==TRUE){q('no')}
}

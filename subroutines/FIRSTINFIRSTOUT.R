FIRSTINFIRSTOUT <-
function(speakerID, proposition){
	activations=rep(0, length(proposition))	
	for (i in 1:(length(activations)-1)){	
		activations[i]=proposition[[i]]$activation
		if('referentMatch'%in%names(proposition[[i]])){activations[i]=activations[i]+proposition[[i]]$referentMatch}	
		if(!'referentMatch'%in%names(proposition[[i]])){activations[i]=activations[i]+proposition[[i]]$match}	
	}
	proposition=proposition[order(activations, decreasing=TRUE)]
proposition
}

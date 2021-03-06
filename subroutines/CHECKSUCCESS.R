CHECKSUCCESS <-
function(speakerID, proposition, situation){
	solutionMethod=world$solutionMethod; distinctiveness=world$distinctiveness; topicCopy=world$topicCopy; frequency=world$frequency; referenceThreshold=world$referenceThreshold; generalization=world$generalization; wordOrder=world$wordOrder
	speaker=population[[speakerID]]
	elaborate='unnecessary'
	verb=proposition$verb
	if(verb$type=='twoPlace'){
		external=proposition$external
		internal=proposition$internal
		external$distractorTyping=VMATCH(external[, grep('^D\\d',names(external))], verb[,grep('^Int\\d',names(verb))])
		internal$distractorTyping=VMATCH(internal[, grep('^D\\d',names(internal))], verb[,grep('^Ext\\d',names(verb))])
		if(max(external$typing, internal$typing) < (max(external$distractorTyping, internal$distractorTyping) + distinctiveness)){elaborate='necessary'}  
		if('marker'%in%names(external)  | 'marker'%in%names(internal)){elaborate='unnecessary'} 
		actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external','internal')
		if(generalization==TRUE & elaborate=='necessary'){	
			index=speaker$usageHistory$index
			yangIndex=TRUE %in% c(index$no < ((index$yes + index$no)/log(index$yes + index$no)))
			if(yangIndex==TRUE){
				externalRole=ifelse(actor=='external', 'actor', 'undergoer'); internalRole=ifelse(actor=='internal', 'actor', 'undergoer')
				externalPerson=external$person; internalPerson=internal$person
				if('extMarker'%in%names(verb)){
					data=index[index$role==externalRole & index$person==externalPerson, ]
					if(data$no < ((data$yes + data$no)/log(data$yes + data$no)) & externalPerson!=internalPerson){elaborate='unnecessary'}
				}
				if('intMarker'%in%names(verb)){
					data=index[index$role==internalRole & index$person==internalPerson, ]
					if(data$no < ((data$yes + data$no)/log(data$yes + data$no)) & externalPerson!=internalPerson){elaborate='unnecessary'}
		}	}	}
		if(elaborate=='necessary'){	
			topic=external$topic
			pronouns=speaker$nouns[speaker$nouns$person==external$person & speaker$nouns$productionEffort>referenceThreshold,]
			if(external$person!=3){
				if(nrow(pronouns)>1){
					pronouns$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], pronouns[,grep('^D\\d',names(pronouns))])	
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=TRUE),]
					external=pronouns[1,]
			}	}
			if(external$person==3){
				distractors=situation$personA
				if(ACTOR(verb[,grep('Ext\\d',names(verb))], verb[,grep('Int\\d',names(verb))])==2){distractors=situation$personU}
				if(sum(distractors==external$person, na.rm=TRUE)==1){	
					pronouns$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], pronouns[,grep('^D\\d',names(pronouns))])	
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=TRUE),]
					external=pronouns[1,]
			}	}	
			external$topic=topic
			external$typing=VMATCH(external[, grep('^D\\d',names(external))], verb[,grep('^Ext\\d',names(verb))])
			collostructionFrequency=speaker$collostructions$SV[intersect(grep(paste('^', external$ID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency
			if(length(collostructionFrequency)!=0){external$collostruction=collostructionFrequency} else {external$collostruction=0}
			pronouns=speaker$nouns[speaker$nouns$person==internal$person & speaker$nouns$productionEffort>referenceThreshold,]
			topic=internal$topic
			if(internal$person!=3){	
				if(nrow(pronouns)>1){
					pronouns$match=VMATCH(verb[,grep('^Int\\d',names(verb))], pronouns[,grep('^D\\d',names(pronouns))])	
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=TRUE),]
					internal=pronouns[1,]
			}	}
			if(internal$person==3){
				distractors=situation$personA
				if(ACTOR(verb[,grep('Ext\\d',names(verb))], verb[,grep('Int\\d',names(verb))])==2){distractors=situation$personU}
				if(sum(distractors==internal$person, na.rm=TRUE)==1){	
					pronouns$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], pronouns[,grep('^D\\d',names(pronouns))])	
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=TRUE),]
					internal=pronouns[1,]
			}	}	
			internal$topic=topic
			internal$typing=VMATCH(internal[,grep('^D\\d',names(internal))], verb[,grep('^Int\\d',names(verb))])
			collostructionFrequency=speaker$collostructions$OV[intersect(grep(paste('^', internal$ID, '$', sep=''), speaker$collostructions$OV$O),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$OV$V)),]$frequency
			if(length(collostructionFrequency)!=0){internal$collostruction=collostructionFrequency} else {internal$collostruction=0}
			external$distractorTyping=VMATCH(external[, grep('^D\\d',names(external))], verb[,grep('^Int\\d',names(verb))])
			internal$distractorTyping=VMATCH(internal[, grep('^D\\d',names(internal))], verb[,grep('^Ext\\d',names(verb))])
			if(max(external$typing, internal$typing) > (max(external$distractorTyping, internal$distractorTyping) + distinctiveness)){elaborate='unnecessary'}  
		}
		if(generalization==TRUE & wordOrder==TRUE & sum(speaker$wordOrder$success)>8){				
				yangWordOrder=TRUE %in% 
				c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
				c(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
				c(sum(speaker$wordOrder[grep('V$', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
				c(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
				c(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) 
				if(yangWordOrder==TRUE){
					standard=speaker$wordOrder$order[(sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))]
					if(length(standard)==0){
						if(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))){standard='^A'}
						if(sum(speaker$wordOrder[grep('V$', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))){standard='V$'}
						if(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))){standard='UV'}
						if(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=TRUE), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))){standard='VU'}
					}
					actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')
					undergoer=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'internal', 'external')
					order=paste(names(proposition), collapse=''); order=gsub(actor, 'A', order); order=gsub(undergoer, 'U', order); order=gsub('verb', 'V', order); order=gsub('target', '', order)
					if(grepl(standard, order)){elaborate='unnecessary'}
		}		}	
		if(elaborate=='necessary'){	
			markers=speaker$nouns[speaker$nouns$person==3, ]
			markers=markers[sample(nrow(markers)),]
			if(solutionMethod=='random'){solutionMethod=sample(c('internal', 'external'),1)}
			if(solutionMethod=='firstArgument'){solutionMethod=ifelse(grep('internal',names(proposition)) < grep('external',names(proposition)),'internal','external')}
			if(solutionMethod=='secondArgument'){solutionMethod=ifelse(grep('internal',names(proposition)) > grep('external',names(proposition)),'internal','external')}
			if(solutionMethod=='worstPerformer'){solutionMethod=ifelse((external$typing-external$distractorTyping) < (internal$typing-internal$distractorTyping), 'external', 'internal')}
			if(solutionMethod=='firstFail'){solutionMethod=ifelse(grep('internal',names(proposition)) < grep('external',names(proposition)) & internal$typing < internal$distractorTyping, 'internal','external')}
			if(solutionMethod=='bestMarker'){
				markers$match=VMATCH(verb[,grep('^Int\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markers$internalScore=CANDIDATESCORE(markers, type='nounMarker')
				markers$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markers$externalScore=CANDIDATESCORE(markers, type='nounMarker')
				if(max(markers$internalScore) == max(markers$externalScore)){solutionMethod=sample(c('internal','external'), 1)}
				if(max(markers$internalScore) > max(markers$externalScore)){solutionMethod='internal'}
				if(max(markers$internalScore) < max(markers$externalScore)){solutionMethod='external'}
			}
			if(solutionMethod=='internal' | solutionMethod=='both'){
				markers$match=VMATCH(verb[,grep('^Int\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=TRUE),]
				markers$distractorMatch=VMATCH(verb[,grep('^Ext\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markerID=0
				for (i in 1:nrow(markers)){
					if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
						break()
				}	}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=TRUE),]$ID}
				internal$markerID=markerID
				internal$marker=speaker$nouns[speaker$nouns$ID==markerID,]$form
				if(frequency=='absolute'){internal$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
				if(frequency=='relative'){internal$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$nounMarker}
				internal$markerRecency=speaker$nouns[speaker$nouns$ID==markerID,]$recency
				markerCollostruction=speaker$collostructions$OV[intersect(grep(paste('^', markerID, '$', sep=''), speaker$collostructions$OV$O),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$OV$V)),]$frequency
				if(length(markerCollostruction)!=0){internal$markerCollostruction=markerCollostruction} else {internal$markerCollostruction=0}			
			}
			if(solutionMethod=='external' | solutionMethod=='both'){
				markers$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=TRUE),]
				markers$distractorMatch=VMATCH(verb[,grep('^Int\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markerID=0
				for (i in 1:nrow(markers)){
					if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
						break()
				}	}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=TRUE),]$ID}
				external$markerID=markerID
				external$marker=speaker$nouns[speaker$nouns$ID==markerID,]$form
				if(frequency=='absolute'){external$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
				if(frequency=='relative'){external$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$nounMarker}
				external$markerRecency=speaker$nouns[speaker$nouns$ID==markerID,]$recency
				markerCollostruction=speaker$collostructions$SV[intersect(grep(paste('^', markerID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency
				if(length(markerCollostruction)!=0){external$markerCollostruction=markerCollostruction} else {external$markerCollostruction=0}			
		}	}	
		proposition$external=external
		proposition$internal=internal
	}
proposition
}

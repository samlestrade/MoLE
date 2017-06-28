SUMMARY <-
function(){
  	summary=list()
	agent=population[[1]]	
	nouns=agent$nouns
	summary$generation=agent$generation
	##success
	par(mfrow=c(1,3))
	classes=grep('D\\d', names(graveyard$brains[[1]]$nouns))/length(grep('D\\d', names(graveyard$brains[[1]]$nouns)))
	plot(graveyard$summary$generation, graveyard$summary$successRate, ylim=c(0,1), main='Success', xlab='generation', ylab='success')
	lines(lowess(graveyard$summary$generation, graveyard$summary$successRate))
	##rest
	generation=vector()
	for(i in seq(2, length(graveyard$brains), 2)){
		order=graveyard$brains[[i]]$wordOrder
			if(TRUE%in%
				c((sum(graveyard$brains[[i]]$wordOrder$success)-graveyard$brains[[i]]$wordOrder$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) |
				c(sum(graveyard$brains[[i]]$wordOrder[grep('^A', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) |
				c(sum(graveyard$brains[[i]]$wordOrder[grep('V$', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) |
				c(sum(graveyard$brains[[i]]$wordOrder[grep('UV', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) |
				c(sum(graveyard$brains[[i]]$wordOrder[grep('VU', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) 
			){generation=c(generation, graveyard$brains[[i]]$generation)}
	}	
	order=generation
	generation=vector()
	for(i in seq(2, length(graveyard$brains), 2)){
		topic=graveyard$brains[[i]]$topicPosition
		if(graveyard$brains[[i]]$topicPosition$success[2] < (sum(graveyard$brains[[i]]$topicPosition$success)/log(sum(graveyard$brains[[i]]$topicPosition$success)))){
			generation=c(generation, graveyard$brains[[i]]$generation)
	}	}
	topic=generation
	generation=vector()
	for(i in seq(2, length(graveyard$brains), 2)){
		index=graveyard$brains[[i]]$usageHistory$index
		if(TRUE%in%c(index$no<((index$yes+index$no)/log(index$yes+index$no)))){generation=c(generation, graveyard$brains[[i]]$generation)}
	}	
	index=generation
	generation=vector()
	for(i in seq(2, length(graveyard$brains), 2)){
		flag=graveyard$brains[[i]]$usageHistory$flag$person
		if(TRUE%in%c(flag$no<((flag$yes+flag$no)/log(flag$yes+flag$no)))){generation=c(generation, graveyard$brains[[i]]$generation)}
	}	
	person=generation
	generation=vector()
	for(i in seq(2, length(graveyard$brains), 2)){
		flag=graveyard$brains[[i]]$usageHistory$flag$actor
		if(TRUE%in%c(flag$no<((flag$yes+flag$no)/log(flag$yes+flag$no)))){generation=c(generation, graveyard$brains[[i]]$generation)}
	}
	actor=generation
	generation=vector()
	for(i in seq(2, length(graveyard$brains), 2)){
		flag=graveyard$brains[[i]]$usageHistory$flag$undergoer
		if(TRUE%in%c(flag$no<((flag$yes+flag$no)/log(flag$yes+flag$no)))){generation=c(generation, graveyard$brains[[i]]$generation)}
	}
	undergoer=generation
	#generalizations
	summary$order=list(order=1:agent$generation, yang=data.frame())
		summary$order$order[!summary$order$order%in%order]=0
		yang=data.frame(order=c('A first', 'V final', 'UV', 'VU'), generation=0, stringsAsFactors=FALSE)
		for (i in seq(2, length(graveyard$brains), 2)){
			generation=graveyard$brains[[i]]$generation
			order=graveyard$brains[[i]]$wordOrder
			if(length(order$order[(sum(order$success)-order$success) < (sum(order$success)/log(sum(order$success)))])!=0){
				spec=order$order[(sum(order$success)-order$success) < (sum(order$success)/log(sum(order$success)))]
				if(spec%in%yang$order){yang[yang$order==spec,]$generation=paste(yang[yang$order==spec,]$generation, generation)}
				if(!spec%in%yang$order){yang[nrow(yang)+1,]$order=spec; yang[nrow(yang),]$generation=generation}
			}
			if(sum(graveyard$brains[[i]]$wordOrder[grep('^A', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))){
				yang[yang$order=='A first', ]$generation=paste(yang[yang$order=='A first', ]$generation, generation)
			}
			if(sum(graveyard$brains[[i]]$wordOrder[grep('V$', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))){
				yang[yang$order=='V final', ]$generation=paste(yang[yang$order=='V final', ]$generation, generation)
			}
			if(sum(graveyard$brains[[i]]$wordOrder[grep('UV', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))){
				yang[yang$order=='UV', ]$generation=paste(yang[yang$order=='UV', ]$generation, generation)
			}
			if(sum(graveyard$brains[[i]]$wordOrder[grep('VU', graveyard$brains[[i]]$wordOrder$order, invert=TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))){
				yang[yang$order=='VU', ]$generation=paste(yang[yang$order=='VU', ]$generation, generation)
		}	}
		yang$generation=gsub('^0 ', '', yang$generation)
		summary$order$yang=yang
	summary$topic=1:length(graveyard$brains); summary$topic[!summary$topic%in%topic]=0
	summary$index=1:length(graveyard$brains); summary$index[!summary$index%in%index]=0
	summary$person=1:length(graveyard$brains); summary$person[!summary$person%in%person]=0
	summary$actor=1:length(graveyard$brains); summary$actor[!summary$actor%in%actor]=0
	summary$undergoer=1:length(graveyard$brains); summary$undergoer[!summary$undergoer%in%undergoer]=0
	#noun markers
	flag=agent$collostructions$flag
	flag$person=ifelse(flag$N%in%nouns[nouns$person!=3,]$ID, 'local', 'third')
	markers=head(sort(tapply(flag$frequency, flag$marker, sum), decreasing=TRUE))
	markers=data.frame(ID=names(markers), form='', roleScore=0, frequency=markers, netto=0, proportion=0, semWeight=0, local=0, third=0, stringsAsFactors=FALSE)
	for(i in 1:nrow(markers)){
		markers[i,]$form=nouns[nouns$ID==markers$ID[i],]$form
		markers[i,]$roleScore=rowMeans(nouns[nouns$ID==markers$ID[i],1:5], na.rm=TRUE)
		markers[i,]$netto=nouns[nouns$ID==markers$ID[i],]$nounMarker
		markers[i,]$semWeight=nouns[nouns$ID==markers$ID[i],]$semanticWeight
		markers[i,c('local', 'third')]=tapply(flag[flag$marker==markers$ID[i],]$frequency, flag[flag$marker==markers$ID[i],]$person, sum)
	}
	markers$proportion=markers$frequency/sum(flag$frequency)
	summary$markers=markers
	summary$nounMarkerUse1=sum(agent$collostructions$flag$frequency)/agent$age
	summary$nounMarkerUse12=sum(agent$collostructions$flag$frequency)/(agent$age*(1-world$proportionIntrans))
	#local person
	local=graveyard$brains[[1]]$nouns
	local=local[local$person!=3,]
	local$generation=0
	for (i in seq(2, length(graveyard$brains), 2)){
		new=graveyard$brains[[i]]$nouns; new=new[new$person!=3,]; new$generation=graveyard$brains[[i]]$generation
		local=rbind(local, new)
	}
	local=local[,c(ncol(local), 1:(ncol(local)-1))]
	summary$first=local[local$person==1,]
	summary$second=local[local$person==2,]
	#activation
	plot(nouns$recency, nouns$activation)
	plot(nouns$frequency, nouns$activation)
summary
}

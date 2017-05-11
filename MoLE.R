world=list(
	#general
	nAgents=2, #number of founding agents
	deathAge=2000,	
	procreationAge=.55,	#at which point (relative to their death age) do agents procreate? If NA, no offspring. Best to procreate after semUpdate;)
	crossover=T, #If true, lexicon of off spring is combination of those of parents. If false, each parent will get a child with identical lexicon 
	replace=T,	#should minor modifications be made to non-used words?
	weigh=T, 	#in comparing meanings and determining whose the actor, should meaning dimensions be equally important (F) or should first dimension be more important than second, but less important than second plus third, etc. (T). Slows down simulation in combination with high number of events per situation (>10)
	#lexicon
	distinctions=c(rep(2,5),rep(9,4)), #dimensionality and distinctionality of meaning representations (distinctions will be normalized to 0--1 range).
	wordLength=8:10,	#initial length of words
	vowels=c('a','e','i','o','u','y'),	#alphabet constituting the words
	consonants=c('b','d','f','g','h','j','k','l','m','n','p','r','s','t','w'),	
	nNouns=499,	#number of nouns in the lexicon
	nVerbs=199,	#number of verbs in the lexicon
	proportionIntrans=0.2,	#proportion of intransitive verbs in both lexicon and events. Probably .5 in real life, but smaller in the interest of argument marking
	linkingPreference=5,	#preference of external (internal) predicate role for higher (lower) values ("prominent performers"). linkingPreference is odds of highest against lowest role/value. 1 is no preference.
	local=T, 	#do agents have the words/the possibility to refer to themselves
	#situation
	useCommonGround=T, 	#do speech participants share a common ground or are all words/concepts equally likely and accessible.
	commonGroundStart=3, #number of elements (excluding speech participants) that are present in common ground when conversation starts. Elements are randomly selected from lexicon.
	dahlS=c(21, 10, 21, 44),	#odds for intransitive subject to be 1, 2, 3Animate, and 3Inanimate person respectively (based on Dahl 2000, 45-51)
	dahlA=c(38, 22, 33, 7),	#odds for external role to be 1, 2, 3Animate, and 3Inanimate person. First three numbers are summed if local==F. 
	dahlO=c(3, 3, 10, 84),	#odds for internal role to be 1, 2, 3Animate, and 3Inanimate person. 
	oddsNewA=1/30,	#odds for a non common-ground element to enter as A argument of one of the events in the situation (element will be added to the common ground if discussed; cf. DuBois 1987: 828, Table 7)
	oddsNewOther=1/4,	#odds for a non common-ground element to enter as S or O argument of one of the events in the situation (element will be added to the common ground if discussed)
	referenceNoise=.2,	#how much "referential" noise is there in the world (0--1)? The less noise, the closer the world matches the concepts and relations in the language.
	roleNoise=.3,		#how much noise is there in the world with respect to the event roles that nouns are expected and found to perform. 
	nEvents=10:20,		#number of events that are ongoing in speech situation, one of which is selected to talk about. If set to 1, no distractor events occur.  
	#conversation
	nTurns=5:20, 	#what is the range of communicative turns conversations consist of (before common ground is reset)
	talkAge=.05, 	#at which point (relative to their death age) do agents start to talk? (Until then, they only listen) If zero, less learning from parents
	turnChange=c(2,1),	#odds for speech-act participants to change speech-act roles
	personTopicality=c(2, 1, 2, 2),	#preference for speaker, addressee, animate third person, and inanimate third person respectively to be the topic of the utterance and participant in a situation (based on Dahl's S and A numbers)
	topicContinuity=c(3,1),	#odds for continuing with the same topic vs starting a new one
	#production
	checkSuccess=T, 	#should expected recovery of meaning be checked?
	solutionMethod='bestMarker',		#if check success shows utterance should be elaborated, how is this done? Otions: firstFail, bestMarker, worstPerformer, random, secondArgument, internal, external, both
	reductionFrequencyThreshold=.05,	#relative frequency threshold at which forms get reduced. 
	reductionCollostructionThreshold=3,	#absolute collostruction-frequency threshold at which forms get reduced.
	reductionRecencyThreshold=2,	#idem for recency
	formSetFrequency=3, #number of times an item has to be used before its form is set, after which its representation will no longer change
	suffixThreshold=6, #productionEffort threshold (=nchar) at which words markers suffixed to their host
	refCheck=T, 	#should referential threshold be reached for words to refer?
	referenceThreshold=4, #production effort (=nchar)  necessary for an utterance to be sufficiently referential (a la Ariel). If lower, a more expressive expression is added sentence first.
	generalization=T,	#from second generation onwards only
	firstInFirstOut=T,	#is utterance production incremental?
	#production/interpretation
	distinctiveness=0.05,	#if two forms are similar in meaning (or in role typing in case of global marking), how big should the difference be for the speaker to think the distinction is sufficiently clear?
	candidateScoring='all',	#In what order should candidates be considered (first one to suffice is selected): by activation, frequency, match, economy, collostruction, all. 
	frequency='relative',	#as argument, or role or index marker vs absolute
	activationImpact=1/5,	#if candidateOrdering=='all', how should (rescaled) activation be weighed with respect to match? Activation is function of frequency and recency. Impact=1:equally, impact=below 1: impact times less important, impact=above 1: impact times more important.
	collostructionImpact=1/5,	#if candidateOrdering=='all', how should (rescaled) collostruction frequency be weighed with respect to match? Also used by VERBMORPHOLOGY
	semanticWeightImpact=1/10,	#if candidateOrdering=='all', how should semantic weight be weighed with respect to match (given Grice: do not say more than necessary)
	economyImpact=1/10,	#if candidateOrdering=='all', how should economy be weighed with respect to match (given Grice: do not say more than necessary)
	recencyDamper=10,	#decreases activation of most recent items (a la Mandelbrot) [RESCALE(jitter(log((frequency+1)/(recency+1+recencyImpact)), factor=activationNoise))]
	activationNoise=2,	#noise factor that is added to activation values of items [RESCALE(jitter(log((frequency+1)/(recency+1+recencyImpact)), factor=activationNoise))]
	functionBlocking=T, 	#should frequent usage for some function (argument, role marker, index marker) inhibit other functions? (only applies if frequency=relative). And: should reference to certain person values block others?
	wordOrder=T,	#should agents try to use word-order generalizations to mark/determine roles?
	topicCopy=T,	#should a (pronominal) copy of a reestablished topic be put adjacent to the verb (a la Givon; only applies if topicFirst has been derived)? 
	#change
	semUpdateAge=.5, #at which point (relative to their death age) do agents update their lexical representations? Should be lower than procreationAge for cultural evolution to apply
	erosion=T,	#should forms erode?
	erosionMax=2,	#how short may form representations become in number of characters?
	formBlocking=T,	#should agent refrain from reducing forms if this leads to ambiguity?
	desemanticization=T, 	#should forms desemanticize?
	desemanticizationCeiling=.4, #percentage of utterances in which an item occurs at which it desemanticizes maximally (.3?)
	desemanticizationPower=2,	#Development of thresholds for subsequent dimensions to be removed. 1 for linear development. Best between 1 and 2? The lower, the more difficult to desemanticize, as the threshold develops linearly to the same target (desemanticizationCeiling)
	minimalSpecification=1,	#minimum number of dimensions along which referential items have to be specified (in the presence of other candidate expressions for same person). If null, words will be replaced once meaningless
	verbalRoleMarker=F, 	#can VMs be distinctive for role (within person)? Cf. Bhat...
	semUpdateThreshold=.02, 	#proportion of number of utterances in which a construction has to occur before it is fused/lexicalized
	#data management
	saveAll=F	#do not store usageHistory in graveyard
)

RESCALE=function(x){ 
	if(max(x)==min(x)){x=rep(0, length(x))}
	if(max(x)!=min(x)){max=max(abs(x)); x=x/max}
x
}

MAX=function(vector, rank=1, value=F, rank.adjust=T, forceChoice=F){
	max=sort(unique(vector),decreasing=T)
	if(length(max)==1){
		if(forceChoice==T){index=sample(length(vector), 1)}
		if(forceChoice==F){index=1:length(vector)}
	}
	if(length(max) > 1){
		if(rank.adjust==T){if(rank[length(rank)] > length(max)){rank=min(rank):length(max)}} #don't ask for more than there is
		max=max[rank] 
		index=1:length(vector)
		index=index[vector%in%max]
		if(length(index) > length(rank) & forceChoice==T){
			if(length(rank)==1){index=index[sample(length(index), 1)]}
			if(length(rank) > 1){index=index[order(vector[index], decreasing=T)[1:length(rank)]]}
	}	}	
	if(value==T){out=vector[index]}
	if(value==F){out=index}
out
}

MIN=function(vector, rank=1, value=F, rank.adjust=T, forceChoice=F){
	min=sort(unique(vector),decreasing=F)
	if(length(min)==1){
		if(forceChoice==T){index=sample(length(vector), 1)}
		if(forceChoice==F){index=1:length(vector)}
	}
	if(length(min) > 1){
		if(rank.adjust==T){if(rank[length(rank)] > length(min)){rank=min(rank):length(min)}}
		min=min[rank]
		index=1:length(vector)
		index=index[vector%in%min]
		if(length(index) > length(rank) & forceChoice==T){
			if(length(rank)==1){index=index[sample(length(index), 1)]}
			if(length(rank) > 1){index=index[order(vector[index], decreasing=F)[1:length(rank)]]}
	}	}	
	if(value==T){out=vector[index]}
	if(value==F){out=index}
out
}

ALLNAS=function(x){
	if(is.data.frame(x)){allnas=rowSums(is.na(x))==ncol(x)}
	if(is.vector(x)){allnas=sum(is.na(x))==length(x)}
as.vector(allnas)
}

#returns -1 for vectors with NA only
VMATCH=function(x, y, incomparable=0){
	weigh=world$weigh
	if(length(x) <= length(y)){dimensions=1:length(x)}; if(length(y) < length(x)){dimensions=1:length(y)}
	x=x[dimensions]; y=y[dimensions]
	impacts=rep(1, length(dimensions))	
	if(weigh==T){impacts=length(dimensions):1}
	if(is.data.frame(y)){
		if(nrow(y) > 1){
			if(is.vector(x)){z=as.data.frame(t(replicate(nrow(y),x)), stringsAsFactors=F)}
			if(is.data.frame(x)){z=as.data.frame(t(replicate(nrow(y),unlist(x))), stringsAsFactors=F)}
			diffs=abs(z-y)
			impacts=as.data.frame(t(replicate(nrow(diffs),impacts)), stringsAsFactors=F)
			impacts[is.na(diffs)]=NA
			diffs=diffs*impacts
			vmatch=1-(rowSums(diffs, na.rm=T)/rowSums(impacts, na.rm=T))
			vmatch[ALLNAS(diffs)]=incomparable
		} else {
			diffs=abs(x-y)
			diffs=diffs*impacts
			vmatch=ifelse(ALLNAS(diffs), incomparable, 1-(rowSums(diffs, na.rm=T)/sum(impacts[!(is.na(x) | is.na(y))])))
		}
	} else {
		diffs=abs(x-y)
		diffs=diffs*impacts
		vmatch=ifelse(ALLNAS(diffs), incomparable, 1-(sum(diffs, na.rm=T)/sum(impacts[!(is.na(x) | is.na(y))])))
	}
vmatch
}

FMATCH=function(target, lexicon){	#calculates edit distance
	ncol=max(nchar(target), nchar(lexicon$form))
	forms=data.frame(matrix(0, nrow=nrow(lexicon), ncol=ncol), stringsAsFactors=F)
	forms$form=lexicon$form
	for (i in 1:ncol){
		forms[,i]=gsub(paste('.{', i-1,'}(.).*', sep=''),'\\1',target)!=gsub(paste('.{', i-1,'}(.).*', sep=''),'\\1',forms$form)
	}
	max=nchar(target)
	forms[,1:max]=forms[,1:max]*t(replicate(nrow(forms), max:1))	#onset priority
	forms$distance=rowSums(forms[,1:ncol])
	fmatch=1/(1+forms$distance)
fmatch
}

ACTOR=function(x, y){
	x=VMATCH(rep(1, length(x)), x)
	y=VMATCH(rep(1, length(y)), y)
	actor=ifelse(x<y,2,1)	#if tie, first argument is actor
actor
}

FORMS=function(n, length=world$wordLength, vowels=world$vowels, consonants=world$consonants){
	NEWFORM=function(length){
		if(length(length)>1){length=sample(length, 1)}
		form=rep(c('C','V'),length)
		start=sample(2,1)	#allow for CV and VC
		form=form[start:(length + start-1)]
		form[form=='V']=sample(vowels, sum(form=='V'), replace=T)
		form[form=='C']=sample(consonants, sum(form=='C'), replace=T)
		form=paste(form, collapse='')
	}	
	forms=NEWFORM(length)
	ADDFORM=function(length){
		word=NEWFORM(length)
		if(!(word%in%forms)){
			forms[length(forms) + 1]=word
		}
		forms
	}
	while(length(forms) < n){forms=ADDFORM(length)}
forms
}

VERBS=function(n=world$nVerbs){
	cut=F; if(n==1){n=2; cut=T} #minimally 2 entries for dataframe
	distinctions=world$distinctions; linkingPreference=world$linkingPreference; proportionIntrans=world$proportionIntrans; reductionRecencyThreshold=world$reductionRecencyThreshold
	cols=length(distinctions)
	verbs=as.data.frame(replicate(cols,rep(0,n)),stringsAsFactors=F)
	names(verbs)=gsub('^V(\\d)','D\\1',names(verbs))
	for (i in 1:cols){verbs[,i]=sample(seq(0,1,length.out=distinctions[i]),n,rep=T)}
	external=as.data.frame(replicate(cols,rep(0,n)),stringsAsFactors=F)
	for (i in 1:cols){external[,i]=sample(seq(1,0,length.out=distinctions[i]),n,rep=T, prob=seq(linkingPreference, 1, length.out=distinctions[i]))}	#external role of two-place predicates are more prominent
	internal=as.data.frame(replicate(cols,rep(0,n)),stringsAsFactors=F)
	for (i in 1:cols){internal[,i]=sample(seq(1,0,length.out=distinctions[i]),n,rep=T, prob=seq(1, linkingPreference, length.out=distinctions[i]))}
	names(external)=gsub('^V(\\d)','Ext\\1',names(external))
	names(internal)=gsub('^V(\\d)','Int\\1',names(internal))
	if(proportionIntrans > 0){
		intrans=sample(n,ceiling(proportionIntrans*n),replace=F)
		internal[intrans,]=NA
		for (i in 1:cols){external[intrans,i]=sample(seq(0,1,length.out=distinctions[i]),length(intrans),rep=T)}	#external roles of intransitives can be anything
	}	
	verbs=cbind(verbs, external, internal)	
	verbs$type='twoPlace'
	if(proportionIntrans > 0){verbs[intrans,]$type='onePlace'}
	verbs$ID=1:n
	verbs$form=FORMS(n)
	verbs$frequency=0
	verbs$recency=reductionRecencyThreshold
	verbs$activation=0
	verbs$productionEffort=nchar(verbs$form)
	verbs$semanticWeight=1
	if(cut==T){verbs=verbs[1,]}
verbs
}

NOUNS=function(n=world$nNouns, local=world$local){
	cut=F; if(n==1){n=2; cut=T}	#minimally 2 entries for dataframe
	distinctions=world$distinctions; reductionRecencyThreshold=world$reductionRecencyThreshold
	cols=length(distinctions)
	nouns=as.data.frame(replicate(cols,rep(0,n)), stringsAsFactors=F)
	names(nouns)=gsub('^V(\\d)','D\\1',names(nouns))
	for (i in 1:cols)
		nouns[,i]=sample(seq(0,1,length.out=distinctions[i]),n,rep=T)
	nouns$person=3
	nouns$ID=1:n
	nouns$form=FORMS(n)
	nouns$frequency=0
	nouns$argument=0
	nouns$nounMarker=0
	nouns$verbMarker=0
	nouns$recency=reductionRecencyThreshold
	nouns$activation=0
	nouns$productionEffort=nchar(nouns$form)
	nouns$semanticWeight=1
	if(local==T){nouns[MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1:2, forceChoice=T),]$person=1:2}
	if(cut==T){nouns=nouns[1,]}
nouns
}

FIRSTSPEAKER=function(){
	local=world$local; distinctions=world$distinctions
	verbs=VERBS()
	nouns=NOUNS()
	values=vector(); for(i in 1:length(distinctions)){values=c(values, seq(0,1,length.out=distinctions[i]))}
	flag=list(
		person=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=F), 
		actor=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=F),
		undergoer=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=F)
	)
	usageHistory=list(
		verbs=data.frame(matrix(0, nrow=0, ncol=length(grep('D\\d',names(verbs))) + 1, dimnames=list(NULL,c('verb',paste('D',1:9, sep='')))), stringsAsFactors=F),
		nouns=data.frame(matrix(0, nrow=0, ncol=length(grep('D\\d',names(nouns))) + 1, dimnames=list(NULL,c('noun',paste('D',1:9, sep='')))), stringsAsFactors=F),
		index=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=F),	#cf. Yang's log operations later on. Only with minimally 4 exceptions, yang threshold is minority indeed
		flag=flag
	)
	flag=data.frame(marker=1, N=1, frequency=0, stringsAsFactors=F); flag=flag[-1,]
	index=data.frame(marker=1, N=1, frequency=0, stringsAsFactors=F); index=index[-1,]
	SV=data.frame(S=1, V=1, frequency=0, stringsAsFactors=F); SV=SV[-1,]
	OV=data.frame(O=1,V=1, frequency=0, stringsAsFactors=F); OV=OV[-1,]
	collostructions=list(SV=SV, OV=OV, index=index, flag=flag)
	wordOrder=data.frame(order=c('AVU', 'AUV', 'VAU', 'VUA', 'UAV', 'UVA'), freq=0, success=0, stringsAsFactors=F)
	topic=data.frame(role=c('actor','undergoer'), topic=0, stringsAsFactors=F)
	topicPosition=data.frame(position=c('first', 'other'), freq=0, success=0, stringsAsFactors=F)
	commonGround=vector()
	list(age=0, generation=1, fertile='yes', semupdate=0, verbs=verbs, nouns=nouns, usageHistory=usageHistory, commonGround=commonGround, collostructions=collostructions, topic=topic, wordOrder=wordOrder, topicPosition=topicPosition)
}

FOUND=function(nAgents=world$nAgents){
	firstspeaker=FIRSTSPEAKER()
	population<<- list()
	for (i in 1:nAgents){population[[i]]<<- firstspeaker}
	names(population)<<- toupper(FORMS(nAgents, length=6))
	graveyard<<- list(summary=data.frame(name=as.character(vector()), generation=as.numeric(vector()), successRate=as.numeric(vector()), meanNounMarkerFrequency=as.numeric(vector()), topnounMarkerFrequency=as.numeric(vector()), meanverbMarkerFrequency=as.numeric(vector()), topverbMarkerFrequency=as.numeric(vector()), stringsAsFactors=F), brains=list(), history=data.frame(generation=vector(), change=vector(), principle=vector(), ID=vector(), person=vector(), N=vector(), marker=vector()), dataBase=list())
	graveyard$brains[[length(graveyard$brains) + 1]]<<- population[[1]]
	graveyard$brains[[length(graveyard$brains)]]$generation<<- 0
}


################ PRODUCTION GRAMMAR ###################

SITUATION=function(speakerID){
	useCommonGround=world$useCommonGround; commonGroundStart=world$commonGroundStart; dahlS=world$dahlS; dahlA=world$dahlA; dahlO=world$dahlO; oddsNewA=world$oddsNewA; oddsNewOther=world$oddsNewOther; distinctions=world$distinctions; proportionIntrans=world$proportionIntrans; nEvents=world$nEvents; roleNoise=world$roleNoise; referenceNoise=world$referenceNoise; local=world$local; personTopicality=world$personTopicality; roleTopicality=world$roleTopicality; topicContinuity=world$topicContinuity; oddsSg=world$oddsSg
	if(local==T){	#local person always known: adjust oddsNew for Dahl numbers
		oddsNewA=oddsNewA/(sum(world$dahlA[3:4])/sum(world$dahlA))	
		oddsNewOther=oddsNewOther/(sum(world$dahlS[3:4]+world$dahlO[3:4])/sum(world$dahlS+world$dahlO))
	}
	speaker=population[[speakerID]]
	nEvents=sample(nEvents, 1, prob=nEvents)	#situations with multiple events are more likely than situations with single event
	#develop commonGround
	if(useCommonGround==T){
		commonGround=speaker$commonGround	
		if(length(commonGround) < commonGroundStart){
			commonGround=c(commonGround, sample(speaker$nouns[!speaker$nouns$ID%in%commonGround,]$ID, commonGroundStart-length(commonGround))) 
	}	} 
	#preparation
	power=9*(1-roleNoise)	#more noise->lower power, reduces impact of VMATCH in type-match selection below
	intransVerbs=speaker$verbs[speaker$verbs$type=='onePlace',]	
	transVerbs=speaker$verbs[speaker$verbs$type=='twoPlace',]	
	nouns=speaker$nouns	
	#intrans events (external)
	intransExternals=vector(); intransActions=vector(); intransExternalsPerson=vector()	#will be overwritten if applicable, otherwise necessary for later
	nIntrans=round(proportionIntrans*nEvents,0)
	if(nIntrans==0){nIntrans=sample(0:1,1, prob=c(1-proportionIntrans, proportionIntrans))}	#otherwise, with low number of events and low prop of intransitives, never intransitive events
	if(nIntrans!=0){
		if(local==F){	#count locals as animates
			intransExternals=sample(c('3A', '3I'), nIntrans, replace=T, prob=c(sum(dahlS[1:3]),dahlS[4]))
			intransExternalsPerson=intransExternals
		}	
		if(local==T){
			intransExternals=sample(c(1, 2, '3A', '3I'), nIntrans, replace=T, prob=dahlS)
			intransExternalsPerson=intransExternals
			intransExternals[intransExternals%in%1:2]=nouns[MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1, forceChoice=T),]$ID
		}
		if(useCommonGround==T){
			intransExternals[intransExternals%in%c('3A','3I')]=paste(intransExternals[intransExternals%in%c('3A','3I')],sample(c('old','new'), length(intransExternals[intransExternals%in%c('3A','3I')]), replace=T, prob=c(1, oddsNewOther)), sep='')
			intransExternals[intransExternals=='3Aold']=sample(commonGround, sum(intransExternals=='3Aold'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			intransExternals[intransExternals=='3Iold']=sample(commonGround, sum(intransExternals=='3Iold'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			intransExternals[intransExternals=='3Anew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), sum(intransExternals=='3Anew'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			intransExternals[intransExternals=='3Inew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), sum(intransExternals=='3Inew'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
		}
		if(useCommonGround==F){
			intransExternals[intransExternals=='3A']=sample(nouns[nouns$person==3,]$ID, sum(intransExternals=='3A'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3,]))	
			intransExternals[intransExternals=='3I']=sample(nouns[nouns$person==3,]$ID, sum(intransExternals=='3I'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3,]))	
		}
		intransActions=intransVerbs[1:length(intransExternals),]
		for (i in 1:nrow(intransActions)){intransActions[i,]=intransVerbs[sample(nrow(intransVerbs), 1, prob=VMATCH(nouns[nouns$ID==intransExternals[i],],intransVerbs[,grep('^Ext\\d',names(intransVerbs))])^power),]}
	}
	#trans events
	transExternals=vector(); transActions=vector(); transExternalsPerson=vector(); internals=vector(); transActions=vector(); internalsPerson=vector()
	nTrans=(nEvents-nIntrans)
	if(nTrans!=0){
		if(local==F){
			transExternals=sample(c('3A', '3I'), nTrans, replace=T, prob=c(sum(dahlA[1:3]),dahlA[4]))
			internals=sample(c('3A', '3I'), nTrans, replace=T, prob=c(sum(dahlO[1:3]),dahlO[4]))
			transExternalsPerson=transExternals
			internalsPerson=internals
		}
		if(local==T){
			transExternals=sample(c(1, 2, '3A', '3I'), nTrans, replace=T, prob=dahlA)
			transExternalsPerson=transExternals
			internals=sample(c(1, 2, '3A', '3I'), nTrans, replace=T, prob=dahlO)
			internalsPerson=internals
			transExternals[transExternals%in%1:2]=nouns[MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1, forceChoice=T),]$ID
			internals[internals%in%1:2]=nouns[MAX(VMATCH(rep(1, length(distinctions)), nouns[, grep('D\\d', names(nouns))]), 1, forceChoice=T),]$ID
		}
		if(useCommonGround==T){
			transExternals[transExternals%in%c('3A','3I')]=paste(transExternals[transExternals%in%c('3A','3I')],sample(c('old','new'), length(transExternals[transExternals%in%c('3A','3I')]), replace=T, prob=c(1, oddsNewA)), sep='')
			internals[internals%in%c('3A','3I')]=paste(internals[internals%in%c('3A','3I')],sample(c('old','new'), length(internals[internals%in%c('3A','3I')]), replace=T, prob=c(1, oddsNewOther)), sep='')
			transExternals[transExternals=='3Aold']=sample(commonGround, 			sum(transExternals=='3Aold'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			transExternals[transExternals=='3Iold']=sample(commonGround, 			sum(transExternals=='3Iold'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			transExternals[transExternals=='3Anew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(transExternals=='3Anew'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			transExternals[transExternals=='3Inew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(transExternals=='3Inew'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			internals[internals=='3Aold']=sample(commonGround, 			sum(internals=='3Aold'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			internals[internals=='3Iold']=sample(commonGround, 			sum(internals=='3Iold'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$ID%in%commonGround,]))	
			internals[internals=='3Anew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(internals=='3Anew'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
			internals[internals=='3Inew']=sample(setdiff(nouns[nouns$person==3,]$ID, commonGround), 	sum(internals=='3Inew'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3 & !nouns$ID%in%commonGround,]))	
		}
		if(useCommonGround==F){
			transExternals[transExternals=='3A']=sample(nouns[nouns$person==3,]$ID, sum(transExternals=='3A'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person==3,]))	
			transExternals[transExternals=='3I']=sample(nouns[nouns$person==3,]$ID, sum(transExternals=='3I'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3,]))	
			internals[internals=='3A']=sample(nouns[nouns$person==3,]$ID, 	sum(internals=='3A'), replace=T, prob=VMATCH(rep(1, length(distinctions)), nouns[nouns$person== 3,]))	
			internals[internals=='3I']=sample(nouns[nouns$person==3,]$ID, 	sum(internals=='3I'), replace=T, prob=VMATCH(rep(0, length(distinctions)), nouns[nouns$person==3,]))	
		}
		transActions=transVerbs[1:length(transExternals),]
		for (i in 1:nrow(transActions)){transActions[i,]=transVerbs[sample(nrow(transVerbs), 1, prob=((VMATCH(nouns[nouns$ID==transExternals[i],],transVerbs[,grep('^Ext\\d',names(transVerbs))])+VMATCH(nouns[nouns$ID==internals[i],],transVerbs[,grep('^Int\\d',names(transVerbs))]))/2)^power),]}
	}	
	actors=nouns[match(c(intransExternals, transExternals),nouns$ID),c(grep('D\\d',names(nouns)),grep('person',names(nouns)))]
	names(actors)=gsub('D','A', names(actors))
	names(actors)=gsub('person','personA', names(actors))
	undergoers=nouns[match(c(intransExternals, internals),nouns$ID), c(grep('D\\d',names(nouns)),grep('person',names(nouns)))]	#include externals for equal-sized data frame
	names(undergoers)=gsub('D','U',names(undergoers))
	names(undergoers)=gsub('person','personU',names(undergoers))
	undergoers$personU=c(rep(NA, nIntrans), internalsPerson)
	actors$personA=c(intransExternalsPerson, transExternalsPerson)
	actions=rbind(intransActions, transActions)
	#make sure actor of action is indeed in actor position (verb selection is independent of this, only about match with V columns)
	for(i in 1:nrow(actions)){
		if(ACTOR(actions[i, grep('Ext\\d',names(actions))], actions[i, grep('Int\\d',names(actions))])==2){ff=actors[i,]; actors[i,]=undergoers[i,]; undergoers[i,]=ff}
	}
	intrans=grep('onePlace',actions$type)
	actions=actions[,grep('D\\d',names(actions))]
	names(actions)=gsub('D','V',names(actions))
	situation=cbind(actions, actors, undergoers)
	targetCols=sample((1:ncol(situation))[-grep('person',names(situation))], round(referenceNoise*length(grep('[^person]',names(situation)))), prob=rep(1:length(distinctions), 3))	#@prob: higher meaning cols more likely to vary
	nRows=round(referenceNoise*nrow(situation))
	if(nRows==0){nRows=sample(0:1,1, prob=c(1-referenceNoise, referenceNoise))}	#if number of rows is small, still allow for noise
	targetRows=sample(nrow(situation), nRows)
	if(length(targetCols) > 0 & length(targetRows) > 0){
		for(i in targetCols){
			situation[targetRows,i]=sample(seq(0, 1, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[i])))]), replace=T, length(targetRows))	
	}	}
	#make sure referents are fully specified
	for (j in grep('^V',names(situation))){	#fill V columns randomly
		if(NA%in%situation[,j]){
			situation[is.na(situation[,j]),j]=sample(seq(0, 1, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]), sum(is.na(situation[,j])), replace=T)
	}	}
	for (j in grep('^A',names(situation))){	#A columns with more prominent values	
		if(NA%in%situation[,j]){
			situation[is.na(situation[,j]),j]=sample(seq(0, 1, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]), sum(is.na(situation[,j])), replace=T, prob=seq(1,3,length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]))
	}	}
	for (j in grep('^U',names(situation))){	#and U columns with lower values	
		if(NA%in%situation[,j]){
			situation[is.na(situation[,j]),j]=sample(seq(1, 0, length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]), sum(is.na(situation[,j])), replace=T, prob=seq(1,3,length.out=distinctions[as.numeric(sub('.*?(\\d+)','\\1',names(situation[j])))]))
	}	}
	continueTopic=sample(1:0, 1, prob=topicContinuity)
	situation$target=0
	situation$topic=''
	if(continueTopic==1){
		topicCandidates=nouns[nouns$recency==min(nouns$recency),]
		if(nrow(topicCandidates)>1){topicCandidates=topicCandidates[max(topicCandidates$argument, topicCandidates$verbMarker) >= topicCandidates$nounMarker,]}
		if(nrow(topicCandidates)==0){topicCandidates=nouns[nouns$recency==min(nouns$recency),]} #if none of the candidates qualifies argument criterium, only use recency
		topic=ifelse(nrow(topicCandidates)>1, sample(topicCandidates$ID,1), topicCandidates$ID)	
		if(nouns[nouns$ID==topic,]$person==3){	
			matchA=VMATCH(nouns[nouns$ID==topic,], situation[,grep('^A\\d', names(situation))]); matchA[grep(3, situation$personA, invert=T)]=-1
			target=MAX(matchA, forceChoice=T)
			situation[target, grep('^A\\d', names(situation))]=nouns[nouns$ID==topic,grep('^D\\d', names(nouns))]	#replace referential properties of topic
			situation$target[target]=1
			situation$topic[target]='actor'
			#DuBois: pref for actor topic..., otherwise:
			#matchU=VMATCH(nouns[nouns$ID==topic,], situation[,grep('^U\\d', names(situation))]); matchU[grep(3, situation$personU, invert=T)]=-1
			#if(max(matchU)>max(matchA)){target=MAX(matchU, forceChoice=T); situation[target, grep('^U\\d', names(situation))]=nouns[nouns$ID==topic,grep('^D\\d', names(nouns))]; situation$target[target]=1; situation$topic[target]='undergoer'}
			#if(max(matchA)>=max(matchU)){target=MAX(matchA, forceChoice=T); situation[target, grep('^A\\d', names(situation))]=nouns[nouns$ID==topic,grep('^D\\d', names(nouns))]; situation$target[target]=1; situation$topic[target]='actor'}		
		}
		if(nouns[nouns$ID==topic,]$person!=3){
			personTopic=nouns[nouns$ID==topic,]$person
			targetCandidates=unique(grep(personTopic, situation$personA), grep(personTopic, situation$personU))
			if(length(targetCandidates)==0){targetCandidates=1:nrow(situation)}
			target=ifelse(length(targetCandidates)==1, targetCandidates, sample(targetCandidates, 1))
			situation$target[target]=1
			situation$topic[target]=ifelse(situation$personA[target]==personTopic, 'actor', 'undergoer')
	}	}
	if(continueTopic==0){
		personTopic=sample(c(1,2,'3A','3I'),1, prob=personTopicality)
		targetCandidates=unique(grep(personTopic, situation$personA), grep(personTopic, situation$personU))
		if(length(targetCandidates)==0){targetCandidates=1:nrow(situation)}
		target=ifelse(length(targetCandidates)==1, targetCandidates, sample(targetCandidates, 1))
		situation$target[target]=1
		situation$topic[target]=ifelse(situation$personA[target]==personTopic, 'actor', 'undergoer')
	}
	if(is.na(situation[target,]$personU) & situation[target,]$topic=='undergoer'){situation[target,]$topic='actor'}
	situation$personA=gsub('A|I','',situation$personA)
	situation$personU=gsub('A|I','',situation$personU)
	if(length(intrans)!=0){
		situation[intrans, grep('^U\\d',names(situation))]=NA
	}
unique(situation)
}

CANDIDATESCORE=function(lexicon, type='referringExpression'){
	candidateScoring=world$candidateScoring; frequency=world$frequency; functionBlocking=world$functionBlocking; distinctiveness=world$distinctiveness; activationImpact=world$activationImpact; semanticWeightImpact=world$semanticWeightImpact; economyImpact=world$economyImpact; collostructionImpact=world$collostructionImpact
	if(candidateScoring=='match'){candidateScore=lexicon$match}
	if(candidateScoring=='semanticWeight'){candidateScore=1/lexicon$semanticWeight}	#the lighter, the better
	if(candidateScoring=='economy'){candidateScore=1/lexicon$productionEffort}
	if(candidateScoring=='recency'){candidateScore=1/(lexicon$recency + 1)}	#recency starts with 0...
	if(candidateScoring=='collostruction'){candidateScore=Score(lexicon$collostruction, decreasing=T)}
	if(candidateScoring=='frequency'){
		if(frequency=='absolute' | !'argument'%in%names(lexicon)){candidateScore=lexicon$frequency}	
		if(frequency=='relative' & 'argument'%in%names(lexicon)){	#i.e., if noun lexicon is involved
			if(type=='referringExpression'){
				if(functionBlocking==F){candidateScore=lexicon$argument}
				if(functionBlocking==T){candidateScore=lexicon$argument-lexicon$nounMarker-lexicon$verbMarker}
			}
			if(type=='nounMarker'){
				if(functionBlocking==F){candidateScore=lexicon$nounMarker}
				if(functionBlocking==T){candidateScore=lexicon$nounMarker-lexicon$argument-lexicon$verbMarker}
			}
			if(type=='verbMarker'){
				if(functionBlocking==F){candidateScore=lexicon$verbMarker}
				if(functionBlocking==T){candidateScore=lexicon$verbMarker-lexicon$argument-lexicon$nounMarker}
	}	}	}
	if(candidateScoring=='activation'){candidateScore=lexicon$activation}
	if(candidateScoring=='all'){
		if(type=='nounMarker'){lexicon$collostruction=lexicon$nounMarker}
		if(type=='verbMarker'){lexicon$collostruction=lexicon$verbMarker}
		if(type=='pronoun'){lexicon$collostruction=lexicon$argument}
		candidateScore=lexicon$match + 
			activationImpact*lexicon$activation +  	
			collostructionImpact*RESCALE(lexicon$collostruction) +
			semanticWeightImpact*min(lexicon$semanticWeight)/lexicon$semanticWeight + 	#lighter is better
			economyImpact*min(lexicon$productionEffort)/lexicon$productionEffort	
	}		
candidateScore
}

SELECTVERB=function(speakerID, situation, actorID=NULL, undergoerID=NULL){
	roleNoise=world$roleNoise; distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	verbTarget=target[,grep('^V\\d',names(target))]
	targetTransitivity=ifelse(is.na(target$personU), 'onePlace', 'twoPlace')
	verbs=speaker$verbs[speaker$verbs$type==targetTransitivity,]
	verbs=verbs[sample(nrow(verbs)),]
	verbs$match=VMATCH(verbTarget, verbs[,grep('^D\\d',names(verbs))])
	verbs$collostruction=0
	if(!is.null(actorID) & is.null(undergoerID)){	#simplification: not known at this point how grammatical and semantic rol match up
		collostructions=speaker$collostructions$SV[speaker$collostructions$SV$S==actorID,]
		verbs[verbs$ID%in%collostructions$V,]$collostruction=collostructions[na.omit(match(verbs$ID, collostructions$V)),]$frequency
	}
	if(!is.null(undergoerID)){
		collostructions=speaker$collostructions$OV[speaker$collostructions$OV$O==undergoerID,]
		verbs[verbs$ID%in%collostructions$V,]$collostruction=collostructions[na.omit(match(verbs$ID, collostructions$V)),]$frequency
	}
	verbOrder=order(CANDIDATESCORE(verbs, type='referringExpression'), decreasing=T)
	verb=''
	if(nrow(situation) > 1){
		verbDistractors=situation[situation$target==0,]
		verbDistractors=unique(verbDistractors[,grep('^V\\d',names(verbDistractors))])
		verbDistractors=verbDistractors[!VMATCH(verbTarget, verbDistractors)==1,]
		if(nrow(verbDistractors)!=0){
			for (i in verbOrder){
				distractorMatch=MAX(VMATCH(verbs[i,grep('^D\\d',names(verbs))], verbDistractors), value=T, forceChoice=T)
				if(verbs[i,]$match > (distractorMatch + distinctiveness)){
					verb=verbs[i,]
					break()
	}	}	}	}
	if(nrow(situation)==1){	
		for (i in verbOrder){
			if(verbs[i,]$match > max(verbs[i,]$match)-distinctiveness){	#if preferred word comes close enough to best expression...
					verb=verbs[i,]
					break()
	}	}	}	
	if(!is.data.frame(verb)){verb=verbs[MAX(verbs$match, forceChoice=T),]}
	verb$topic=ifelse(target$topic=='verb', 1, 0)
verb
}

SELECTACTOR=function(speakerID, situation, verbID=NULL){
	distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	verb=speaker$verbs[speaker$verbs$ID==verbID,]
	target=situation[situation$target==1,]
	distractors=situation[situation$target==0,]
	personA=target$personA
	nouns=speaker$nouns[speaker$nouns$person==personA,]
	nouns=nouns[sample(nrow(nouns)),]
	actorTarget=target[,grep('^A\\d',names(target))]
	actor=''
	nouns$match=VMATCH(actorTarget, nouns[,grep('^D\\d',names(nouns))])
	nouns$collostruction=0
	if(!is.null(verbID)){
		if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1){
			collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==verbID,]
			nouns[nouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$S)),]$frequency
		} else {
			collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==verbID,]
			nouns[nouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$O)),]$frequency
	}	}
	actorOrder=order(CANDIDATESCORE(nouns, type='referringExpression'), decreasing=T)
	if(personA==3){		
		if(nrow(situation) > 1){
			actorDistractors=unique(distractors[distractors$personA==3,grep('^A\\d',names(distractors))])
			if(nrow(actorDistractors)!=0){actorDistractors=actorDistractors[!VMATCH(actorTarget, actorDistractors)==1,]}
			if(nrow(actorDistractors)!=0){
				for (i in actorOrder){
					distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], actorDistractors), value=T, forceChoice=T)
					if(nouns[i,]$match > (distractorMatch + distinctiveness)){
						actor=nouns[i,]
						break()
		}	}	}	}
		if(nrow(situation)==1){	
			actor=nouns[1,]
	}	}	
	if(!is.data.frame(actor)){actor=nouns[MAX(nouns$match, forceChoice=T),]}
	names(actor)[grep('match',names(actor))]='referentMatch'
	actor$topic=ifelse(target$topic=='actor', 1, 0)
actor
}

SELECTUNDERGOER=function(speakerID, situation, verbID=NULL){
	distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	verb=speaker$verbs[speaker$verbs$ID==verbID,]
	target=situation[situation$target==1,]
	distractors=situation[situation$target==0,]
	personU=target$personU
	nouns=speaker$nouns[speaker$nouns$person==personU,]
	nouns=nouns[sample(nrow(nouns)),]
	undergoerTarget=target[,grep('^U\\d',names(target))]
	undergoer=''
	nouns$match=VMATCH(undergoerTarget, nouns[,grep('^D\\d',names(nouns))])
	nouns$collostruction=0
	if(!is.null(verbID)){
		if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1){
			collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==verbID,]
			nouns[nouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$O)),]$frequency
		} else {
			collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==verbID,]
			nouns[nouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$S)),]$frequency
	}	}
	undergoerOrder=order(CANDIDATESCORE(nouns, type='referringExpression'), decreasing=T)
	if(personU==3){
		if(nrow(situation) > 1){
			undergoerDistractors=unique(distractors[distractors$personU==3,grep('^U\\d',names(distractors))])
			if(nrow(undergoerDistractors)!=0){undergoerDistractors=undergoerDistractors[!is.na(undergoerDistractors$U1),]}
			if(nrow(undergoerDistractors)!=0){undergoerDistractors=undergoerDistractors[!VMATCH(undergoerTarget, undergoerDistractors)==1,]}
			if(nrow(undergoerDistractors)!=0){
				for (i in undergoerOrder){
					distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], undergoerDistractors), value=T, forceChoice=T)
					if(nouns[i,]$match > (distractorMatch + distinctiveness)){
						undergoer=nouns[i,]
						break()
		}	}	}	}
		if(nrow(situation)==1){	
			undergoer=nouns[1,]
	}	}	
	if(!is.data.frame(undergoer)){undergoer=nouns[MAX(nouns$match, forceChoice=T),]}	
	names(undergoer)[grep('match',names(undergoer))]='referentMatch'
	undergoer$topic=ifelse(target$topic=='undergoer', 1, 0)
undergoer
}

PROPOSITION=function(speakerID, situation){
	distinctiveness=world$distinctiveness
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	topic=target$topic
	targetTransitivity=ifelse(is.na(target$personU), 'onePlace', 'twoPlace')
	if(targetTransitivity=='onePlace'){
		if(topic=='actor'){external=SELECTACTOR(speakerID, situation); verb=SELECTVERB(speakerID, situation, actorID=external$ID)}	
		if(topic=='verb'){verb=SELECTVERB(speakerID, situation); external=SELECTACTOR(speakerID, situation, verb=verb$ID)}
		externalCollostructionFrequency=speaker$collostructions$SV[intersect(grep(paste('^', external$ID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency
		if(length(externalCollostructionFrequency)!=0){external$collostruction=externalCollostructionFrequency} else {external$collostruction=0}
		external$typing=VMATCH(external[,grep('^D\\d',names(external))], verb[,grep('^Ext\\d',names(verb))])
		proposition=list(verb=verb, external=external)
	}
	if(targetTransitivity=='twoPlace'){
		if(topic=='actor'){actor=SELECTACTOR(speakerID, situation); verb=SELECTVERB(speakerID, situation, actorID=actor$ID); undergoer=SELECTUNDERGOER(speakerID, situation, verbID=verb$ID)}	
		if(topic=='undergoer'){undergoer=SELECTUNDERGOER(speakerID, situation); verb=SELECTVERB(speakerID, situation, undergoerID=undergoer$ID); actor=SELECTACTOR(speakerID, situation, verbID=verb$ID)}	
		if(topic=='verb'){verb=SELECTVERB(speakerID, situation); actor=SELECTACTOR(speakerID, situation, verbID=verb$ID); undergoer=SELECTUNDERGOER(speakerID, situation, verbID=verb$ID)}	
		if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1){
			external=actor; internal=undergoer} else {
			external=undergoer; internal=actor
		}
		internalCollostructionFrequency=speaker$collostructions$OV[intersect(grep(paste('^', internal$ID, '$', sep=''), speaker$collostructions$OV$O),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$OV$V)),]$frequency
		if(length(internalCollostructionFrequency)!=0){internal$collostruction=internalCollostructionFrequency} else {internal$collostruction=0}
		externalCollostructionFrequency=speaker$collostructions$SV[intersect(grep(paste('^', external$ID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency
		if(length(externalCollostructionFrequency)!=0){external$collostruction=externalCollostructionFrequency} else {external$collostruction=0}
		external$typing=VMATCH(external[,grep('^D\\d',names(external))], verb[,grep('^Ext\\d',names(verb))])
		internal$typing=VMATCH(internal[,grep('^D\\d',names(internal))], verb[,grep('^Int\\d',names(verb))])
		proposition=list(verb=verb, external=external, internal=internal)
	}
	proposition=proposition[sample(length(proposition))]
	proposition$target=target
proposition
}

REFCHECK=function(speakerID, proposition, situation){
	referenceThreshold=world$referenceThreshold; distinctions=world$distinctions; distinctiveness=world$distinctiveness; frequency=world$frequency
	speaker=population[[speakerID]]
	target=situation[situation$target==1,]
	distractors=situation[situation$target==0,]
	nounCandidates=speaker$nouns[speaker$nouns$productionEffort > referenceThreshold,]
	nounCandidates=nounCandidates[sample(nrow(nounCandidates)),]
	verb=proposition$verb
	external=proposition$external
	if(ACTOR(verb[,grep('^Ext', names(verb))], verb[, grep('^Int', names(verb))])==1){
		externalTarget=target[,grep('^A',names(target))]
		externalDistractors=unique(distractors[,grep('^A',names(distractors))])
	}
	if(ACTOR(verb[,grep('^Ext', names(verb))], verb[, grep('^Int', names(verb))])==2){
		externalTarget=target[,grep('^U',names(target))]
		externalDistractors=unique(distractors[,grep('^U',names(distractors))])
	}
	if(nrow(externalDistractors)!=0){externalDistractors=externalDistractors[!VMATCH(externalTarget, externalDistractors)==1,]}
	if(nchar(external$form) <= referenceThreshold){
		if(!'extMarkerID'%in%names(verb)){
			proposition$verb$extMarkerID=external$ID
			proposition$verb$extMarker=external$form
			proposition$verb$extMarkerFrequency=ifelse(frequency=='absolute',speaker$nouns[speaker$nouns$ID==external$ID,]$frequency, speaker$nouns[speaker$nouns$ID==external$ID,]$verbMarker)
		}
		topic=external$topic; person=external$person
		marker=0; if('markerID'%in%names(external)){marker=external[,grep('^marker', names(external))]}
		nouns=nounCandidates[nounCandidates$person==person,]	
		if(person==3){nouns$match=VMATCH(externalTarget, nouns)}	#match non-local arguments with real-world argument
		if(person!=3){
			if(nrow(nouns)>0){nouns$match=VMATCH(verb[,grep('^Ext', names(verb))], nouns); marker=0}	#match local pronoun with role and remove marker
			if(nrow(nouns)==0){	#if there's no local pronominal paradigm yet, select prominent noun for local ref
				nouns=nounCandidates[nounCandidates$person==3,]
				nouns$match=VMATCH(external[,grep('D\\d', names(external))], nouns)	
		}	}
		nouns$collostruction=0
		collostructions=speaker$collostructions$SV[speaker$collostructions$SV$V==verb$ID,]
		nouns[nouns$ID%in%collostructions$S,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$S)),]$frequency
		argumentOrder=order(CANDIDATESCORE(nouns), decreasing=T)
		if(person!=3){argument=nouns[argumentOrder[1],]}
		if(person==3){		
			argument=''
			if(nrow(situation) > 1){
				if(nrow(externalDistractors)!=0){
					for (i in argumentOrder){
						distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], externalDistractors), value=T, forceChoice=T)
						if(nouns[i,]$match > (distractorMatch + distinctiveness)){
							argument=nouns[i,]
							break()
			}	}	}	}
			if(nrow(situation)==1){	
				argument=nouns[argumentOrder[1],]
			}
			if(!is.data.frame(argument)){argument=nouns[MAX(nouns$match, forceChoice=T),]}	
		}
		argument$typing=VMATCH(argument[,grep('^D\\d',names(argument)),], verb[,grep('^Ext\\d',names(verb)),])
		argument$topic=topic; argument$person=person
		if(is.data.frame(marker)){argument=cbind(argument, marker)}
		proposition$external=argument
		proposition$verb$extMarkerTarget=proposition$external$ID
	}	
	if(verb$type=='twoPlace'){
		internal=proposition$internal
		if(ACTOR(verb[,grep('^Ext', names(verb))], verb[, grep('^Int', names(verb))])==1){
			internalTarget=target[,grep('^U',names(target))]
			internalDistractors=unique(distractors[,grep('^U',names(distractors))])
		}
		if(ACTOR(verb[,grep('^Ext', names(verb))], verb[, grep('^Int', names(verb))])==2){
			internalTarget=target[,grep('^A',names(target))]
			internalDistractors=unique(distractors[,grep('^A',names(distractors))])
		}
		if(nrow(internalDistractors)!=0){internalDistractors=internalDistractors[!VMATCH(internalTarget, internalDistractors)==1,]}
		if(nchar(internal$form) <= referenceThreshold){
			if(!'intMarkerID'%in%names(verb)){	
				proposition$verb$intMarkerID=internal$ID
				proposition$verb$intMarker=internal$form
				proposition$verb$intMarkerFrequency=ifelse(frequency=='absolute',speaker$nouns[speaker$nouns$ID==internal$ID,]$frequency, speaker$nouns[speaker$nouns$ID==internal$ID,]$verbMarker)
			}
			topic=internal$topic; person=internal$person
			marker=0; if('markerID'%in%names(internal)){marker=internal[,grep('^marker', names(internal))]}
			nouns=nounCandidates[nounCandidates$person==person,]	
			if(person==3){nouns$match=VMATCH(internalTarget, nouns)}	
			if(person!=3){
				if(nrow(nouns)>0){nouns$match=VMATCH(verb[,grep('^Int', names(verb))], nouns); marker=0}	
				if(nrow(nouns)==0){	
					nouns=nounCandidates[nounCandidates$person==3,]
					nouns$match=VMATCH(internal[,grep('D\\d', names(internal))], nouns)	
			}	}
			nouns$collostruction=0
			collostructions=speaker$collostructions$OV[speaker$collostructions$OV$V==verb$ID,]
			nouns[nouns$ID%in%collostructions$O,]$collostruction=collostructions[na.omit(match(nouns$ID, collostructions$O)),]$frequency
			argumentOrder=order(CANDIDATESCORE(nouns), decreasing=T)
			if(person!=3){argument=nouns[argumentOrder[1],]}
			if(person==3){		
				argument=''
				if(nrow(situation) > 1){
					if(nrow(internalDistractors)!=0){
						for (i in argumentOrder){
							distractorMatch=MAX(VMATCH(nouns[i,grep('^D\\d',names(nouns))], internalDistractors), value=T, forceChoice=T)
							if(nouns[i,]$match > (distractorMatch + distinctiveness)){
								argument=nouns[i,]
								break()
				}	}	}	}
				if(nrow(situation)==1){	
					argument=nouns[argumentOrder[1],]
				}
				if(!is.data.frame(argument)){argument=nouns[MAX(nouns$match, forceChoice=T),]}	
			}
			argument$typing=VMATCH(argument[,grep('^D\\d',names(argument)),], verb[,grep('^Int\\d',names(verb)),])
			argument$topic=topic; argument$person=person
			if(is.data.frame(marker)){argument=cbind(argument, marker)}
			if(is.data.frame(marker)){argument=cbind(argument, marker)}
			proposition$internal=argument
			proposition$verb$intMarkerTarget=proposition$internal$ID		
	}	}
proposition
}

FIRSTINFIRSTOUT=function(speakerID, proposition){
	activations=rep(0, length(proposition))	
	for (i in 1:(length(activations)-1)){	#skip target
		activations[i]=proposition[[i]]$activation
		if('referentMatch'%in%names(proposition[[i]])){activations[i]=activations[i]+proposition[[i]]$referentMatch}	#for arguments
		if(!'referentMatch'%in%names(proposition[[i]])){activations[i]=activations[i]+proposition[[i]]$match}	#for verbs
	}
	proposition=proposition[order(activations, decreasing=T)]
proposition
}

GENERALIZE=function(speakerID, proposition, situation){
	distinctiveness=world$distinctiveness; frequency=world$frequency; suffixThreshold=world$suffixThreshold; distinctions=world$distinctions; topicCopy=world$topicCopy; wordOrder=world$wordOrder
	speaker=population[[speakerID]]
	#check generalizations(using Yang's tolerance principle)
	yangIndex=TRUE %in% c(speaker$usageHistory$index$no < ((speaker$usageHistory$index$yes + speaker$usageHistory$index$no)/log(speaker$usageHistory$index$yes + speaker$usageHistory$index$no)))
	yangPerson=TRUE %in% c(speaker$usageHistory$flag$person$no < ((speaker$usageHistory$flag$person$yes + speaker$usageHistory$flag$person$no)/log(speaker$usageHistory$flag$person$yes + speaker$usageHistory$flag$person$no)))
	yangRole=TRUE %in% c(speaker$usageHistory$flag$actor$no < ((speaker$usageHistory$flag$actor$yes + speaker$usageHistory$flag$actor$no)/log(speaker$usageHistory$flag$actor$yes + speaker$usageHistory$flag$actor$no))) | TRUE %in% c(speaker$usageHistory$flag$undergoer$no < ((speaker$usageHistory$flag$undergoer$yes + speaker$usageHistory$flag$undergoer$no)/log(speaker$usageHistory$flag$undergoer$yes + speaker$usageHistory$flag$undergoer$no)))
	yangTopic=FALSE; yangWordOrder=FALSE
	#first check word order...
	if(wordOrder==T & sum(speaker$wordOrder$success)>8){	#n exceptions should minimally be 4(=8/ln(8)) for Yang
		yangTopic=TRUE %in% speaker$topicPosition$success[speaker$topicPosition$position=='other'] < (sum(speaker$topicPosition$success)/log(sum(speaker$topicPosition$success)))
		yangWordOrder=TRUE %in% 
			#any particular order
			c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
			#A first
			c(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
			#UV
			c(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
			#VU
			c(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) 
		if(yangWordOrder==T){
			##first general generalizations
			#A first
			if(TRUE %in% c(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
				proposition=AGENTFIRSTPROD(proposition)
			}
			if(proposition$verb$type=='twoPlace'){
				actor=ifelse(ACTOR(proposition$verb[,grep('^Ext\\d', names(proposition$verb))], proposition$verb[,grep('^Int\\d', names(proposition$verb))])==1, 'external', 'internal')
				undergoer=ifelse(ACTOR(proposition$verb[,grep('^Ext\\d', names(proposition$verb))], proposition$verb[,grep('^Int\\d', names(proposition$verb))])==1, 'internal', 'external')
				#UV
				if(TRUE %in% c(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
					if(grep(actor, names(proposition)) < grep('verb', names(proposition))){proposition=proposition[c(actor, undergoer, 'verb', 'target')]}
					if(grep(actor, names(proposition)) > grep('verb', names(proposition))){proposition=proposition[c(undergoer, 'verb', actor, 'target')]}				
				}
				#VU
				if(TRUE %in% c(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
					if(grep(actor, names(proposition)) < grep('verb', names(proposition))){proposition=proposition[c(actor, 'verb', undergoer, 'target')]}
					if(grep(actor, names(proposition)) > grep('verb', names(proposition))){proposition=proposition[c('verb', undergoer, actor, 'target')]}				
				}
				##then particular orders (which overrule general tendencies)
				if(TRUE %in% c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))){
					order=speaker$wordOrder$order[c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success))))]
					order=unlist(strsplit(order, '')); order=gsub('A', actor, order); order=gsub('U', undergoer, order); order=gsub('V', 'verb', order)
					proposition=proposition[c(order, 'target')]
			}	}
			if(yangTopic==T){	#overrules word order!
				proposition=TOPICFIRST(speakerID, proposition)
	}	}	}
	#... then morphological marking (since solutionMethod sometimes dependent on word order)
	if(TRUE %in% c(yangIndex, yangPerson, yangRole)){
		if(proposition$verb$type=='onePlace'){
			verbRole='external'
			person=proposition$external$person
			semRole='actor'
			firstArgument=list(person=person, verbRole=verbRole, semRole=semRole, props=proposition$external[,grep('^D\\d', names(proposition$external))])		
		}
		if(proposition$verb$type=='twoPlace'){
			actorPerspective=ifelse(ACTOR(proposition$verb[,grep('^Ext', names(proposition$verb))], proposition$verb[,grep('^Int', names(proposition$verb))])==1, 'external', 'internal')
			verbRole=names(proposition[names(proposition)%in%c('internal', 'external')])[1]
			person=proposition[[verbRole]]$person
			semRole=ifelse(actorPerspective==verbRole, 'actor', 'undergoer')
			firstArgument=list(person=person, verbRole=verbRole, semRole=semRole, props=proposition[[verbRole]][,grep('^D\\d', names(proposition[[verbRole]]))])		
			verbRole=names(proposition[names(proposition)%in%c('internal', 'external')])[2]
			person=proposition[[verbRole]]$person
			semRole=ifelse(actorPerspective==verbRole, 'actor', 'undergoer')
			secondArgument=list(person=person, verbRole=verbRole, semRole=semRole, props=proposition[[verbRole]][,grep('^D\\d', names(proposition[[verbRole]]))])		
	}	}
	if(yangIndex==T){
		#first argument
		index=F
		data=speaker$usageHistory$index[speaker$usageHistory$index$person==firstArgument$person & speaker$usageHistory$index$role==firstArgument$semRole,]
		if(TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))){index=T}		
		if(firstArgument$verbRole=='external' & 'extMarker'%in%names(proposition$verb) | firstArgument$verbRole=='internal' & 'intMarker'%in%names(proposition$verb)){index=F}
		if(index==T){	
			markers=speaker$nouns[speaker$nouns$person==firstArgument$person & speaker$nouns$productionEffort<=suffixThreshold,]
			if(nrow(markers)==0){markers=speaker$nouns[speaker$nouns$person==firstArgument$person,]}
			if(nrow(markers)!=0){
				markers=markers[sample(nrow(markers)),]
				if(firstArgument$verbRole=='external'){markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])}
				if(firstArgument$verbRole=='internal'){markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])}
				markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=T),]
				markerID=markers$ID[1]
				markerTarget=proposition[[firstArgument$verbRole]]$ID
				if(firstArgument$verbRole=='external'){
					proposition$verb$extMarkerID=markerID
					proposition$verb$extMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
					proposition$verb$extMarkerTarget=markerTarget
					if(frequency=='absolute'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
					if(frequency=='relative'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
				}	
				if(firstArgument$verbRole=='internal'){
					proposition$verb$intMarkerID=markerID
					proposition$verb$intMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
					proposition$verb$intMarkerTarget=markerTarget
					if(frequency=='absolute'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
					if(frequency=='relative'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
		}	}	}
		#second argument
		if(proposition$verb$type=='twoPlace'){
			index=F
			data=speaker$usageHistory$index[speaker$usageHistory$index$person==secondArgument$person & speaker$usageHistory$index$role==secondArgument$semRole,]
			if(TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))){index=T}		
			if(secondArgument$verbRole=='external' & 'extMarker'%in%names(proposition$verb)){index=F}
			if(secondArgument$verbRole=='internal' & 'intMarker'%in%names(proposition$verb)){index=F}
			if(index==T){	
				markers=speaker$nouns[speaker$nouns$person==secondArgument$person & speaker$nouns$productionEffort<=suffixThreshold,]
				if(nrow(markers)!=0){
					markers=markers[sample(nrow(markers)),]
					if(secondArgument$verbRole=='external'){markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])}
					if(secondArgument$verbRole=='internal'){markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])}
					markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=T),]
					markerID=markers$ID[1]
					markerTarget=proposition[[secondArgument$verbRole]]$ID
					if(secondArgument$verbRole=='external'){
						proposition$verb$extMarkerID=markerID
						proposition$verb$extMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
						proposition$verb$extMarkerTarget=markerTarget
						if(frequency=='absolute'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
						if(frequency=='relative'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
					}	
					if(secondArgument$verbRole=='internal'){
						proposition$verb$intMarkerID=markerID
						proposition$verb$intMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
						proposition$verb$intMarkerTarget=markerTarget
						if(frequency=='absolute'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
						if(frequency=='relative'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
	}	}	}	}	}	
	#flag
	if(yangPerson==T | yangRole==T){
		#first argument
		data=speaker$usageHistory$flag$person[speaker$usageHistory$flag$person$role==firstArgument$semRole & speaker$usageHistory$flag$person$person==firstArgument$person,]
		flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	#can flagging rule be derived at general level?
		#or for specific dimensions of semantic role?
		if(flag==FALSE){	
			values=unlist(rep(firstArgument$props, distinctions)); values[is.na(values)]=-1
			values=values[1:length(speaker$usageHistory$flag[[firstArgument$semRole]]$value)]		#necessary for economically stored resurrected agents;)
			data=speaker$usageHistory$flag[[firstArgument$semRole]][speaker$usageHistory$flag[[firstArgument$semRole]]$value==values,]
			flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	#can flagging rule be derived at general level?
		}
		if(flag==T){
			#flag through pronouns
			done=F
			topic=proposition[[firstArgument$verbRole]]$topic
			pronouns=speaker$nouns[speaker$nouns$person==firstArgument$person & speaker$nouns$productionEffort>world$referenceThreshold,]
			if(nrow(pronouns)>1){	#pronominal paradigm
				if(firstArgument$verbRole=='internal'){
					pronouns$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
					pronouns$typing=pronouns$match
				}
				if(firstArgument$verbRole=='external'){
					pronouns$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
					pronouns$typing=pronouns$match
				}
				pronoun=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=T)[1],]
				pronoun$collostruction=pronoun$argument
				if(firstArgument$person!=3){
					proposition[[firstArgument$verbRole]]=pronoun
					proposition[[firstArgument$verbRole]]$topic=topic
					done=T		
				}	
				if(firstArgument$person==3){		
					if(firstArgument$semRole=='actor'){distractors=situation$personA}
					if(firstArgument$semRole=='undergoer'){distractors=situation$personU}
					if(sum(distractors==firstArgument$person, na.rm=T)==1){	#only third-person pronoun if single third-person referent
						proposition[[firstArgument$verbRole]]=pronoun
						proposition[[firstArgument$verbRole]]$topic=topic
						done=T
			}	}	}
			if(done==F){
				markers=speaker$nouns[sample(nrow(speaker$nouns)), ]
				if(firstArgument$verbRole=='internal'){
					markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					markers$distractorMatch=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
				}
				if(firstArgument$verbRole=='external'){
					markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					markers$distractorMatch=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])		
				}
				markers=markers[markers$person==3,]
				markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=T),]
				markerID=0
				for (i in 1:nrow(markers)){
					if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
						break()
				}	}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=T),]$ID}
				proposition[[firstArgument$verbRole]]$markerID=markerID
				proposition[[firstArgument$verbRole]]$marker=speaker$nouns[speaker$nouns$ID==markerID,]$form
				if(frequency=='absolute'){proposition[[firstArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
				if(frequency=='relative'){proposition[[firstArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$nounMarker}
				proposition[[firstArgument$verbRole]]$markerRecency=speaker$nouns[speaker$nouns$ID==markerID,]$recency
				if(firstArgument$verbRole=='external'){markerCollostruction=speaker$collostructions$SV[intersect(grep(paste('^', markerID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', proposition$verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency}
				if(firstArgument$verbRole=='internal'){markerCollostruction=speaker$collostructions$OV[intersect(grep(paste('^', markerID, '$', sep=''), speaker$collostructions$OV$O),grep(paste('^', proposition$verb$ID, '$', sep=''), speaker$collostructions$OV$V)),]$frequency}
				if(length(markerCollostruction)!=0){proposition[[firstArgument$verbRole]]$markerCollostruction=markerCollostruction} else {proposition[[firstArgument$verbRole]]$markerCollostruction=0}			
		}	}
		#second argument
		if(proposition$verb$type=='twoPlace'){
			data=speaker$usageHistory$flag$person[speaker$usageHistory$flag$person$role==secondArgument$semRole & speaker$usageHistory$flag$person$person==secondArgument$person,]
			flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	#can flagging rule be derived at general level?
			if(flag==FALSE){	#or for specific dimensions of verb role?
				values=unlist(rep(secondArgument$props, distinctions)); values[is.na(values)]=-1
				values=values[1:length(speaker$usageHistory$flag[[secondArgument$semRole]]$value)]		#necessary for economically stored resurrected agents;)
				data=speaker$usageHistory$flag[[secondArgument$semRole]][speaker$usageHistory$flag[[secondArgument$semRole]]$value==values,]
				flag=TRUE %in% c(data$no < (data$yes + data$no)/log(data$yes + data$no))	#can flagging rule be derived at general level?
			}
			if(flag==T){
				#flag through pronouns
				done=F
				topic=proposition[[secondArgument$verbRole]]$topic
				pronouns=speaker$nouns[speaker$nouns$person==secondArgument$person & speaker$nouns$productionEffort>world$referenceThreshold,]
				if(nrow(pronouns)>1){	#pronominal paradigm
					if(secondArgument$verbRole=='internal'){
						pronouns$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
						pronouns$typing=pronouns$match
					}
					if(secondArgument$verbRole=='external'){
						pronouns$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], pronouns[,grep('^D\\d',names(pronouns))])
						pronouns$typing=pronouns$match
					}
					pronoun=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=T)[1],]
					pronoun$collostruction=pronoun$argument
					if(secondArgument$person!=3){
						proposition[[secondArgument$verbRole]]=pronoun
						proposition[[secondArgument$verbRole]]$topic=topic
						done=T		
					}	
					if(secondArgument$person==3){		
						if(secondArgument$semRole=='actor'){distractors=situation$personA}
						if(secondArgument$semRole=='undergoer'){distractors=situation$personU}
						if(sum(distractors==secondArgument$person, na.rm=T)==1){	
							proposition[[secondArgument$verbRole]]=pronoun
							proposition[[secondArgument$verbRole]]$topic=topic
							done=T
				}	}	}
				if(done==F){
					markers=speaker$nouns[speaker$nouns$person==3, ]
					markers=markers[sample(nrow(markers)),]
					if(secondArgument$verbRole=='internal'){
						markers$match=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
						markers$distractorMatch=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
					}
					if(secondArgument$verbRole=='external'){
						markers$match=VMATCH(proposition$verb[,grep('^Ext\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])
						markers$distractorMatch=VMATCH(proposition$verb[,grep('^Int\\d',names(proposition$verb))], markers[,grep('^D\\d',names(markers))])		
					}
					markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=T),]
					markerID=0
					for (i in 1:nrow(markers)){
						if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
						markerID=markers[i,]$ID
							break()
					}	}
					if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=T),]$ID}
					proposition[[secondArgument$verbRole]]$markerID=markerID
					proposition[[secondArgument$verbRole]]$marker=speaker$nouns[speaker$nouns$ID==markerID,]$form
					if(frequency=='absolute'){proposition[[secondArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
					if(frequency=='relative'){proposition[[secondArgument$verbRole]]$markerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$nounMarker}
					proposition[[secondArgument$verbRole]]$markerRecency=speaker$nouns[speaker$nouns$ID==markerID,]$recency
					if(secondArgument$verbRole=='external'){markerCollostruction=speaker$collostructions$SV[intersect(grep(paste('^', markerID, '$', sep=''), speaker$collostructions$SV$S),grep(paste('^', proposition$verb$ID, '$', sep=''), speaker$collostructions$SV$V)),]$frequency}
					if(secondArgument$verbRole=='internal'){markerCollostruction=speaker$collostructions$OV[intersect(grep(paste('^', markerID, '$', sep=''), speaker$collostructions$OV$O),grep(paste('^', proposition$verb$ID, '$', sep=''), speaker$collostructions$OV$V)),]$frequency}
					if(length(markerCollostruction)!=0){proposition[[secondArgument$verbRole]]$markerCollostruction=markerCollostruction} else {proposition[[secondArgument$verbRole]]$markerCollostruction=0}			
	}	}	}	}
proposition	
}

AGENTFIRSTPROD=function(proposition){
	target=proposition$target
	if(proposition$verb$type=='onePlace'){	#desirable like this for intransitives? Maybe dependent on semantic role 
		proposition=proposition[c('external',names(proposition)[names(proposition)!='external'])]
	}
	if(proposition$verb$type=='twoPlace'){
		actor=ifelse(ACTOR(proposition$verb[,grep('^Ext\\d', names(proposition$verb))], proposition$verb[,grep('^Int\\d', names(proposition$verb))])==1, 'external', 'internal')
		proposition=proposition[c(actor,names(proposition)[names(proposition)!=actor])]
	}
	proposition$target=target
proposition
}

TOPICFIRST=function(speakerID, proposition){
	topicCopy=world$topicCopy
	target=proposition$target
	if(proposition$verb$type=='onePlace'){
		if(proposition$verb$topic==1){proposition=proposition[c('verb','external', 'target')]}
		if(proposition$external$topic==1){proposition=proposition[c('external','verb', 'target')]}
	}
	if(proposition$verb$type=='twoPlace'){
		if(proposition$verb$topic==1){proposition=proposition[c('verb',names(proposition)[names(proposition)!='verb'])]}
		if(proposition$external$topic==1){proposition=proposition[c('external',names(proposition)[names(proposition)!='external'])]}
		if(proposition$internal$topic==1){proposition=proposition[c('internal',names(proposition)[names(proposition)!='internal'])]}
	}
	proposition$target=target
	if(topicCopy==T & proposition$verb$topic!=1){proposition=TOPICCOPY(speakerID, proposition)}
proposition
}

TOPICCOPY=function(speakerID, proposition){
	distinctiveness=world$distinctiveness; frequency=world$frequency
	speaker=population[[speakerID]]
	markerID=0
	if(proposition$verb$type=='onePlace' & proposition$external$recency!=0){	#only copy for reestablished/non-continuous topics (cf. Givon; idem for twoPlace below)
		markers=speaker$nouns[speaker$nouns$person==proposition$external$person,]	
		markers=markers[sample(nrow(markers)),]
		markers$match=VMATCH(proposition$external[,grep('^D\\d',names(proposition$external))], markers[,grep('^D\\d',names(markers))])
		if(!'extMarkerID'%in%names(proposition$verb)){
			markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=T),]
			for (i in 1:nrow(markers)){
				if(markers[i,]$match > (1-distinctiveness)){
					markerID=markers[i,]$ID
					break()
			}	}
			if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=T),]$ID}
			markerTarget=proposition$external$ID
	}	}
	if(proposition$verb$type=='twoPlace'){
		if(proposition$external$topic==1 & proposition$external$recency!=0 & !'extMarkerID'%in%names(proposition$verb)){
			markerTarget=proposition$external$ID
			markers=speaker$nouns[speaker$nouns$person==proposition$external$person,]	
			markers=markers[sample(nrow(markers)),]
			markers$match=VMATCH(proposition$external[,grep('^D\\d',names(proposition$external))], markers[,grep('^D\\d',names(markers))])
			markers$distractorMatch=0
			markers[markers$person==proposition$external$person, ]$distractorMatch=VMATCH(proposition$internal[,grep('^D\\d',names(proposition$internal))], markers[markers$person==proposition$external$person,grep('^D\\d',names(markers))])
			markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=T),]
			for (i in 1:nrow(markers)){
				if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
					break()
				}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=T),]$ID}
		}	}
		if(proposition$internal$topic==1 & proposition$internal$recency!=0 &!'intMarkerID'%in%names(proposition$verb)){
			markerTarget=proposition$internal$ID
			markers=speaker$nouns[speaker$nouns$person==proposition$internal$person,]	
			markers=markers[sample(nrow(markers)),]
			markers$match=VMATCH(proposition$internal[,grep('^D\\d',names(proposition$internal))], markers[,grep('^D\\d',names(markers))])
			markers$distractorMatch=0
			markers[markers$person==proposition$internal$person, ]$distractorMatch=VMATCH(proposition$external[,grep('^D\\d',names(proposition$external))], markers[markers$person==proposition$internal$person,grep('^D\\d',names(markers))])
			markers=markers[order(CANDIDATESCORE(markers, type='verbMarker'), decreasing=T),]
			for (i in 1:nrow(markers)){
				if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
					break()
				}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=T),]$ID}
		}	}
		if(proposition$internal$topic==1 & markerID!=0){
			proposition$verb$intMarkerID=markerID
			proposition$verb$intMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
			proposition$verb$intMarkerTarget=markerTarget
			if(frequency=='absolute'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
			if(frequency=='relative'){proposition$verb$intMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
	}	}		
	if(proposition$external$topic==1  & markerID!=0){
		proposition$verb$extMarkerID=markerID
		proposition$verb$extMarker=speaker$nouns[speaker$nouns$ID==markerID,]$form
		proposition$verb$extMarkerTarget=markerTarget	
		if(frequency=='absolute'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$frequency}
		if(frequency=='relative'){proposition$verb$extMarkerFrequency=speaker$nouns[speaker$nouns$ID==markerID,]$verbMarker}
	}	
proposition
}

CHECKSUCCESS=function(speakerID, proposition, situation){
	solutionMethod=world$solutionMethod; distinctiveness=world$distinctiveness; topicCopy=world$topicCopy; frequency=world$frequency; referenceThreshold=world$referenceThreshold; generalization=world$generalization; wordOrder=world$wordOrder
	speaker=population[[speakerID]]
	elaborate='unnecessary'
	verb=proposition$verb
	if(verb$type=='twoPlace'){
		external=proposition$external
		internal=proposition$internal
		external$distractorTyping=VMATCH(external[, grep('^D\\d',names(external))], verb[,grep('^Int\\d',names(verb))])
		internal$distractorTyping=VMATCH(internal[, grep('^D\\d',names(internal))], verb[,grep('^Ext\\d',names(verb))])
		if(max(external$typing, internal$typing) < (max(external$distractorTyping, internal$distractorTyping) + distinctiveness)){elaborate='necessary'}  #elaboration is necessary if best typing match leads to wrong distribution of roles
		if('marker'%in%names(external)  | 'marker'%in%names(internal)){elaborate='unnecessary'} #but not if (one of) the roles are marked one way or another
		actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external','internal')
		#first try if indexes are informative 
		if(generalization==T & elaborate=='necessary'){	
			index=speaker$usageHistory$index
			yangIndex=TRUE %in% c(index$no < ((index$yes + index$no)/log(index$yes + index$no)))
			if(yangIndex==T){
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
		#next try appropriate pronominal case form 
		if(elaborate=='necessary'){	
			topic=external$topic
			pronouns=speaker$nouns[speaker$nouns$person==external$person & speaker$nouns$productionEffort>referenceThreshold,]
			if(external$person!=3){
				if(nrow(pronouns)>1){
					pronouns$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], pronouns[,grep('^D\\d',names(pronouns))])	
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=T),]
					external=pronouns[1,]
			}	}
			if(external$person==3){
				distractors=situation$personA
				if(ACTOR(verb[,grep('Ext\\d',names(verb))], verb[,grep('Int\\d',names(verb))])==2){distractors=situation$personU}
				if(sum(distractors==external$person, na.rm=T)==1){	#only single third-person referent
					pronouns$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], pronouns[,grep('^D\\d',names(pronouns))])	
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=T),]
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
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=T),]
					internal=pronouns[1,]
			}	}
			if(internal$person==3){
				distractors=situation$personA
				if(ACTOR(verb[,grep('Ext\\d',names(verb))], verb[,grep('Int\\d',names(verb))])==2){distractors=situation$personU}
				if(sum(distractors==internal$person, na.rm=T)==1){	#only single third-person referent
					pronouns$match=VMATCH(verb[,grep('^Ext\\d',names(verb))], pronouns[,grep('^D\\d',names(pronouns))])	
					pronouns=pronouns[order(CANDIDATESCORE(pronouns, type='pronoun'), decreasing=T),]
					internal=pronouns[1,]
			}	}	
			internal$topic=topic
			internal$typing=VMATCH(internal[,grep('^D\\d',names(internal))], verb[,grep('^Int\\d',names(verb))])
			collostructionFrequency=speaker$collostructions$OV[intersect(grep(paste('^', internal$ID, '$', sep=''), speaker$collostructions$OV$O),grep(paste('^', verb$ID, '$', sep=''), speaker$collostructions$OV$V)),]$frequency
			if(length(collostructionFrequency)!=0){internal$collostruction=collostructionFrequency} else {internal$collostruction=0}
			#check if case form works
			external$distractorTyping=VMATCH(external[, grep('^D\\d',names(external))], verb[,grep('^Int\\d',names(verb))])
			internal$distractorTyping=VMATCH(internal[, grep('^D\\d',names(internal))], verb[,grep('^Ext\\d',names(verb))])
			if(max(external$typing, internal$typing) > (max(external$distractorTyping, internal$distractorTyping) + distinctiveness)){elaborate='unnecessary'}  
		}
		##and check if word order is informative (if generalizations are made)
		if(generalization==T & wordOrder==T & sum(speaker$wordOrder$success)>8){	#n exceptions should minimally be 4(=8/ln(8)) for Yang
				yangWordOrder=TRUE %in% 
				#any particular order
				c((sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
				#A first
				c(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
				#UV
				c(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) |
				#VU
				c(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))) 
				if(yangWordOrder==T){
					standard=speaker$wordOrder$order[(sum(speaker$wordOrder$success)-speaker$wordOrder$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))]
					if(length(standard)==0){
						if(sum(speaker$wordOrder[grep('^A', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))){standard='^A'}
						if(sum(speaker$wordOrder[grep('UV', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))){standard='UV'}
						if(sum(speaker$wordOrder[grep('VU', speaker$wordOrder$order, invert=T), ]$success) < (sum(speaker$wordOrder$success)/log(sum(speaker$wordOrder$success)))){standard='VU'}
					}
					actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')
					undergoer=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'internal', 'external')
					order=paste(names(proposition), collapse=''); order=gsub(actor, 'A', order); order=gsub(undergoer, 'U', order); order=gsub('verb', 'V', order); order=gsub('target', '', order)
					if(grepl(standard, order)){elaborate='unnecessary'}
		}		}	
		##elaboration still necessary?
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
				markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=T),]
				markers$distractorMatch=VMATCH(verb[,grep('^Ext\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markerID=0
				for (i in 1:nrow(markers)){
					if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
						break()
				}	}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=T),]$ID}
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
				markers=markers[order(CANDIDATESCORE(markers, type='nounMarker'), decreasing=T),]
				markers$distractorMatch=VMATCH(verb[,grep('^Int\\d',names(verb))], markers[,grep('^D\\d',names(markers))])
				markerID=0
				for (i in 1:nrow(markers)){
					if(markers[i,]$match > (markers[i,]$distractorMatch + distinctiveness)){
					markerID=markers[i,]$ID
						break()
				}	}
				if(markerID==0){markerID=markers[MAX(markers$match, forceChoice=T),]$ID}
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

REDUCE=function(speakerID, proposition){
	erosionMax=world$erosionMax; reductionFrequencyThreshold=world$reductionFrequencyThreshold; blocking=world$formBlocking; reductionCollostructionThreshold=world$reductionCollostructionThreshold; reductionRecencyThreshold=world$reductionRecencyThreshold
	speaker=population[[speakerID]]
	reductionFrequencyThreshold=reductionFrequencyThreshold*speaker$age
	verb=proposition$verb
	if(nchar(verb$form)>erosionMax & (verb$frequency > reductionFrequencyThreshold | verb$recency <= reductionRecencyThreshold)){	
		form=gsub('(.*).','\\1',verb$form)
		if(blocking==F){proposition$verb$form=form}
		if(blocking==T){if(!form%in%speaker$verbs$form){proposition$verb$form=form}}
	}	
	if('extMarkerID'%in%names(verb)){
		#if frequent and recent or predictable (through collostruction)
		if(nchar(verb$extMarker)>erosionMax & (proposition$verb$extMarkerFrequency > reductionFrequencyThreshold | speaker$nouns[speaker$nouns$ID==proposition$verb$extMarkerID,]$recency <= reductionRecencyThreshold)){
			form=gsub('(.*).','\\1',verb$extMarker)
			if(blocking==F){proposition$verb$extMarker=form}
			if(blocking==T){if(!form%in%speaker$nouns$form){proposition$verb$extMarker=form}}
	}	}
	if('intMarkerID'%in%names(proposition$verb)){
		#if frequent and recent or predictable (through collostruction)
		if(nchar(verb$intMarker)>erosionMax & (proposition$verb$intMarkerFrequency > reductionFrequencyThreshold | speaker$nouns[speaker$nouns$ID==proposition$verb$intMarkerID,]$recency <= reductionRecencyThreshold)){
			form=gsub('(.*).','\\1',verb$intMarker)
			if(blocking==F){proposition$verb$intMarker=form}
			if(blocking==T){if(!form%in%speaker$nouns$form){proposition$verb$intMarker=form}}
	}	}
	external=proposition$external
	if(nchar(external$form)>erosionMax & (external$frequency > reductionFrequencyThreshold | external$recency <= reductionRecencyThreshold | external$collostruction > reductionCollostructionThreshold)){
		form=gsub('(.*).','\\1',external$form)
		if(blocking==F){proposition$external$form=form}
		if(blocking==T){if(!form%in%speaker$nouns$form){proposition$external$form=form}}
	}
	if('marker'%in%names(proposition$external)){
		if(nchar(external$marker)>erosionMax & (proposition$external$markerFrequency > reductionFrequencyThreshold | proposition$external$markerRecency <= reductionRecencyThreshold | proposition$external$markerCollostruction > reductionCollostructionThreshold)){
			form=proposition$external$marker
			if(blocking==F){proposition$external$marker=form}
			if(blocking==T){if(!form%in%speaker$nouns$form){proposition$external$marker=form}}
	}	}
	if(proposition$verb$type=='twoPlace'){
		internal=proposition$internal
		if(nchar(internal$form)>erosionMax & (internal$frequency > reductionFrequencyThreshold | internal$recency <= reductionRecencyThreshold | internal$collostruction > reductionCollostructionThreshold)){
			form=gsub('(.*).','\\1',internal$form)
			if(blocking==F){proposition$internal$form=form}
			if(blocking==T){if(!form%in%speaker$nouns$form){proposition$internal$form=form}}
		}
		if('marker'%in%names(proposition$internal)){
			if(nchar(internal$marker)>erosionMax & (proposition$internal$markerFrequency > reductionFrequencyThreshold | proposition$internal$markerRecency <= reductionRecencyThreshold | proposition$internal$markerCollostruction > reductionCollostructionThreshold)){
				form=proposition$internal$marker
				if(blocking==F){proposition$internal$marker=form}
				if(blocking==T){if(!form%in%speaker$nouns$form){proposition$internal$marker=form}}
	}	}	}
proposition
}

PREPARE=function(speakerID, proposition, situation){
	generalization=world$generalization; refCheck=world$refCheck; checkSuccess=world$checkSuccess; firstInFirstOut=world$firstInFirstOut; erosion=world$erosion
	prep=proposition
	if(refCheck==T){prep=REFCHECK(speakerID, prep, situation)}	#first check whether referential expressions are strong enough
	if(firstInFirstOut==T){prep=FIRSTINFIRSTOUT(speakerID, prep)}	#order ingredients of proposition by activation before other principles apply
	if(generalization==T & population[[speakerID]]$generation>1){prep=GENERALIZE(speakerID, prep, situation)}		#apply observed rules (overrules FIFO)
	if(checkSuccess==T){prep=CHECKSUCCESS(speakerID, prep, situation)}
	if(erosion==T){prep=REDUCE(speakerID, prep)}
prep
}

PRODUCE=function(speakerID, prep){
	suffixThreshold=world$suffixThreshold
	utterance=paste(names(prep), collapse=' '); utterance=gsub(' target', '', utterance)
	if('verb'%in%names(prep)){
		verb=prep$verb$form
		if('intMarkerID'%in%names(prep$verb)){		#internal marker closest to verb (cf. Dryer; not exploited by hearer!)
			if(nchar(prep$verb$intMarker) <= suffixThreshold){marker=prep$verb$intMarker; verb=paste(verb, marker, sep='')}	#first suffixes...
		}
		if('extMarkerID'%in%names(prep$verb)){
			if(nchar(prep$verb$extMarker) <= suffixThreshold){marker=prep$verb$extMarker; verb=paste(verb, marker, sep='')}
		}
		if('intMarkerID'%in%names(prep$verb)){																				
			if(nchar(prep$verb$intMarker) > suffixThreshold){marker=prep$verb$intMarker; verb=paste(verb, marker, sep=' ')}	#...then adpositions
		}
		if('extMarkerID'%in%names(prep$verb)){
			if(nchar(prep$verb$extMarker) > suffixThreshold){marker=prep$verb$extMarker; verb=paste(verb, marker, sep=' ')}
		}
	}
	if('internal'%in%names(prep)){
		internal=prep$internal$form
		if('marker'%in%names(prep$internal)){	
			if(nchar(prep$internal$marker) <= suffixThreshold){marker=prep$internal$marker; internal=paste(internal, marker, sep='')}
			if(nchar(prep$internal$marker) > suffixThreshold){marker=prep$internal$marker; internal=paste(internal, marker, sep=' ')}
		}
		if('verb'%in%names(prep)){
			if(nchar(internal)<suffixThreshold & !'intMarker'%in%names(prep$verb)){	#again, internal marker closest to verb
				verb=paste(verb, internal, sep='')
				utterance=gsub('\\s?internal', '', utterance)
	}	}	}
	if('external'%in%names(prep)){
		external=prep$external$form
		if('marker'%in%names(prep$external)){	
			if(nchar(prep$external$marker) <= suffixThreshold){marker=prep$external$marker; external=paste(external, marker, sep='')}
			if(nchar(prep$external$marker) > suffixThreshold){marker=prep$external$marker; external=paste(external, marker, sep=' ')}
		}
		if('verb'%in%names(prep)){
			if(nchar(external)<suffixThreshold & !'extMarker'%in%names(prep$verb)){
				verb=paste(verb, external, sep='')
				utterance=gsub('\\s?external', '', utterance)
	}	}	}
	if(grepl('verb', utterance)){utterance=gsub('verb', verb, utterance)}
	if(grepl('internal', utterance)){utterance=gsub('internal', internal, utterance)}
	if(grepl('external', utterance)){utterance=gsub('external', external, utterance)}
	utterance=gsub('^\\s','',utterance)
utterance
}

########## INTERPRETATION GRAMMAR #################

DECOMPOSE=function(hearerID, form){
	suffixThreshold=world$suffixThreshold; erosionMax=world$erosionMax
	if(erosionMax==0){erosionMax=1} #no zero morphemes
	hearer=population[[hearerID]]
	lexicon=hearer$nouns
	suffixes=lexicon[lexicon$productionEffort <= (suffixThreshold + 1),]$form	#plus 1, for reduced forms may become suffixes too
	suffixes=suffixes[nchar(suffixes)  >= erosionMax]		#suffixes must be minimally erosionMax long (should be automatically satisfied...)
	composition=vector()
	if(length(suffixes)!=0){
		suffixes[nchar(suffixes)>erosionMax]=gsub('.$','.?$',suffixes[nchar(suffixes)>erosionMax])
		suffixes[nchar(suffixes)==erosionMax]=gsub('.$','.$',suffixes[nchar(suffixes)==erosionMax])
		suffixes=unique(suffixes)
		for (i in 1:length(suffixes)){
			if(grepl(suffixes[i], form) & nchar(gsub(suffixes[i],'',form))  >=erosionMax){
				stem=gsub(suffixes[i], '', form)
				suffix=gsub(paste('.*(',suffixes[i],')',sep=''), '\\1', form)
				composition[length(composition) + 1]=paste(stem, suffix, sep='-')
				for (j in 1:length(suffixes)){ #recursive, max 2...
					if(grepl(suffixes[j], stem) & nchar(gsub(suffixes[j],'',stem))  >=erosionMax){
						suffix2=paste(gsub(paste('.*(',suffixes[j],')',sep=''), '\\1', stem), suffix, sep='-')
						stem2=gsub(suffixes[j], '', stem)
						composition[length(composition) + 1]=paste(stem2, suffix2, sep='-')
	}	}	}	}	}
unique(composition)
}

ANALYZE=function(hearerID, utterance, situation){
	refCheck=world$refCheck; referenceThreshold=world$referenceThreshold
	hearer=population[[hearerID]]
	actions=unique(situation[, grep('^V',names(situation))])
	As=situation[, grep('^A',names(situation))]; names(As)=gsub('A', 'D', names(As))
	Us=situation[, grep('^U',names(situation))]; names(Us)=gsub('U', 'D', names(As))
	objects=unique(rbind(As, Us))
	forms=unlist(strsplit(utterance,'\\s'))
	forms=forms[forms!='']
	analysis=data.frame(word=1, form='', option=1, stem='', role='', stringsAsFactors=F)
	n=1
	for (i in 1:length(forms)){
		analysis[n,]$word=i
		analysis[n,]$form=forms[i]
		analysis[n,]$option=1
		analysis[n,]$stem=forms[i]
		analysis[n,]$role='?'
		n=n + 1
		decomposables=DECOMPOSE(hearerID, forms[i])
		if(length(decomposables)!=0){
			m=2
			for (word in decomposables){
				analysis[n,]$word=i
				analysis[n,]$form=forms[i]
				analysis[n,]$option=m
				analysis[n,]$stem=unlist(strsplit(word,'-'))[1]
				analysis[n,]$role='?'
				n=n + 1
				analysis[n,]$word=i
				analysis[n,]$form=unlist(strsplit(word,'-'))[2]
				analysis[n,]$option=m
				analysis[n,]$stem=unlist(strsplit(word,'-'))[2]
				analysis[n,]$role='suffix'
				n=n + 1
				if(length(unlist(strsplit(word,'-'))) > 2){
					analysis[n,]$word=i
					analysis[n,]$form=unlist(strsplit(word,'-'))[3]
					analysis[n,]$option=m
					analysis[n,]$stem=unlist(strsplit(word,'-'))[3]
					analysis[n,]$role='suffix'
					n=n + 1
				}		
				m=m + 1
	}	}	}
	analysis=analysis[!is.na(analysis$stem),]
	analysis$verbID=0; analysis$verb=''; analysis$verbType=''; analysis$verbMatch=0; analysis$actionMatch=0; analysis$verbScore=0
	analysis$nounID=0; analysis$noun=''; analysis$nounPerson=0; analysis$nounMatch=0; analysis$objectMatch=0; analysis$nounScore=0
	analysis$nounMarkerID=0; analysis$nounMarker=''; analysis$nounMarkerPerson=0; analysis$nounMarkerMatch=0
	analysis$verbMarkerID=0; analysis$verbMarker=''; analysis$verbMarkerPerson=0; analysis$verbMarkerMatch=0
	for (i in 1:nrow(analysis)){
		if(analysis$role[i]=='suffix'){
			candidates=hearer$nouns
			candidates$match=FMATCH(analysis$stem[i], candidates)
			entry=candidates[MAX(candidates$match, forceChoice=F),]
			if(nrow(entry)>1){entry=entry[MAX(entry$activation, forceChoice=T),]}
			analysis$nounMarkerID[i]=entry$ID
			analysis$nounMarkerPerson[i]=entry$person
			analysis$nounMarker[i]=entry$form
			analysis$nounMarkerMatch[i]=entry$match
			analysis$verbMarkerID[i]=entry$ID
			analysis$verbMarkerPerson[i]=entry$person
			analysis$verbMarker[i]=entry$form
			analysis$verbMarkerMatch[i]=entry$match
			analysis$nounID[i]=entry$ID	#suffix could be incorporated noun
			analysis$nounPerson[i]=entry$person
			analysis$noun[i]=entry$form
			analysis$nounMatch[i]=entry$match
			analysis$objectMatch[i]=MAX(VMATCH(hearer$nouns[hearer$nouns$ID==analysis$nounID[i],], objects), forceChoice=T, value=T)
		}
		if(analysis$role[i]=='?'){
			candidates=hearer$verbs	#the rest could be a verb...
			candidates$match=FMATCH(analysis$stem[i], candidates)
			entry=candidates[MAX(candidates$match, forceChoice=F),]
			if(nrow(entry)>1){entry=entry[MAX(entry$activation, forceChoice=T),]}
			analysis$verbID[i]=entry$ID
			analysis$verb[i]=entry$form
			analysis$verbType[i]=ifelse(entry$type=='onePlace','onePlace','twoPlace') 
			analysis$verbMatch[i]=entry$match
			analysis$actionMatch[i]=MAX(VMATCH(hearer$verbs[hearer$verbs$ID==analysis$verbID[i],], actions), forceChoice=T, value=T)
			candidates=hearer$nouns	#... or a noun
			candidates$match=FMATCH(analysis$stem[i], candidates)
			entry=candidates[MAX(candidates$match, forceChoice=F),]
			if(nrow(entry)>1){entry=entry[MAX(entry$activation, forceChoice=T),]}
			analysis$nounID[i]=entry$ID
			analysis$nounPerson[i]=entry$person
			analysis$noun[i]=entry$form
			analysis$nounMatch[i]=entry$match
			if(analysis$nounPerson[i]==3){analysis$objectMatch[i]=MAX(VMATCH(hearer$nouns[hearer$nouns$ID==analysis$nounID[i],], objects), forceChoice=T, value=T)}
			if(analysis$nounPerson[i]!=3){analysis$objectMatch[i]=ifelse(analysis$nounPerson[i]%in%unique(situation$personA, situation$personU), 1, -1)}
			if(nrow(analysis)>(i+2)){
				if(analysis$role[i+1]=='suffix' & analysis$role[i+2]=='suffix'){analysis$nounMatch[i]=-1}	#nouns can have single suffix only (change once number is implemented)
			}
			analysis$verbMarkerID[i]=entry$ID	#or a verb adposition
			analysis$verbMarker[i]=entry$form
			analysis$verbMarkerPerson[i]=entry$person
			analysis$verbMarkerMatch[i]=entry$match
			analysis$nounMarkerID[i]=entry$ID	#or a noun adposition
			analysis$nounMarkerPerson[i]=entry$person
			analysis$nounMarker[i]=entry$form
			analysis$nounMarkerMatch[i]=entry$match
	}	}
	analysis$verbScore=analysis$verbMatch * analysis$actionMatch
	analysis$nounScore=analysis$nounMatch * analysis$objectMatch
	if(nrow(analysis)==1){	#if there's only 1 word in the utterance
		analysis$role=ifelse(analysis$verbScore > analysis$nounScore, 'verb', '?')	#default interpretation is noun (cf. H & K)
	} 
	if(nrow(analysis) > 1){
		#first identify verb
		candidates=grep('\\?',analysis$role)
		for (i in candidates){
			arguments=intersect(grep(analysis$word[i], analysis$word, invert=T), grep('\\?', analysis$role))
			argumentScores=tapply(analysis[arguments, ]$nounScore, analysis[arguments,]$word, max)
			analysis$verbScore[i]=prod(analysis$verbScore[i], argumentScores)
		}
		verbIndex=MAX(analysis$verbScore)
		if(length(verbIndex) > 1){	#unresolved? Check plausibility verb suffixex
			suffixScores=rep(0, length(verbIndex))
			for(j in 1:length(verbIndex)){
				suffixes=intersect(grep(analysis$word[verbIndex[j]], analysis$word), grep(analysis$option[verbIndex[j]], analysis$option)); suffixes=intersect(suffixes, grep('suffix', analysis$role))
				suffixScores[j]=prod(analysis[suffixes, ]$verbMarkerMatch)
			}
			verbIndex=verbIndex[MAX(suffixScores)]
		}
		if(length(verbIndex) > 1){	#still unresolved? Check plausibility of alternative analysis
			verbIndex=verbIndex[MIN(analysis[verbIndex,]$nounScore)]
		}
		if(length(verbIndex) > 1){verbIndex=sample(verbIndex, 1)}
		analysis$role[verbIndex]='verb'
		suffixes=grep('suffix',analysis$role)
		if((verbIndex + 1)%in%suffixes){	
			analysis[verbIndex + 1,]$role='verbSuffix'
			if((verbIndex + 2)%in%suffixes){analysis[verbIndex + 2,]$role='verbSuffix'}
		}
		if('suffix'%in%analysis$role){analysis[analysis$role=='suffix',]$role='nounSuffix'}
		analysis=analysis[!(analysis$word==analysis$word[verbIndex] & analysis$option!=analysis$option[verbIndex]),]
		#penalize combinations of nouns with local markers (to be removed if possessive marking is modelled)
		wrong=intersect(grep('nounSuffix', analysis$role), grep(3, analysis$nounMarkerPerson, invert=T))
		#and combinations of nouns with multiple markers (to be removed if case stacking is allowed)
		wrong=unique(c(wrong, intersect(grep('nounSuffix', analysis$role),(grep('nounSuffix', analysis$role) + 1))))
		if(length(wrong)!=0){
			for (i in wrong){
				if(analysis$role[i-1]=='?'){analysis$nounScore[i-1]=0}
				if(analysis$role[i-1]=='nounSuffix'){analysis$nounScore[i-2]=0}
		}	}
		#choose morphological analysis
		for (word in unique(analysis[grep('\\?',analysis$role),]$word)){
			option=MAX(analysis[intersect(grep('\\?', analysis$role), grep(word, analysis$word)),]$nounScore, forceChoice=T)
			analysis=analysis[!(analysis$word==word & analysis$option!=option),]
		}		
		row.names(analysis)=1:nrow(analysis)
	}
	analysis$topic=0
analysis
}

GROUP=function(hearerID, analysis){
	distinctiveness=world$distinctiveness; topicCopy=world$topicCopy
	hearer=population[[hearerID]]
	verb=grep(paste('verb','$',sep=''), analysis$role)
	if(length(verb)==0){verb=0}
	nounSuffixes=grep('nounSuffix',analysis$role)
	verbSuffixes=grep('verbSuffix',analysis$role)
	verbAdposition=verb + 1     #verbAdposition (for topic cross reference)... 
	while(verbAdposition%in%verbSuffixes){verbAdposition=verbAdposition + 1}   #... on top of verb suffixes
	yangIndex=TRUE %in% c(hearer$usageHistory$index$no < ((hearer$usageHistory$index$yes + hearer$usageHistory$index$no)/log(hearer$usageHistory$index$yes + hearer$usageHistory$index$no)))
	if(yangIndex==T & !'verbsuffix'%in%analysis$role){	#reanalyze verbAdposition as verbSuffix if index=T and no other verb suffixes (then no proper suffix was available)
		person=analysis[verbAdposition,]$verbMarkerPerson
		data=hearer$usageHistory$index[hearer$usageHistory$index$person==person,]
		data=data[data$no < (data$yes + data$no)/log(data$yes + data$no),]
		if(nrow(data)!=0){verbAdposition=0}
	}
	words=list()
	for (i in 1:nrow(analysis)){    
		if(i==1 & i!=verb){words[[i]]='?'}
		if(i==verb){words[[i]]='verb'}
		if(i%in%nounSuffixes){words[[i]]='nounSuffix'}
		if(i%in%verbSuffixes){words[[i]]='verbSuffix'}	
		if(i==verbAdposition){words[[i]]=c('verbAdposition','?')}    
		if(i > 1 & i!=verb & i!=verbAdposition & !i%in%nounSuffixes & !i%in%verbSuffixes & analysis$nounPerson[i]==3){words[[i]]=c('nounAdposition','?')}	#only non-local noun markers. to be removed if possessive marking is modelled
		if(i > 1 & i!=verb & i!=verbAdposition & !i%in%nounSuffixes & !i%in%verbSuffixes & analysis$nounPerson[i]!=3){words[[i]]=c('?')}
	}                       
	nAnalyses=1
	for(i in 1:length(words)){nAnalyses=nAnalyses*length(words[[i]])}
	options=as.data.frame(replicate(length(words),rep('',nAnalyses)),stringsAsFactors=F)
	if(nAnalyses==1){options=as.data.frame(t(replicate(length(words),'')), stringsAsFactors=F)}
	for (i in 1:ncol(options)){
		options[,i]=rep(sort(rep(words[[i]],nrow(unique(options)))), length.out=nAnalyses)
	}
	if(ncol(options)>1){	
		impossible=vector()
		n=1
		for (i in 1:nrow(options)){
			for (j in 2:ncol(options)){
				if(options[i,j]=='nounSuffix' & options[i,j-1]!='?'){impossible[n]=i; n=n + 1; break}
				if(options[i,j]=='verbSuffix' & options[i,j-1]!='verb' & options[i,j-1]!='verbSuffix'){impossible[n]=i; n=n + 1; break}
				if(options[i,j]=='nounAdposition' & options[i,j-1]!='?' & options[i,j-1]!='nounSuffix'){impossible[n]=i; n=n + 1; break}
				if(options[i,j]=='verbAdposition' & options[i,j-1]!='verb' & options[i,j-1]!='verbSuffix'){impossible[n]=i; n=n + 1}
		}	}
		if(length(impossible)!=0){options=options[-impossible,]}
	}
	grouping=list(analysis)
	if(nrow(options)!=0){
		for (i in 1:nrow(options)){
			grouping[[i]]=analysis
			grouping[[i]]$role=as.character(options[i,])
	}	}
grouping
}

TYPEMATCH=function(hearerID, analysis){
	hearer=population[[hearerID]]
	if('?'%in%analysis$role){
		verb=analysis[analysis$role=='verb',]
		todo='external'; if(verb$verbType=='twoPlace'){todo=c(todo, 'internal')}
		todo=todo[!todo%in%analysis$role]
		verbSemantics=hearer$verbs[hearer$verbs$ID==verb$verbID,]
		analysis$externalScore=0
		analysis$externalScore=VMATCH(verbSemantics[,grep('^Ext\\d',names(verbSemantics))], hearer$nouns[match(analysis$nounID,hearer$nouns$ID), grep('^D\\d',names(hearer$nouns))])
		if(verb$verbType=='twoPlace'){	
			analysis$internalScore=0
			analysis[!is.na(analysis$nounID),]$internalScore=VMATCH(verbSemantics[,grep('^Int\\d',names(verbSemantics))], hearer$nouns[match(analysis[!is.na(analysis$nounID),]$nounID,hearer$nouns$ID), grep('^D\\d',names(hearer$nouns))])
		}
		if(length(todo)==2 & length(grep('\\?',analysis$role)) > 1){
			externalMax=MAX(analysis[grep('\\?',analysis$role),]$externalScore, value=T)
			if(length(externalMax) > 1){externalMax=0}
			internalMax=MAX(analysis[grep('\\?',analysis$role),]$internalScore, value=T)
			if(length(internalMax) > 1){internalMax=0}
			if(externalMax > internalMax){	
				analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$externalScore)],]$role='external'; todo=todo[todo!='external']
			if(internalMax > externalMax){	
				analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$internalScore)],]$role='internal'; todo=todo[todo!='internal']
		}	}
		if(length(todo)==2 & length(grep('\\?',analysis$role))==1){
			if(analysis[grep('\\?',analysis$role),]$externalScore > (analysis[grep('\\?',analysis$role),]$internalScore)){
				analysis[grep('\\?',analysis$role),]$role='external'; todo=todo[todo!='external']
			}
			if(analysis[grep('\\?',analysis$role),]$internalScore > (analysis[grep('\\?',analysis$role),]$externalScore)){
				analysis[grep('\\?',analysis$role),]$role='internal'; todo=todo[todo!='internal']
		}	}	
		if(length(todo)==1 & 'external'%in%todo){
			if(length(grep('\\?',analysis$role))==1){
				analysis[grep('\\?',analysis$role),]$role='external'; todo=todo[todo!='external']
			}
			if(length(grep('\\?',analysis$role)) > 1){
				externalMax=MAX(analysis[grep('\\?',analysis$role),]$externalScore)
				if(length(externalMax)==1){
					analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$externalScore)],]$role='external'; todo=todo[todo!='external']
		}	}	}
		if(length(todo)==1 & 'internal'%in%todo){
			if(length(grep('\\?',analysis$role))==1){
				analysis[grep('\\?',analysis$role),]$role='internal'; todo=todo[todo!='internal']
			}
			if(length(grep('\\?',analysis$role)) > 1){
				internalMax=MAX(analysis[grep('\\?',analysis$role),]$internalScore, value=T)
				if(length(internalMax)==1){
					analysis[grep('\\?',analysis$role)[MAX(analysis[grep('\\?',analysis$role),]$internalScore)],]$role='internal'; todo=todo[todo!='internal']}
	}	}	}	}
analysis
}

PROTOINTERPRETATION=function(hearerID, analysis){
	#proto reflexive
	if(length(grep('\\?', analysis$role))==2 & length(unique(analysis[grep('\\?', analysis$role),]$nounID))==1 & !'internal'%in%analysis$role & !'external'%in%analysis$role){
		analysis[grep('\\?', analysis$role),]$role=sample(c('external','internal'))
	}
	if('?'%in%analysis$role){
		if(analysis[analysis$role=='verb',]$verbType=='onePlace'){
			if(!'external'%in%analysis$role){
				typeMatch=TYPEMATCH(hearerID, analysis)
				if('external'%in%typeMatch$role){analysis[grep('external', typeMatch$role),]$role='external'}
		}	}
		if(analysis[analysis$role=='verb',]$verbType=='twoPlace'){
			if(!'external'%in%analysis$role | !'internal'%in%analysis$role){
				typeMatch=TYPEMATCH(hearerID, analysis)
				if(!'external'%in%analysis$role & 'external'%in%typeMatch$role){analysis[grep('external',typeMatch$role),]$role='external'}
				if(!'internal'%in%analysis$role & 'internal'%in%typeMatch$role){analysis[grep('internal',typeMatch$role),]$role='internal'}
	}	}	}
	#one left
	if(length(grep('\\?', analysis$role))==1 & !'external'%in%analysis$role){analysis[grep('\\?', analysis$role),]$role='external'}
	if(length(grep('\\?', analysis$role))==1 & !'internal'%in%analysis$role){analysis[grep('\\?', analysis$role),]$role='internal'}
	#guess
	if(length(grep('\\?', analysis$role))>1){
		if(!'internal'%in%analysis$role & !'external'%in%analysis$role){analysis[sample(grep('\\?', analysis$role),2),]$role=c('external', 'internal')}
		if(!'internal'%in%analysis$role){analysis[sample(grep('\\?', analysis$role),1),]$role='internal'}
		if(!'external'%in%analysis$role){analysis[sample(grep('\\?', analysis$role),1),]$role='external'}
	}	
analysis
}

AGENTFIRSTINT=function(hearerID, analysis){
	hearer=population[[hearerID]]
	if(length(grep('\\?', analysis$role))>1){
		verb=analysis[analysis$role=='verb',]
		if(verb$verbType=='twoPlace'){
			verbSemantics=hearer$verbs[hearer$verbs$ID==verb$verbID,]
			actor=ifelse(ACTOR(verbSemantics[,grep('^Ext\\d',names(verbSemantics))], verbSemantics[,grep('^Int\\d',names(verbSemantics))])==1,'external','internal')
			if(!actor%in%analysis$role){analysis[grep('\\?', analysis$role)[1],]$role=actor}
	}	}
analysis
}

NOUNMORPHOLOGY=function(hearerID, analysis){
	distinctiveness=world$distinctiveness
	hearer=population[[hearerID]]
	nouns=hearer$nouns; verbs=hearer$verbs
	if('nounSuffix'%in%analysis$role | 'nounAdposition'%in%analysis$role){	
		analysis$nounSuffixExternalScore=0; analysis$nounSuffixInternalScore=0		
		for (i in grep('nounSuffix', analysis$role)){
			nounSuffixProfile=nouns[nouns$ID==analysis[i,]$nounMarkerID,grep('^D\\d',names(nouns))]
			external=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Ext\\d',names(verbs))]
			internal=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Int\\d',names(verbs))]
			analysis[i,]$nounSuffixExternalScore=VMATCH(nounSuffixProfile, external)
			analysis[i,]$nounSuffixInternalScore=VMATCH(nounSuffixProfile, internal)
		}
		if(length(MAX(analysis$nounSuffixExternalScore, forceChoice=F))==1){
			external=MAX(analysis$nounSuffixExternalScore)
			if(analysis[external,]$nounSuffixExternalScore > (analysis[external,]$nounSuffixInternalScore + distinctiveness)){	#if nounSuffix is cleary external
				if(analysis[external,]$nounSuffixExternalScore > analysis[MAX(analysis$nounSuffixExternalScore, 2, forceChoice=T),]$nounSuffixExternalScore + distinctiveness){	#and sufficiently better than second best
					if('external'%in%analysis$role){analysis[analysis$role=='external',]$role='?'}	#overwrite alternative analyses
					analysis[external-1,]$role='external'
		}	}	}	
		if(length(MAX(analysis$nounSuffixInternalScore, forceChoice=F))==1){
			internal=MAX(analysis$nounSuffixInternalScore)
			if(analysis[internal,]$nounSuffixInternalScore > (analysis[internal,]$nounSuffixExternalScore + distinctiveness)){	
				if(analysis[internal,]$nounSuffixInternalScore > analysis[MAX(analysis$nounSuffixInternalScore, 2, forceChoice=T),]$nounSuffixInternalScore + distinctiveness){	
					if('internal'%in%analysis$role){analysis[analysis$role=='internal',]$role='?'}
					analysis[internal-1,]$role='internal'
	}	}	}	}	
	if('nounAdposition'%in%analysis$role){	
		analysis$nounAdpositionExternalScore=0; analysis$nounAdpositionInternalScore=0	#future work: allow for oblique roles
		for (i in grep('nounAdposition', analysis$role)){
			nounAdpositionProfile=nouns[nouns$ID==analysis[i,]$nounID,grep('^D\\d',names(nouns))]
			external=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Ext\\d',names(verbs))]
			internal=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,grep('^Int\\d',names(verbs))]
			analysis[i,]$nounAdpositionExternalScore=VMATCH(nounAdpositionProfile, external)
			analysis[i,]$nounAdpositionInternalScore=VMATCH(nounAdpositionProfile, internal)
		}
		if(length(MAX(analysis$nounAdpositionExternalScore, forceChoice=F))==1){
			external=MAX(analysis$nounAdpositionExternalScore)
			if(analysis[external,]$nounAdpositionExternalScore > (analysis[external,]$nounAdpositionInternalScore + distinctiveness)){	
				if(analysis[external,]$nounAdpositionExternalScore > analysis[MAX(analysis$nounAdpositionExternalScore, 2, forceChoice=T),]$nounAdpositionExternalScore + distinctiveness){	
					if('external'%in%analysis$role){analysis[analysis$role=='external',]$role='?'}
					if(analysis[external-1,]$role!='nounSuffix'){	
						analysis[external-1,]$role='external'	
					}
					if(analysis[external-1,]$role=='nounSuffix'){	#marker overrules suffix with same host
						analysis[external-2,]$role='external'		
		}	}	}	}	
		if(length(MAX(analysis$nounAdpositionInternalScore, forceChoice=F))==1){
			internal=MAX(analysis$nounAdpositionInternalScore)
			if(analysis[internal,]$nounAdpositionInternalScore > (analysis[internal,]$nounAdpositionExternalScore + distinctiveness)){	
				if(analysis[internal,]$nounAdpositionInternalScore > analysis[MAX(analysis$nounAdpositionInternalScore, 2, forceChoice=T),]$nounAdpositionInternalScore + distinctiveness){	
					if('internal'%in%analysis$role){analysis[analysis$role=='internal',]$role='?'}
					if(analysis[internal-1,]$role!='nounSuffix'){	
						analysis[internal-1,]$role='internal'	
					}
					if(analysis[internal-1,]$role=='nounSuffix'){	
						analysis[internal-2,]$role='internal'		
	}	}	}	}	}	
analysis
}

WORDORDER=function(hearerID, analysis){
	hearer=population[[hearerID]]
	positions=grep('\\?|^verb$', analysis$role)
	verb=hearer$verbs[hearer$verbs$ID==analysis[analysis$role=='verb',]$verbID,]
	actor=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')
	undergoer=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'internal', 'external')
	verbPosition=grep(grep('^verb$', analysis$role), positions)
	if(sum(hearer$wordOrder$success)>8){	#n exceptions should minimally be 4(=8/ln(8)) for Yang
		yangTopic=TRUE %in% hearer$topicPosition$success[hearer$topicPosition$position=='other'] < (sum(hearer$topicPosition$success)/log(sum(hearer$topicPosition$success)))
		yangWordOrder=TRUE %in% 
			#any particular order
			c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) |
			#A first
			c(sum(hearer$wordOrder[grep('^A', hearer$wordOrder$order, invert=T), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) |
			#UV
			c(sum(hearer$wordOrder[grep('UV', hearer$wordOrder$order, invert=T), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) |
			#VU
			c(sum(hearer$wordOrder[grep('VU', hearer$wordOrder$order, invert=T), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success)))) 
		if(yangTopic==T){analysis$topic[1]=1}
		if(yangWordOrder==T){
			if(analysis[analysis$role=='verb',]$verbType=='twoPlace'){
				##no particular order
				if(!TRUE %in% c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
					#A first
					if(TRUE %in% c(sum(hearer$wordOrder[grep('^A', hearer$wordOrder$order, invert=T), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
						if(analysis[1,]$role=='?' & !actor%in%analysis$role){analysis[1,]$role=actor}
					}
					#UV
					if(TRUE %in% c(sum(hearer$wordOrder[grep('UV', hearer$wordOrder$order, invert=T), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
						if(verbPosition!=1){if(analysis[positions[verbPosition-1],]$role=='?' & !undergoer%in%analysis$role){analysis[positions[verbPosition-1],]$role=undergoer}}
					}	
					#VU
					if(TRUE %in% c(sum(hearer$wordOrder[grep('VU', hearer$wordOrder$order, invert=T), ]$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
						if((verbPosition+1)<=length(positions)){if(analysis[positions[verbPosition+1],]$role=='?' & !undergoer%in%analysis$role){analysis[positions[verbPosition+1],]$role=undergoer}}
				}	}
				##particular orders
				if(TRUE %in% c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))){
					order=hearer$wordOrder$order[c((sum(hearer$wordOrder$success)-hearer$wordOrder$success) < (sum(hearer$wordOrder$success)/log(sum(hearer$wordOrder$success))))]
					order=unlist(strsplit(order, ''))
					actorPosition=grep('A', order)-grep('V', order)
					if((verbPosition+actorPosition)>0 & (verbPosition+actorPosition)<=length(positions)){
						if(!actor%in%analysis$role & analysis[positions[verbPosition+actorPosition],]$role=='?'){
							analysis[positions[verbPosition+actorPosition],]$role=actor
					}	}
					undergoerPosition=grep('U', order)-grep('V', order)
					if( (verbPosition+undergoerPosition)>0 & (verbPosition+undergoerPosition)<=length(positions) ){
						if(!undergoer%in%analysis$role & analysis[positions[verbPosition+undergoerPosition],]$role=='?'){
							analysis[positions[verbPosition+undergoerPosition],]$role=undergoer
	}	}	}	}	}	}
analysis			
}

VERBMORPHOLOGY=function(hearerID, analysis){
	topicCopy=world$topicCopy; refCheck=world$refCheck; referenceThreshold=world$referenceThreshold; distinctiveness=world$distinctiveness; collostructionImpact=world$collostructionImpact
	hearer=population[[hearerID]]
	nouns=hearer$nouns
	verbs=hearer$verbs
	if('verbSuffix'%in%analysis$role | 'verbAdposition'%in%analysis$role){
		analysis$verbMarkerTarget='?'
		yangTopic=ifelse(sum(hearer$wordOrder$success)>8 & TRUE %in% hearer$topicPosition$success[hearer$topicPosition$position=='other'] < (sum(hearer$topicPosition$success)/log(sum(hearer$topicPosition$success))), TRUE, FALSE)
		yangIndex=TRUE %in% c(hearer$usageHistory$index$no < ((hearer$usageHistory$index$yes + hearer$usageHistory$index$no)/log(hearer$usageHistory$index$yes + hearer$usageHistory$index$no)))
	}
	if('verbSuffix'%in%analysis$role){	
		verbSuffixes=grep('verbSuffix', analysis$role)
		targets=grep('\\?|internal|external', analysis$role)
		verb=hearer$verbs[hearer$verbs$ID==analysis[grep('^verb$',analysis$role),]$verbID,]
		###reanalyze?
		if(verb$type=='onePlace'){
			if(length(verbSuffixes)==1 & length(targets)==0){analysis[verbSuffixes,]$role='?'}
		}
		if(verb$type=='twoPlace'){
			if(length(targets)<2 & 'verbAdposition'%in%analysis$role){	#if too few targets, first reanalyze verb adposition
				analysis[grep('verbAdposition',analysis$role), ]$role='?'
				targets=grep('\\?|internal|external', analysis$role)
			}
			if(length(targets)<2 & length(verbSuffixes)==1){analysis[verbSuffixes,]$role='?'}	#still too few targets, reanalyze verb suffix as pro-index
			if(length(targets)==1 & length(verbSuffixes)==2){analysis[verbSuffixes[2],]$role='?'}	
			if(length(targets)==0 & length(verbSuffixes)==2){analysis[verbSuffixes,]$role=c('internal', 'external')}	#if no targets, reanalyse as pro-indexes; inner verb suffix has internal role
		}
		verbSuffixes=grep('verbSuffix', analysis$role)
	}
	if('verbAdposition'%in%analysis$role){		
		if(analysis[1,]$role!='verb'){analysis[grep('verbAdposition',analysis$role), ]$verbMarkerTarget=1}
	}
	#check again
	if('verbSuffix'%in%analysis$role){	
		proto=PROTOINTERPRETATION(hearerID, analysis)	#only adds unknown roles, does not overwrite anything							
		###intransitives
		if(verb$type=='onePlace'){
			if(length(targets)==1 & length(verbSuffixes)==1){
				#simple person match
				if(analysis[verbSuffixes,]$verbMarkerPerson==analysis[targets,]$nounPerson){
					analysis[verbSuffixes,]$verbMarkerTarget=targets
				}
				#recruitment
				if(refCheck==T & nchar(analysis[verbSuffixes,]$form)<=referenceThreshold){
					analysis[verbSuffixes,]$verbMarkerTarget=targets
		}	}	}	
		###transitives
		if(verb$type=='twoPlace'){
			#single suffix
			if(length(verbSuffixes)==1){
				v=verbSuffixes
				role='unclear'
				person=analysis[v,]$verbMarkerPerson
				data=hearer$usageHistory$index[hearer$usageHistory$index$person==person,]
				data=data[data$no < (data$yes + data$no)/log(data$yes + data$no),]
				if(nrow(data)!=0){
					role=data$role[MAX((data$yes + data$no)/log(data$yes + data$no), forceChoice=T)]
					if(role=='actor'){role=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')}
					if(role=='undergoer'){role=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'internal', 'external')}
				}
				#single person pair
				if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))==1){	
					analysis[v,]$verbMarkerTarget=targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)]
				}
				#person pairs with multiple targets: use role?
				if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))>1){
					if(role%in%analysis$role){
						analysis[v,]$verbMarkerTarget=grep(role, analysis$role)
					} else if(role%in%proto$role){
						analysis[v,]$verbMarkerTarget=grep(role, proto$role)
				}	}
				#no match, recruitment?
				if(refCheck==T & !analysis[v,]$verbMarkerPerson %in% analysis[targets,]$nounPerson & nchar(analysis[v,]$form)<=referenceThreshold & length(targets)!=0){
					#single target
					if(length(grep(3, analysis[targets,]$nounPerson))==1){
						analysis[v,]$verbMarkerTarget=targets[grep(3, analysis[targets,]$nounPerson)]
					}
					#multiple targets
					if(length(grep(3, analysis[targets,]$nounPerson))>1){
						if(role%in%analysis$role){
							analysis[v,]$verbMarkerTarget=grep(role, analysis$role)
						} else if(role%in%proto$role){
							analysis[v,]$verbMarkerTarget=grep(role, proto$role)
						}
						#else, use prominence
						if(nrow(data)==0){
							analysis[v,]$verbMarkerTarget=targets[MAX(VMATCH(rep(1, length(grep('^D\\d', names(nouns)))), nouns[match(analysis[targets,]$nounID, nouns$ID), grep('^D\\d', names(nouns))]), forceChoice=T)]
			}	}	}	}
			#multiple suffixes
			if(length(verbSuffixes)==2){
				for(v in verbSuffixes){
					#unique person pairs
					if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))==1 & length(grep(analysis[v,]$verbMarkerPerson, analysis[verbSuffixes,]$verbMarkerPerson))==1){	
						analysis[v,]$verbMarkerTarget=targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)]
					}	
					#multiple person pairs with different targets: internal suffix internal role 
					if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))>1){
						if(grep(v, verbSuffixes)==1){
							if('internal'%in%analysis$role){
								analysis[v,]$verbMarkerTarget=grep('internal', analysis$role)
							} else if('internal'%in%proto$role){
								analysis[v,]$verbMarkerTarget=grep('internal', proto$role)
						}	}
						if(grep(v, verbSuffixes)==2){
							if('external'%in%analysis$role){
								analysis[v,]$verbMarkerTarget=grep('external', analysis$role)
							} else if ('external'%in%proto$role){
								analysis[v,]$verbMarkerTarget=grep('external', proto$role)
					}	}	}
					#multiple person pairs with single target
					if(length(grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson))==1 & length(grep(analysis[v,]$verbMarkerPerson, analysis[verbSuffixes,]$verbMarkerPerson))==2){
						#first try role (if explicitly marked)
						if(grep(v, verbSuffixes)==1){
							if('internal'%in%analysis$role){analysis[v,]$verbMarkerTarget=grep('internal', analysis$role)}
						}
						if(grep(v, verbSuffixes)==2){
							if('external'%in%analysis$role){analysis[v,]$verbMarkerTarget=grep('external', analysis$role)}
						}
						#next try collostructions
						if(!'internal'%in%analysis$role & !'external'%in%analysis$role){
							collostructions=hearer$collostructions$index[hearer$collostructions$index$N==analysis[targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)],]$nounID,]
							collostruction=rep(0, 2)
							if(nrow(collostructions)!=0){collostruction[analysis[verbSuffixes,]$verbMarkerID%in%collostructions$marker]=collostructions[na.omit(match(analysis[verbSuffixes,]$verbMarkerID, collostructions$marker)),]$frequency}
							analysis[verbSuffixes[MAX(collostruction, forceChoice=T)],]$verbMarkerTarget=targets[grep(analysis[v,]$verbMarkerPerson, analysis[targets,]$nounPerson)]
					}	}	
					#no match, recruitment?
					if(refCheck==T & analysis[v,]$verbMarkerTarget=='?'){
						if(nchar(analysis[v,]$form)<=referenceThreshold & length(targets)!=0){
							if(length(grep(3,analysis[targets,]$nounPerson))==1){
								analysis[v,]$verbMarkerTarget=targets[grep(3,analysis[targets,]$nounPerson)]
							}
							if(length(grep(3,analysis[targets,]$nounPerson))>1){
								if(grep(v, verbSuffixes)==1){
									if('internal'%in%analysis$role){
										analysis[v,]$verbMarkerTarget=grep('internal', analysis$role)
									} else if ('internal'%in%proto$role){
								analysis[v,]$verbMarkerTarget=grep('internal', proto$role)
								}	}
								if(grep(v, verbSuffixes)==2){
									if('external'%in%analysis$role){
										analysis[v,]$verbMarkerTarget=grep('external', analysis$role)
									} else if ('external'%in%proto$role){
										analysis[v,]$verbMarkerTarget=grep('external', proto$role)
					}	}	}	}	}
					targets=targets[!targets%in%analysis[verbSuffixes,]$verbMarkerTarget]
		}	}	}						
		##role resolution
		targets=analysis[verbSuffixes,]$verbMarkerTarget
		if(!'?'%in%targets){
			if(length(verbSuffixes)==2){	
				if(analysis[targets[1],]$role=='?' & !'internal'%in%analysis$role){analysis[targets[1],]$role='internal'}
				if(analysis[targets[2],]$role=='?' & !'external'%in%analysis$role){analysis[targets[2],]$role='external'}
			}
			if(length(verbSuffixes)==1){
				if(analysis[targets,]$role=='?'){
					#if generalization is made
					if(yangIndex==T){
						person=analysis[verbSuffixes,]$verbMarkerPerson
						data=hearer$usageHistory$index[hearer$usageHistory$index$person==person,]
						data=data[data$no < (data$yes + data$no)/log(data$yes + data$no),]
						#if relevant generalization is made
						if(nrow(data)!=0){
							role=data$role[MAX((data$yes + data$no)/log(data$yes + data$no), forceChoice=T)]
							if(role=='actor'){role=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'external', 'internal')}
							if(role=='undergoer'){role=ifelse(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1, 'internal', 'external')}
							if(!role%in%analysis$role){analysis[targets,]$role==role}
					}	}
					#if no generalization is made and suffix is distinctive for role (e.g. in case of fusion of role-distinctive pronoun)
					if(analysis[targets,]$role=='?'){
						verbSuffix=analysis[verbSuffixes,]$verbMarkerID
						externalMatch=VMATCH(verb[,grep('^Ext\\d',names(verb))], nouns[nouns$ID==verbSuffix,grep('^D\\d',names(nouns))])
						internalMatch=VMATCH(verb[,grep('^Int\\d',names(verb))], nouns[nouns$ID==verbSuffix,grep('^D\\d',names(nouns))])
						externalCollostruction=sum(hearer$collostructions$SV[grep(paste('^', verbSuffix, '$', sep=''), hearer$collostructions$SV$S),]$frequency)
						internalCollostruction=sum(hearer$collostructions$OV[grep(paste('^', verbSuffix, '$', sep=''), hearer$collostructions$OV$O),]$frequency)
						externalCollostruction=externalCollostruction/(externalCollostruction+internalCollostruction); if(is.na(externalCollostruction)){externalCollostruction=0}
						internalCollostruction=internalCollostruction/(internalCollostruction+externalCollostruction); if(is.na(internalCollostruction)){internalCollostruction=0}
						role=ifelse((externalMatch+collostructionImpact*externalCollostruction)>=(internalMatch+collostructionImpact*internalCollostruction), 'external', 'internal')
						if(!role%in%analysis$role){analysis[targets,]$role==role}
	}	} 	}	}	}
analysis
}

INTERPRET.INT=function(hearerID, analysis, situation){	
	interpretation=list()
	hearer=population[[hearerID]]
	verbs=hearer$verbs; nouns=hearer$nouns
	#result
	verb=''
	if('verb'%in%analysis$role){
		verb=verbs[verbs$ID==analysis[analysis$role=='verb',]$verbID,]
		verb$form=analysis[analysis$role=='verb',]$stem
		if('verbSuffix'%in%analysis$role){
			for(i in grep('verbSuffix', analysis$role)){
				if('internal'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='internal',]$nounPerson=analysis[i,]$verbMarkerPerson
					verb$intMarkerID=analysis[i,]$verbMarkerID
					verb$intMarker=analysis[i,]$stem
					verb$intMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$intMarkerFrequency=nouns[nouns$ID==verb$intMarkerID,]$verbMarker
					verb$intMarkerSemanticWeight=nouns[nouns$ID==verb$intMarkerID,]$semanticWeight
				}
				if('external'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='external',]$nounPerson=analysis[i,]$verbMarkerPerson
					verb$extMarkerID=analysis[i,]$verbMarkerID
					verb$extMarker=analysis[i,]$stem
					verb$extMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$extMarkerFrequency=nouns[nouns$ID==verb$extMarkerID,]$verbMarker
					verb$extMarkerSemanticWeight=nouns[nouns$ID==verb$extMarkerID,]$semanticWeight
		}	}	}
		if('verbAdposition'%in%analysis$role){	#NB verbAdpositions overrule verbSuffixes... 
			for(i in grep('verbAdposition', analysis$role)){
				if('external'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='external',]$nounPerson=analysis[i,]$nounPerson
					verb$extMarkerID=analysis[i,]$nounID
					verb$extMarker=analysis[i,]$stem
					verb$extMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$extMarkerFrequency=nouns[nouns$ID==verb$extMarkerID,]$verbMarker
					verb$extMarkerSemanticWeight=nouns[nouns$ID==verb$extMarkerID,]$semanticWeight
				}
				if('internal'%in%analysis[analysis[i,]$verbMarkerTarget,]$role){	
					analysis[analysis$role=='internal',]$nounPerson=analysis[i,]$nounPerson
					verb$intMarkerID=analysis[i,]$nounID
					verb$intMarker=analysis[i,]$stem
					verb$intMarkerTarget=analysis[analysis[i,]$verbMarkerTarget,]$nounID
					verb$intMarkerFrequency=nouns[nouns$ID==verb$intMarkerID,]$verbMarker
					verb$intMarkerSemanticWeight=nouns[nouns$ID==verb$intMarkerID,]$semanticWeight
	}	}	}	}
	internal=''
	if('internal'%in%analysis$role){
		if(!length(grep('internal',analysis$role)) > 1){
			internal=nouns[nouns$ID==analysis[analysis$role=='internal',]$nounID,]
			internal$form=analysis[analysis$role=='internal',]$stem
			internal$person=analysis[analysis$role=='internal',]$nounPerson
			internal$topic=0
			if(analysis$topic[1]!=1 & nouns[nouns$ID==internal$ID,]$recency==min(nouns$recency)){internal$topic=1}
			if(analysis$topic[1]==1 & grep('internal', analysis$role)==1){internal$topic=1}
			if(grep('internal', analysis$role) < nrow(analysis)){
				if(analysis$role[grep('internal',analysis$role) + 1]=='nounSuffix'){
					internal$markerID=analysis[grep('internal',analysis$role) + 1,]$nounMarkerID
					internal$marker=analysis[grep('internal',analysis$role) + 1,]$stem
					internal$markerFrequency=nouns[nouns$ID==internal$markerID,]$nounMarker
					internal$markerSemanticWeight=nouns[nouns$ID==internal$markerID,]$semanticWeight
					if((grep('internal',analysis$role) + 1) < nrow(analysis)){
						if(analysis$role[grep('internal',analysis$role) + 2]=='nounAdposition'){
							internal$markerID=analysis[grep('internal',analysis$role) + 2,]$nounID
							internal$marker=analysis[grep('internal',analysis$role) + 2,]$stem
							internal$markerFrequency=nouns[nouns$ID==internal$markerID,]$nounMarker
							internal$markerSemanticWeight=nouns[nouns$ID==internal$markerID,]$semanticWeight
				}	}	}
				if(analysis$role[grep('internal',analysis$role) + 1]=='nounAdposition'){
					internal$markerID=analysis[grep('internal',analysis$role) + 1,]$nounID
					internal$marker=analysis[grep('internal',analysis$role) + 1,]$stem
					internal$markerFrequency=nouns[nouns$ID==internal$markerID,]$nounMarker
					internal$markerSemanticWeight=nouns[nouns$ID==internal$markerID,]$semanticWeight
	}	}	}	}				
	external=''
	if('external'%in%analysis$role){
		if(!length(grep('external',analysis$role)) > 1){
			external=nouns[nouns$ID==analysis[analysis$role=='external',]$nounID,]
			external$form=analysis[analysis$role=='external',]$stem
			external$person=analysis[analysis$role=='external',]$nounPerson
			external$topic=0
			if(analysis$topic[1]!=1 & nouns[nouns$ID==external$ID,]$recency==min(nouns$recency)){external$topic=1}
			if(analysis$topic[1]==1 & grep('external', analysis$role)==1){external$topic=1}
			if(grep('external', analysis$role) < nrow(analysis)){
				if(analysis$role[grep('external',analysis$role) + 1]=='nounSuffix'){
					external$markerID=analysis[grep('external',analysis$role) + 1,]$nounMarkerID
					external$marker=analysis[grep('external',analysis$role) + 1,]$stem
					external$markerFrequency=nouns[nouns$ID==external$markerID,]$nounMarker
					external$markerSemanticWeight=nouns[nouns$ID==external$markerID,]$semanticWeight
					if((grep('external',analysis$role) + 1) < nrow(analysis)){
						if(analysis$role[grep('external',analysis$role) + 2]=='nounAdposition'){
							external$markerID=analysis[grep('external',analysis$role) + 2,]$nounID
							external$marker=analysis[grep('external',analysis$role) + 2,]$nounMarker
							external$marker=analysis[grep('external',analysis$role) + 2,]$stem
							external$markerFrequency=nouns[nouns$ID==external$markerID,]$nounMarker
							external$markerSemanticWeight=nouns[nouns$ID==external$markerID,]$semanticWeight
				}	}	}
				if(analysis$role[grep('external',analysis$role) + 1]=='nounAdposition'){
					external$markerID=analysis[grep('external',analysis$role) + 1,]$nounID
					external$marker=analysis[grep('external',analysis$role) + 1,]$nounMarker
					external$marker=analysis[grep('external',analysis$role) + 1,]$stem
					external$markerFrequency=nouns[nouns$ID==external$markerID,]$nounMarker
					external$markerSemanticWeight=nouns[nouns$ID==external$markerID,]$semanticWeight
	}	}	}	}		
	if(nrow(situation) > 1){
		if(is.data.frame(verb)){
			situation$verbMatch=VMATCH(verb[,grep('^D\\d',names(verb))], situation[,grep('^V\\d',names(situation))])
		} else {situation$verbMatch=-1}
		if(is.data.frame(external)){
			if(external$person==3){
				candidates=situation$personA
				if(sum(candidates==3, na.rm=T)>1){situation$externalMatch=VMATCH(external[,grep('^D\\d',names(external))], situation[,grep('^A\\d',names(situation))])}
				if(sum(candidates==3, na.rm=T)==1){situation$externalMatch=ifelse(external$person==situation$personA, 1, -1)}
				if(sum(candidates==3, na.rm=T)==0){situation$externalMatch=-1}
			}
			if(external$person!=3){situation$externalMatch=ifelse(external$person==situation$personA, 1, -1)}
		} else {situation$externalMatch=-1}
		if(is.data.frame(internal)){	
			if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==1){
				if(internal$person==3){
					candidates=situation$personU
					if(sum(candidates==3, na.rm=T)>1){situation$internalMatch=VMATCH(internal[,grep('^D\\d',names(internal))], situation[,grep('^U\\d',names(situation))])}
					if(sum(candidates==3, na.rm=T)==1){situation$internalMatch=ifelse(internal$person==situation$personU, 1, -1)}
					if(sum(candidates==3, na.rm=T)==0){situation$internalMatch=-1}
				}
				if(internal$person!=3){situation$internalMatch=ifelse(internal$person==situation$personU, 1, -1)}
			}
			if(ACTOR(verb[,grep('^Ext\\d',names(verb))], verb[,grep('^Int\\d',names(verb))])==2){
				if(internal$person==3){
					candidates=situation$personA
					if(sum(candidates==3, na.rm=T)>1){situation$internalMatch=VMATCH(internal[,grep('^D\\d',names(internal))], situation[,grep('^A\\d',names(situation))])}
					if(sum(candidates==3, na.rm=T)==1){situation$internalMatch=ifelse(internal$person==situation$personA, 1, -1)}
					if(sum(candidates==3, na.rm=T)==0){situation$internalMatch=-1}
				}
				if(internal$person!=3){situation$internalMatch=ifelse(internal$person==situation$personA, 1, -1)}
				if(is.data.frame(external)){
					if(external$person==3){
						candidates=situation$personU
						if(sum(candidates==3, na.rm=T)>1){situation$externalMatch=VMATCH(external[,grep('^D\\d',names(external))], situation[,grep('^U\\d',names(situation))])}
						if(sum(candidates==3, na.rm=T)==1){situation$externalMatch=ifelse(external$person==situation$personU, 1, -1)}
						if(sum(candidates==3, na.rm=T)==0){situation$externalMatch=-1}
					}
					if(external$person!=3){situation$externalMatch=ifelse(external$person==situation$personU, 1, -1)}
				} else {situation$externalMatch=-1}
			}
		} else {situation$internalMatch=-1; if(NA%in%situation$U1){situation[is.na(situation$U1),]$internalMatch=0}}	
		situation$penalty=length(grep('\\?', analysis$role))*-1 + length(grep('\\?', analysis[grep('verbSuffix', analysis$role),]$verbMarkerTarget))*-1
		situation$totalMatch=situation$verbMatch + situation$externalMatch + situation$internalMatch + situation$penalty
		target=situation[MAX(situation$totalMatch),]
		if(nrow(target) > 1){
			if(1%in%target$target){target=target[target$target==1,]} else {target=target[sample(nrow(target),1),]}
		}
	} else {target=situation}
	interpretationOrder=analysis$role
	interpretationOrder=interpretationOrder[interpretationOrder%in%c('external','internal','verb')]
	interpretationOrder=c(interpretationOrder, 'target')
	interpretation=list(verb=verb, external=external, internal=internal, target=target)
	interpretation=interpretation[interpretationOrder]
interpretation	
}

INTERPRET=function(hearerID, utterance, situation){
	wordOrder=world$wordOrder; frequency=world$frequency
	hearer=population[[hearerID]]
	nEvents=nrow(situation) 
	interpretation=list()
	analysis=ANALYZE(hearerID, utterance, situation)
	grouping=GROUP(hearerID, analysis)
	for(j in 1:length(grouping)){
		#first use explicit role marking
		grouping[[j]]=NOUNMORPHOLOGY(hearerID, grouping[[j]])
		#then word order (if still necessary)
		if(wordOrder==T){
			if('verb'%in%grouping[[j]]$role){
				grouping[[j]]=WORDORDER(hearerID, grouping[[j]])
		}	}
		#then verb morphology (idem)
		grouping[[j]]=VERBMORPHOLOGY(hearerID, grouping[[j]])
		solved=T
		if('verb'%in%grouping[[j]]$role){
			if(grouping[[j]][grouping[[j]]$role=='verb',]$verbType=='twoPlace'){
				if(!'external'%in%grouping[[j]]$role | !'internal'%in%grouping[[j]]$role){
					solved=F
			}	}
			if(grouping[[j]][grouping[[j]]$role=='verb',]$verbType=='onePlace'){
				if(!'external'%in%grouping[[j]]$role){
					solved=F
			}	}
			if(solved==F){	
				grouping[[j]]=PROTOINTERPRETATION(hearerID, grouping[[j]])	
		}	}
		interpretation.int=INTERPRET.INT(hearerID, grouping[[j]], situation)
		if(interpretation.int$target$totalMatch>0){interpretation[[length(interpretation) + 1]]=interpretation.int}
	}	
	if(length(interpretation)==1){scores=1}
	if(length(interpretation) > 1){
		if(nEvents > 1){
			scores=vector()
			for(i in 1:length(interpretation)){
				scores[i]=interpretation[[i]]$target$totalMatch
		}	}
		if(nEvents==1){
			scores=vector()
			for(i in 1:length(interpretation)){
				verb=interpretation[[i]]$verb
				scores[i]=0
					if('extMarkerID'%in%names(verb)){
					if(frequency=='relative'){scores[i]=scores[i] + interpretation[[i]]$verb$extMarkerFrequency/max(hearer$nouns$verbMarker)}
					if(frequency=='absolute'){scores[i]=scores[i] + interpretation[[i]]$verb$extMarkerFrequency/max(hearer$nouns$frequency)}
				}
				if('intMarkerID'%in%names(verb)){
					if(frequency=='relative'){scores[i]=scores[i] + interpretation[[i]]$verb$intMarkerFrequency/max(hearer$nouns$verbMarker)}
					if(frequency=='absolute'){scores[i]=scores[i] + interpretation[[i]]$verb$intMarkerFrequency/max(hearer$nouns$frequency)}
				}
				if(is.data.frame(interpretation[[i]]$external)){
					external=interpretation[[i]]$external
					collostruction=population[[hearerID]]$collostructions$SV[intersect(grep(paste('^',verb$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$V), grep(paste('^',external$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$S)),]$frequency
					if(length(collostruction)==0){collostruction=0}
					interpretation[[i]]$external$collostruction=collostruction
					if(frequency=='relative'){
						scores[i]=scores[i] + interpretation[[i]]$external$argument/max(hearer$nouns$argument) + interpretation[[i]]$external$collostruction/max(hearer$collostructions$SV$freq)
						if('marker'%in%names(external)){scores[i]=scores[i] + external$markerFrequency/max(hearer$nouns$nounMarker)}
					}
					if(frequency=='absolute'){
						scores[i]=scores[i] + interpretation[[i]]$internal$argument/max(hearer$nouns$frequency) + interpretation[[i]]$external$collostruction/max(hearer$collostructions$SV$freq)
						if('marker'%in%names(external)){scores[i]=scores[i] + external$markerFrequency/max(hearer$nouns$frequency)}
				}	}
				if(is.data.frame(interpretation[[i]]$internal)){
					internal=interpretation[[i]]$internal
					collostruction=population[[hearerID]]$collostructions$SV[intersect(grep(paste('^',verb$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$V), grep(paste('^',internal$ID,'$',sep=''), population[[hearerID]]$collostructions$SV$S)),]$frequency
					if(length(collostruction)==0){collostruction=0}
					interpretation[[i]]$internal$collostruction=collostruction
					if(frequency=='relative'){
						scores[i]=scores[i] + interpretation[[i]]$internal$argument/max(hearer$nouns$argument) + interpretation[[i]]$internal$collostruction/max(hearer$collostructions$OV$freq)
						if('marker'%in%names(internal)){scores[i]=scores[i] + internal$markerFrequency/max(hearer$nouns$nounMarker)}
					}
					if(frequency=='absolute'){
						scores[i]=scores[i] + interpretation[[i]]$internal$argument/max(hearer$nouns$frequency) + interpretation[[i]]$internal$collostruction/max(hearer$collostructions$OV$freq)
						if('marker'%in%names(internal)){scores[i]=scores[i] + internal$markerFrequency/max(hearer$nouns$frequency)}
	}	}	}	}	}
	if(length(interpretation)!=0){
		interpretation=interpretation[[MAX(scores, forceChoice=T)]]
	} else {interpretation='?'}	
interpretation
}

########### INTERACTION ##################

SUCCESS=function(proposition, interpretation, situation){
	distinctiveness=world$distinctiveness
	success=0
	if(is.list(interpretation)){
		if(nrow(situation) > 1){
			success=ifelse(interpretation$target$target==1, 1, 0)
		} else {
			if(!'verb'%in%names(interpretation) | !'verb'%in%names(proposition)){
				intended=proposition$external[grep('D\\d',names(proposition$external))]
				understood=interpretation$external[grep('D\\d',names(interpretation$external))]
			}
			if('verb'%in%names(interpretation) & 'verb' %in%names(proposition)){
				if(proposition$verb$type=='onePlace'){intended=cbind(proposition$verb, proposition$external); intended=intended[grep('D\\d',names(intended))]}
				if(proposition$verb$type=='twoPlace'){intended=cbind(proposition$verb, proposition$external, proposition$internal); intended=intended[grep('D\\d',names(intended))]}
				if(interpretation$verb$type=='onePlace'){
					understood=interpretation$verb[grep('D\\d',names(interpretation$verb))]
					if('external'%in%names(interpretation)){understood=cbind(interpretation$verb, interpretation$external); understood=understood[grep('D\\d',names(understood))]}
				}
				if(interpretation$verb$type=='twoPlace'){
					understood=interpretation$verb[grep('D\\d',names(interpretation$verb))]
					if('external'%in%names(interpretation) & 'internal'%in%names(interpretation)){understood=cbind(understood, interpretation$external, interpretation$internal); understood=understood[grep('D\\d',names(understood))]}
					if('external'%in%names(interpretation) & !'internal'%in%names(interpretation)){understood=cbind(understood, interpretation$external); understood=understood[grep('D\\d',names(understood))]}
					if('internal'%in%names(interpretation) & !'external'%in%names(interpretation)){
						external=interpretation$internal
						external[grep('D\\d',names(external))]=NA
						understood=cbind(understood, external, interpretation$internal); understood=understood[grep('D\\d',names(understood))]
			}	}	}
			success=ifelse(VMATCH(intended, understood) > (1-distinctiveness), 1, 0)
	}	}
success
}

FREQUPDATE=function(agentID, meaning, success){
	distinctions=world$distinctions; local=world$local; commonGround=world$useCommonGround; recencyDamper=world$recencyDamper; activationNoise=world$activationNoise
	agent=population[[agentID]]
	agent$nouns$recency=agent$nouns$recency + 1
	agent$verbs$recency=agent$verbs$recency + 1
	if(is.list(meaning)){
		if(is.data.frame(meaning$verb)){
			verbID=meaning$verb$ID
			if(success==1){
				agent$verbs[agent$verbs$ID==verbID,]$frequency=agent$verbs[agent$verbs$ID==verbID,]$frequency + 1
				agent$verbs[agent$verbs$ID==verbID,]$recency=0
				agent$usageHistory$verbs[nrow(agent$usageHistory$verbs) + 1,]$verb=verbID
				agent$usageHistory$verbs[nrow(agent$usageHistory$verbs),grep('D\\d',names(agent$usageHistory$verbs))]=meaning$target[, grep('V\\d',names(meaning$target))]
				if('extMarker'%in%names(meaning$verb)){
					agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$frequency=agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$frequency + 1
					agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$recency=0
					agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$verbMarker=agent$nouns[agent$nouns$ID==meaning$verb$extMarkerID,]$verbMarker + 1
					if(is.data.frame(meaning$external)){
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=meaning$verb$extMarkerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$external[, grep('D\\d',names(meaning$external))]
						pair=intersect(grep(paste('^',meaning$verb$extMarkerID, '$', sep=''), agent$collostructions$index$marker),grep(paste('^',meaning$external$ID, '$', sep=''), agent$collostructions$index$N))
						if(length(pair)!=0){agent$collostructions$index$frequency[pair]=agent$collostructions$index$frequency[pair] + 1}
						if(length(pair)==0){
							agent$collostructions$index[nrow(agent$collostructions$index) + 1,]$marker=meaning$verb$extMarkerID
							agent$collostructions$index[nrow(agent$collostructions$index),]$N=meaning$external$ID
							agent$collostructions$index[nrow(agent$collostructions$index),]$frequency=1
				}	}	}
				if('intMarker'%in%names(meaning$verb)){
					agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$frequency=agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$frequency + 1
					agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$recency=0
					agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$verbMarker=agent$nouns[agent$nouns$ID==meaning$verb$intMarkerID,]$verbMarker + 1
					if(is.data.frame(meaning$internal)){
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=meaning$verb$intMarkerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$internal[, grep('D\\d',names(meaning$internal))]
						pair=intersect(grep(paste('^',meaning$verb$intMarkerID, '$', sep=''), agent$collostructions$index$marker),grep(paste('^',meaning$internal$ID, '$', sep=''), agent$collostructions$index$N))
						if(length(pair)!=0){agent$collostructions$index$frequency[pair]=agent$collostructions$index$frequency[pair] + 1}
						if(length(pair)==0){
							agent$collostructions$index[nrow(agent$collostructions$index) + 1,]$marker=meaning$verb$intMarkerID
							agent$collostructions$index[nrow(agent$collostructions$index),]$N=meaning$internal$ID
							agent$collostructions$index[nrow(agent$collostructions$index),]$frequency=1
				}	}	}
				if(is.data.frame(meaning$external)){
					person=grep(meaning$external$person, agent$usageHistory$index$person)
					semRole='actor'
					if(meaning$verb$type=='twoPlace'){
						if(ACTOR(meaning$verb[,grep('Ext\\d',names(meaning$verb))], meaning$verb[,grep('Int\\d',names(meaning$verb))])==2){
							semRole='undergoer'
					}	}
					semRole2=grep(semRole,agent$usageHistory$index$role)
					if('extMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$yes=agent$usageHistory$index[intersect(person, semRole2),]$yes + 1
					}
					if(!'extMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$no=agent$usageHistory$index[intersect(person, semRole2),]$no + 1
					}
					values=unlist(rep(meaning$external[, grep('^D\\d', names(meaning$external))], distinctions)); values[is.na(values)]=-1
					if('markerID'%in%names(meaning$external)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$yes=agent$usageHistory$flag$person[intersect(person, semRole2),]$yes + 1
						if(meaning$external$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes+1}
					}	}	
					if(!'markerID'%in%names(meaning$external)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$no=agent$usageHistory$flag$person[intersect(person, semRole2),]$no + 1				
						if(meaning$external$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no+1}
				}	}	}	
				if(meaning$verb$type=='twoPlace' & is.data.frame(meaning$internal)){
					person=grep(meaning$internal$person, agent$usageHistory$index$person)
					semRole=ifelse(ACTOR(meaning$verb[,grep('Ext\\d',names(meaning$verb))], meaning$verb[,grep('Int\\d',names(meaning$verb))])==1, 'undergoer', 'actor')
 					semRole2=grep(semRole,agent$usageHistory$index$role)
					if('intMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$yes=agent$usageHistory$index[intersect(person, semRole2),]$yes + 1
					}
					if(!'intMarker'%in%names(meaning$verb)){
						agent$usageHistory$index[intersect(person, semRole2),]$no=agent$usageHistory$index[intersect(person, semRole2),]$no + 1
					}
					values=unlist(rep(meaning$internal[, grep('^D\\d', names(meaning$internal))], distinctions)); values[is.na(values)]=-1
					if('markerID'%in%names(meaning$internal)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$yes=agent$usageHistory$flag$person[intersect(person, semRole2),]$yes + 1
						if(meaning$internal$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$yes+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$yes+1}
					}	}	
					if(!'markerID'%in%names(meaning$internal)){
						agent$usageHistory$flag$person[intersect(person, semRole2),]$no=agent$usageHistory$flag$person[intersect(person, semRole2),]$no + 1				
						if(meaning$internal$person==3){
							if(semRole=='actor'){agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no=agent$usageHistory$flag$actor[agent$usageHistory$flag$actor$value==values,]$no+1}
							if(semRole=='undergoer'){agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no=agent$usageHistory$flag$undergoer[agent$usageHistory$flag$undergoer$value==values,]$no+1}
			}	}	}	}
			if(is.data.frame(meaning$external)){
				subjectID=meaning$external$ID
				macroRole=ifelse(ACTOR(meaning$verb[grep('Ext\\d',names(meaning$verb))], meaning$verb[grep('Int\\d',names(meaning$verb))])==1, 'actor', 'undergoer')
				if(meaning$external$topic==1){
					if(grep('external', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$freq=agent$topicPosition[agent$topicPosition$position=='first',]$freq+1} 
					if(grep('external', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$freq=agent$topicPosition[agent$topicPosition$position=='other',]$freq+1} 
					if(success==1){
						if(grep('external', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$success=agent$topicPosition[agent$topicPosition$position=='first',]$success+1} 
						if(grep('external', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$success=agent$topicPosition[agent$topicPosition$position=='other',]$success+1} 
						agent$topic[agent$topic$role==macroRole,]$topic=agent$topic[agent$topic$role==macroRole,]$topic+1
				}	}
				if(success==1){
					agent$nouns[agent$nouns$ID==subjectID,]$frequency=agent$nouns[agent$nouns$ID==subjectID,]$frequency + 1
					agent$nouns[agent$nouns$ID==subjectID,]$recency=0
					agent$nouns[agent$nouns$ID==subjectID,]$argument=agent$nouns[agent$nouns$ID==subjectID,]$argument + 1
					agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=subjectID
					if(meaning$external$person!=3){	#NB difference local and third-person pronouns
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('^Ext\\d',names(meaning$verb))]
					}
					if(meaning$external$person==3){	
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('A\\d',names(meaning$target))] 
						if(meaning$verb$type=='twoPlace'){
							if(macroRole=='undergoer'){
								agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('U\\d',names(meaning$target))] 
					}	}	}
					SV=intersect(grep(paste('^',subjectID, '$', sep=''), agent$collostructions$SV$S),grep(paste('^',verbID, '$', sep=''), agent$collostructions$SV$V))
					if(length(SV)!=0){agent$collostructions$SV$frequency[SV]=agent$collostructions$SV$frequency[SV] + 1}
					if(length(SV)==0){
						agent$collostructions$SV[nrow(agent$collostructions$SV) + 1,]$S=subjectID
						agent$collostructions$SV[nrow(agent$collostructions$SV),]$V=verbID
						agent$collostructions$SV[nrow(agent$collostructions$SV),]$frequency=1
					}
					if('marker'%in%names(meaning$external)){
						markerID=as.numeric(meaning$external$markerID)
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=markerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('Ext\\d',names(meaning$verb))]
						person=meaning$external$person
						agent$nouns[agent$nouns$ID==markerID,]$frequency=agent$nouns[agent$nouns$ID==markerID,]$frequency + 1
						agent$nouns[agent$nouns$ID==markerID,]$recency=0
						agent$nouns[agent$nouns$ID==markerID,]$nounMarker=agent$nouns[agent$nouns$ID==markerID,]$nounMarker + 1
						markerN=intersect(grep(paste('^',markerID, '$', sep=''), agent$collostructions$flag$marker),grep(paste('^',subjectID, '$', sep=''), agent$collostructions$flag$N))
						if(length(markerN)!=0){agent$collostructions$flag$frequency[markerN]=agent$collostructions$flag$frequency[markerN] + 1}
						if(length(markerN)==0){	
							agent$collostructions$flag[nrow(agent$collostructions$flag) + 1,]$marker=markerID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$N=subjectID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$frequency=1
					}	}	
					if(meaning$external$person==3 & commonGround==T){
						if(!meaning$external$ID%in%agent$commonGround){
							agent$commonGround=c(agent$commonGround, meaning$external$ID)
			}	}	}	}
			if(meaning$verb$type=='twoPlace' & is.data.frame(meaning$internal)){
				wordOrder=names(meaning); wordOrder=wordOrder[wordOrder!='target']
				actor=ifelse(ACTOR(meaning$verb[grep('Ext\\d',names(meaning$verb))], meaning$verb[grep('Int\\d',names(meaning$verb))])==1, 'external', 'internal')
				undergoer=ifelse(ACTOR(meaning$verb[grep('Ext\\d',names(meaning$verb))], meaning$verb[grep('Int\\d',names(meaning$verb))])==1, 'internal', 'external')
				wordOrder=gsub(actor,'A',wordOrder); wordOrder=gsub(undergoer,'U',wordOrder); wordOrder=gsub('verb','V',wordOrder)
				wordOrder=paste(wordOrder, collapse='')
				agent$wordOrder[agent$wordOrder$order==wordOrder,]$freq=agent$wordOrder[agent$wordOrder$order==wordOrder,]$freq + 1
				if(success==1){agent$wordOrder[agent$wordOrder$order==wordOrder,]$success=agent$wordOrder[agent$wordOrder$order==wordOrder,]$success + 1}
				objectID=meaning$internal$ID
				macroRole=ifelse(ACTOR(meaning$verb[grep('Ext\\d',names(meaning$verb))], meaning$verb[grep('Int\\d',names(meaning$verb))])==1, 'undergoer', 'actor')
				if(meaning$internal$topic==1){
					if(grep('internal', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$freq=agent$topicPosition[agent$topicPosition$position=='first',]$freq+1} 
					if(grep('internal', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$freq=agent$topicPosition[agent$topicPosition$position=='other',]$freq+1} 
					if(success==1){
						if(grep('internal', names(meaning))==1){agent$topicPosition[agent$topicPosition$position=='first',]$success=agent$topicPosition[agent$topicPosition$position=='first',]$success+1} 
						if(grep('internal', names(meaning))!=1){agent$topicPosition[agent$topicPosition$position=='other',]$success=agent$topicPosition[agent$topicPosition$position=='other',]$success+1} 
						agent$topic[agent$topic$role==macroRole,]$topic=agent$topic[agent$topic$role==macroRole,]$topic+1
				}	}
				if(success==1){
					agent$nouns[agent$nouns$ID==objectID,]$frequency=agent$nouns[agent$nouns$ID==objectID,]$frequency + 1
					agent$nouns[agent$nouns$ID==objectID,]$recency=0
					agent$nouns[agent$nouns$ID==objectID,]$argument=agent$nouns[agent$nouns$ID==objectID,]$argument + 1					
					agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=objectID
					if(meaning$internal$person!=3){	
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('^Int\\d',names(meaning$verb))]
					}
					if(meaning$internal$person==3){	
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('U\\d',names(meaning$target))] 
						if(meaning$verb$type=='twoPlace'){
							if(macroRole=='actor'){
								agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$target[, grep('A\\d',names(meaning$target))] 
					}	}	}
					OV=intersect(grep(paste('^',objectID, '$', sep=''), agent$collostructions$OV$O),grep(paste('^',verbID, '$', sep=''), agent$collostructions$OV$V))
					if(length(OV)!=0){agent$collostructions$OV$frequency[OV]=agent$collostructions$OV$frequency[OV] + 1}
					if(length(OV)==0){	
						agent$collostructions$OV[nrow(agent$collostructions$OV) + 1,]$O=objectID
						agent$collostructions$OV[nrow(agent$collostructions$OV),]$V=verbID
						agent$collostructions$OV[nrow(agent$collostructions$OV),]$frequency=1
					}
					if('marker'%in%names(meaning$internal)){
						markerID=as.numeric(meaning$internal$markerID)
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns) + 1,]$noun=markerID
						agent$usageHistory$nouns[nrow(agent$usageHistory$nouns),grep('D\\d',names(agent$usageHistory$nouns))]=meaning$verb[, grep('Int\\d',names(meaning$verb))]
						person=meaning$internal$person
						agent$nouns[agent$nouns$ID==markerID,]$frequency=agent$nouns[agent$nouns$ID==markerID,]$frequency + 1
						agent$nouns[agent$nouns$ID==markerID,]$recency=0
						agent$nouns[agent$nouns$ID==markerID,]$nounMarker=agent$nouns[agent$nouns$ID==markerID,]$nounMarker + 1
						markerN=intersect(grep(paste('^',markerID, '$', sep=''), agent$collostructions$flag$marker),grep(paste('^',objectID, '$', sep=''), agent$collostructions$flag$N))
						if(length(markerN)!=0){agent$collostructions$flag$frequency[markerN]=agent$collostructions$flag$frequency[markerN] + 1}
						if(length(markerN)==0){	
							agent$collostructions$flag[nrow(agent$collostructions$flag) + 1,]$marker=markerID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$N=objectID
							agent$collostructions$flag[nrow(agent$collostructions$flag),]$frequency=1
					}	}
					if(meaning$internal$person==3 & commonGround==T){
						if(!meaning$internal$ID%in%agent$commonGround){
							agent$commonGround=c(agent$commonGround, meaning$internal$ID)
	}	}	}	}	}	}
	agent$nouns$activation=RESCALE(jitter(log((agent$nouns$frequency+1))/(agent$nouns$recency+1+recencyDamper), factor=activationNoise))	#+1 for log and to prevent division by zero; 
	agent$verbs$activation=RESCALE(jitter(log((agent$verbs$frequency+1))/(agent$verbs$recency+1+recencyDamper), factor=activationNoise))
	if(success==1){
		agent$usageHistory$verbs=unique(agent$usageHistory$verbs)
		agent$usageHistory$nouns=unique(agent$usageHistory$nouns)
	}
agent
}

EROSION=function(hearerID, interpretation){
	hearer=population[[hearerID]]; reductionFrequencyThreshold=world$reductionFrequencyThreshold*hearer$age; erosionMax=world$erosionMax; formSetFrequency=world$formSetFrequency
	if(is.data.frame(interpretation$verb)){
		if(hearer$verbs[hearer$verbs$ID==interpretation$verb$ID,]$form!=interpretation$verb$form){
			if(nchar(interpretation$verb$form) >= erosionMax){
				ID=interpretation$verb$ID
				if(hearer$verbs[hearer$verbs$ID==ID,]$frequency < formSetFrequency){
					if(nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)==nchar(interpretation$verb$form)){hearer$verbs[hearer$verbs$ID==ID,]$form=gsub('.$', substr(interpretation$verb$form, nchar(interpretation$verb$form), nchar(interpretation$verb$form)), hearer$verbs[hearer$verbs$ID==ID,]$form)}
					if(nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)>nchar(interpretation$verb$form)){hearer$verbs[hearer$verbs$ID==ID,]$form=gsub('.$','',hearer$verbs[hearer$verbs$ID==ID,]$form)}
					if(nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)<nchar(interpretation$verb$form)){hearer$verbs[hearer$verbs$ID==ID,]$form=paste(hearer$verbs[hearer$verbs$ID==ID,]$form, substr(interpretation$verb$form, nchar(hearer$verbs[hearer$verbs$ID==ID,]$form) + 1, nchar(hearer$verbs[hearer$verbs$ID==ID,]$form) + 1), sep='')}
				}
				hearer$verbs[hearer$verbs$ID==ID,]$productionEffort=nchar(hearer$verbs[hearer$verbs$ID==ID,]$form)
				if(hearer$verbs[hearer$verbs$ID==ID,]$productionEffort==0){
					hearer$verbs=hearer$verbs[hearer$verbs$ID!=ID,]
					graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'verb removed', 'EROSION', ID, '', '', '')
				}
		}	}
		if('extMarkerID'%in%names(interpretation$verb)){
			markerID=interpretation$verb$extMarkerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$verb$extMarker){
				if(nchar(interpretation$verb$extMarker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$verb$extMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$verb$extMarker, nchar(interpretation$verb$extMarker), nchar(interpretation$verb$extMarker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$verb$extMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$verb$extMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$verb$extMarker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'verb marker removed', 'EROSION', markerID, '', '', '')
		}	}	}	}	}
		if('intMarkerID'%in%names(interpretation$verb)){
			markerID=interpretation$verb$intMarkerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$verb$intMarker){
				if(nchar(interpretation$verb$intMarker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$verb$intMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$verb$intMarker, nchar(interpretation$verb$intMarker), nchar(interpretation$verb$intMarker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$verb$intMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$verb$intMarker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$verb$intMarker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'verb marker removed', 'EROSION', markerID, '', '', '')
	}	}	}	}	}	}
	if(is.data.frame(interpretation$external)){	
		if(hearer$nouns[hearer$nouns$ID==interpretation$external$ID, ]$form!=interpretation$external$form){
			if(nchar(interpretation$external$form) >= erosionMax){
				ID=interpretation$external$ID
				if(hearer$nouns[hearer$nouns$ID==ID,]$frequency < formSetFrequency){
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)==nchar(interpretation$external$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$', substr(interpretation$external$form, nchar(interpretation$external$form), nchar(interpretation$external$form)), hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)>nchar(interpretation$external$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)<nchar(interpretation$external$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=paste(hearer$nouns[hearer$nouns$ID==ID,]$form, substr(interpretation$external$form, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1), sep='')}
					hearer$nouns[hearer$nouns$ID==ID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)
					if(hearer$nouns[hearer$nouns$ID==ID,]$productionEffort==0){
						hearer$nouns=hearer$nouns[hearer$nouns$ID!=ID,]
						graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, '(pro)noun removed', 'EROSION', ID, '', '', '')
		}	}	}	}
		if('marker'%in%names(interpretation$external)){
			markerID=interpretation$external$markerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$external$marker){
				if(nchar(interpretation$external$marker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$external$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$external$marker, nchar(interpretation$external$marker), nchar(interpretation$external$marker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$external$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$external$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$external$marker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'noun marker removed', 'EROSION', markerID, '', '', '')
	}	}	}	}	}	}
	if(is.data.frame(interpretation$internal)){	
		if(hearer$nouns[hearer$nouns$ID==interpretation$internal$ID,]$form!=interpretation$internal$form){
			if(nchar(interpretation$internal$form) >= erosionMax){
				ID=interpretation$internal$ID
				if(hearer$nouns[hearer$nouns$ID==ID,]$frequency < formSetFrequency){
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)==nchar(interpretation$internal$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$', substr(interpretation$internal$form, nchar(interpretation$internal$form), nchar(interpretation$internal$form)), hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)>nchar(interpretation$internal$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==ID,]$form)}
					if(nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)<nchar(interpretation$internal$form)){hearer$nouns[hearer$nouns$ID==ID,]$form=paste(hearer$nouns[hearer$nouns$ID==ID,]$form, substr(interpretation$internal$form, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==ID,]$form) + 1), sep='')}
					hearer$nouns[hearer$nouns$ID==ID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==ID,]$form)
					if(hearer$nouns[hearer$nouns$ID==ID,]$productionEffort==0){
						hearer$nouns=hearer$nouns[hearer$nouns$ID!=ID,]
						graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, '(pro)noun removed', 'EROSION', ID, '', '', '')
		}	}	}	}
		if('marker'%in%names(interpretation$internal)){
			markerID=interpretation$internal$markerID
			if(hearer$nouns[hearer$nouns$ID==markerID,]$form!=interpretation$internal$marker){
				if(nchar(interpretation$internal$marker) >= erosionMax){
					if(hearer$nouns[hearer$nouns$ID==markerID,]$frequency < formSetFrequency){
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)==nchar(interpretation$internal$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$', substr(interpretation$internal$marker, nchar(interpretation$internal$marker), nchar(interpretation$internal$marker)), hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)>nchar(interpretation$internal$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=gsub('.$','',hearer$nouns[hearer$nouns$ID==markerID,]$form)}
						if(nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)<nchar(interpretation$internal$marker)){hearer$nouns[hearer$nouns$ID==markerID,]$form=paste(hearer$nouns[hearer$nouns$ID==markerID,]$form, substr(interpretation$internal$marker, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1, nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form) + 1), sep='')}
						hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort=nchar(hearer$nouns[hearer$nouns$ID==markerID,]$form)
						if(hearer$nouns[hearer$nouns$ID==markerID,]$productionEffort==0){
							hearer$nouns=hearer$nouns[hearer$nouns$ID!=markerID,]
							graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'noun marker removed', 'EROSION', markerID, '', '', '')
	}	}	}	}	}	}
hearer
}

NOUNDESEMANTICIZATION=function(agent){	#Cf. Heine en Kuteva p.39: freq is epiphenomenon of extension, not cause; extension by combinatorial flexibility
	distinctions=world$distinctions; minimalSpecification=world$minimalSpecification; desemanticizationCeiling=world$desemanticizationCeiling; power=world$desemanticizationPower
	dims=length(distinctions)	
	steps=dims-1-minimalSpecification	
	factor=(desemanticizationCeiling*agent$age-8)/steps^power	#8 is minimum freq from which Yang applies
	steps=round(factor*(0:steps)^power + 8)
	nouns=table(agent$usageHistory$nouns$noun)
	nouns=names(nouns[nouns>steps[1]])	#i.e. >8
	for(noun in nouns){
		if(!noun%in%agent$nouns$ID){break}
		nounTargets=agent$usageHistory$nouns[agent$usageHistory$nouns$noun==noun,grep('^D\\d',names(agent$usageHistory$nouns))]
		#change meaning values
		for(i in 1:ncol(nounTargets)){
			values=table(nounTargets[,i])
			change=MAX(values, forceChoice=T)
			if(sum(values[-change])<(sum(values)/log(sum(values)))){
				agent$nouns[agent$nouns$ID==noun, i]=as.numeric(names(values)[change])
				graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, '(pro)noun meaning changed', 'NOUNDESEMANTICIZATION', noun, '', '', '')
		}	}
		#remove meaning values
		if(agent$nouns[agent$nouns$ID==noun,]$semanticWeight>(minimalSpecification/dims)){
			if(nrow(nounTargets)>steps[sum(is.na(agent$nouns[agent$nouns$ID==noun,grep('^D\\d',names(agent$nouns))])) + 1]){		
				nounProfile=agent$nouns[agent$nouns$ID==noun,grep('^D\\d',names(agent$nouns))]
				vars=rep(0, ncol(nounProfile))
				for(i in 1:length(vars)){vars[i]=sum(nounProfile[,i]!=nounTargets[,i], na.rm=T)}
				agent$nouns[agent$nouns$ID==noun, MAX(vars, forceChoice=T)]=NA
				agent$nouns[agent$nouns$ID==noun,]$semanticWeight=(length(grep('^D\\d',names(agent$nouns)))-sum(is.na(agent$nouns[agent$nouns$ID==noun,grep('^D\\d',names(agent$nouns))])))/length(grep('^D\\d',names(agent$nouns)))
				graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, '(pro)noun meaning dimension removed', 'NOUNDESEMANTICIZATION', noun, '', '', '')
	}	}	}
agent
}

VERBDESEMANTICIZATION=function(agent){	#Cf. Heine en Kuteva p.39: freq is epiphenomenon of extension, not cause; extension by combinatorial flexibility
	distinctions=world$distinctions; minimalSpecification=world$minimalSpecification; desemanticizationCeiling=world$desemanticizationCeiling; power=world$desemanticizationPower
	dims=length(distinctions)	
	steps=dims-1-minimalSpecification	
	factor=(desemanticizationCeiling*agent$age-8)/steps^power
	steps=round(factor*(0:steps)^power + 8)
	verbs=xtabs(~agent$usageHistory$verbs$verb)
	verbs=names(verbs[verbs>steps[1]])
	for(verb in verbs){
		verbTargets=agent$usageHistory$verbs[agent$usageHistory$verbs$verb==verb,grep('^D\\d',names(agent$usageHistory$verbs))]
		#change meaning values
		for(i in 1:ncol(verbTargets)){
			values=table(verbTargets[,i])
			change=MAX(values, forceChoice=T)
			if(sum(values[-change])<(sum(values)/log(sum(values)))){
				agent$verbs[agent$verbs$ID==verb, i]=as.numeric(names(values)[change])
				graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'verb meaning changed', 'VERBDESEMANTICIZATION', verb, '', '', '')
		}	}
		#remove meaning values
		if(agent$verbs[agent$verbs$ID==verb,]$semanticWeight>(minimalSpecification/dims)){
			if(nrow(verbTargets)>steps[sum(is.na(agent$verbs[agent$verbs$ID==verb,grep('^D\\d',names(agent$verbs))])) + 1]){		
				#action
				actionProfile=agent$verbs[agent$verbs$ID==verb,grep('^D\\d',names(agent$verbs))]
				vars=rep(0, ncol(verbTargets))
				for(i in 1:length(vars)){vars[i]=sum(actionProfile[,i]!=verbTargets[,i], na.rm=T)}
				agent$verbs[agent$verbs$ID==verb,  MAX(vars, forceChoice=T)]=NA
				agent$verbs[agent$verbs$ID==verb, ]$semanticWeight=(length(grep('^D\\d',names(agent$verbs)))-sum(is.na(agent$verbs[agent$verbs$ID==verb, grep('^D\\d',names(agent$verbs))])))/length(grep('^D\\d',names(agent$verbs)))
				#external
				extProfile=agent$verbs[agent$verbs$ID==verb, grep('^Ext\\d',names(agent$verbs))]
				vars=rep(0, ncol(extProfile))
				performerProfiles=agent$nouns[match(agent$collostructions$SV[grep(paste('^',verb, '$',sep=''),agent$collostructions$SV$V),]$S, agent$nouns$ID),grep('^D\\d',names(agent$nouns))]
				for(i in 1:length(vars)){vars[i]=sum(extProfile[,i]!=performerProfiles[,i], na.rm=T)}
				agent$verbs[agent$verbs$ID==verb,  grep('^Ext\\d',names(agent$verbs))[MAX(vars, forceChoice=T)]]=NA
				#internal
				intProfile=agent$verbs[agent$verbs$ID==verb, grep('^Int\\d',names(agent$verbs))]
				vars=rep(0, ncol(intProfile))
				performerProfiles=agent$nouns[match(agent$collostructions$OV[grep(paste('^',verb, '$',sep=''),agent$collostructions$OV$V),]$O, agent$nouns$ID),grep('^D\\d',names(agent$nouns))]
				for(i in 1:length(vars)){vars[i]=sum(intProfile[,i]!=performerProfiles[,i], na.rm=T)}
				agent$verbs[agent$verbs$ID==verb, grep('^Int\\d',names(agent$verbs))[MAX(vars, forceChoice=T)]]=NA
				graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'verb meaning dimension removed', 'VERBDESEMANTICIZATION', verb, '', '', '')
	}	}	}	
agent
}

PERSONUPDATE=function(agent){
	personThreshold=world$semUpdateThreshold*agent$age; referenceThreshold=world$referenceThreshold; verbalRoleMarker=world$verbalRoleMarker
	nouns=agent$nouns
	index=agent$collostructions$index
	if(nrow(index)!=0){
		index=index[order(index$N),]
		index$person=nouns[match(index$marker, nouns$ID),]$person
		index=index[!is.na(index$person),]
		index2=unique(index[,c('N','person')]); index2$frequency=0	#in case of multiple verb markers with same person...
		for(person in unique(index2$person)){index2[index2$person==person,]$frequency=tapply(index[index$person==person, ]$frequency, index[index$person==person, ]$N, sum)}
		index2=index2[index2$frequency>personThreshold,]
		if(nrow(index2)!=0){
			for(i in 1:nrow(index2)){
				if(index2$person[i]!=nouns[nouns$ID==index2$N[i],]$person){
					new=nouns[nouns$ID==index2$N[i],]
					new$ID=max(nouns$ID) + 1
					new$person=index2$person[i]
					new$form=gsub('.$','',new$form)	#otherwise, source form would block future erosion
					nouns=rbind(nouns, new)
					rownames(nouns)=1:nrow(nouns)
					graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'new pro', 'PERSONUPDATE', new$ID, new$person, index2$N[i], '')
		}	}	}
		#remove redundant local pronouns
		localPros=nouns[nouns$person!=3 & nouns$argument>nouns$verbMarker & nouns$productionEffort>referenceThreshold,]
		if(nrow(localPros)>1){
			localPros=localPros[sample(nrow(localPros)),]
			for(i in 1:(nrow(localPros)-1)){
				if(1%in%VMATCH(localPros[i,c(grep('D\\d', names(localPros)), grep('person', names(localPros)))], localPros[(i + 1):nrow(localPros),c(grep('D\\d', names(localPros)), grep('person', names(localPros)))])){	
					nouns=nouns[nouns$ID!=localPros$ID[i],]
					graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'pro removed', 'PERSONUPDATE', localPros$ID[i], localPros$person[i], '', '')
		}	}	}
		#remove redundant indexes
		if(nrow(nouns[nouns$productionEffort<=referenceThreshold,])>1){
			indexes=nouns[nouns$productionEffort<=referenceThreshold,]
			indexes=indexes[indexes$verbMarker > indexes$nounMarker & indexes$verbMarker > indexes$argument,]	#clear indexes
			if(nrow(indexes)>1){
				indexes=indexes[order(indexes$verbMarker),]	#start removal with least popular index
				if(verbalRoleMarker==F){
					for(i in 1:(nrow(indexes)-1)){
						if(sum(grepl(indexes[i,]$person, indexes[(i+1):nrow(indexes),]$person))>0){
							nouns=nouns[nouns$ID!=indexes$ID[i],]
							graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'index removed', 'PERSONUPDATE', indexes$ID[i], indexes$person[i], '', '')
				}	}	}
				if(verbalRoleMarker==T){
					for(i in 1:(nrow(indexes)-1)){
						if(1%in%VMATCH(indexes[i,c(grep('D\\d', names(indexes)), grep('person', names(indexes)))], indexes[(i + 1):nrow(indexes),c(grep('D\\d', names(indexes)), grep('person', names(indexes)))])){	
							nouns=nouns[nouns$ID!=indexes$ID[i],]
							graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'index removed', 'PERSONUPDATE', indexes$ID[i], indexes$person[i], '', '')
		}	}	}	}	}
		agent$nouns=nouns
	}
agent
}

FUSE=function(agent){
	threshold=world$semUpdateThreshold; wordLength=world$wordLength
	threshold=threshold*agent$age
	nouns=agent$nouns
	refs=nouns[nouns$frequency==0 | (nouns$verbMarker+nouns$nounMarker) < (nouns$frequency/log(nouns$frequency)),]	#referential subset
	flag=agent$collostructions$flag
	flag=flag[flag$frequency>threshold,]
	if(nrow(flag)!=0){
		for(i in 1:nrow(flag)){
			if(nouns[nouns$ID==flag$marker[i],]$nounMarker > max(nouns[nouns$ID==flag$marker[i],]$argument, nouns[nouns$ID==flag$marker[i],]$verbMarker)){	#if clear marker
				new=nouns[nouns$ID==flag$N[i],]		
				new$ID=max(agent$nouns$ID) + 1
				vowels=unlist(strsplit(c(new$form, nouns[nouns$ID==flag$marker[i],]$form), '')); vowels=unique(vowels[vowels%in%world$vowels])
				consonants=unlist(strsplit(c(new$form, nouns[nouns$ID==flag$marker[i],]$form), '')); consonants=unique(consonants[consonants%in%world$consonants])
				length=nchar(new$form) + nchar(nouns[nouns$ID==flag$marker[i],]$form)
				if(length>max(wordLength)){length=max(wordLength)}
				new$form=FORMS(1, length, vowels=vowels, consonants=consonants)
				if(new$form%in%nouns$form){	#if form already in use, try again (being more creative)
					new$form=FORMS(1, length, vowels=vowels, consonants=consonants)
					solution=sample(c('vowels','consonants','length'), 1)
					if(solution=='vowels'){vowels=c(vowels, sample(world$vowels, 1))}
					if(solution=='consonants'){consonants=c(consonants, sample(world$consonants, 1))}
					if(solution=='length' & length<max(wordLength)){length=length+1}
				}
				change=grep('NA', nouns[nouns$ID==flag$marker[i],grep('D\\d',names(nouns))], invert=T)
				new[,change]=nouns[nouns$ID==flag$marker[i],change]	#mix semantics of host with that of marker
				new$frequency=flag$frequency[i]
				new$argument=flag$frequency[i]
				new$verbMarker=0	
				new$nounMarker=0	
				new$productionEffort=nchar(new$form)
				new$semanticWeight=(length(grep('D\\d', names(agent$nouns)))-length(grep('NA', new)))/length(grep('D\\d', names(agent$nouns)))
				new$recency=0
				if(!1%in%VMATCH(new[,c(grep('D\\d', names(new)), grep('person', names(new)))], refs[,c(grep('D\\d', names(refs)), grep('person', names(refs)))])){	#if combination contributes....
					agent$nouns=rbind(agent$nouns,new)
					rownames(agent$nouns)=1:nrow(agent$nouns)
					graveyard$history[nrow(graveyard$history) + 1,]<<-c(agent$generation, 'new word', 'FUSE', new$ID, new$person, flag$N[i], flag$marker[i])
	}	}	}	}
agent
}

SEMUPDATE=function(agentID){
	semUpdateAge=world$semUpdateAge; deathAge=world$deathAge; nNouns=world$nNouns; nVerbs=world$nVerbs; local=world$local
	agent=population[[agentID]]
	if(agent$semupdate==0){
		if(agent$age>semUpdateAge*deathAge){
			agent=VERBDESEMANTICIZATION(agent)
			agent=NOUNDESEMANTICIZATION(agent)
			agent=FUSE(agent)
			agent=PERSONUPDATE(agent)
			agent$semupdate=1
	}	}
	agent$nouns=agent$nouns[agent$nouns$semanticWeight>0,]
	if(nrow(agent$nouns)<nNouns){
		newNouns=NOUNS(nNouns-nrow(agent$nouns), local=F)
		newNouns$ID=max(agent$nouns$ID)+(1:nrow(newNouns))
		agent$nouns=rbind(agent$nouns, newNouns)
	}
	if(local==T){
		if(!1%in%agent$nouns$person){
			agent$nouns[MAX(VMATCH(rep(1, length(grep('D\\d', names(agent$nouns)))), agent$nouns[, grep('D\\d', names(agent$nouns))]), 1, forceChoice=T),]$person=1
		} 
		if(!2%in%agent$nouns$person){
			agent$nouns[MAX(VMATCH(rep(1, length(grep('D\\d', names(agent$nouns)))), agent$nouns[, grep('D\\d', names(agent$nouns))]), 2, forceChoice=T),]$person=2
	}	}
	agent$verbs=agent$verbs[agent$verbs$semanticWeight>0,]
	if(nrow(agent$verbs)<nVerbs){
		newNouns=VERBS(nNouns-nrow(agent$verbs))
		newNouns$ID=max(agent$verbs$ID)+(1:nrow(newNouns))
		agent$verbs=rbind(agent$verbs, newNouns)
	}
agent
}

PROCREATE=function(speakerID, hearerID){
	procreationAge=world$procreationAge; deathAge=world$deathAge; desemanticization=world$desemanticization; crossover=world$crossover; replace=world$replace; distinctions=world$distinctions; wordLength=world$wordLength; vowels=world$vowels; consonants=world$consonants
	if(population[[speakerID]]$fertile=='yes' & population[[hearerID]]$fertile=='yes'){
		if((population[[speakerID]]$age + population[[hearerID]]$age)/2 > (procreationAge*deathAge)){
			#procreate
			offspring1=population[[speakerID]]
			if(crossover==F){offspring2=population[[hearerID]]}
			if(crossover==T){
				mix=sample(intersect(offspring1$nouns$ID, population[[hearerID]]$nouns$ID), .5*nrow(offspring1$nouns))
				offspring1$nouns[match(mix, offspring1$nouns$ID),]=population[[hearerID]]$nouns[match(mix,population[[hearerID]]$nouns$ID),]
				mix=sample(intersect(offspring1$verbs$ID, population[[hearerID]]$verbs$ID), .5*nrow(offspring1$verbs))
				offspring1$verbs[match(mix,offspring1$verbs$ID), ]=population[[hearerID]]$verbs[match(mix,population[[hearerID]]$verbs$ID), ]
				offspring2=offspring1
			}
			offspring1$age=0; offspring2$age=0
			offspring1$semupdate=0; offspring2$semupdate=0
			offspring1$generation=offspring1$generation + 1; offspring2$generation=offspring2$generation + 1
			if(replace==T){
				zeros=offspring1$nouns[offspring1$nouns$frequency==0,]$ID
				if(length(zeros)!=0){
					cols=sample(1:length(distinctions), length(zeros), replace=T)
					for (i in 1:length(zeros)){
					 	if(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$semanticWeight < 1){
							offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('NA',offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('^D\\d',names(offspring1$nouns))])[1]]=sample(seq(0,1,length.out=distinctions[grep('NA',offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('^D\\d',names(offspring1$nouns))])[1]]),1)	#restores first meaning dimension with NA
							offspring1$nouns[offspring1$nouns$ID==zeros[i],]$semanticWeight=(length(grep('^D\\d',names(offspring1$nouns)))-sum(is.na(offspring1$nouns[offspring1$nouns$ID==zeros[i],grep('^D\\d',names(offspring1$nouns))])))/length(grep('^D\\d',names(offspring1$nouns)))	
                    				} else {offspring1$nouns[offspring1$nouns$ID==zeros[i],cols[i]]=sample(seq(0,1,length.out=distinctions[cols[i]]),1)}
                    				if(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$productionEffort < min(wordLength)){
                    					plus=gsub('.*(.)$', '\\1', offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form)
                    					plus=ifelse(plus%in%vowels, sample(consonants, 1), sample(vowels, 1))
                    					offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form=paste(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form, plus, sep='')
                    					offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$productionEffort=nchar(offspring1$nouns[offspring1$nouns$ID==zeros[i], ]$form)
                    			}	}
					offspring2$nouns[match(zeros, offspring1$nouns$ID),]=offspring1$nouns[match(zeros, offspring1$nouns$ID),]
			}	}
			offspring1$nouns$frequency=0; offspring2$nouns$frequency=0
			offspring1$nouns$recency=0; offspring2$nouns$recency=0
			offspring1$nouns$activation=0; offspring2$nouns$activation=0
			offspring1$nouns$nounMarker=0; offspring2$nouns$nounMarker=0
			offspring1$nouns$verbMarker=0; offspring2$nouns$verbMarker=0
			offspring1$nouns$argument=0; offspring2$nouns$argument=0
			offspring1$verbs$frequency=0; offspring2$verbs$frequency=0
			offspring1$verbs$recency=0; offspring2$verbs$recency=0
			offspring1$verbs$activation=0; offspring2$verbs$activation=0
			offspring1$usageHistory$nouns=offspring1$usageHistory$nouns[-(1:nrow(offspring1$usageHistory$nouns)),]; offspring2$usageHistory$nouns=offspring2$usageHistory$nouns[-(1:nrow(offspring2$usageHistory$nouns)),]
			offspring1$usageHistory$verbs=offspring1$usageHistory$verbs[-(1:nrow(offspring1$usageHistory$verbs)),]; offspring2$usageHistory$verbs=offspring2$usageHistory$verbs[-(1:nrow(offspring2$usageHistory$verbs)),]
			values=vector(); for(i in 1:length(distinctions)){values=c(values, seq(0,1,length.out=distinctions[i]))}
			offspring1$usageHistory$flag=list(
				person=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=F), 
				actor=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=F),
				undergoer=data.frame(dimension=rep(1:length(distinctions), distinctions), value=values, yes=0, no=4, stringsAsFactors=F)
			)	
			offspring2$flag=offspring1$flag
			offspring1$usageHistory$index=data.frame(role=rep(c('actor', 'undergoer'), each=3), person=rep(1:3, 2), yes=0, no=4, stringsAsFactors=F); offspring2$index=offspring1$index	
			offspring1$collostructions$SV=offspring1$collostructions$SV[-(1:nrow(offspring1$collostructions$SV)),]; offspring2$collostructions$SV=offspring2$collostructions$SV[-(1:nrow(offspring2$collostructions$SV)),]
			offspring1$collostructions$OV=offspring1$collostructions$OV[-(1:nrow(offspring1$collostructions$OV)),]; offspring2$collostructions$OV=offspring2$collostructions$OV[-(1:nrow(offspring2$collostructions$OV)),]
			offspring1$collostructions$flag=offspring1$collostructions$flag[-(1:nrow(offspring1$collostructions$flag)),]; offspring2$collostructions$flag=offspring2$collostructions$flag[-(1:nrow(offspring2$collostructions$flag)),]
			offspring1$collostructions$index=offspring1$collostructions$index[-(1:nrow(offspring1$collostructions$index)),]; offspring2$collostructions$index=offspring2$collostructions$index[-(1:nrow(offspring2$collostructions$index)),]
			offspring1$topic=data.frame(role=c('actor','undergoer'),topic=0, stringsAsFactors=F); offspring2$topic=offspring1$topic
			offspring1$wordOrder=data.frame(order=c('AVU','AUV', 'VAU', 'VUA', 'UAV', 'UVA'),freq=0, success=0, stringsAsFactors=F); offspring2$wordOrder=offspring1$wordOrder
			offspring1$topicPosition=data.frame(position=c('first', 'other'), freq=0, success=0, stringsAsFactors=F); offspring2$topicPosition=offspring1$topicPosition
			population[[length(population) + 1]] <<- offspring1; names(population)[length(population)] <<- toupper(FORMS(1, length=6))	
			population[[length(population) + 1]] <<- offspring2; names(population)[length(population)] <<- toupper(FORMS(1, length=6))	
			population[[speakerID]]$fertile <<- 'no'; population[[hearerID]]$fertile <<- 'no'
			cat(paste('\n', paste(names(population)[c(length(population)-1, length(population))], collapse=' and '),'were born', '\n\n'))
} 	}	}

DIE=function(agentID){
	deathAge=world$deathAge; saveAll=world$saveAll
	agent=population[[agentID]]
	if(agent$age > deathAge){
		graveyard$summary[nrow(graveyard$summary) + 1,]$name <<- names(population)[[agentID]]
		graveyard$summary[nrow(graveyard$summary),]$generation <<- agent$generation
		graveyard$summary[nrow(graveyard$summary),]$successRate <<- sum(agent$verbs$frequency)/agent$age
		graveyard$summary[nrow(graveyard$summary),]$meanNounMarkerFrequency <<- sum(agent$collostructions$flag$frequency)/sum(agent$verbs$frequency)
		graveyard$summary[nrow(graveyard$summary),]$topnounMarkerFrequency <<- ifelse(nrow(agent$collostructions$flag)!=0, MAX(tapply(agent$collostructions$flag$frequency, agent$collostructions$flag$marker, sum),1, forceChoice=T, value=T)/sum(agent$verbs$frequency), 0)
		graveyard$summary[nrow(graveyard$summary),]$meanverbMarkerFrequency <<- sum(agent$collostructions$index$frequency)/sum(agent$verbs$frequency)
		graveyard$summary[nrow(graveyard$summary),]$topverbMarkerFrequency <<- ifelse(nrow(agent$collostructions$index)!=0, MAX(tapply(agent$collostructions$index$frequency, agent$collostructions$index$marker, sum),1, forceChoice=T, value=T)/sum(agent$verbs$frequency), 0)
		graveyard$brains[length(graveyard$brains) + 1] <<- population[agentID]
		if(saveAll==F){
			#rm usage history for nouns and verbs
			graveyard$brains[[length(graveyard$brains)]]$usageHistory <<- graveyard$brains[[length(graveyard$brains)]]$usageHistory[-c(1,2)]	
			#only save frequent collostructions 
			graveyard$brains[[length(graveyard$brains)]]$collostructions$SV <<- graveyard$brains[[length(graveyard$brains)]]$collostructions$SV[graveyard$brains[[length(graveyard$brains)]]$collostructions$SV$frequency>10,] 
			graveyard$brains[[length(graveyard$brains)]]$collostructions$OV <<- graveyard$brains[[length(graveyard$brains)]]$collostructions$OV[graveyard$brains[[length(graveyard$brains)]]$collostructions$OV$frequency>10,] 
			graveyard$brains[[length(graveyard$brains)]]$collostructions$index <<- graveyard$brains[[length(graveyard$brains)]]$collostructions$index[graveyard$brains[[length(graveyard$brains)]]$collostructions$index$frequency>10,] 
			graveyard$brains[[length(graveyard$brains)]]$collostructions$flag <<- graveyard$brains[[length(graveyard$brains)]]$collostructions$flag[graveyard$brains[[length(graveyard$brains)]]$collostructions$flag$frequency>10,] 
			#only save binary meaning dimensions
			save=table(graveyard$brains[[length(graveyard$brains)]]$flag$external$dimension); save=names(save[save==2])
			graveyard$brains[[length(graveyard$brains)]]$flag$external <<- graveyard$brains[[length(graveyard$brains)]]$flag$external[graveyard$brains[[length(graveyard$brains)]]$flag$external$dimension%in%save,]
			save=table(graveyard$brains[[length(graveyard$brains)]]$flag$internal$dimension); save=names(save[save==2])
			graveyard$brains[[length(graveyard$brains)]]$flag$internal <<- graveyard$brains[[length(graveyard$brains)]]$flag$internal[graveyard$brains[[length(graveyard$brains)]]$flag$internal$dimension%in%save,]
			save=table(graveyard$brains[[length(graveyard$brains)]]$flag$actor$dimension); save=names(save[save==2])
			graveyard$brains[[length(graveyard$brains)]]$flag$actor <<- graveyard$brains[[length(graveyard$brains)]]$flag$actor[graveyard$brains[[length(graveyard$brains)]]$flag$actor$dimension%in%save,]
			save=table(graveyard$brains[[length(graveyard$brains)]]$flag$undergoer$dimension); save=names(save[save==2])
			graveyard$brains[[length(graveyard$brains)]]$flag$undergoer <<- graveyard$brains[[length(graveyard$brains)]]$flag$undergoer[graveyard$brains[[length(graveyard$brains)]]$flag$undergoer$dimension%in%save,]
		}
		population <<- population[-agentID]
		cat(paste('\n', graveyard$summary[nrow(graveyard$summary),]$name,'died', '\n\n'))
}	}

TURN=function(speakerID, hearerID){
	erosion=world$erosion
	situation<<- SITUATION(speakerID)
	proposition<<- PROPOSITION(speakerID, situation)
	prep<<- PREPARE(speakerID, proposition, situation)
	utterance<<- PRODUCE(speakerID, prep)
	interpretation<<- INTERPRET(hearerID, utterance, situation)
	population[[speakerID]]$age <<- population[[speakerID]]$age + 1
	population[[hearerID]]$age <<- population[[hearerID]]$age + 1
	success=SUCCESS(proposition, interpretation, situation)
	population[[speakerID]] <<- FREQUPDATE(speakerID, prep, success)
	population[[hearerID]] <<- FREQUPDATE(hearerID, interpretation, success)
	if(success==1){
		if(erosion==T){population[[hearerID]] <<- EROSION(hearerID, interpretation)}
	}
utterance
}

TALK=function(nTurns, output=T){
	talkAge=world$talkAge; desemanticization=world$desemanticization; semUpdateAge=world$semUpdateAge; procreationAge=world$procreationAge; deathAge=world$deathAge; turnChange=world$turnChange
	speakerID=sample(length(population), 1)
	if(length(population)==2){hearerID=setdiff(1:length(population),speakerID)}
	if(length(population) > 2){hearerID=sample(setdiff(1:length(population),speakerID),1)}
	#make it less likely (not impossible) for young agents to talk with each other
	if(population[[speakerID]]$age < (talkAge*deathAge) & population[[hearerID]]$age < (talkAge*deathAge) & length(population)>2){
		speakerID=sample(setdiff(1:length(population),speakerID), 1)
		hearerID=sample(setdiff(1:length(population),speakerID),1)
	}	
	for (i in 1:nTurns){
		if(population[[speakerID]]$age < (talkAge*deathAge) & population[[hearerID]]$age > (talkAge*deathAge)){
			changeID=speakerID
			speakerID=hearerID
			hearerID=changeID
		}
		utterance=TURN(speakerID, hearerID)
		cat(paste(names(population)[[speakerID]], ': ', utterance, '\n', sep='')); flush.console()
		changeTurn=sample(1:0, 1, prob=turnChange)
		if(changeTurn==1){
			changeID=speakerID
			speakerID=hearerID
			hearerID=changeID
	}	}
	#semupdate
	if(!is.na(semUpdateAge)){population[[speakerID]]<<- SEMUPDATE(speakerID); population[[hearerID]]<<- SEMUPDATE(hearerID)}
	#reset common ground
	population[[speakerID]]$commonGround <<- vector()
	population[[hearerID]]$commonGround <<- vector()
	PROCREATE(speakerID, hearerID)
	if(speakerID > hearerID){DIE(speakerID); DIE(hearerID)}	#without condition, ref to second participant may be wrong after death of first
	if(hearerID > speakerID){DIE(hearerID); DIE(speakerID)}
}

RUN=function(nHours=1){
	nTurns=world$nTurns
	start=proc.time()[3]
	stop=proc.time()[3]
	while(((stop-start)/60/60)<nHours){
		TALK(sample(nTurns,1))
		stop=proc.time()[3]
}	}

MULTIRUN=function(nHours=24, nLineages=3, name='lineage', nAgents=2, close=T, ...){
	parameters=list(...)
	if(!'data'%in%dir()){dir.create('data')}
	if(length(parameters)!=0){for(i in 1:length(parameters)){world[[names(parameters)[i]]]<<-parameters[[i]]}}
	for(j in 1:nLineages){
		FOUND(nAgents)
		RUN(nHours)
		save.image(paste('data/', name, '-', j, '.rdata', sep=''))
	}
	if(close==T){q('no')}
}


SUMMARY=function(){
  	summary=list()
	agent=population[[1]]	#want usageHistory in graveyard schoongeveegd
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
				c(sum(graveyard$brains[[i]]$wordOrder[grep('^A', graveyard$brains[[i]]$wordOrder$order, invert=T), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) |
				c(sum(graveyard$brains[[i]]$wordOrder[grep('UV', graveyard$brains[[i]]$wordOrder$order, invert=T), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) |
				c(sum(graveyard$brains[[i]]$wordOrder[grep('VU', graveyard$brains[[i]]$wordOrder$order, invert=T), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) 
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
		yang=data.frame(order=c('A first', 'UV', 'VU'), generation=0, stringsAsFactors=F)
		for (i in seq(2, length(graveyard$brains), 2)){
			generation=graveyard$brains[[i]]$generation
			order=graveyard$brains[[i]]$wordOrder
			if(length(order$order[(sum(order$success)-order$success) < (sum(order$success)/log(sum(order$success)))])!=0){
				spec=order$order[(sum(order$success)-order$success) < (sum(order$success)/log(sum(order$success)))]
				if(spec%in%yang$order){yang[yang$order==spec,]$generation=paste(yang[yang$order==spec,]$generation, generation)}
				if(!spec%in%yang$order){yang[nrow(yang)+1,]$order=spec; yang[nrow(yang),]$generation=generation}
			}
			if(sum(graveyard$brains[[i]]$wordOrder[grep('^A', graveyard$brains[[i]]$wordOrder$order, invert=T), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))){
				yang[yang$order=='A first', ]$generation=paste(yang[yang$order=='A first', ]$generation, generation)
			}
			if(sum(graveyard$brains[[i]]$wordOrder[grep('UV', graveyard$brains[[i]]$wordOrder$order, invert=T), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))){
				yang[yang$order=='UV', ]$generation=paste(yang[yang$order=='UV', ]$generation, generation)
			}
			if(sum(graveyard$brains[[i]]$wordOrder[grep('VU', graveyard$brains[[i]]$wordOrder$order, invert=T), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))){
				yang[yang$order=='VU', ]$generation=paste(yang[yang$order=='VU', ]$generation, generation)
		}	}
		yang$generation=gsub('^0 ', '', yang$generation)
		summary$order$yang=yang
	summary$topic=1:agent$generation; summary$topic[!summary$topic%in%topic]=0
	summary$index=1:agent$generation; summary$index[!summary$index%in%index]=0
	summary$person=1:agent$generation; summary$person[!summary$person%in%person]=0
	summary$actor=1:agent$generation; summary$actor[!summary$actor%in%actor]=0
	summary$undergoer=1:agent$generation; summary$undergoer[!summary$undergoer%in%undergoer]=0
	#noun markers
	flag=agent$collostructions$flag
	flag$person=ifelse(flag$N%in%nouns[nouns$person!=3,]$ID, 'local', 'third')
	markers=head(sort(tapply(flag$frequency, flag$marker, sum), decreasing=T))
	markers=data.frame(ID=names(markers), form='', roleScore=0, frequency=markers, netto=0, proportion=0, semWeight=0, local=0, third=0, stringsAsFactors=F)
	for(i in 1:nrow(markers)){
		markers[i,]$form=nouns[nouns$ID==markers$ID[i],]$form
		markers[i,]$roleScore=rowMeans(nouns[nouns$ID==markers$ID[i],1:5], na.rm=T)
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

HISTORY=function(principle, unique=T){
	principle=toupper(principle)
	if(unique==T){print(unique(graveyard$history[graveyard$history$principle==principle,]))}
	if(unique==F){print(graveyard$history[graveyard$history$principle==principle,])}
}

CHECKMARKER=function(ID, generation='max', steps=1){
	ID=1
	if(generation=='max'){generation=length(graveyard$brains)}
	for(i in seq(2, generation*2, steps*2)){print(graveyard$brains[[i]]$generation); print(graveyard$brains[[i]]$nouns[graveyard$brains[[i]]$nouns$ID==ID,])}
}


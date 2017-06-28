VMATCH <-
function(x, y, incomparable=0){
	weigh=world$weigh
	if(length(x) <= length(y)){dimensions=1:length(x)}; if(length(y) < length(x)){dimensions=1:length(y)}
	x=x[dimensions]; y=y[dimensions]
	impacts=rep(1, length(dimensions))	
	if(weigh==TRUE){impacts=length(dimensions):1}
	if(is.data.frame(y)){
		if(nrow(y) > 1){
			if(is.vector(x)){z=as.data.frame(t(replicate(nrow(y),x)), stringsAsFactors=FALSE)}
			if(is.data.frame(x)){z=as.data.frame(t(replicate(nrow(y),unlist(x))), stringsAsFactors=FALSE)}
			diffs=abs(z-y)
			impacts=as.data.frame(t(replicate(nrow(diffs),impacts)), stringsAsFactors=FALSE)
			impacts[is.na(diffs)]=NA
			diffs=diffs*impacts
			vmatch=1-(rowSums(diffs, na.rm=TRUE)/rowSums(impacts, na.rm=TRUE))
			vmatch[ALLNAS(diffs)]=incomparable
		} else {
			diffs=abs(x-y)
			diffs=diffs*impacts
			vmatch=ifelse(ALLNAS(diffs), incomparable, 1-(rowSums(diffs, na.rm=TRUE)/sum(impacts[!(is.na(x) | is.na(y))])))
		}
	} else {
		diffs=abs(x-y)
		diffs=diffs*impacts
		vmatch=ifelse(ALLNAS(diffs), incomparable, 1-(sum(diffs, na.rm=TRUE)/sum(impacts[!(is.na(x) | is.na(y))])))
	}
vmatch
}

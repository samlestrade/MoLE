TURN <-
function(speakerID, hearerID){
	erosion=world$erosion
	situation= SITUATION(speakerID)
	proposition= PROPOSITION(speakerID, situation)
	prep= PREPARE(speakerID, proposition, situation)
	utterance= PRODUCE(speakerID, prep)
	interpretation= INTERPRET(hearerID, utterance, situation)
	population[[speakerID]]$age = population[[speakerID]]$age + 1
	population[[hearerID]]$age = population[[hearerID]]$age + 1
	success=SUCCESS(proposition, interpretation, situation)
	population[[speakerID]] = FREQUPDATE(speakerID, prep, success)
	population[[hearerID]] = FREQUPDATE(hearerID, interpretation, success)
	if(success==1){
		if(erosion==TRUE){population[[hearerID]] = EROSION(hearerID, interpretation)}
	}
utterance
}

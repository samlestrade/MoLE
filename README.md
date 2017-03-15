# MoLE
R package for modeling language evolution, to be added to CRAN eventually.

to run, :
source('.../MoLE.R')  #loads premature package
FOUND(2)  #founds a population with 2 agents
RUN()   #starts simulation for specified number of minutes (default is 1, for multiple generations to develop, the simulation has to run for hours).

After founding, the R workspace consist of an object with model settings ("world"), a number of subroutines, a population of two agents, and an object in which a summary of death agents will be stored ("graveyard"). 

After running for some time, also the latest speech situation, proposition, utterance, and interpretation are part of the workspace.

More information and instructions will follow in the package description, this page is mostly for reviewing purposes.

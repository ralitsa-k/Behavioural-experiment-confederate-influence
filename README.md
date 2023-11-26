
### This is the wrangling of very messy data coming from Gorilla platform and the analysis of an experiment in Psychology with many hypothesis.
### The analysis includes multiple linear regression, logistic regression, linear mixed effects models, and correlations. 

**Experiment summary**
The main gist is that participants were presented with options of fine art images to choose from.
After the participants chose one of the options they saw a video of someone else making a choice. 
The confederates who made the choice either mimicked the participant's choice or not. Or they
only mimicked the motor movement of the participant, i.e. they chose a different option but with the same hand. 
The control condition was motor or choice mimicking in 50-50 ratio. 

Multiple measures after the choice task measured the closeness and affect of each participant towards
the confederates. The main hypotheses is that there would be a difference between the affect of participants
influenced by the different types of mimicking. 

**load_data.R** (need data)
Collects all excell .csv files and combines them into meaningful files depending on the different questionnaires collected by Gorilla. 
This data is super messy, but each questionnaire has its own identifiers, so it can be pulled from the mass of .csv files. 
This file takes long to load, so only run necessary parts. 

**Analysis1.R** 
This is wrangling and data for the main hypothesis. Specifically, the analysis of 'liking of confederates'. 
This data required a multinomial logisitc regression, as the outcome var. contains three levels. 
I have included assumptions checks and plots for the logistic regression. 
This script also contains Chronbach's alpha checks testing the internal consistency, ie of the items within the questionnaire. 

**Analysis_rest.R**
Wrangling, analysis and plots for other questionnaires from the study, including Affiliation, Affective state, Maze game, Art interest, Rapport, and all of the above put together in a single score of 'liking of the confederate'. Individual differences are used as a modulating factor. 

**Analysis_rating.R**
Wrangling, analysis and plots for liking as measured by a rating scale before and after the experiment. Hence, we are here controlling for baseline. 






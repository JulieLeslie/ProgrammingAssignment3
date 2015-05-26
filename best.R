#put your instructions up here
best <- function(state,outcome){
	#set the warnings off
	options(warn = -1)
	
	#read the data. check it exists first
	x <- file.exists("outcome-of-care-measures.csv")
	if (!x) {
		stop("I can't find the data I need to check. Please check the working directory you are in")
	}
	mydata <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	#check the state is valid
	if (!any(mydata[,7]==state)) {
		stop("The state code you entered wasn't quite right or there are no entries. Check that you are using the two letter code for the state.")
	}
	
	#create functions for the switch to work
	#first one is for heart attack - get the columns and sort then pick the first one
	ha <- function(mydata,state){
		getdata <- data.frame(name=mydata[,2],code=mydata[,7],oc1=as.numeric(mydata[,11]),oc2=as.numeric(mydata[,29]))
		getdata2 <- subset(getdata, getdata$code==state, na.rm=TRUE)
		a <- getdata2[with(getdata2,order(oc1,oc2)),]
		b <- as.character(a[1,1])
		b
	}
	#heart failure
	hf <- function(mydata,state){
		getdata <- data.frame(name=mydata[,2],code=mydata[,7],oc1=as.numeric(mydata[,17]),oc2=as.numeric(mydata[,35]))
		getdata2 <- subset(getdata, getdata$code==state, na.rm=TRUE)
		a <- getdata2[with(getdata2,order(oc1,oc2)),]
		b <- as.character(a[1,1])
		b
	}
	#pneumonia
	p <- function(mydata,state){
		getdata <- data.frame(name=mydata[,2],code=mydata[,7],oc1=as.numeric(mydata[,23]),oc2=as.numeric(mydata[,41]))
		getdata2 <- subset(getdata, getdata$code==state, na.rm=TRUE)
		a <- getdata2[with(getdata2,order(oc1,oc2)),]
		b <- as.character(a[1,1])
		b
	}
	
	#Use the switch to activate the relevant function and return the hospital
	z <- switch(outcome,"heart attack"=ha(mydata,state),"heart failure"=hf(mydata,state),"pneumonia"=p(mydata,state),stop("You haven't entered a correct health outcome. Choose from heart attack, heart failure or pneumonia."))
	
	z
}


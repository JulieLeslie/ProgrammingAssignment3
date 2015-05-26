#This function returns the top hospitals in a selected state (using the two letter state code) for a given health outcome.
#the default number of top ranking hospitals is 5, but can be set
rankhospital <- function(state,outcome,num="best"){
	#set the warnings off
	options(warn = -1)
	
	#sort out how many records are going to be returned and if they want the best or worst performers
	if (!is.numeric(num)) {
		if (num=="best") {
			toportail <- "head"
		}
		else if (num == "worst") {
			toportail <- "tail"
		}
		else {
			stop("The number of records you want to return is invalid. Please enter a number or best or worst to get the top or bottom 5 records")
		}
		num <- as.numeric(5)
	}
	else {
		if (num<0) {
			toportail <- "tail"
			num <- abs(num)
		}
		else{
			toportail <- "head"
		}
	}

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
	ha <- function(mydata,state,num){
		getdata <- data.frame(ID=mydata[,1],Hospital.name=mydata[,2],code=mydata[,7],Rate=as.numeric(mydata[,11]))
		getdata2 <- subset(getdata, getdata$code==state, na.rm=TRUE)
		getdata2 <- getdata2[complete.cases(getdata2),]
		a <- getdata2[with(getdata2,order(Rate, Hospital.name)),]
		a$Rank <- as.integer(ave(a$Rate,FUN=rank))
		if (toportail=="head") {
			b <- head(a,num)
		}
		else {
			b <- tail(a,num)
		}
		b
	}
	#heart failure
	hf <- function(mydata,state, num){
		getdata <- data.frame(ID=mydata[,1],Hospital.name=mydata[,2],code=mydata[,7],Rate=as.numeric(mydata[,17]))
		getdata2 <- subset(getdata, getdata$code==state, na.rm=TRUE)
		getdata2 <- getdata2[complete.cases(getdata2),]
		a <- getdata2[with(getdata2,order(Rate,Hospital.name)),]
		a$Rank <- as.integer(ave(a$Rate,FUN=rank))
		if (toportail=="head") {
			a <- head(a,num)
		}
		else {
			a <- tail(a,num)
		}
		a <- data.frame(a$ID,a$Hospital.name,a$Rate,a$Rank)
		a
	}
	#pneumonia
	p <- function(mydata,state,num){
		getdata <- data.frame(ID=mydata[,1],Hospital.name=mydata[,2],code=mydata[,7],Rate=as.numeric(mydata[,23]))
		getdata2 <- subset(getdata, getdata$code==state, na.rm=TRUE)
		getdata2 <- getdata2[complete.cases(getdata2),]
		a <- getdata2[with(getdata2,order(Rate,Hospital.name)),]
		a$Rank <- as.integer(ave(a$Rate,FUN=rank))
		if (toportail=="head") {
			a <- head(a,num)
		}
		else {
			a <- tail(a,num)
		}
		a <- data.frame(a$ID,a$Hospital.name,a$Rate,a$Rank)
		a
	}
	
	#Use the switch to activate the relevant function and return the hospital
	z <- switch(outcome,"heart attack"=ha(mydata,state,num),"heart failure"=hf(mydata,state,num),"pneumonia"=p(mydata,state,num),stop("You haven't entered a correct health outcome. Choose from heart attack, heart failure or pneumonia."))
	
	z
}


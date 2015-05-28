#This function returns the top ranked hospitals anywhere for a given health outcome.

rankall <- function(outcome,num="best"){
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
	
	#create functions for the switch to work
	#first one is for heart attack - get the columns and sort then pick the first one
	ha <- function(mydata,num){
		getdata <- data.frame(Hospital=mydata[,2],state=mydata[,7],Rate=as.numeric(mydata[,11]))
		getdata2 <- na.omit(getdata)
		a <- getdata2[with(getdata2,order(Rate, Hospital)),]
		a$Rate <- NULL
		if (toportail=="head") {
			a <- head(a,num)
		}
		else {
			a <- tail(a,num)
		}
		a
	}
	#heart failure
	hf <- function(mydata, num){
		getdata <- data.frame(ID=mydata[,1],Hospital=mydata[,2],State=mydata[,7],Rate=as.numeric(mydata[,17]))
		getdata2 <- na.omit(getdata)
		a <- getdata2[with(getdata2,order(Rate, Hospital)),]
		a$Rate <- NULL
		if (toportail=="head") {
			a <- head(a,num)
		}
		else {
			a <- tail(a,num)
		}
		a
	}
	#pneumonia
	p <- function(mydata,num){
		getdata <- data.frame(ID=mydata[,1],Hospital=mydata[,2],State=mydata[,7],Rate=as.numeric(mydata[,23]))
		getdata2 <- na.omit(getdata)
		a <- getdata2[with(getdata2,order(Rate, Hospital)),]
		a$Rate <- NULL
		if (toportail=="head") {
			a <- head(a,num)
		}
		else {
			a <- tail(a,num)
		}
		a
	}
	
	#Use the switch to activate the relevant function and return the hospital
	z <- switch(outcome,"heart attack"=ha(mydata,num),"heart failure"=hf(mydata,num),"pneumonia"=p(mydata,num),stop("You haven't entered a correct health outcome. Choose from heart attack, heart failure or pneumonia."))
	
	z
}



## testing system time useing system.time function according to instructer 

system.time({
  n<-100
  r<-numeric(n)
  for( i in 1:n){
    x<-rnorm(n)
    r[i]<-mean(x)
  }
})

## weekly quize 
#quize 1 

set.seed(1)
rpois(5, 2)

## quiz 4 
?dpois


### use of swirl libaray to complete the task 

library(swirl)
 # call the function that run the swirl funtion 
swirl()

## programming assignment 

#----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------

## Writing the function for finding best hospitals in a state 

best<-function(state,outcome){
  #reading the data set 
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #reduce data set 
  r_data<-data.frame(cbind(data[,2],
                           data[,7],
                           data[,11],
                           data[,17],
                           data[,23]),
                     stringsAsFactors=FALSE)
  # change column name
  colnames(r_data)<-c("hospitals","states","heart attack","heart failure", "pneumonia")
  #find valid state
  if (! state %in% r_data[,"states"]){
    stop("invalid state")
  }
  #find valid outcome 
  if(! outcome %in% c("heart attack","heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  # selecte cases that maches with desired state 
  ss<-r_data[,"states"]==state
  s_data<-r_data[ss,]
  # make outcome variable numeric 
  s_data[,outcome]<-as.numeric(s_data[,outcome])
  #delete missing values
  s_data<-s_data[!is.na(s_data[,outcome]),]
  #order outcome variable 
  s_data<-s_data[order(s_data[,outcome]),]
  #vector of minimum value of outcome variable 
  hnames<-s_data[s_data[,outcome]==min(s_data[,outcome]),1]
  #ordering according of alphabetly as instructed 
  sort(hnames)[1]
  
}

best("TX", "heart attack")
best("TX", "heart failure")
best("TX", "pneumonia")
best("MD", "heart attack")



### --------------------------------------------------------------------------------------------

##--------------------------------------------------------------------------------------------

##--------------------------------------------------------------------------------------------

## creating function of rankhospital 

rankhospital<-function(state,outcome,num=1){
  #reading the data set 
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #reduce data set 
  r_data<-data.frame(cbind(data[,2],
                           data[,7],
                           data[,11],
                           data[,17],
                           data[,23]),
                     stringsAsFactors=FALSE)
  # change column name
  colnames(r_data)<-c("hospitals","states","heart attack","heart failure", "pneumonia")
  #find valid state
  if (! state %in% r_data[,"states"]){
    stop("invalid state")
  }
  #find valid outcome 
  if(! outcome %in% c("heart attack","heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  # selecte cases that maches with desired state 
  ss<-r_data[,"states"]==state
  s_data<-r_data[ss,]
  # make outcome variable numeric 
  s_data[,outcome]<-as.numeric(s_data[,outcome])
  #delete missing values
  s_data<-s_data[!is.na(s_data[,outcome]),]
  #order outcome variable 
  s_data<-s_data[order(s_data[,outcome]),]
  s_data$rank<-1:nrow(s_data)
  
  ##select correct number of row 
  if(is.numeric(num)){
    vv<-1:num
  } else {
    if(num=="best"){
      vv<-1
    }
    if(num=="worst"){
      vv<-nrow(s_data)
    }
  }
  s_data<-s_data[vv,]
  ## select correct variables
  ttt<-which(colnames(s_data)==outcome)
  paste(hospitas=s_data[,1],Rate=s_data[,ttt],Rank=s_data[,6])
  
}



#### ----------------------------------------------------------------------------------

#####---------------------------------------------------------------------------------------------

### 3rd function 

rankall <- function(outcome, num = 'best') {
  ## Read outcome data
  
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack
                               outcomes[, 17],  # heart failure
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check outcome is valid
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  hRank <- data.frame()
  
  for(state in sort(unique(rates[,"state"]))){
    
    ## Get only the hospitals in this state
    hRates <- rates[(rates[, "state"] == state), ]
    
    ## Convert outcome rate to numberic, gets a warning
    hRates[, outcome] <- as.numeric(hRates[, outcome])
    
    ## Remove NA values
    hRates <- hRates[!is.na(hRates[, outcome]), ]
    
    ## convert num argument to valid rank
    
    if(num == "best") {
      rnum <- 1 
    } else if (num == "worst") {
      rnum <- nrow(hRates) 
    }
    else {rnum = num}
    
    
    ## Order by outcome rate & hospital name
    hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
    
    hName <- hRates[rnum,1]
    
    hRank <- rbind(hRank,
                   data.frame(hospital = hName,
                              state = state))
  }
  
  ## Return dataframe
  hRank
  
  
}



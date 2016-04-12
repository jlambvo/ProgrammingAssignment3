# best <- function(state,outcome){
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character") #read in data
  data <- data[data$State==state,] #subset to defined state
  
      if(tolower(outcome)=="heart attack"){
            c <- 11
      }
      
      if(tolower(outcome)=="heart failure"){
            c <- 17
      }
  
      if(tolower(outcome)=="pneumonia"){
            c <- 23
      }
        
      complete <- !is.na(suppressWarnings(as.numeric(data[,c])))
      heartAttack <- data.frame(
        "Rate"=as.numeric(data[complete,c]),
        "State"=data[complete,"State"],
        "Hospital"=data[complete,"Hospital.Name"]
      )
      
      m <- min(heartAttack$Rate)
      match <- heartAttack[heartAttack$Rate==m,]
      match <- match[order("Hospital","State","Rate"),]
      return(as.character(match$Hospital))
}

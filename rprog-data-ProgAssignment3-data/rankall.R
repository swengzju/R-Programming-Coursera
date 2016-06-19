rankall <- function(outcome, num = "best") {
        readdata <- read.csv("outcome-of-care-measures.csv", 
                             na.strings = "Not Available", colClasses = "character")
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }
        splitdata <- split(readdata, readdata$State)
        statename <- c()
        hospitalname <- c()
        for(data in splitdata){
                statename <- c(statename, data[1, 7])
                if(outcome == "heart attack"){
                        df <- data.frame(data[, 2], as.numeric(data[, 11]))
                        ordered <- df[order(df[, 2], df[, 1], na.last = NA), ]
                        
                        if(num == "best"){
                                result <- as.character(ordered[1, 1])
                        }
                        else if(num == "worst"){
                                result <- as.character(ordered[nrow(ordered), 1])
                        }
                        else if(num > nrow(ordered)){
                                result <- NA
                        }
                        else{
                                result <- as.character(ordered[num, 1])
                        } 
                        hospitalname <- c(hospitalname, result)
                }
                if(outcome == "heart failure"){
                        df <- data.frame(data[, 2], as.numeric(data[, 17]))
                        ordered <- df[order(df[, 2], df[, 1], na.last = NA), ]
                        
                        if(num == "best"){
                                result <- as.character(ordered[1, 1])
                        }
                        else if(num == "worst"){
                                result <- as.character(ordered[nrow(ordered), 1])
                        }
                        else if(num > nrow(ordered)){
                                result <- NA
                        }
                        else{
                                result <- as.character(ordered[num, 1])
                        }  
                        hospitalname <- c(hospitalname, result)
                }
                if(outcome == "pneumonia"){
                        df <- data.frame(data[, 2], as.numeric(data[, 23]))
                        ordered <- df[order(df[, 2], df[, 1], na.last = NA), ]
                       
                        if(num == "best"){
                                result <- as.character(ordered[1, 1])
                        }
                        else if(num == "worst"){
                                result <- as.character(ordered[nrow(ordered), 1])
                        }
                        else if(num > nrow(ordered)){
                                result <- NA
                        }
                        else{
                                result <- as.character(ordered[num, 1])
                        }
                        hospitalname <- c(hospitalname, result)
                }
        }
        table <- as.data.frame(cbind(hospitalname, statename))
        names(table) <- c("hospital", "state")
        table        
}
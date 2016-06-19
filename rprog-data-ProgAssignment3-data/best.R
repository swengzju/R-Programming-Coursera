best <- function(state, outcome) {
        readdata <- read.csv("outcome-of-care-measures.csv", 
                         na.strings = "Not Available", colClasses = "character")
        if(!state %in% readdata[, 7]){
                stop("invalid state")
        }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }
        data <- readdata[readdata$State == state, ]
        if(outcome == "heart attack"){
        df <- data.frame(data[, 2], as.numeric(data[, 11]))
        ordered <- df[order(df[, 2], df[, 1]), ]
        result <- as.character(ordered[1, 1])
        }
        if(outcome == "heart failure"){
                df <- data.frame(data[, 2], as.numeric(data[, 17]))
                ordered <- df[order(df[, 2], df[, 1]), ]
                result <- as.character(ordered[1, 1])
        }
        if(outcome == "pneumonia"){
                df <- data.frame(data[, 2], as.numeric(data[, 23]))
                ordered <- df[order(df[, 2], df[, 1]), ]
                result <- as.character(ordered[1, 1])
        }
        result
}

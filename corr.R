corr <- function(directory, threshold = 0){
        setwd(paste("~/Desktop/R", directory, sep="/"))
        files <- list.files()
        result <- numeric()
        for(i in seq_along(files)){    
                data <- read.csv(files[i])
                nobs <- sum(complete.cases(data))
                temp <- data[complete.cases(data),]
                if(nobs > threshold){
                        result <- c(result, cor(temp$sulfate, temp$nitrate))
                }
        }
        result
}
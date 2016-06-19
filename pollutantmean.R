pollutantmean <- function(directory, pollutant, id = 1:332){
        data <- data.frame()
        for (i in id){
                n <- sprintf("%03d", i)
                data <- rbind(data, read.csv(paste(getwd(), "/", directory, 
                                                   "/", n, ".csv", sep="")))
                }
        mean <- mean(data[, pollutant], na.rm = TRUE)
        answer <- round(mean, 3)
        answer
}
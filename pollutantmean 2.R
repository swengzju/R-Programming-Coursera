pollutantmean <- function(directory, pollutant, id){
        data <- data.frame()
        files <- list.files(directory)
        myFiles <- paste(directory, files[id], sep="/")
        for (file in myFiles){    
                data <- rbind(data, read.csv(file))
        }
        mean <- mean(data[, pollutant], na.rm = TRUE)
        answer <- round(mean, 3)
        answer
}
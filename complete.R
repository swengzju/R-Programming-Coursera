complete <- function(directory, id = 1:332){
        setwd(paste("~/Desktop/R", directory, sep="/"))
        id.vector <- c()
        nobs.vector <- c()
        for(i in id){    
                data <- read.csv(list.files()[i])
                nobs <- sum(complete.cases(data))
                id.vector <- c(id.vector, i)
                nobs.vector <- c(nobs.vector, nobs)
        }
        table <- as.data.frame(cbind(id.vector, nobs.vector))
        names(table) <- c("id", "nobs")
        print(table)
}

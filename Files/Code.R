setwd("~/Documents/PhD/UTD/Classes/6. Summer 2020/Ex1")


preload <- function(x){
  #This function evaluates Package x. If it is installed, it loads the package. If it is
  #not, it installs it.
  x <- as.character(x)
  if(!require(x,character.only = TRUE)){
    install.packages(x,repos = "https://cloud.r-project.org")
    require(x,character.only = TRUE)
  }
}
preload("readr")
preload("ggplot2")

data <- read_csv("Chicago_Police.csv")

desc <- function(x){
  #This function shows the median, mean, min, and max of a numeric variable
  x <- as.numeric(x)
  median <- c("Median",round(median(x),2))
  mean <- c("Mean", round(mean(x),2))
  min <- c("Min",min(x))
  max <- c("Max",max(x))
  output <- rbind.data.frame(median,mean,min,max)
  colnames(output) <- NULL
  print(output)
}

desc(data$replies)
mean(data$replies)
max(data$replies)

ggplot(data, aes(x=username, y=replies)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip()



plot(data$replies)


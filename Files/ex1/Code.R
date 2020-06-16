setwd("~/Documents/PhD/UTD/Classes/6. Summer 2020/Ex1")#Adjust accordingly


preload <- function(x){
  #This function evaluates Package x. If it is installed, it loads the package. If it is
  #not, it installs it.
  x <- as.character(x)
  if(!require(x,character.only = TRUE)){
    install.packages(x,repos = "https://cloud.r-project.org")
    require(x,character.only = TRUE)
  }
}
preload("readr")#For CSV files loading
preload("ggplot2")#For plots
preload("gridExtra")


data <- read_csv("Chicago_Police.csv")#CPD Tweets
data2 <- read_csv("CPD.csv")#Tweets about police brutality in Chicago

desc <- function(x){
  #This function shows the median, mean, min, and max of a numeric variable
  x <- as.numeric(x)
  median <- c("Median",round(median(x),2))
  mean <- c("Mean", round(mean(x),2))
  min <- c("Min",min(x))
  max <- c("Max",max(x))
  output <- data.frame(rbind(median,mean,min,max))
  row.names(output) <- NULL
  colnames(output) <- NULL
  print(output)
}

pdf(file = "Plot1.pdf", width = 7, height = 5)
ggplot(data[data$replies<quantile(data$replies, .99),], aes(x=replies)) +
  geom_histogram(binwidth=1, colour="black", fill="deepskyblue3",breaks=seq(0, 50, by = 1)) +
  theme_light() + labs(title="Number of Replies to @Chicago_Police Tweets",x ="Number of Replies", y = "Count")+
annotation_custom(tableGrob(desc(data$replies)), xmin=35, xmax=45)
dev.off()


pdf(file = "Plot2.pdf", width = 7, height = 5)
ggplot(data[data$retweets<quantile(data$retweets, .99),], aes(x=retweets)) +
  geom_histogram(binwidth=1, colour="black", fill="deepskyblue3",breaks=seq(0, 50, by = 1)) +
  theme_light() + labs(title="Number of Retweets of @Chicago_Police Tweets",x ="Number of Retweets", y = "Count")+
  annotation_custom(tableGrob(desc(data$retweets)), xmin=35, xmax=45)
dev.off()


pdf(file = "Plot3.pdf", width = 7, height = 5)
ggplot(data[data$favorites<quantile(data$favorites, .99),], aes(x=favorites)) +
  geom_histogram(binwidth=1, colour="black", fill="deepskyblue3",breaks=seq(0, 50, by = 1)) +
  theme_light() + labs(title="Number of Favs of @Chicago_Police Tweets",x ="Number of Favorites", y = "Count")+
  annotation_custom(tableGrob(desc(data$favorites)), xmin=35, xmax=45)
dev.off()


days <- data.frame(table(weekdays(as.Date(data2$date))))
names(days) <- c("DOW","Freq")
days$DOW <- as.character(days$DOW)
days$DOW <- factor(days$DOW, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

pdf(file = "Plot4.pdf", width = 7, height = 5)
ggplot(days, aes(x=DOW, y=Freq)) + geom_bar(colour="black", stat="identity",fill="deepskyblue3") + 
  theme_light() + labs(title="Day of the Week for Tweets Containing \'PoliceBrutality\' in Chicago Area",x ="Day of the Week", y = "Count")
dev.off()

data2$time <- strptime(data2$date, "%Y-%m-%d %H:%M:%S")
data2$time <- as.factor(format(data2$time, "%H"))
time <- data.frame(table(data2$time))
names(time) <- c("TOD","Freq")

pdf(file = "Plot5.pdf", width = 7, height = 5)
ggplot(time, aes(x=TOD, y=Freq)) + geom_bar(colour="black", stat="identity",fill="deepskyblue3") + 
  theme_light() + labs(title="Hour of the Day for Tweets Containing \'PoliceBrutality\' in Chicago Area",x ="Time of the Deek", y = "Count")
dev.off()

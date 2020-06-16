setwd("~/Documents/PhD/UTD/Classes/6. Summer 2020/Ex2")#Adjust accordingly

#I will load some functions to automate coding in R. (Don't forget to add "?raw=TRUE" at the end of the URL)
devtools::source_url("https://github.com/sebmontenegro/functions/blob/master/preload.R?raw=TRUE")

#Packages required:
preload("rtweet")
preload("igraph")
preload("extrafont")
preload("extrafontdb")
preload("hrbrthemes")
preload("ggplot2")#For plots
preload("ggraph")
preload("tidyverse")
preload("readr")#For CSV files loading
preload("gridExtra")#For adding tables to ggplot


data <- read_csv("CPD.csv")

p1 <- data.frame(table(as.Date(data$date)))

tiff("plot1.tiff", units="in", width=7, height=5, res=300)
ggplot(p1[which(as.Date(p1$Var1)>="2020-01-01"),], aes(x=as.Date(Var1), y=Freq)) + geom_line() + theme_light()+
  scale_x_date(date_labels = "%b %Y") + labs(title="Number of \'Police Brutality\' Tweets in the Chicago Area",x ="", y = "Frequancy")
dev.off()

desc <- function(x){
  #This function shows the median, mean, min, and max of a numeric variable
  x <- as.numeric(x)
  median <- c("Median",round(median(x),2))
  mean <- c("Mean", round(mean(x),2))
  min <- c("Min",min(x))
  max <- c("Max",max(x))
  name <- c("User",data$username[which(x==max(x))])
  output <- data.frame(rbind(median,mean,min,max,name))
  row.names(output) <- NULL
  colnames(output) <- NULL
  print(output)
}

tmp1 <- data[data$replies>0,]
tiff("plot1.tiff", units="in", width=7, height=5, res=300)
ggplot(tmp1, aes(x=replies)) +
  geom_histogram(binwidth=1, colour="black", fill="deepskyblue3") +
  theme_light() + labs(title="Number of Replies to \'Police Brutality\' Tweets in the Chicago Area",x ="Number of Replies", y = "Frequancy") +
  annotation_custom(tableGrob(desc(tmp1$replies)), xmin=200, xmax=400)
dev.off()
rm(tmp1)

tmp1 <- data[data$retweets>0,]
tiff("plot2.tiff", units="in", width=7, height=5, res=300)
ggplot(tmp1, aes(x=replies)) +
  geom_histogram(binwidth=1, colour="black", fill="deepskyblue3") +
  theme_light() + labs(title="Number of RT to \'Police Brutality\' Tweets in the Chicago Area",x ="Number of Replies", y = "Frequancy") +
  annotation_custom(tableGrob(desc(tmp1$retweets)), xmin=200, xmax=400)
dev.off()
rm(tmp1)

tmp1 <- data[data$favorites>0,]
tiff("plot3.tiff", units="in", width=7, height=5, res=300)
ggplot(tmp1, aes(x=replies)) +
  geom_histogram(binwidth=1, colour="black", fill="deepskyblue3") +
  theme_light() + labs(title="Number of RT to \'Police Brutality\' Tweets in the Chicago Area",x ="Number of Replies", y = "Frequancy") +
  annotation_custom(tableGrob(desc(tmp1$favorites)), xmin=200, xmax=400)
dev.off()
rm(tmp1)


token <- rtweet::create_token(
  app = "app",
  consumer_key <- "consumer_key",
  consumer_secret <- "consumer_secret",
  access_token <- "access_token",
  access_secret <- "access_secret")

## Check token

rtweet::get_token()


query <- search_tweets2(q="police brutality", n=1000)
# OR ChicagoCAPS01 OR ChicagoCAPS02 OR ChicagoCAPS03 OR ChicagoCAPS04 OR ChicagoCAPS05 OR ChicagoCAPS06 OR ChicagoCAPS07 OR ChicagoCAPS08 OR ChicagoCAPS09 OR ChicagoCAPS10 OR ChicagoCAPS11 OR ChicagoCAPS12 OR ChicagoCAPS14 OR ChicagoCAPS15 OR ChicagoCAPS16 OR ChicagoCAPS17 OR ChicagoCAPS18 OR ChicagoCAPS19 OR ChicagoCAPS20 OR ChicagoCAPS22 OR ChicagoCAPS24 OR ChicagoCAPS25
# same as previous recipe
filter(query, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> rt_g


V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, names(V(rt_g)), "")) 
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, degree(rt_g), 0)) 

tiff("PoliceBrutality.tiff", units="in", width=7, height=7, res=300)
ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.125, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="gray31", segment.colour="dodgerblue",
                  color="gray100", repel=TRUE, family=font_rc, fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships (\'Police Brutality\')", caption="Most retweeted screen names labeled. Darkers edges denotes more retweets. Node size denotes larger degree") +
  theme_graph(base_family=font_rc) +
  theme(legend.position="none")
dev.off()



query <- search_tweets2(q="Chicago_Police", n=1000)
# OR ChicagoCAPS01 OR ChicagoCAPS02 OR ChicagoCAPS03 OR ChicagoCAPS04 OR ChicagoCAPS05 OR ChicagoCAPS06 OR ChicagoCAPS07 OR ChicagoCAPS08 OR ChicagoCAPS09 OR ChicagoCAPS10 OR ChicagoCAPS11 OR ChicagoCAPS12 OR ChicagoCAPS14 OR ChicagoCAPS15 OR ChicagoCAPS16 OR ChicagoCAPS17 OR ChicagoCAPS18 OR ChicagoCAPS19 OR ChicagoCAPS20 OR ChicagoCAPS22 OR ChicagoCAPS24 OR ChicagoCAPS25
# same as previous recipe
filter(query, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> rt_g


V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, names(V(rt_g)), "")) 
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, degree(rt_g), 0)) 

tiff("Chicago_Police.tiff", units="in", width=7, height=7, res=300)
ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.125, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="gray31", segment.colour="dodgerblue",
                  color="gray100", repel=TRUE, family=font_rc, fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships (@Chicago_Police)", caption="Most retweeted screen names labeled. Darkers edges denotes more retweets. Node size denotes larger degree") +
  theme_graph(base_family=font_rc) +
  theme(legend.position="none")
dev.off()



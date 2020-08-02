## Examining Police Misconduct: Evidence from Chicago
I will study data from the Citizens Police Data Project which can be accessed via [their website](https://www.cpdp.co). The datasets can be downloaded at different grouping levels such as police districts, wards, beat, and neighborhoods. The data are presented in comma-separated value files that contain different variables describing the characteristics of the interaction between the police officer and the complaining witness.

You can access the data and code that I used to clean the data files [here](data).

I will start by providing you with a couple of visualizations that are very interesting in order to understand what's going on within the Chicago PD.

We will need the following packages:
```{r Preload, message=FALSE, warning=FALSE, paged.print=FALSE}
devtools::source_url("https://github.com/sebmontenegro/functions/blob/master/preload.R?raw=TRUE")

preload("tidyverse")
preload("readxl")
preload("dplyr")
preload("zoo")
preload("stringr")
```
The `preload` function can be accessed via Internet from my GitHub. I created this function to load packages and/or install them on the system when they are not already installed.

```{r Data, include=FALSE,cache=TRUE}
load("~/Dropbox/Project/Code/Data.RData")
setwd("~/Dropbox/Project/Code/Plots")
```

This is how the number of allegations of both overtime for the Chicago PD
```{r Plot 1, echo=TRUE}
allegations %>%
  filter(IncidentDate>=as.Date("1999-01-01")) %>%
  count(Date = factor(IncidentDate)) %>%
  ggplot(aes(x=as.Date(Date),y=n))+
  geom_line(color="skyblue4")+
  theme_bw()+
  labs(x ="", y = "Number of Allegations")
```
It has a downward trend and has a drop in 2005 and 2016.


```{r Plot 2, echo=TRUE}
allegations %>%
  filter(Finding!="Unknown") %>%
  count(Category = Category, Finding = Finding) %>%
  filter(n>=15) %>%
  ggplot(aes(x=reorder(Category,-n),y=n,fill=Finding))+
  geom_bar(stat = "identity")+ #facet_wrap(~District,nrow=4)+
  theme_bw()+
  labs(x ="Allegation Category", y = "Number of Allegations")+coord_flip()+
  scale_fill_brewer(palette="Dark2")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),legend.position = "bottom")
```






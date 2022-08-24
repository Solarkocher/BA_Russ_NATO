# library
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(extrafont)
library(ggthemes)

#import data
#konfliktskala=read_csv(here("data","konfliktskala.csv"))
data=konfliktskala

#Create Mean of variables and change type of data
Year.mean <- with(data, ave(Weight, Year, FUN = function(x) mean(x, na.rm = TRUE)))
data_new <- data.frame(data, Year.mean)
Date.mean <- with(data, ave(Weight, Date, FUN = function(x) mean(x, na.rm = TRUE)))
data_new <- data.frame(data, Date.mean)
data_new$Date <- as.Date(data_new$Date)

#load fonts
loadfonts(device = "win")
names(wf[wf=="TT Times New Roman"])

#create plot using data konfliktskala
ggp <-data_new %>%
  tail(200) %>%
  ggplot( aes(x=Year, y=Year.mean)) + 
  geom_line( color="red") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  ggtitle("NATO-Russia: Degree of Cooperation between 1997-2022") +
  labs(x = "Year", y = "Mean Degree of Cooperation")+
  theme_minimal ()+
  theme(
    plot.title = element_text(family="serif" ,color="black", size=14, face="bold", hjust = 0.5),
    axis.title.y = element_text(family="serif", color="black", size=12, face="bold"),#  axis.title.y = element_text(family="serif",color="black", size=12, face="bold")
    axis.title.x = element_text(family="serif", color="black", size=12, face="bold"),#  axis.title.y = element_text(family="serif",color="black", size=12, face="bold")
  )
#create values for vertical lines that will mark obversation points
v_line_case1 <- 1997
v_line_case2 <- 1999
v_line_case3 <- 2002
v_line_case4 <- 2004
v_line_case5 <- 2008
v_line_case6 <- 2014
v_line_case7 <- 2022

#add observation points as vertical lines to the graph
ggp +
  geom_vline(aes(xintercept = v_line_case1))+
  geom_text(aes(x=1996.7, label="Founding Act of 97", y=3), colour="#666666", angle=90, text=element_text(size=10)) +
  geom_vline(aes(xintercept = v_line_case2))+
  geom_text(aes(x=1998.7, label="Kosovo/1.Enlargement", y=3.5), colour="#666666", angle=90, text=element_text(size=10)) +
  geom_vline(aes(xintercept = v_line_case3))+
  geom_text(aes(x=2001.7, label="Rome Declaration", y=2.8), colour="#666666", angle=90, text=element_text(size=10)) +
  geom_vline(aes(xintercept = v_line_case4))+
  geom_text(aes(x=2003.4, label="2. Enlargement", y=3.8), colour="#666666", angle=90, text=element_text(size=10))+
  geom_vline(aes(xintercept = v_line_case5))+
  geom_text(aes(x=2007.5, label="Russo-Georgian War", y=2.7), colour="#666666", angle=90, text=element_text(size=10)) +
  geom_vline(aes(xintercept = v_line_case7))+
  geom_text(aes(x=2021.7, label="Invasion of Ukraine", y=3), colour="#666666", angle=90, text=element_text(size=10))+
  geom_vline(aes(xintercept = v_line_case6))+
  geom_text(aes(x=2013.7, label="Crimea Annexation", y=3), colour="#666666", angle=90, text=element_text(size=10)) 

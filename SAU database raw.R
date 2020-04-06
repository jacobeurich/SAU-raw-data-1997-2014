##### Sea around us fisheries catch data #####
# http://www.seaaroundus.org/
title: "SAU database raw"
author: "Jacob Eurich jacobeurich@ucsb.edu"
date: "10/23/2019 last update"
# data includes all catch data from 1997 - 2014 by EEZ

# Load packages and raw data
rm(list=ls())
setwd("~/Documents/ACTIVE Research/Other Projects/Coral Bleaching and Human Nutrition/Data/SAU fishing data/Full databases")

# require(lmerTest) || {install.packages("lmerTest"); require(lmerTest)}
# require(gridExtra) || {install.packages("gridExtra"); require(gridExtra)}
# library(gridExtra)
# library(lmerTest)
# require(lme4) || {install.packages("lme4"); require(lme4)}
# library(lme4)
# library(lattice)
library(ggplot2)
# require(multcomp) || {install.packages("multcomp"); require(multcomp)}
# library(multcomp)
sessionInfo()


##### Data subsets and cleaning from raw data (2 files) #####
data1 <- read.csv(file="SAU raw database by EEZ 1997-2014 all catch part 1.csv", header=TRUE)
data2 <- read.csv(file="SAU raw database by EEZ 1997-2014 all catch part 2.csv", header=TRUE)
sau_raw <- rbind(data1, data2)

names(sau_raw)
summary_list <- lapply(sau_raw, summary)

df_uniq <- unique(sau_raw$area_name)
length(df_uniq) # 158 countries
df_uniq <- unique(sau_raw$scientific_name)
length(df_uniq) # 1472 unique entries in SAU database

aggregate(tonnes ~ area_name, data=sau_raw, FUN=sum)
aggregate(tonnes ~ year, data=sau_raw, FUN=sum)

sau <- sau_raw
str(sau)
sau$year <- as.numeric(as.character(sau$year))
ii <- 1997:2014


# Subsistence fishing subset
sau_subonly <- subset(sau, fishing_sector=="Subsistence")

ggplot(sau_subonly, aes(x=year, y=tonnes, color = commercial_group, fill = commercial_group, label = commercial_group)) + geom_line(stat="summary", position="dodge", fun.y = "sum") + theme_classic() +
  scale_x_continuous(breaks=ii) + theme(axis.text.x = element_text(angle=45,  hjust = 1)) + scale_y_continuous(expand= c(0,0)) + theme(axis.text = element_text(color="black")) + 
  xlab("") + ylab("Tonnes") + ggtitle("SAU subsistence fishing")


# Reef associated fishes subset 
reeffish <- subset(sau, functional_group=="Medium reef assoc. fish (30 - 89 cm)" | functional_group=="Large reef assoc. fish (>=90 cm)" | functional_group=="Small reef assoc. fish (<30 cm)" )

ggplot(reeffish, aes(x=year, y=tonnes, color = functional_group, fill = functional_group, label = functional_group)) + geom_line(stat="summary", position="dodge", fun.y = "sum") + theme_classic() +
  scale_x_continuous(breaks=ii) + theme(axis.text.x = element_text(angle=45,  hjust = 1)) + scale_y_continuous(expand= c(0,0)) + theme(axis.text = element_text(color="black")) + 
  xlab("") + ylab("Tonnes") + ggtitle("SAU catch of reef associated fishes")


# Country subset
sau_mad <- subset(sau, area_name=="Madagascar")
# write.csv(sau_mad, "Madagascar EEZ SAU data 1997-2014.csv")

ggplot(sau_mad, aes(x=year, y=tonnes, color = fishing_sector, fill = fishing_sector, label = fishing_sector)) + geom_bar(stat="identity") + theme_classic() +
  scale_x_continuous(breaks=ii) + theme(axis.text.x = element_text(angle=45,  hjust = 1)) + scale_y_continuous(expand= c(0,0)) + theme(axis.text = element_text(color="black")) + 
  xlab("") + ylab("Tonnes") + ggtitle("Madagascar SAU catch")


# Subset by piscivores
pred <- subset(sau, scientific_name=="Nemipteridae" | scientific_name=="Priacanthus" | scientific_name=="Mugilidae" | scientific_name=="Monacanthidae" | scientific_name=="Lutjanidae" | scientific_name=="Serranidae" |scientific_name=="Mugilidae" | scientific_name=="Epinephelus" | scientific_name=="Lethrinidae" | scientific_name=="Haemulidae" )
ggplot(pred, aes(x = year, y = tonnes, color = scientific_name)) + geom_line(stat="summary", position="dodge", fun.y = "sum") + theme_classic() +
  scale_x_continuous(breaks=ii) + theme(axis.text.x = element_text(angle=45,  hjust = 1)) + scale_y_continuous(expand= c(0,0)) + theme(axis.text = element_text(color="black")) + 
  xlab("") + ylab("Tonnes") + ggtitle("Piscivores SAU catch")




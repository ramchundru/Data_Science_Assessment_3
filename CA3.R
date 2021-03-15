#importing required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(maps)
library(mapdata)
library(sp)
library(maptools)
library(rgdal)
library(mapproj)
library(scales)
library(treemapify)
library(ckanr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(readr) 
library(dplyr)
# library(xlsx)
library(openxlsx)
library(mice)
library(VIM)
library(Hmisc)
library(foreign)
library(knitr)



#Reading the CSV files and assigning their values into a dataframe.

df2014 <- read.csv("OP Waiting List By Group Hospital 2014.csv", header = TRUE)

df2015 <- read.csv("OP Waiting List By Group Hospital 2015.csv", header= TRUE)

df2016 <- read.csv("OP Waiting List By Group Hospital 2016.csv", header = TRUE)

df2017 <- read.csv("OP Waiting List By Group Hospital 2017.csv", header = TRUE)

df2018 <- read.csv("OP Waiting List By Group Hospital 2018.csv", header = TRUE)


#rename the header if one is different from others
colnames(df2014)[colnames(df2014)=="ï..Archive.Date"] <- "Archive.Date"
colnames(df2015)[colnames(df2015)=="ï..Archive.Date"] <- "Archive.Date"
colnames(df2016)[colnames(df2016)=="ï..Archive.Date"] <- "Archive.Date"
colnames(df2017)[colnames(df2017)=="ï..Archive.Date"] <- "Archive.Date"
colnames(df2017)[colnames(df2017)=="Total"] <- "Count"
colnames(df2017)[colnames(df2017)=="Age.Profile"] <- "Age.Categorisation"
colnames(df2017)[colnames(df2017)=="Specality"] <- "Specialty"
colnames(df2018)[colnames(df2018)=="Total"] <- "Count"
colnames(df2018)[colnames(df2018)=="Time.Band"] <- "Time.Bands"
colnames(df2018)[colnames(df2018)=="Age.Profile"] <- "Age.Categorisation"
colnames(df2018)[colnames(df2018)=="Specality"] <- "Specialty"

#Merging the DataSets
Finaldata <-rbind(df2014,df2015,df2016,df2017,df2018)

#Structure of the data

str(Finaldata)

#identifying Missing Values
missing_values <- aggr(Finaldata, prop = FALSE, numbers = TRUE)

#Summarsing the missing values

summary(missing_values)

#removing unnecassary columns

Finaldata$Hospital.HIPE <- NULL

Finaldata$Specialty.HIPE <- NULL

#Removing the rows Which contains NAs if any

Finaldata<- na.omit(Finaldata)
nrow(Finaldata)

#To identify Na Values in a dataframe
is.na(Finaldata)

#Preprocessing the data

Finaldata<-Finaldata %>% separate(Archive.Date,into=c("Year","Month","Day"),
sep = "-",remove = F)


# changing the structure to integers
Finaldata$Year<-as.integer(Finaldata$Year)

Finaldata$Month<-as.integer(Finaldata$Month)

Finaldata$Day<-as.integer(Finaldata$Day)

Finaldata$Archive.Date<-as.Date(Finaldata$Archive.Date)
# Finaldata$Archive.Date<-date(Finaldata$Archive.Date)
Finaldata$Archive.Date

Finaldata$Adult.Child<-ifelse(Finaldata$Age.Categorisation=="16-64"
                              |Finaldata$Age.Categorisation=="65+","Adult","Child")
#structure of data
str(Finaldata)

#To Check the column names of data
colnames(Finaldata)

#Changing the structure of dataframe
# for doing any correlaton we need to have structure into integers/numeric
Finaldata$Group <- as.integer(Finaldata$Group)

Finaldata$Hospital <- as.integer(Finaldata$Hospital)

Finaldata$Specialty <- as.integer(Finaldata$Specialty)

Finaldata$Age.Categorisation <- as.integer(Finaldata$Age.Categorisation)

Finaldata$Time.Bands <- as.integer(Finaldata$Time.Bands)

Finaldata$Adult.Child <- as.factor(Finaldata$Adult.Child)

Finaldata$Adult.Child <- as.integer(Finaldata$Adult.Child)

Finaldata$Archive.Date <- as.factor(Finaldata$Archive.Date)

Finaldata$Archive.Date <- as.integer(Finaldata$Archive.Date)

str(Finaldata)

# correlation

opar <- par(no.readonly = TRUE)

install.packages("corrplot")
library(corrplot)

corrplot(corr = cor(Finaldata),tl.col = "Black",tl.cex = 0.90)

Finaldata[Finaldata == ""] <- NA

colSums(is.na(Finaldata))
Finaldata <- na.omit(Finaldata)

# principal component analysis on the dataframe

pca <- prcomp(Finaldata, center = TRUE, scale. = TRUE)

pca

summary(pca)

pca$rotation[1:nrow(pca$rotation), 1:4]


# we will look at the eigenvalues and its variances

install.packages("factoextra")
library("factoextra")

eig_values <- get_eigenvalue(pca)

eig_values

# now we will plot the principal component anlysis and variables

fviz_eig(pca, addabels = TRUE, ylim = c(0,50))

pca_for_variables <- get_pca_var(pca)

pca_for_variables

library("corrplot")

corrplot(pca_for_variables$cor, is.corr = FALSE)

fviz_pca_var(principal,col.var = "black")

head(pca_for_variables$cos2,10)

fviz_cos2(pca, choice = "var", axes = 1:2)

fviz_pca_var(pca,col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"),
             repel = TRUE)

head(pca_for_variables$contrib,20)

fviz_contrib(pca, choice = "var", axes =1, top =20)

fviz_contrib(pca, choice = "var", axes =2, top =20)

fviz_contrib(pca, choice = "var", axes =1:5, top =10)

# price variable graphical


fviz_pca_biplot(pca, 
                col.ind = Finaldata$Adult.Child, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "cost")

# power analysis on dataframe

install.packages("pwr")

library(pwr)

effective_size <- cohen.ES(test = "r", size = "large")

effective_size

power_analysis <- pwr.r.test(n = NULL, r = 0.5, sig.level = 0.05,
                             power = 0.95, alternative = "two.sided")

power_analysis
plot(power_analysis)

#Hypothesis Testing
test <- cor.test(Finaldata$Adult.Child, Finaldata$Specialty,
                 method = 'spearman', exact = FALSE) 

test


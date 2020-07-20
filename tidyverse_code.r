#Rscript
#Author: Yunlong Ma
#Date: 2020-07-20
#E-mail: glb-biotech@zju.edu.cn

#This code is used for learning the tidyverse package.

#1 note: it is important to set the mirror for downloading
#@ RStudio > tool > global option > packages > change to China (Beijing) [https] - TUNA Team, Tsinghua University

setwd("C:\\Users\\MYL\\Desktop\\")
setwd("C:/Users\\Administrator\\Desktop\\")

#2 install packages
#install "tidyverse" with its dependencies
if (!requireNamespace("ggplot2", quietly = TRUE))install.packages("ggplot2")
if (!requireNamespace("dbplyr", quietly = TRUE))install.packages("dbplyr")
if (!requireNamespace("broom", quietly = TRUE))install.packages("broom")
if (!requireNamespace("modelr", quietly = TRUE))install.packages("modelr")
if (!requireNamespace("tidyverse", quietly = TRUE))install.packages("tidyverse")


#3 load packages
library(tidyverse)
R 

tidyverse_conflicts()
tidyverse_deps()
tidyverse_logo()
tidyverse_packages()
#tidyverse_update() update the package of tidyverse

library(datasets)
if (!requireNamespace("gapminder", quietly = TRUE))install.packages("gapminder")
library(gapminder)
attach(iris)

#filter
data1 <- iris %>% filter(Species == "virginica") 

data2 <- iris %>% filter(Species == "virginica", Sepal.Length > 6)  


#arrange 
#default is ascending order
data3 <-iris %>% arrange(Sepal.Length)
data4 <-iris %>% arrange(desc(Sepal.Length)) # change to descending order


#mutate() update or add an extra column
data5 <- iris %>% mutate(Sepal.Length = Sepal.Length * 10) #change orignial column to a new value

data6 <- iris %>% mutate(SLMn = Sepal.Length * 10) #establish a new column



#combination of multiple functions
data7 <- iris %>%  filter(Species == "virginica")  %>% mutate(SLMm = Sepal.Length*10) %>% arrange(desc(SLMm))



#summarize: to summarize each column data
data8 <- iris %>%  summarize(medianSL = median(Sepal.Length))

data9 <- iris %>% filter(Species == "virginica") %>% summarize(medianSL=median(Sepal.Length))

data10 <- iris %>% filter(Species == "virginica") %>%  summarize(medianSL = median(Sepal.Length), maxSL = max(Sepal.Length))


#group_by(), to group data frame and analysis

data11<- iris %>% group_by(Species) %>% summarize(medianSL = median(Sepal.Length), maxSL = max(Sepal.Length))

data12 <- iris %>%filter(Sepal.Length>6) %>%  group_by(Species) %>%  summarize(medianPL = median(Petal.Length), maxPL = max(Petal.Length))


#use ggplot2 to plot scatter plot
iris_small <- iris %>%  filter(Sepal.Length > 5)

ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width)) + geom_point()

#add color for differen group
ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width, color = Species)) + geom_point()
ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width, color = Species)) + geom_point(color="red") 
ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width)) + geom_point(aes(color ="red")) 
ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width)) + geom_point(aes(color = Species)) 

#add different dot sizes
ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width,color = Species,size = Sepal.Length)) + geom_point()

ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width,color = Species,size = Sepal.Length)) + geom_point()


#plot in different panels (facet)
ggplot(iris_small, aes(x = Petal.Length,y = Petal.Width)) + geom_point() + facet_wrap(~Species)



#line graph
by_year <- gapminder %>% group_by(year) %>% summarize(medianGdpPerCap = median(gdpPercap))
ggplot(by_year, aes(x = year, y = medianGdpPerCap)) +geom_line() + expand_limits(y=0)

#change line type by using linetype
ggplot(by_year, aes(x = year, y = medianGdpPerCap)) +geom_line(linetype=0) + expand_limits(y=0)
ggplot(by_year, aes(x = year, y = medianGdpPerCap)) +geom_line(linetype=1) + expand_limits(y=0)
ggplot(by_year, aes(x = year, y = medianGdpPerCap)) +geom_line(linetype=2) + expand_limits(y=0)
ggplot(by_year, aes(x = year, y = medianGdpPerCap)) +geom_line(linetype=3) + expand_limits(y=0)
ggplot(by_year, aes(x = year, y = medianGdpPerCap)) +geom_line(linetype=4) + expand_limits(y=0)
ggplot(by_year, aes(x = year, y = medianGdpPerCap)) +geom_line(linetype=5) + expand_limits(y=0)



#bar chart or diagram
by_species <- iris %>% filter(Sepal.Length > 6) %>% group_by(Species) %>% summarize(medianPL=median(Petal.Length))

ggplot(by_species, aes(x = Species, y=medianPL)) + geom_col("red","yellow")


#histogram

ggplot(iris_small, aes(x = Petal.Length)) + geom_histogram(bins=45)
ggplot(iris_small, aes(x = Petal.Length)) + geom_histogram(bins=45) + geom_hline(yintercept = c(5,7.5,10)) + geom_vline(xintercept = c(2,4,6))


#boxplot
ggplot(iris_small, aes(x=Species, y=Sepal.Length)) +  geom_boxplot()
ggplot(iris_small, aes(x=Species, y=Sepal.Length)) +  geom_boxplot(aes(fill=Species))
ggplot(iris_small, aes(x=Species, y=Sepal.Length),aes(fill=Species)) + geom_boxplot() #aes() location is influenced the color fill.
ggplot(iris_small, aes(x=Species, y=Sepal.Length)) +  geom_boxplot(aes(fill="Species"))+scale_fill_manual(values=c("#56B4E9"))

#violin
ggplot(iris_small, aes(x=Species, y=Sepal.Length)) +  geom_violin()
ggplot(iris_small, aes(x=Species, y=Sepal.Length)) +  geom_violin(aes(fill=Species))
ggplot(iris_small, aes(x=Species, y=Sepal.Length)) +  geom_violin(aes(fill="Species"))+scale_fill_manual(values=c("#56B4E9"))









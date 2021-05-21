library(tidyverse)
library(dplyr)
library(dslabs)
library(ggthemes)
library(ggrepel)
data("murders")
head(murders)

data("heights")
head(heights)
prop.table(table(heights$sex))
prop.table(table(heights$height))
hist(heights$height,col = 'BLUE', border = 'Green', main = 'heights histogram')
a <- seq(min(heights$height), max(heights$height), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(heights$height <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

#GGplot Code
ggplot(data = murders)
p<-murders %>% ggplot(aes(population/(10^6),total,label = abb))
murder.rate<-(murders$total)/(murders$population)*10^6
rate<-murders%>% summarise(rate=sum(total)/sum(population)*10^6)%>%.$rate
p+geom_abline(intercept = log10(rate),lty=2,color="black")+ 
  geom_point(aes(col = region, size = murder.rate))+
  geom_text_repel(nudge_x =0.07)+
  scale_x_log10()+scale_y_log10()+
  ggtitle("US Gun Murders Scatterplot")+ xlab("popolation in millions(log scale")+
  ylab("total murders")+scale_color_discrete(name="Region")+theme_excel_new()

#other examples
q<-heights %>% filter(sex=="Male")
p<-q %>% ggplot(aes(sample=height))
p+geom_histogram(binwidth = 1, fill="blue",col="black")+
  xlab("Male heights in inches")+ ggtitle("Histogram")
p+geom_density(fill="green")
p+geom_qq()
params<-q%>% summarise(mean=mean(height),sd=sd(height))
p+geom_qq(dparams = params)+geom_abline()
x1<-p+geom_histogram(aes(x=height),binwidth = 1, fill="blue",col="black")+
  xlab("Male heights in inches")+ ggtitle("Histogram")
x2<-p+geom_histogram(aes(x=height),binwidth = 2, fill="blue",col="black")+
  xlab("Male heights in inches")+ ggtitle("Histogram")
x3<-p+geom_histogram(aes(x=height),binwidth = 3, fill="blue",col="black")+
  xlab("Male heights in inches")+ ggtitle("Histogram")
library(gridExtra)
grid.arrange(x1,x2,x3,ncol = 2)

#Dplyr (Summarise and Group_by)
S<- heights%>% filter(sex=="Male")%>% summarize(average = mean(height),stdDev=sd(height))
S
class(S)
S$average
S$stdDev
S<- heights%>% filter(sex=="Male")%>% 
  summarize(average = mean(height),stdDev=sd(height),
           median = median(height),Max = max(height),min = min(height))

data("murders")
murders<-murders%>%mutate(murder.rate=total/population*100000)
summarise(murders, mean(murder.rate))
us.murder.rate<-murders%>%summarise(rate= sum(total)/sum(population)*100000)
rate
us.murder.rate

#group_by function
murders%>% group_by(region)%>% summarize(median.rate =median(murder.rate))

#sorting data tables
murders %>% arrange(population) %>% head()
murders %>% arrange(desc(murder.rate)) %>% head()
murders %>% arrange(region,murder.rate) %>% head()

#Gapminder Data
data(gapminder)
head(gapminder)
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country,infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("South Korea", "Poland")) %>%
  select(country,infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Russia", "Malaysia")) %>%
  select(country,infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("South Africa", "Thailand")) %>%
  select(country,infant_mortality)
filter(gapminder, year== 1962)  %>% ggplot(aes(fertility, life_expectancy))+ 
  geom_point(aes(col = continent, size = population))
filter(gapminder, year%in% c(1962,2012))  %>% ggplot(aes(fertility, life_expectancy))+ 
  geom_point(aes(col = continent, size = population))+facet_grid(continent~year)
filter(gapminder, year%in% c(1962,1972,1982,1992,2002,2012))  %>% ggplot(aes(fertility, life_expectancy))+ 
  geom_point(aes(col = continent, size = population))+facet_grid(.~year)
filter(gapminder, year%in% c(1962,1972,1982,1992,2002,2012),continent %in% c("Europe","Asia"))  %>% 
  ggplot(aes(fertility, life_expectancy,col = continent))+ 
  geom_point(aes(size = population))+
  facet_wrap(.~year)

#Time Series Plots
filter(gapminder,country %in%c("United States","India"))  %>% 
  ggplot(aes(year, fertility, group=country,col = country))+geom_point()+
  geom_line()
labels<-data.frame(country = c("United States","India"))
filter(gapminder,country %in%c("United States","India"))  %>% 
  ggplot(aes( year, fertility, group=country,col = country))+geom_point()+
  geom_line()
gapminder<-gapminder%>% mutate(dollars.per.day=gdp/population/365)
gapminder%>% .$dollars.per.day %>% head()
gapminder%>%filter(year==1970 & !is.na(gdp))%>%ggplot(aes(dollars.per.day))+
  geom_histogram(fill = "white",color = "black",binwidth = 1)
#log transformed table
gapminder%>%filter(year==1970 & !is.na(gdp))%>%ggplot(aes(log2(dollars.per.day)))+
  geom_histogram(fill = "white",color = "black",binwidth = 1)
#-------------------------------------#------------------------------------------
gapminder%>%filter(year==1970 & !is.na(gdp))%>%ggplot(aes(dollars.per.day))+
  geom_histogram(fill = "white",color = "black",binwidth = 1)+
  scale_x_continuous(trans = "log2")

#geom boxplot
gapminder%>%filter(year==1970 & !is.na(gdp))%>%ggplot(aes(region,dollars.per.day))+
geom_boxplot()+ theme(axis.text.x = element_text(angle = 90,hjust = 1))

#geom boxplot reordered
gapminder%>%filter(year==1970 & !is.na(gdp))%>%
  mutate(region=reorder(region,dollars.per.day, FUN = median)) %>%
  ggplot(aes(region,dollars.per.day,fill = continent,label = country))+
  geom_point()+geom_boxplot()+ theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  scale_y_continuous(trans = "log2")
#---------------------------------------------------------#--------------------------------------

#Data Visualization Principles
heights%>%ggplot(aes(sex,height))+geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.2)

b<-heights%>%filter(sex=="Male")%>%ggplot(aes(height))+geom_histogram(col="blue",fill="white",binwidth = 1)
g<-heights%>%filter(sex=="Female")%>%ggplot(aes(height))+geom_histogram(col="pink",fill="white",binwidth = 1)
heights%>%ggplot(aes(height))+geom_histogram(col="blue",fill="white",binwidth = 1)+
facet_grid(sex~.)  
 
#Slope charts
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat<-dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year))
dat%>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")

#----------------------------------------------------------#------------------------------------

#Vaccines
data("us_contagious_diseases")
disease<-"Measles"
dat<-us_contagious_diseases%>%
  filter(!state%in%c("Hawaii","Alaska") & disease=="Measles")%>%
  mutate(rate=count/population*1000)%>%
  mutate(state=reorder(state,rate))
dat
dat%>% filter(state=="California")%>% ggplot(aes(year,rate))+geom_line()+
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

library(RColorBrewer)
dat%>%ggplot(aes(year,state,fill=rate))+geom_tile(color="grey50")+
  scale_x_continuous(expand=c(0,0))+
  scale_fill_gradientn(colors = brewer.pal(9,"Reds"),trans="sqrt")+
  geom_vline(xintercept=1963,col="green")+theme_minimal()+
  theme(panel.grid = element_blank())+ggtitle(disease)

##======================================================##=======================================================##==================================
#Titanic Survival Assessment

library(titanic)
titanic<-titanic_train%>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)%>%
  mutate(Survived=factor(Survived),
         Pclass=factor(Pclass),
         Sex=factor(Sex))
head(titanic)
?titanic_train
titanic%>%ggplot()+geom_histogram(aes(titanic$Age))+facet_grid(Sex~.)
titanic%>% filter(titanic$Sex=="female")%>% nrow()
titanic%>% filter(titanic$Sex=="male")%>% nrow()
titanic%>%filter(Sex=="female"& Age==c(18:35))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic%>%filter(!is.na(Age))%>%ggplot(aes(sample = Age))+geom_qq(dparams = params)

head(titanic$Survived)
head(titanic$Sex)
titanic%>%filter(Sex=="male")%>%
  ggplot(aes(Survived))+geom_bar(position = position_dodge())
titanic%>%filter(Sex=="female")%>%
  ggplot(aes(Survived))+geom_bar(position = position_dodge())
titanic%>%
  ggplot(aes(Survived))+geom_bar()
z1<-titanic%>%filter(Survived==0)%>% ggplot(aes(x=Age,y=..count..,fill = Survived))+geom_density(alpha = 0.2)
z2<-titanic%>%filter(Survived==1)%>% ggplot(aes(x=Age,y=..count..,fill = Survived))+geom_density(alpha = 0.2,fill="blue")
titanic%>% 
  ggplot(aes(x=Age,y=..count..,fill = Survived))+
  geom_density(alpha = 0.2)+facet_wrap(Sex~Pclass)
titanic%>%ggplot(aes(x=Age,y=..count..,fill = Survived))+
  geom_density(alpha = 0.2)
titanic%>%filter(!Fare==0)%>%ggplot(aes(Survived,log2(Fare)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4)
titanic%>%
  ggplot(aes(Survived,fill = Pclass))+geom_bar()
titanic%>%
  ggplot(aes(Survived,fill = Pclass))+geom_bar(position = position_fill())
titanic%>%
  ggplot(aes(Pclass,fill=Survived))+geom_bar()
titanic%>%
  ggplot(aes(Pclass,fill=Survived))+geom_bar(position = position_fill())

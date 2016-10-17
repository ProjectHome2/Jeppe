#Hvis run from R 
#dir <- dirname(parent.frame(2)$ofile)
#setwd(dir)
#Hvis R studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("homeFull.RData")

head(home) 
range(home[,3])
unique(home[,3])
names(home)

#Load packages
library(dplyr)
library(ggplot2)
library(gridExtra)


#Vigtig: Se paa type af variable: Hvilke type, ex liggetid er difftime. Evt aendrer hvis unoedvendigt.
#Hvad indeholder de: Range, NA?, Hvordan tolkes?
str(home)
summary(home)
table(home$Type, useNA = "always")

#Information about the data
sapply(home, table, useNA = "always")

#Log Prices?
ggplot(home, aes(x=Price)) + geom_histogram(binwidth=1e5)
ggplot(home, aes(x=log(Price))) + geom_histogram(binwidth=0.05)



#Analysis - Number of NA? 
###########################################################################
#

newk = ggplot(home, aes(x=NewKitchen)) + geom_bar(stat = "count")

neww = ggplot(home, aes(x=NewWindows)) + geom_bar(stat = "count")

closeto = ggplot(home, aes(x=CloseToWater)) + geom_bar(stat = "count")

newb = ggplot(home, aes(x=NewBathroom)) + geom_bar(stat = "count")

numbert = ggplot(home, aes(x=as.factor(NumberOfToilets))) + geom_bar(stat = "count") + labs(x = "NumberOfToilets")

numberbed = ggplot(home, aes(x=as.factor(NumberOfBedrooms))) + geom_bar(stat = "count") + labs(x = "NumberOfBedrooms")

manyt = ggplot(home, aes(x=ManyToilets)) + geom_bar(stat = "count")

garage = ggplot(home, aes(x=Garage)) + geom_bar(stat = "count")

reno = ggplot(home, aes(x=Renovation)) + geom_bar(stat = "count")

#############


lev = ggplot(home, aes(x=as.factor(Levels))) + geom_bar(stat = "count") + labs(x = "Levels")

bal = ggplot(home, aes(x=Balcony)) + geom_bar(stat = "count")

large = ggplot(home, aes(x=LargeGround)) + geom_bar(stat = "count")

high = ggplot(home, aes(x=HighHouse)) + geom_bar(stat = "count")

quart = ggplot(home, aes(x=Quarter)) + geom_bar(stat = "count")


grid.arrange(newk, neww, closeto, newb, numbert, numberbed, manyt, garage, reno, 
             ncol = 3)

grid.arrange(newk, neww, closeto, newb, numbert, numberbed, manyt, garage, reno, lev, bal, large, high, quart, 
             ncol = 4)






#Analysis of prices ~ variables
###########################################################################
#Boxplotting
ggplot(home, aes(as.factor(YearOfSale), Price)) + geom_boxplot()

newk = ggplot(home, aes(x=NewKitchen, Price)) + geom_boxplot()

neww = ggplot(home, aes(x=NewWindows, Price)) + geom_boxplot()

closeto = ggplot(home, aes(x=CloseToWater, Price)) + geom_boxplot()

newb = ggplot(home, aes(x=NewBathroom, Price)) + geom_boxplot()

numbert = ggplot(home, aes(x=as.factor(NumberOfToilets), Price)) + geom_boxplot() + labs(x = "NumberOfToilets")

numberbed = ggplot(home, aes(x=as.factor(NumberOfBedrooms), Price)) + geom_boxplot() + labs(x = "NumberOfBedrooms")

manyt = ggplot(home, aes(x=ManyToilets, Price)) + geom_boxplot()

garage = ggplot(home, aes(x=Garage, Price)) + geom_boxplot()

reno = ggplot(home, aes(x=Renovation, Price)) + geom_boxplot()

#############

lev = ggplot(home, aes(x=as.factor(Levels), Price)) + geom_boxplot() + labs(x = "Levels")

bal = ggplot(home, aes(x=Balcony, Price)) + geom_boxplot()

large = ggplot(home, aes(x=LargeGround, Price)) + geom_boxplot()

high = ggplot(home, aes(x=HighHouse, Price)) + geom_boxplot()

quart = ggplot(home, aes(x=Quarter, Price)) + geom_boxplot()


# Get the rest
postal = ggplot(home, aes(x=PostalCode, Price)) + geom_boxplot()
quart = ggplot(home, aes(x=Quarter, Price)) + geom_boxplot()
cond = ggplot(home, aes(x=Condition, Price)) + geom_boxplot()
livingarea = ggplot(home, aes(x=as.factor(LivingArea), Price)) + geom_boxplot()
#Fortsaet saa faar graf med fordelingen af variable ift. pris for alle variable. 

ggplot(home, aes(x=Price, fill = as.factor(SalesPeriod)))  + geom_histogram(bins = 100) + geom_vline(xintercept = mean(home$Price))
ggplot(home, aes(x=SalesPeriod, y=Price)) + geom_point(position = "jitter")

ggplot(home, aes(x=Condition, y=Price)) + geom_point()
ggplot(home, aes(x=Condition, y=Price)) + geom_point(position = "jitter")

#Husk date med ogsaa
#Kont plot normal

grid.arrange(newk, neww, closeto, newb, numbert, numberbed, manyt, garage, reno, 
             ncol = 3)
grid.arrange(newk, neww, closeto, newb, numbert, numberbed, manyt, garage, reno, lev, bal, large, high, quart, 
             ncol = 4)


#Bemaerk kun faa af typen 2 fam. Evt slet disse fra datasaet, og aendre 3 linjer laengere nede. And if so, maybe delete EjdType col afterwards
table(home$EjdType)

home <- home %>% filter(EjdType == "Villa, 1 fam." | EjdType == "Villa, 2 fam.") %>% 
  group_by(Postnr) %>%  
  arrange(Salgsdato)
#Kunne have valgt at arrange by kontantpris, salgsaar, opfoerelsesaar?

#Ide grupperinger af postnumre
table(home$Postnr)
hist(home$Kontantpris)

#Tjek hvor mange missing values vi har,- God til at tjekke hvorvidt vi kan bruge variabel eller ej, og hvordan NA fortolkes
table(home$NytKokken, useNA = "always")


#Opbygning af dplyr m. pipe
df.qty <- data.raw %>%
  filter(PURPOSE_CD == "OFF") %>%
  group_by(UNIT_REFERENCE_NO) %>%
  summarise(
    ZONE_CD = first(ZONE_CD),
    TOTAL_ACC_QTY = sum(AWARDED_QUANTITY_NO),
    AVG_DAILY_ACC_QTY = TOTAL_ACC_QTY / length(unique(BID_OFFER_DATE_DT)),
    AVG_HOURLY_ACC_QTY = AVG_DAILY_ACC_QTY / 24
  ) %>%
  arrange(desc(TOTAL_ACC_QTY)) %>%
  head(n = 10)

#Diverse info
str(home)
levels(home$Boligtilstand) #Har ingen levels
summary(home)
library(psych)
describe(home)

#Evt udregn correlation med cor

#Hvis traet af at skirve home$etelleandet, brug
attach(mtcars)
#Nu kan du blot skrive etellerandet
#Husk til sidst at bruge
detach()



#Graphs
#Evt proev at goere grafer like a beauty queen med xmax ymax osv 
#Ande theme: Proev + theme_dark()
library(ggplot2)
qplot(Boligareal, Kontantpris, data=home)

#Boligtilstand ~ Kontantpris, foerste virker ikke da ikke kan se hvor mange punkter
qplot(Boligtilstand, Kontantpris, data=home)
ggplot(home, aes(x=Boligtilstand, y=Kontantpris)) + geom_point(position = "jitter")

#Postnr ~ Kontantpris
ggplot(home, aes(x=as.factor(Postnr), y=Kontantpris, colour = Boligtilstand)) + geom_point(position = "jitter")

#Salgsdato ~ Kontantpris
ggplot(home, aes(x=Salgsdato, y=Kontantpris, colour = Boligtilstand)) + geom_point()

#Postnr ~ Liggetid
ggplot(home, aes(x=as.factor(Postnr), y=as.integer(Liggetid), colour = Boligtilstand)) + geom_point()


#Smart med wrap
ggplot(home, aes(x=as.integer(Liggetid), y=Kontantpris, colour = Boligtilstand)) + geom_point()
ggplot(home, aes(x=as.integer(Liggetid), y=Kontantpris, colour = Boligtilstand)) + geom_point() + facet_wrap(~Postnr)

#Boxplot
ggplot(home, aes(Boligtilstand, Kontantpris)) + geom_boxplot() 

#Barplot
ggplot(home, aes(x=Price, fill = as.factor(PostalCode))) + xlim(c(0, 7*1e6)) + ylim(c(0,127))  + geom_histogram(bins = 100) + geom_vline(xintercept = mean(home$Price))

#Andet
qplot(Kontantpris, Boligareal, colour = Boligtilstand, shape = EjdType, data=home) #Shape virker kun ved diskrete, mens colour virker ved begge
qplot(Kontantpris, Boligareal, colour = Boligtilstand, shape = as.factor(AntalToiletter), data=home) #Kan bruge as.factor til at goere diskret, dog oftest bedre at kont faar col
ggplot(home, aes(x=Kontantpris, y=Boligareal)) + geom_point() #eller + geom_line()

#Histogram
ggplot(home, aes(x=Postnr)) + geom_histogram(binwidth=5)
ggplot(home, aes(x=Kontantpris)) + geom_histogram(binwidth=1e5)
ggplot(home, aes(x=log(Kontantpris))) + geom_histogram(binwidth=0.05)

pairs(home[,c(1,2,5)])


fit1=lm(Price~SalesPeriod,data=home)
summary(fit1)

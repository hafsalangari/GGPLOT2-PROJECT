mydata =read.csv('C:/Users/ok/Desktop/sta/mydata.csv')
View(mydata)
"FDT of Gender "
AbsFreq=table(mydata$Gender)
AbsFreq
prop.table(AbsFreq)    #Abs. Freq
RelFreq=round(prop.table(AbsFreq), 2)
RelFreq

CumFreq=cumsum(RelFreq)
CumFreq

FDT=cbind(AbsFreq, RelFreq, CumFreq)
FDT

#write a function that creates and FDT of a QL var
FDTQL=function(x){
  ABSFreq=table(x)
  RELFreq=round(prop.table(ABSFreq),2)
  CUMFreq=cumsum(RELFreq)
  FDTx=cbind(ABSFreq, RELFreq, CUMFreq)
  return(FDTx)
  }

FDTQL(mydata$Gender)
##Construction FDT of a Quant variable 
#Loops and conditional functions work in R
#1. Transform the variable into a categorical var based a definition/we specify them

#Lets use the variable tips

summary(mydata$tip)
head(mydata)
#define catgories: small whtn tip<3 meduim when tip is 3>= but less than 7, large otherwise

#selection + Loop
catTips=c()  #create an empty vector

for (k in 1:length(mydata$tip)) {
  if(mydata$tip[k]<3){
    catTips[k]="AsmallTip"
  } else if (mydata$tip[k] >=3 & mydata$tip[k]<7) {
    catTips[k]="BmeduimTip"
  } else {
    catTips[k]="Clargetip"
  }
}

head(catTips)
head(mydata$tip)
#apply the function for FDT of QL
FDTQL(catTips)


#create the FDT 
FDTQL(mydata$Sport)[,2]

fdtSport=FDTQL(mydata$Sport)[,2]
fdtSport

pie(fdtSport, 
    col = rainbow(2), 
    main = 'Sport Distribution')

barplot(fdtSport, 
        col=rainbow(2), 
        main = 'Sport distribution')

fdttip=FDTQL(catTips)[,2]
fdttip

barplot(fdttip, 
        col=rainbow(3), 
        main = 'Tip distribution')

#Descriptive methods
#Univar case 
#Graphs 
#Num vars (hist and density)
head(dfTips)

hist(mydata$tip, 
     col='blue', 
     main = 'Tips distibution')

plot(density(mydata$tip), 
     col='#0033FF', 
     main='Tips distribution')


plot(density(mydata$total_bill), 
     col='#0033FF', 
     main='Total Bill distribution')

y=read.csv("timeToOffice.csv")
names(y)


hist(y$T)
plot(density(y$T))



#ggplot 
install.packages("ggplot2")
library(ggplot2)
View(mydata)

str(mydata)       # structure
names(mydata)     # column names
head(mydata)      # first few rows



install.packages("tidyverse")


library(tidyverse)

# 2. Inspect your dataset
View(mydata)  # as suggested in Section 3.1.1

# -----------------------------------------------------------
# 3. Scatterplot (Section 3.2 - First Steps)
# -----------------------------------------------------------
ggplot(data = mydata) + 
  geom_point(mapping = aes(x = Age, y = Monthly_Spending_USD))

ggplot(data = mydata) + 
  geom_point(mapping = aes(x = Age, y = Monthly_Spending_USD, color = Gender))

# -----------------------------------------------------------
# 4. Aesthetic mappings (Section 3.3)
# -----------------------------------------------------------
ggplot(data = mydata) + 
  geom_point(mapping = aes(x = Age, y = Monthly_Spending_USD, size = Satisfaction))

ggplot(data = mydata) + 
  geom_point(mapping = aes(x = Age, y = Monthly_Spending_USD, shape = Sport))

# -----------------------------------------------------------
# 5. Facets (Section 3.5)
# -----------------------------------------------------------
ggplot(data = mydata) + 
  geom_point(mapping = aes(x = Age, y = Monthly_Spending_USD)) + 
  facet_wrap(~ Gender, nrow = 2)

ggplot(data = mydata) + 
  geom_point(mapping = aes(x = Age, y = Monthly_Spending_USD)) + 
  facet_grid(Gender ~ Sport)

# -----------------------------------------------------------
# 6. Geoms (Section 3.6)
# -----------------------------------------------------------
ggplot(data = mydata, mapping = aes(x = Age, y = Monthly_Spending_USD)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mydata, mapping = aes(x = Age, y = Monthly_Spending_USD)) + 
  geom_point(mapping = aes(color = Gender)) + 
  geom_smooth()

# -----------------------------------------------------------
# 7. Statistical transformations (Section 3.7)
# -----------------------------------------------------------
ggplot(data = mydata) + 
  geom_bar(mapping = aes(x = Sport))

# Proportion example (after_stat) from instructions
ggplot(data = mydata) + 
  geom_bar(mapping = aes(x = Sport, y = after_stat(prop), group = 1))

# -----------------------------------------------------------
# 8. Position adjustments (Section 3.8)
# -----------------------------------------------------------
ggplot(data = mydata) + 
  geom_bar(mapping = aes(x = Sport, fill = Gender))

ggplot(data = mydata) + 
  geom_bar(mapping = aes(x = Sport, fill = Gender), position = "dodge")

ggplot(data = mydata) + 
  geom_point(mapping = aes(x = Age, y = Monthly_Spending_USD), position = "jitter")

# -----------------------------------------------------------
# 9. Coordinate systems (Section 3.9)
# -----------------------------------------------------------
ggplot(data = mydata, mapping = aes(x = Gender, y = Monthly_Spending_USD)) + 
  geom_boxplot()

ggplot(data = mydata, mapping = aes(x = Gender, y = Monthly_Spending_USD)) + 
  geom_boxplot() +
  coord_flip()

# -----------------------------------------------------------
# 10. Layered grammar of graphics template (Section 3.10)
# -----------------------------------------------------------
ggplot(data = mydata) + 
  geom_bar(
    mapping = aes(x = Sport, fill = Gender),
    stat = "count", 
    position = "dodge"
  ) +
  coord_flip()



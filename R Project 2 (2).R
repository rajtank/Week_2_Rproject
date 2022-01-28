#1 plotting last name that includes "Plotting Basics : Lastname"
print("Plotting Basics: Raj Tank")

#2 installing some neccesary libraries
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

#3 loading BullTroutRML2 dataset
df=data("BullTroutRML2")
print(df)

#4 printing the first and last 3 records from BullTroutRML2 dataset
first_last_data=headtail(BullTroutRML2,n = 3)
print(first_last_data)

#5 removing all the records except Harrison Lake
filtered_data=filter(BullTroutRML2,lake=="Harrison")
print(filtered_data)

#6 displaying the first and last recors from filteres data of BullTroutRML5
records=headtail(filtered_data,n=5)
print(records)

#7 displaying the sturcture of filtered BullTroutRML2
str(filtered_data)

#8 displaying the summary of BullTroutRML2
summary(BullTroutRML2)

#9 ploting scatterplot accordig to the provided details
plot(age~fl,data=filtered_data,xlim=c(0,500),ylim=c(0,15),
     main="Plot 1: Harrison Lake Trout",ylab="Age(yrs)",
     xlab="Fork Length(mm)",pch=16)

#10 plotting histogram of age
hist(filtered_data$age,ylab="Frequency",xlab="Age(yrs)",
     main = "Plot 2:Harrison Fish Age Distribution",
     col="cadetblue",col.main="cadetblue")

#11 plotting overdense plot
age_species =filtered_data$age/mean(filtered_data$age)
ggplot(data=filtered_data, aes(y=age)) +geom_density()+
  geom_point(data=filtered_data, aes(y=age, x=fl,size=age_species),alpha =age_species,
  color="green") +lims(x = c(0,500),y=c(0,15))+
  labs(title = "Plot 3: Harrison Density Shaded by Era"
  ,x = "Fork Length(mm)",y = "Age(yrs)")

#12 creating object "tmp" stores first and last records of BullTroutRML2
tmp<-headtail(filtered_data,n = 3)
print(tmp)

#13 dislaying Era column in new tmp object
tmp$era

#14 creating pchs vector with argument values for + and x
pchs<-c(3,4)

#15  creating vector with two element "red" and "gray60"
cols<-c("red","gray60")

plot(BullTroutRML2)
boxplot(BullTroutRML2)

#16 converting tmp era values to numeric
num=as.numeric(tmp$era)
print(num)

#17 intializing the col vector with tmp era values
cols[tmp$era]

#18 creating plot between age and fork lengh
plot(age~fl,data=filtered_data,main="Plot 4: Symbol & color by Era",
     xlim=c(0,500),ylim=c(0,15),ylab="Age(yrs)",
     xlab="Fork Length(mm)",pch=pchs,col=cols)

#19 plotting a regression line ovea the plot 4
plot(age~fl,data=filtered_data,main="Plot 5: Regression Overlay",
     xlim=c(0,500),ylim=c(0,15),ylab="Age(yrs)",
     xlab="Fork Length(mm)",pch=pchs,col=cols)
regression_line=lm(age~fl,data=filtered_data)
abline(regression_line,lty=2,lwd=2)

#20 placing a legend on plot 5
plot(age~fl,data=filtered_data,main="Plot 6: Legend Overlay",
     xlim=c(0,500),ylim=c(0,15),ylab="Age(yrs)",
     xlab="Fork Length(mm)",pch=pchs,col=cols)
regression_line=lm(age~fl,data=filtered_data)
abline(regression_line,lty=2,lwd=2)
#placing legend at the top of the graph
legend("topleft", inset=c(0.05), legend=levels(filtered_data$era), pch=pchs,
       col=cols,bty="n",title="Era",cex = 0.70)

#21 commit your code in Github repo







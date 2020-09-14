# Import libraries
library(ggplot2)
library(tidyverse)

# Pull our data
mpg_data <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
suspension <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Perform multiple linear regression on mpg data to predict mpg
head(mpg_data) 
lm(mpg ~ AWD + `vehicle weight` + `ground clearance` ,data=mpg_data)
summary(lm(mpg ~ AWD + `vehicle weight` + `ground clearance` ,data=mpg_data))

# Only the ground clearance seemed to effect mpg. Lets try with other variables
lm(mpg ~ `spoiler angle` + `vehicle length` + AWD + `ground clearance` ,data=mpg_data)
summary(lm(mpg ~ `spoiler angle` + `vehicle length` + AWD + `ground clearance` ,data=mpg_data))

# Check all of them just to be sure
summary(lm(mpg ~ `spoiler angle` + `vehicle length` + AWD + `ground clearance` + `vehicle weight`, data=mpg_data))

# Summary statistics for suspension data
summarize_suspension <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),Standard_Deviation=sd(PSI),Num_Vehicles=n())

# Perform t-test on suspension table
t.test(suspension$PSI, mu=1500)


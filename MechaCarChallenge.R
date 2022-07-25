######Deliverable 1###########
#import lib
library(dplyr)

#open csv as df
mech_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#review data
head(mech_df)

#mult-value regression & save in variable
regress <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mech_df) 

#determine p value and r2 value
summary(regress)


######Deliverable 2###########
##2.a - Create the total_summary DF
#import & read csv
suspen_df <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
head(suspen_df)

#find mean, median, variance, and standard deviation of the PSI column.
total_summary <-summarize(suspen_df, 'Mean' = mean(PSI), 'Median' = median(PSI), 'Variance' = var(PSI), 'Stanard Deviation' = sd(PSI))
head(total_summary)

#2.b
lot_summary <- suspen_df %>% group_by(Manufacturing_Lot) %>% summarize('Mean' = mean(PSI), 'Median' = median(PSI), 'Variance' = var(PSI), 'Stanard Deviation' = sd(PSI))

######Deliverable 3###########
#del 3 - t test 
t.test(suspen_df$PSI, mu = 1500)

#lot 1
lot1 <- subset(suspen_df, Manufacturing_Lot == "Lot1")
t.test(lot1$PSI, mu = 1500)

#lot 2
lot2 <- subset(suspen_df, Manufacturing_Lot == "Lot2")
t.test(lot2$PSI, mu = 1500)

#lot 3
lot3 <- subset(suspen_df, Manufacturing_Lot == "Lot3")
t.test(lot3$PSI, mu = 1500)


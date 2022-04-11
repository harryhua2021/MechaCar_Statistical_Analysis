#Deliverable 1------------------------------------------------------------

# Linear Regression to Predict MPG

# Use the library() function to load the dplyr package.
library(dplyr)
# Import and read in the MechaCar_mpg.csv file as a dataframe. 
mechacar_mpg_df <- read.csv(file='/Users/bahua/Desktop/MechaCar_Statistical_Analysis/resources/MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
head(mechacar_mpg_df)
# Perform linear regression using the lm() function. 
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacar_mpg_df)
# Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = mechacar_mpg_df))

#Deliverable 2------------------------------------------------------------

# Supension Coil Lot Analysis

# read csv to dataframe
suspension_coil_df <- read.csv(file = './resources/Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
head(suspension_coil_df)
# create a summary dataframe
total_summary <- suspension_coil_df %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
# create summaries for each lot
lot_summary <- suspension_coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

#Deliverable 3 -----------------------------------------------------------

#perform t-test to determine if the PSI across 

# Peform t-test across all Lots
t.test(suspension_coil_df$PSI,mu = 1500)
# Peform t-test on Lot 1
t.test(subset(Suspension,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# Peform t-test on Lot 2
t.test(subset(Suspension,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# Peform t-test on Lot 3
t.test(subset(Suspension,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

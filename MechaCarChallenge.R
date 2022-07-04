# Linear Regression to Predict MPG
# Use Libarary Function to load the dplyr package
library(dplyr)
# Import and read in the MechaCar_mpg.csv file as a dataframe.
MechaCar_df <- read.csv(file='./Resources/MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
head(MechaCar_df)
# Perform linear regression using the lm() function
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = MechaCar_df)
# Use summary() to find the p-value and r-squared value
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = MechaCar_df))

# Create Visualizations for the Trip Analysis
# read csv to dataframe
suspension_df <- read.csv(file = './Resources/Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
head(suspension_df)
# create a summary dataframe
total_summary <- suspension_df %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
# create summaries for each lot
lot_summary <- suspension_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

#T-Tests on Suspension Coils
t.test(suspension_df$PSI, mu=mean(suspension_df$PSI))

t.test(subset(suspension_df,Manufacturing_Lot=="Lot1")$PSI, mu = mean(suspension_df$PSI))
t.test(subset(suspension_df,Manufacturing_Lot=="Lot2")$PSI, mu = mean(suspension_df$PSI))
t.test(subset(suspension_df,Manufacturing_Lot=="Lot3")$PSI, mu = mean(suspension_df$PSI))
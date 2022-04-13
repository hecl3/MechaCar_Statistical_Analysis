library(dplyr)
mechacar_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

mechacar_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar_table)
summary(mechacar_lm)

suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

total_summary <- suspension_table %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')

t.test(suspension_table$PSI,mu = 1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

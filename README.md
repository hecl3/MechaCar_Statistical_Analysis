# MechaCar_Statistical_Analysis

## Linear Regression to Predict MPG

![image](https://user-images.githubusercontent.com/94264746/163088519-a2142a17-d61b-4909-b6cb-c860a4a867d3.png)

**Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?**

It looks like vehicle weight and spoiler angle both provided a non-ramdom amount of variance to the dataset.

**Is the slope of the linear model considered to be zero? Why or why not?**

It is not considered to be zero as there is a difference between the dependent and independent variables.

**Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?**

It does look like this model would effectively predict the MPG of the prototypes, in that it accurately calculates the relationship between the dimensions of each car vs the construction of said cars.

## Summary Statistics on Suspension Coils

**Total Summary**

![image](https://user-images.githubusercontent.com/94264746/163089430-25b102dc-1674-45b1-9f35-04df6630b43e.png)

**Lot Summary**

![image](https://user-images.githubusercontent.com/94264746/163089791-0f9a8614-d967-4685-9b7a-3dd8e3c3763e.png)



**The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?**

Going off the data we have it does not look like all lots meet this design specification. Lots 1 and 2 both meet the requirements of 1500, but 3 does not. It's a small difference of a few points but it's worth looking into in order to get it up to standard.

## T-Tests on Suspension Coils

![image](https://user-images.githubusercontent.com/94264746/163090340-4375823f-3ca4-4c33-9e77-d95df3afb334.png)


## Study Design: MechaCar vs Competition

Write a short description of a statistical study that can quantify how the MechaCar performs against the competition. In your study design, think critically about what metrics would be of interest to a consumer: for a few examples, cost, city or highway fuel efficiency, horse power, maintenance cost, or safety rating.

**What metric or metrics are you going to test?**

For this test I would look at and compare fuel efficiency, maintenance required over time, and the overall safety rating to the general cost.

**What is the null hypothesis or alternative hypothesis?**

Here the null hypothesis would be that a low maintenaince, high fuel efficiency, and high safety car would be more expensive.

**What statistical test would you use to test the hypothesis? And why?**

I would use the 2 Sample T Test in order to compare the multiple samples needed to determine

**What data is needed to run the statistical test?**

For this we would need the price of each car model, the individual safety rating, the fuel efficiency, and the maintenance costs.

library(readxl)
neighborhood = read_excel('/Users/alecreynoso/Desktop/City of San Jose Project/COSJ Project CC demographic.xlsx')

#Created a filter dataset without the 3rd & keyes neighborhood to find avg of the 3 neighborhood demographics
filtered_data = neighborhood[neighborhood$Neighborhood != '3rd & Keyes', ]

#Created a filtered dataset with only 3rd & keyes neighborhood
third_keyes = neighborhood[neighborhood$Neighborhood == '3rd & Keyes', ]


### Bascom, Roosevelt, & Willow Glen analysis ###

## Average of total population
pop_avg = mean(filtered_data$Population)
print(pop_avg)

# Average of kids ages 5 and under
under_five_avg = mean(filtered_data$`Ages under 5`)
print(under_five_avg)

## Average of kids ages 5 to 19
five_up_avg = mean(filtered_data$`Ages 5-19`)
print(five_up_avg)

## Average salary 50k - 100k salary
fifty_hundrend_avg = mean(filtered_data$`50k-75k` + filtered_data$`75k-100k`)
print(fifty_hundrend_avg)

## Average salary 100k - 150k
'100_150_avg' = mean(filtered_data$`100k-150k`)
print(`100_150_avg`)

## Average salary 150k - 200k
'150_200_avg' = mean(filtered_data$`150k-200k`)
print(`150_200_avg`)

##Average salary 200k and up
'200_up_avg' = mean(filtered_data$`<200k`)
print(`200_up_avg`)

## Find ratio between in program and out of program students
in_program_avg = mean(filtered_data$`In Program`)
print(in_program_avg)


notin_program_avg = mean(filtered_data$`Not In Program`)
print(notin_program_avg)

ratio = in_program_avg / notin_program_avg
print(ratio)

ratio1 = notin_program_avg / in_program_avg
print(ratio1)

#Estimated demand for a recreation center in the 3rd & Keyes neighborhood
keyes_demand = ratio * third_keyes$`Ages 5-19`
print(keyes_demand)

## Calculating a 95% confidence interval

#Standard errors
se_in = sqrt(in_program_avg)
se_out = sqrt(notin_program_avg)

#Calculate the standard error of the ratio using the delta method
se_ratio = sqrt((se_in/in_program_avg)^2 + (se_out/notin_program_avg)^2)*ratio

#Desired confidence level
CI = 0.95

#Find critical value for desired confidence level
z = qnorm(1- (1 - CI) / 2)

#Calculate margin of error
moe = z * se_ratio

#Calculate confidence interval
lower_bound = ratio - moe
upper_bound = ratio + moe

#Print CI
cat("Confidence Interval", lower_bound, "-", upper_bound)

#Estimated Confidence interval demand for a recreation center in the 3rd & Keyes neighborhood
LB = lower_bound * third_keyes$`Ages 5-19`
UP = upper_bound * third_keyes$`Ages 5-19`
mid = ratio * third_keyes$`Ages 5-19`
print(cbind(LB, mid, UP))

### 3rd & Keyes analysis ###

## Average of total population
pop_avg_key = third_keyes$Population
print(pop_avg_key)

# Average of kids ages 5 and under
under_five_avg_key = third_keyes$`Ages under 5`
print(under_five_avg_key)

## Average of kids ages 5 to 19
five_up_avg_key = third_keyes$`Ages 5-19`
print(five_up_avg_key)

## Average salary 50k - 100k salary
fifty_hundrend_avg_key = third_keyes$`50k-75k` + third_keyes$`75k-100k`
print(fifty_hundrend_avg_key)

## Average salary 100k - 150k
'100_150_avg_key' = third_keyes$`100k-150k`
print(`100_150_avg_key`)

## Average salary 150k - 200k
'150_200_avg_key' = third_keyes$`150k-200k`
print(`150_200_avg_key`)

##Average salary 200k and up
'200_up_avg_key' = third_keyes$`<200k`
print(`200_up_avg_key`)









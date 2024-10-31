##### R Project 5: Central Limit Theorem
##### Name: Rafael Mejia
##### Version Number: 3



## (C1) Load workspace.



## (C2) Table of carrier
	# Code
 c2 <- table(carrier)


	# Copy and paste results here
#AirTran     Alaska   American      Delta      Envoy ExpressJet   Frontier   Hawaiian 
# 29         52         58         92         88        111         45         13 
# JetBlue    SkyWest  Southwest     United US Airways     Virgin 
# 39        101         59         51         52         13 


	# What carrier appears most in the population?
#ExpressJet

	# What carrier appears least in the population?


#Hawaiian and virgin




## (C3) Find estimates for the Mean, Variance, Standard Deviation Estimates for ontime using Built-In R Functions

	# Mean Code

 c3_1 <-mean(ontime)

	# Copy and paste mean results here
 #75.2538
	
	# Variance Code
	c3_2<-var(ontime)
	#c3_2
	
	# Copy and paste variance results here
#152.2388

	# Standard Deviation Code
c3_3<-sd(ontime)
#c3_3
	
	# Copy and paste standard deviation results here

	#12.33851
	



## (C4) Histogram of ontime
	## Remember to save your plot and also submit it to Gradescope.

	# Code:
  hist(ontime,
       right = FALSE,
       breaks = c(0,10,20,30,40,50,60,70,80,90,100),
       ylim = c(0,800)
       )



	# Describe your histogram:

#The histogram is left skewed.




# (C5) Shapiro-Wilk test on ontime

	# Code:
shapiro.test(ontime)

	# Copy and paste results here
#Shapiro-Wilk normality test

#data:  ontime
#W = 0.92955, p-value < 2.2e-16


	# Do you think that your population is normally distributed (based on the p-value)?

	# Reason: no because the p value is very low, which suggests it is not normal


	

# (C6) Copy and paste function, then run it. (After you run it, you'll see a new function added in the Environment window on the right side.)



sampling_jp <- function(dataset, seed_value, num_samples, sizejp){
  samples1 <- matrix(NA, nrow = num_samples, ncol = sizejp)
  set.seed(seed_value)
  for(i in 1:num_samples){
    samples1[i,] <- sample(dataset, size = sizejp, replace = TRUE)
  }
  rowmeans1 <- rowMeans(samples1)
  graph <- hist(rowmeans1, right = FALSE, xlab = "Sample Means",
                main = "Histogram",
                sub = paste("Size = ", sizejp))
  sw <- shapiro.test(rowmeans1)
  result <- list(SampleMeans = rowmeans1, Shapiro = sw, Histogram = graph)
  return(result)
}




# (Q7) Give a brief description of num_samples if the value for it is 24.

	# Number of Samples (more information than "number of samples" is needed):
# it will make a matrix of 24 samples







# (Q8) Describe what each individual part is doing in (C6) for code provided


	# sample() does: 
#takes a sample of the specified size from the elements of x using either with or without replacement.




	# dataset does:
# dataset is just the vector of each element

	# size = sizejp does (also include what sizejp means):
# sizejp is how many elements from dataset we want to choose

	# replace = TRUE does:
# replace = TRUE is saying we should do sampling with replacement





# (C9) Build sampling distribution of samples of ontime
	## Remember to save your histogram and also submit it to Gradescope.

	# Code:
run1 <-sampling_jp(ontime, 499, 144, 15)

#run1

	
	# Copy and paste Shapiro-Wilk test results here

#Shapiro

#Shapiro-Wilk normality test

#data:  rowmeans1
#W = 0.96988, p-value = 0.002909


	

# (Q10) From (C9): Does it appear normally distributed?  Why or why not?
# Normally Distributed (Yes or No):no

# Reason using histogram:

#the left skew indicates its not normal

# Reason using the Shapiro-Wilk Test:
# the p value being very low indicates that it is normally distributed




# (C11) Build sampling distribution of samples of ontime
	## Remember to save your histogram and also submit it to Gradescope.

	# Code:
run2 <-sampling_jp(ontime, 499, 144, 1206)


	
  # Copy and paste Shapiro-Wilk test results here

#Shapiro-Wilk normality test

#data:  rowmeans1
#W = 0.99673, p-value = 0.9883




# (Q12) From (C11): Does it appear normally distributed?  Why or why not?
	# Normally Distributed (Yes or No):yes

	# Reason using histogram:
#the bell indicates normal distribution


	# Reason using the Shapiro-Wilk Test:
#the high p value indicates normal distribution




# (Q13) CLT?
	# Do your results for (C9) contradict the CLT?:

	# Reason: no because the sample size was small so we had less normal distribution


	# Do your results for (C11) contradict the CLT?:

	# Reason: no because as we got a bigger sample size it had a more normal distribution






# (Q14) Expected Value of the Sample Mean using CLT formulas

	# Write down the explicit formula for E(Xbar): E(Xbar) = E(X) = mu


	# Code for first sample size:
  q14_1 <- mean(ontime)


	# Copy and paste results here

 #  75.2538


	# Code for second sample size:

  q14_2 <- mean(ontime)

	# Copy and paste results here

 
  # 75.2538
	
	
	
# (Q15) Standard Deviation of the Sample Mean using CLT formulas

	# Write down the explicit formula for SD(Xbar):SD(Xbar) = SD(X) = sqrt(V(X))


	# Code for first sample size:
  Q15_1 <- sd(ontime)

	# Copy and paste results here
 # 12.33851



	# Code for second sample size:
  Q15_2<- sd(ontime)


	# Copy and paste results here
  
	
  # 12.33851
	
	
# (C16) Average of the Sample Means from C9 and C11

	# Code for average of sample means from C9
  q16_1 <- mean(run1$SampleMeans)

	# Copy and paste results here

  # 75.16477

	# Code for average of sample means from C11

  q16_2 <- mean(run2$SampleMeans)
	# Copy and paste results here

	# 75.25138
	
	
	
	
# (C17) Standard Deviation of the Sample Means from C9 and C11

	# Code for standard deviation of sample means from C9
  q17_1 <- sd(run1$SampleMeans)
  

	# Copy and paste results here

#3.240439

	# Code for standard deviation of sample means from C11
  q17_2 <- sd(run2$SampleMeans)

	# Copy and paste results here
  #0.3404848
	
	


# (Q18) Does the CLT approximation appear to get better as the sample size increased? Why or why not?

	# Does it get better?
#yes

	# Reason:
# the variance gets a lot smaller while the mean barely changes.




# (Q19)
# What was the main sample statistic used?
#the mean


# When we found the sample statistic value, did we find it from the same population each time?

#yes

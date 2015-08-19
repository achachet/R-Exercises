# 08-02 T Confidence intervals: Gosset's sleep dataset
# 
# Use data(sleep), based on Gosset's Biometrika paper. A group of 10 patients 
# used medication 1 (group 1) and then medication 2 to get extra hours of sleep. 
# Is there a better medication than the other?
#
# Bonus 1: use 3 different functions to calculate the confidence interval
# Bonus 2: plot the graph showing the pairs of extra sleep for each patient
#

# METHOD 1: manual calculation
data(sleep)
med1<-sleep$extra[sleep$group==1]; med2<-sleep$extra[sleep$group==2]
sleep_diff<-med2-med1
n<-length(med1)
mean(sleep_diff)+c(-1,1)*qt(0.975,n-1)*sd(sleep_diff)/sqrt(n)

# METHOD 2: t.test on the two PAIRED groups
# Important comment: in this case, the same individuals are tested TWICE. This is why
# we can use a PAIRED test, which removes the variability between individuals. 
t.test(sleep$extra[sleep$group==2], sleep$extra[sleep$group==1], paired=TRUE)

# The same test without pairing would not be conclusive because 0 cannot be
# excluded from the confidence interval
t.test(sleep$extra[sleep$group==2], sleep$extra[sleep$group==1])

# Note the difference between those two tests: 
#     - With pairing, p-value is 0.2% (highly statistically significant)
#     - W/o  pairing, p-value is 7.9% (NOT statistically significant)
# Also, the df in each case are not the same: with pairing, 10-1, 
# without: 17,776


# METHOD 3
# It's more or less same as the above, but it uses the difference directly 
# in t.test.
t.test(sleep$extra[sleep$group==2]-sleep$extra[sleep$group==1])


# GRAPH
( ggplot(sleep, aes(x=group, y=extra, group=ID))
+ geom_line(size=1, aes(colour=ID))
+ geom_point(size=10, shape=21, fill="salmon", alpha=.5)
)


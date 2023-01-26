#######################################################
#
# 	Categorical Predictors
#
######################################################


####  We begin with an example trying to predict levels of 
####  post-traumatic stress disorder with both a qualitative
####  or categorical variable (whether or not a woman was 
####  sexually abused as a child) and also a quantitative 
####  variable cpa, which measures some degree of child 
####  physical abuse.


library(faraway)

data(sexab, package="faraway")
sexab
by(sexab, sexab$csa, summary)
windows()
par(mfrow=c(1,2))
plot(ptsd ~ csa, sexab)
plot(ptsd~ cpa, pch=as.character(csa), sexab)


t.test(ptsd ~ csa, sexab, var.equal=TRUE)


###  Dummy variables (indicator variables) can be used 
###  to turn categorical/qualitative factors on or off

d1 <- ifelse(sexab$csa == "Abused", 1, 0)
d2 <- ifelse(sexab$csa == "NotAbused", 1, 0)
lmod <- lm(ptsd ~ d1 + d2, sexab)
sumary(lmod)
#### Note the warning about singularities... invertibility
#### issue here... can you see why?  The columns are not 
#### linearly independent: examine the model matrix...

model.matrix(lmod)


#### so the model is "overdetermined": d1 = 1 iff d2 = 0, so 
#### one of these can be eliminated.  Let's eliminate d1.
lmod <- lm(ptsd ~ d2, sexab) 
sumary(lmod)
####  The intercept here is the mean of the abused group, 
####  the coef on d2 is -7.24522, and so if d2=1, we get
####  the mean ptsd of the non-abused group: 11.941 - 1*(-7.245)
####  about 4.7 (see the previous boxplots and two sample t-test
####  output) 




####  I'll just make the boxplots again, and indicate the means 
####  on them with red boxes.
windows()
boxplot(ptsd ~ csa, sexab, horizontal=F)
m <- as.numeric(tapply(sexab$ptsd, sexab$csa, mean))
points(m, pch=22, col="red", lwd=3)




####  Alternatively, you could eliminate the intercept term (the 
####  column of 1s in the model matrix):


lmod <- lm(ptsd ~ d1 + d2 - 1, sexab)
sumary(lmod)
####  This is nice because you get the group means straight away in 
####  the output.  However, you do not get the t-test for the 
####  difference in the means.  Also, the hypo tests there in the 
####  output (with t- and p-values) are testing to see if those 
####  coefs differ significantly from 0, which, duh... of course they 
####  should.  Also, this approach (dropping the intercept term)
####  doesn't work when you have more than one categorical variable,
####  and R^2 isn't computed correctly when the intercept is not included. 
####  So....  DON'T LEAVE OUT THE INTERCEPT TERM.  INSTEAD, 
####  LEAVE OUT ONE OF THE INDICATOR/DUMMY VARIABLES.




#### R will actually make dummy variables for you:
lmod <- lm(ptsd~csa, sexab)
sumary(lmod)
class(sexab$csa)  
### R sees csa as a factor variable automatically when a variable 
### takes non-numeric values.  If, however, you have a categorical 
### variable that takes numeric values, you can force R to recognize  
### it as a factor with factor() or as.factor().

### Also one of these two dummy variables is to be taken as a
### reference level.  R automatically takes the first variable in the 
### list as reference.  It might make more sense in our example
### to use "NotAbused" as reference (this is done on the dataset 
### itself, not when creating the linear model with lm()).
sexab$csa <- relevel(sexab$csa, ref="NotAbused")
sexab$csa
lmod <- lm(ptsd ~ csa, sexab)
sumary(lmod)







######## Factors and Quantitative Predictors...  together at last!
#   
#   This is not a problem at all!  There are several kinds of models
#   to consider:
#
#
#   1. Use same regression line for all factor levels- that is, forget 
#   about the factor/qualitative/categorical variable...  just git it 
#   on outta heah: y ~ x
#
#   2. Use the factor but no quantitative variable: y ~ d   
#
#   3. Make separate regression models for each factor level, but 
#   these lines have the same slope: y ~ x + d
#
#   4. Make separate regression models for each factor level, and 
#   allow them to have different slopes: y ~ x + d + d:x
#   or simply like this:   y ~ x*d
#
#### Begin with the fullest model:
lmod4 <- lm(ptsd ~ cpa*csa, sexab)
sumary(lmod4)

model.matrix(lmod4)

windows()
par(mfrow=c(1,3))
plot(ptsd ~ cpa, sexab, pch=as.numeric(csa), 
     main="Two lines, different slopes")
abline(3.96, 0.764)
abline(3.96+6.86, .764-.314, lty=2)
sumary(lmod4)

### The interaction term in the previous model is statistically
### insignificant, so let's drop it and build two regression models
### with same slope. 
lmod3 <- lm(ptsd ~ cpa+csa, sexab)
sumary(lmod3)

plot(ptsd~cpa, sexab, pch=as.numeric(csa), 
     main="Two lines, same slopes")
abline(3.98, 0.551)
abline(3.98+6.27, 0.551, lty=2)

### We can get a CI for the coef on the csaAbused variable
### which is the effect, or, amount added to the intercept coef
### to get the abused line:
confint(lmod3)[3,]

### We should check the usual regression assumptions (L.I.N.E.)
plot(fitted(lmod3), residuals(lmod3), pch=as.numeric(sexab$csa), 
     xlab="Fitted", ylab="Residuals", main="Residuals vs. fits for same
slope model")


### We began with a model that just considers the factor as a 
### predictor.  We can also lose the factor/categorical/qualitative 
### variable and just build a standard one-predictor, simple least 
### squares model:
lmod1 <- lm(ptsd ~ cpa, sexab)
sumary(lmod1)
### Comparing to the previous model, we see the csa indicator is indeed
### important in terms of R^2 and also that the size of the coef on cpa
### is reduced with the addition of the categorical factor.









#########################################################
##
##
##   Interpretation with Interaction Terms
##   NEW EXAMPLE: Natural gas consumption (1000 cu.ft.) as response vs. 
##   Average weekly temp in C and over two seasons (the first 
##   no insulation; in the second, insulation had been installed).


data(whiteside, package="MASS")

whiteside

windows()
require(ggplot2)
ggplot(aes(x=Temp, y=Gas), data=whiteside) + geom_point() + facet_grid(~
                                                                         Insul) + geom_smooth(method="lm")


###  So it seems the insulation makes a difference! The relationships 
###  appear linear, so let's model!

lmod <- lm(Gas ~ Temp*Insul, whiteside)
sumary(lmod)


### We could predit the gas consumption to fall at a rate of about 
### 393.2 cu.ft. per 1 deg C increase in temperature with no insulation, and 
### fall at a rate of about 393.2 - 115.3 = 277.9 cu.ft. per 1 deg C temp. 
### increase in temp in the presence of insulation.  

###  If you center the data point patterns at the mean of the predictor, it 
###  becomes easy to see the difference between the average consumptions at 
###  the average temperatures:

mean(whiteside$Temp)
head(whiteside)
whiteside$ctemp <- whiteside$Temp - mean(whiteside$Temp)
head(whiteside)
lmodc <- lm(Gas ~ ctemp*Insul, whiteside)
sumary(lmodc)
###  This difference in average consumption at the average temperature is 
###  4936.8 cu.ft. -1567.9 cu.ft. = 3368.9 cu.ft.  












####$$$$$$$     Factors (categorical predictors) with multiple levels


###  For a factor with f levels, we need to create f-1 dummy or indicator 
###  variables, d_2, d_3, ..., d_f, where  d_i = 1{factor is at level i}
###  Take level 1 to be reference.  

###   Example: 125 male fruitflies divided randomly into five groups of size 25.  
###   Response: longevity in days.  Group #1: solitary (isolated); Group #2: 
###   individually with a virgin female each day (low).  
###   Group #3: eight virgin females each day (high).
###   Group #4: 1 pregnant female (one).  Group #5: 8 pregnant females (many).

###   Thorax length was also measured as a predictor- this is known to affect 
###   longevity.  


data(fruitfly, package="faraway")
plot(longevity ~ thorax, fruitfly, pch=unclass(activity))
legend(0.63, 100, levels(fruitfly$activity), pch=1:5)

###   The multiple levels make it difficult to distinguish the groups, 
###   even with the symbols.  Let's make five individual scatterplots.


windows()
require(ggplot2)
ggplot(aes(x=thorax, y=longevity), data=fruitfly) + 
  geom_point() + facet_wrap( ~activity)


lmod <- lm(longevity ~ thorax*activity, fruitfly)
sumary(lmod)

###  Isolated is reference.  The reg line for this group 
###  is longevity = -50.24197 + 136.12676 * thorax length
###
###  For the "one" group, the reg. line is
###  longevity = (-50.24197+6.51716) + (136.12676-4.67713) * thorax length
###
###  etc. 


###  You can see the model matrix with
model.matrix(lmod)
plot(lmod)

### You should see the LINE assumptions seem fairly satisfied, with the rather
### obvious exception of the constant variance requirement (homoskedasticity).
### We let this alone for now...  
###  We want to know if the model can be simplified- that is, can we get rid 
###  the interaction term in the model and use a model with constant slope
###  across all factor levels?  We can't do this with the sumary(lmod) output 
###  because this gives four individual t-tests, and we just want one.  
###  So do ANOVA:


anova(lmod)
## The interaction between thorax length and activity is statistically 
## insignificant, which suggests we can use regression lines with the 
## same slopes.  This analysis is done sequentially, meaning for this 
## example that thorax is tested by itsef first for significance.  Then
## activity is tested for statistical significance once thorax is included 
## in the model... then the interaction term is tested for significance 
## given thorax and activity variables are already in the model, and on and on...
## (well, in general, you would keep going... that's all we have here in this 
## example)

## A word of caution: lines that look parallel or close to parallel may
## lead you to thinking you can get rid of the interaction term.  However, 
## it is important to think about units because if you strech such a graph
## in the left-right direction, they get more parallel....  if you compress, 
## they look LESS PARALLEL!!! 

windows()
x=-100
y=-100
plot(y~x, xlim=c(0,10), ylim=c(0,10)) 
abline(4, .05)
abline(6, -.05)

## Refitting without the interaction term...
lmodp <- lm(longevity ~ thorax + activity, fruitfly)
sumary(lmodp)

### Back at that anova(lmod) output... the thorax variable is tested first 
### here, and then the activity variable is tested for significance once 
### thorax has entered the model.

drop1(lmodp, test="F")
# This (above) tests each term relative to the full model, and shows each 
# term to be significant even after the other term is included (p-values are small)



## Model coefs:
sumary(lmodp)
##  You see the activityone and activitymany groups don't add much in the way 
##  response- their intercept values are only about 2, 3, or 4 away from the
##  reference level of -48.75.  The low sex group survives about 7 days 
##  less than reference, and the high sex group survives about 20 days less.  
##  But- we might consider making Bonferroni simultaneous inferences here... 
##  in doing so, the significance to these terms would disappear (indicating they are 
##  not statistically significantly difference from 0).  However, if you look
##  at the sheer magnitudes... 20 days less of life seems significant.  



##  Checking diagnostics:
windows()
par(mfrow=c(1,2))
plot(residuals(lmodp) ~fitted(lmodp), pch=unclass(fruitfly$activity), 
     xlab="Fitted", ylab="Residuals")
abline(h=0)


##  Looks like some non-constant variance
##  Try a log-transformation

lmodl <- lm(log(longevity) ~ thorax + activity, fruitfly)
plot(residuals(lmodl) ~ fitted(lmodl),pch=unclass(fruitfly$activity), 
     xlab="Fits", ylab="Residuals")
abline(h=0)

##########  YAY!  The log transform looks great... of course remember now
####   that your model is modeling the log of the response as a linear 
####   function, and so interpretation of the results can be trickier.  

sumary(lmodl)
exp(coef(lmodl)[3:6])
## Using a log transform for the response, this indicates the 
## model coefficients contribute really toward a multiplicative effect..
## the high sexual activity group has 65.75384% the lifetime of the 
## isolated fruitflies.  

##  We can verify that thorax is unrelated to the sexual activity
##  (as it should be since the flies were randomly assigned to the 
##  five groups)

lmodh <- lm(thorax ~ activity, fruitfly)
anova(lmodh) 


##  but see what occurs if we omit thorax from the model for longevity:

lmodu <- lm(log(longevity) ~ activity, fruitfly)
sumary(lmodu)


##  Now you can see that the magnitudes of the effects haven't changed much
##  but that the standard errors are much larger.  So thorax is included 
##  in the model in order to increase the precision of the estimates.  You
##  can think of thorax as a covariate in this problem (see ANCOVA)


























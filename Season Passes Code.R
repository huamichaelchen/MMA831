##################################
# Code for the Season Pass Choice Model - MMA 831
# Modified from Chapman & Feit (2015) 
# 
#################################################################


### season pass data

# alternative code to load the data from website
pass.df <- read.csv("http://goo.gl/J8MH6A")
str(pass.df)
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))  # Normally, the base level tend to be No*
summary(pass.df)
table (pass.df$Pass,pass.df$Promo) # cross-tab


### Logistic regression with glm()

# initial logistic regression model
pass.m1 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)

# how the coef translates to an odds ratio
# plogis() translate logis to probability 
# .5 is the base line for probability...???  ====> probability = 0.5 means indifference 
# 1 is the base line for odds....? -=======> Odds = 1 means indifference, if and only if it is a binomial choice model 

plogis(0.3888)                          # outcome % - inverse logit function
plogis(0.3888) / (1-plogis(0.3888))     # odds = ratio of outcome % to alternative %
exp(0.3888)                             # identical  ==> odds of purchase 

#####
# translate logit to probability (0.5) or odds (1)
#####

# odds ratio for sales
exp(coef(pass.m1))
# confidence intervals
exp(confint(pass.m1))


### at first it looks like the promotion is working
### but is this really the right model? check Channel
table(pass.df$Pass, pass.df$Channel)


# visualization
install.packages("vcd")
library(vcd)    # install if needed

doubledecker(table(pass.df))


# Model 2: add the effect of channel
pass.m2 <- glm(Pass ~ Promo + Channel, data=pass.df, family=binomial)
summary(pass.m2)

# updated coefs and odds ratios
exp(coef(pass.m2))
exp(confint(pass.m2))   # if you see lower bound is < 1 and upper bound is > 1, this means it's undeterministic 

# Model 3: add the interaction of promotion and channel
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel,  # try interactions between Promo:Channel
               data=pass.df, family=binomial)
summary(pass.m3)

# updated coefs and odds ratios
exp(confint(pass.m3))

#### stil compare with the base line odds, which is 1 for binomial decision model, and the following is a combination effect
#PromoBundle:ChannelMail  0.02795867   0.09102369
#PromoBundle:ChannelPark  0.03135437   0.11360965
####

pass.df$Channel <- factor(pass.df$Channel, levels=c("Mail", "Email", "Park"))

##########################################################
####
#### 
#### extras on visualization for logistic coefficients

# plot the coefs
#install.packages("coefplot")
library(coefplot)
coefplot(pass.m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         title="Coefficients for Season Pass by Factor", ylab="Factor")

#### plot the odds ratio confidence intervals
####
pass.ci <- data.frame(confint(pass.m2))     # coef confidence intervals
pass.ci$X50 <- coef(pass.m2)                # add the midpoint estimate

# plot odds
# install.packages("ggplot2")
library(ggplot2)
pass.ci$Factor <- rownames(pass.ci)           # for ggplot2 to use in its model
pass.ci

# ggplot of odds ratios
# first: a plot by factor (x=) of the midpoint (y), high (ymax) and low (ymin)
p <- ggplot(pass.ci[-1, ], 
            aes(x=Factor, y=exp(X50), ymax=exp(X97.5..), ymin=exp(X2.5..)))

# ... displaying those elements as points & error bars
p <- p + geom_point(size=4) + geom_errorbar(width=0.25)

# ... adding a vertical line at an odds ratio of 1.0 (no change)
p <- p + geom_hline(yintercept=1, linetype="dotted", size=1.5, color="red")

# now plot it with titles
p + ylab("Likehood by Factor (odds ratio, main effect)") +
  ggtitle(paste("95% CI: Card sign up odds by factor")) + coord_flip()


### exercise for reader ==>
### does this add anything to our interpretation? Intercept model
pass.m3 <- glm(Pass ~ Promo * Channel, data=pass.df, family=binomial)
summary(pass.m3)
##########################################################



#Introduction to Linear Models

###Questions###
#Which predictor variables have a significant statstical relationship with a response variable?
#How much does the response change when the predictor changes?
#How much of response does predictor explain?

#Linear models repesent a family of statistical tests. Corellations, ANOVAs, linear regressions, oh my...

#Let's download some crab data. 
#Let's try plotting carapace_length against body_depth to see if there is a linear relationship. 
#There seems to be. Let's try a linear model. 
crabs <- read_csv("https://raw.githubusercontent.com/sjspielman/datascience_for_biologists/master/spring2020/rmd_lessons/lm_files/crabs.csv")
dplyr::glimpse(crabs)
ggplot(crabs, mapping = aes(x = carapace_length, y = body_depth)) + geom_point()
model_fit <- lm(body_depth ~ carapace_length, data = crabs)
summary(model_fit)

Call:
  lm(formula = body_depth ~ carapace_length, data = crabs)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.96587 -0.47864  0.07071  0.49976  1.43543 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -1.15527    0.20517  -5.631 6.09e-08 ***
  carapace_length  0.47300    0.00624  75.803  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6266 on 198 degrees of freedom
Multiple R-squared:  0.9667,	Adjusted R-squared:  0.9665 
F-statistic:  5746 on 1 and 198 DF,  p-value: < 2.2e-16

#The call reiterates the linear model. 
#The residuals should be about normally distributed. 
#The coefficients 
#The intercepts is the y-intercept? Given a standard error?

#Let's give it another go. This time, let's change the point size, add a "lm" line to geom_smooth, and annotate with some text. 
ggplot(crabs, mapping = aes(x = carapace_length, y = body_depth)) + geom_point(size = 0.5) + geom_smooth (method = "lm", color = "navy", fill = "deeppink4") + annotate("text", x = 20, y = 30, label = "R^2 == 0.966") +theme_bw()
#Can barely see it but standard error is pink. More noticable towards 0. 
# in geom_smooth se=FALSE "turns off" standard error
#We need quantities for the residuals. The model fit just shows us summary stats. We need the table of residuals. 
#broom::tidy(model_fit) gives us a nice summary.
#broom::augment(model_fit) pulls up all the data that was spit out when fitting out model. 
broom::augment(model_fit)
# A tibble: 200 x 8
body_depth carapace_length .fitted   .resid .std.resid    .hat .sigma    .cooksd
<dbl>           <dbl>   <dbl>    <dbl>      <dbl>   <dbl>  <dbl>      <dbl>
  1        7              16.1    6.46  0.540      0.875   0.0304   0.627 0.0120    
2        7.4            18.1    7.41 -0.00596   -0.00963 0.0244   0.628 0.00000116
3        7.7            19      7.83 -0.132     -0.212   0.0220   0.628 0.000508  
4        8.2            20.1    8.35 -0.152     -0.245   0.0193   0.628 0.000590  
5        8.2            20.3    8.45 -0.247     -0.397   0.0188   0.628 0.00151   
6        9.8            23      9.72  0.0764     0.123   0.0132   0.628 0.000101  
7        9.8            23.8   10.1  -0.302     -0.485   0.0118   0.628 0.00141   
8       10.4            24.5   10.4  -0.0331    -0.0532  0.0107   0.628 0.0000153 
9        9.7            24.2   10.3  -0.591     -0.949   0.0112   0.627 0.00510   
10       10.3            25.2   10.8  -0.464     -0.744   0.00973  0.627 0.00272   
# … with 190 more rows
#The fitted is the predicted value. The residual is the difference between the predicted value and the true value. 
#We save these values to a new object. 
#Are the residuals normally distributed? We will use qqline. The closer to the line the closer they are to being normally distributed. 
#Lines with a clear curvature, multiple curves, or flat then curved would be problematic. 
#Something categorical might be important. 
ggplot(crabs, mapping = aes(x = sex, y = body_depth, color = sex)) + geom_jitter()
#If we are to do an ANOVA, we will use a categorical variable but it might has similar dist across categories. 
#An ANOVA assumes that one thing is the baseline condition. R does this alphabetically. 
#How does being male affect body_depth relative to the baseline. In this case, the baseline is female. 
model_fit <- lm(body_depth ~ sex, data = crabs)
summary(model_fit)

Call:
  lm(formula = body_depth ~ sex, data = crabs)

Residuals:
  Min     1Q Median     3Q    Max 
-7.624 -2.449  0.076  2.463  7.376 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  13.7240     0.3420  40.134   <2e-16 ***
  sexM          0.6130     0.4836   1.268    0.206    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.42 on 198 degrees of freedom
Multiple R-squared:  0.00805,	Adjusted R-squared:  0.00304 
F-statistic: 1.607 on 1 and 198 DF,  p-value: 0.2064

#p-value = 0.206, no significant affect of sex on body_depth
anova_model_fit <- aov(model_fit)
summary(anova_model_fit)
Df Sum Sq Mean Sq F value Pr(>F)
sex           1   18.8   18.79   1.607  0.206
Residuals   198 2315.3   11.69 

#Same p-value, similar residuals, df = n-1 so 1 because two sexes. 
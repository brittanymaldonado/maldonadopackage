#model assumes two predictors are independent..usually this is violated in actuality
model_fit <- lm(body_depth ~ color + carapace_length, data = crabs)
summary(model_fit)
Call:
  lm(formula = body_depth ~ color + carapace_length, data = crabs)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.31623 -0.22544  0.00332  0.27120  1.08043 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -0.996643   0.123044   -8.10 5.65e-14 ***
  colororange      1.044956   0.055373   18.87  < 2e-16 ***
  carapace_length  0.451781   0.003899  115.87  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3749 on 197 degrees of freedom
Multiple R-squared:  0.9881,	Adjusted R-squared:  0.988 
F-statistic:  8204 on 2 and 197 DF,  p-value: < 2.2e-16

#An orange crab, when you control for carapace length, will be 1.044956 mm longer than an average crab. 
#Carapace length, when controlled for color, average body depth increases by 0.451781 mm per 1 mm increase in carapace length.
#Check that the residuas are normally distributed:

augment_fit <- broom::augment(model_fit)
qqnorm(augment_fit$.resid)
qqline(augment_fit$.resid)

#Let's plot orange and blue on diff lines 
ggplot(crabs, aes(x = carapace_length, y = body_depth, color = color)) + geom_point() + labs (x = "Carapace length (mm)", y = "Body depth (mm)", color = "Crab color")+ scale_color_manual(values = c("blue","orange")) + geom_smooth(method = "lm") + annotate("text", x = 20, y = 30, label = "R^2 == 0.988", size = 5)

#Let's check carapace width for lienarity 
> ggplot(crabs, aes(x = carapace_width, y = body_depth)) + geom_point()
#Appears linear. Let's go for it. 
> model_fit <- lm(body_depth ~ carapace_width + carapace_length, data = crabs)
> summary(model_fit)

Call:
  lm(formula = body_depth ~ carapace_width + carapace_length, data = crabs)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.78651 -0.35206 -0.01679  0.35397  1.61916 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -0.65396    0.17540  -3.728 0.000252 ***
  carapace_width  -0.45995    0.04636  -9.922  < 2e-16 ***
  carapace_length  0.97907    0.05126  19.099  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.513 on 197 degrees of freedom
Multiple R-squared:  0.9778,	Adjusted R-squared:  0.9776 
F-statistic:  4336 on 2 and 197 DF,  p-value: < 2.2e-16

#Body_depth will decrease when carapace_width increases 1 mm assuming carapace_length does not change. 
#wider = shallower assuming no change in length
#Body_depth increases when carapace_length increases 1 mm issuming carapace_width does not change. 
#longer=deeper assuming no change in width
#Explains about 97% of variation in body_depth

augment_fit <- broom::augment(model_fit)
qqnorm(augment_fit$.resid)
qqline(augment_fit$.resid)

#Residuals look good
# Let's plot it 
> ggplot(crabs, aes(x = carapace_length, y = body_depth, color = carapace_width)) + geom_point(size = 2) + labs(x = "Carapace length (mm)", y = "Body depth (mm)", color = "Carapace width (mm)") + scale_color_distiller(palette = "Reds") + annotate("text", x = 25, y = 30, label = "R^2 == 0.977", size = 5) + theme(legend.position = "bottom")
# all of the above has been assuming independence. 

#Now we're going to assume there's interactions. 
#If you run things as interactive and get something different than running them as additive (independent), 
#it is likely they ARE interactive. 
#Looking for interactions is easy in R. 
#Let's try carapace_length (numerical) and color (categorical).
#Replace + with *
> model_fit <- lm(body_depth ~ carapace_length*color, data = crabs)
> summary(model_fit)

Call:
  lm(formula = body_depth ~ carapace_length * color, data = crabs)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.22825 -0.19428 -0.01018  0.22544  1.01383 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                 -0.667829   0.165192  -4.043 7.59e-05 ***
  carapace_length              0.440842   0.005358  82.282  < 2e-16 ***
  colororange                  0.327083   0.252010   1.298  0.19585    
carapace_length:colororange  0.022331   0.007655   2.917  0.00394 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.368 on 196 degrees of freedom
Multiple R-squared:  0.9886,	Adjusted R-squared:  0.9885 
F-statistic:  5681 on 3 and 196 DF,  p-value: < 2.2e-16

#When controlling for color, the average crab's body depth will icnrease by 0.44 mm per 1 mm increase in length. 
#Significant
#when controlling for carapace_length, orange crab's body depth 0.32 mm deeper than average crab.
#Not significant.. interesting 
#Interaction effect coefficient is carapace_length:colororange
#This suggests a significant diff in the trend in body depth between orange crab and blue crab 
#Color by itself is not a significant predictor
#Different results from the last model
#Let's take a look at the residuals

>augment_fit <- broom::augment(model_fit)
>qqnorm(augment_fit$.resid)
>qqline(augment_fit$.resid)

#They look about normal. 
#If we get a significant interaction intercept.. 
#we then ignore the results of the model fitting for independence. 
#This is NEW evidence that suggests DEPENDENCE. 

#Let's try this with two numerical variables, carapace_length and carapace_width. 
> model_fit <- lm(body_depth ~ carapace_length+carapace_width, data = crabs)
> summary(model_fit)

Call:
  lm(formula = body_depth ~ carapace_length + carapace_width, data = crabs)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.78651 -0.35206 -0.01679  0.35397  1.61916 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -0.65396    0.17540  -3.728 0.000252 ***
  carapace_length  0.97907    0.05126  19.099  < 2e-16 ***
  carapace_width  -0.45995    0.04636  -9.922  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.513 on 197 degrees of freedom
Multiple R-squared:  0.9778,	Adjusted R-squared:  0.9776 
F-statistic:  4336 on 2 and 197 DF,  p-value: < 2.2e-16

> model_fit <- lm(body_depth ~ carapace_length*carapace_width, data = crabs)
> summary(model_fit)

Call:
  lm(formula = body_depth ~ carapace_length * carapace_width, data = crabs)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.78303 -0.36237 -0.00037  0.36302  1.60703 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    -0.1104486  0.6225446  -0.177    0.859    
carapace_length                 0.9572709  0.0566013  16.913   <2e-16 ***
  carapace_width                 -0.4725088  0.0483878  -9.765   <2e-16 ***
  carapace_length:carapace_width  0.0005010  0.0005505   0.910    0.364    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5132 on 196 degrees of freedom
Multiple R-squared:  0.9779,	Adjusted R-squared:  0.9775 
F-statistic:  2889 on 3 and 196 DF,  p-value: < 2.2e-16

> augment_fit <- broom::augment(model_fit)
> qqnorm(augment_fit$.resid)
> qqline(augment_fit$.resid)

#The interactive values look pretty similar to the independent values. 
#The interactive intercept is not significant. The residuals look good. 
#This leads me to the conclusion that these two variables do NOT have an interactive effect. 
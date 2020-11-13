# I figured out how to change lines in plots with more than one line! 
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, group = sex)) + geom_line(aes(linetype=sex))+ facet_wrap(vars(genus)) +
  +     labs(title = "Observed genera through time",
             +          x = "Year of observation",
             +          y = "Number of individuals") +
  +     theme_bw() +
  +     theme(text=element_text(size = 16))

#FUNCTIONS AND REPEATABILITY
#How many times did you do something, it worked, and you can't remember how you did it?
#Informative variable names
#Consistent indenting and spacing
#Functions that make our lives easier
#A function should be intuitively named and do one thing (e.g. sum)

#Making a function
#function_name is the name, all the stuff between {} is the computation
#return() allows you to get something back and keep using it 


#local scope = things that are created in a function only exist there

#Enter a function without () to see the code behind it

#Set detaults for functions
calc_shrub_vol <- function(length = 1, width=1, height = 1){volume <- length*width*height 
+ return(volume)}
#So that if you do not input a value the default is used
#Can provide arguments in any order as long as they are named
calc_shrub_vol(9,8)
#The above will use a default value for height (1)

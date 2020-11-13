#16 Sept 2020

#Data Visualization (cont.)

#Avoiding colors 

#Facet Plot
ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) + geom_line() + facet_wrap(facet = vars(genus))
#Each panel is a facet and the whole thing is a "facet plot," advantage: see each line individually,
#disadvantage: axis is set by biggest genus, so sometimes it is bulky

#Do we have multiple trend lines per facet?
#let's add some lines. 2 per facet. Let's color based on sex. 

yearly_sex_counts <- surveys_complete %>% count(year, sex, genus)
yearly_sex_counts
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) + geom_line() + facet_wrap(facet = vars(genus))

#It worked! Looks like most of them are pretty similar except Dipodomys spp. 
#facet_grid gives us more control than facet_wrap, specify rows and columns

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) + geom_line() + facet_grid(rows = vars(sex), cols = vars(genus))

#Or try something with less emphasis on years

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) + geom_line() + facet_grid(rows = vars(genus))

#theme_bw makes white background with gray gridlines, easier on the eyes

#Relabelling axes and using different themes
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n)) + geom_line() + facet_wrap(vars(genus)) + labs(title = "Observed genera through time", x = "Year of observation", y = "Number of individuals") + theme_linedraw() + theme(text = element_text(size = 7))


#make it ugly
custom_theme <- theme(axis.text.x = element_text(angle = 120, size = 4, family = "Sans"), axis.text.y = element_text(angle = 30, size = 4, colour = "grey20"), text = element_text(angle = 180, size = 24, colour = "blue"))
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = "tomato")) + geom_line() + facet_wrap(vars(genus))+ labs(title = "Observed genera through time", x = "Year of observation", y = "Number of individuals") + custom_theme

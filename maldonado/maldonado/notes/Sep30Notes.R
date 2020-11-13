#lm function for linear models
#include it in geom_smooth to fit a line to data
ggplot(calc_f, mapping = aes(x = date, y = tmin)) + geom_point() + geom_smooth(method = "lm")

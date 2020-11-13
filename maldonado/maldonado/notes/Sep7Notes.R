07-09-2020

Working in tidyverse

> surveys <- read_csv("/cloud/project/data/portal_data_joined.csv")
  How to import a data file as a tibble instead of a data frame
> str(surveys)
  Getting the structure of the data file
> select(surveys, month, plot_id, genus)
  Select for certain columns in the tibble by listing the data frame first and then all columns you want.
> select(surveys, -month, -plot_id, -genus)
  Same thing but select for everything but the columns listed

Filter allows us to include/exclude data based on some criterion
  > filter(surveys, year == 1991)
Filters for just the year 1991
  > filter(surveys, year != 1991)
Filters for everything but in the year 1991

How to fitler and select at the same time?
  set a new variable that has filtered data
  Select from that new variable
    > surveys_1991 <- filter(surveys, year == 1991)
    > surveys_91_weight <- select(surveys_1991, weight, genus)
  This is a roundabout way to select for weight just from the year 1991. 
  Could also nest these functions. 
    > surveys_weight <- select(filter(surveys, year !=1991), genus, weight)
  Like so. 
Or, use pipe. Ctrl + Shft + M
  > surveys %>% 
    + filter(year ==1991) %>% 
    + select(genus,weight)
Avoid creating intermediate files. Lower memory impact. 
If we want to save the output, we will need to input a value. Up arrow to start of pipe, go to front, and enter value. 
How to assign a value at the getgo. 
> smol_mammals <- surveys %>% 
  + filter (weight >10) %>% 
  + select(year, sex, genus)


Mutating data
Two functions in one! Easy to make new data columns based on existing.
> surveys_kg <- surveys %>% 
  + mutate(weight_kg = weight/1000)
> select(surveys_kg, weight_kg, weight)
# A tibble: 34,786 x 2
weight_kg weight
<dbl>  <dbl>
  1    NA         NA
2    NA         NA
3    NA         NA
4    NA         NA
5    NA         NA
6    NA         NA
7    NA         NA
8    NA         NA
9     0.218    218
10    NA         NA
# … with 34,776 more rows
It worked!
  
We can also stack these things. 
> surveys_kg <- surveys %>% 
  + mutate(weight_kg=weight/1000,
           +        weight_not_kg = weight*2)
Use shift + Enter to go down to next line

Can also select against missing data. 


Challenge #2 
> surveys_hhalf <- surveys %>% 
  + filter(!is.na(hindfoot_length), hindfoot_length < 30) %>% 
  + mutate(hindfoot_half = hindfoot_length/2) %>% 
  + select(species_id, hindfoot_half) 

Group_by function
> surveys %>% 
  + group_by(sex) %>% 
  + summarize(mean_weight = mean(weight, na.rm = TRUE))
`summarise()` ungrouping output (override with `.groups` argument)
# A tibble: 3 x 2
sex   mean_weight
<chr>       <dbl>
  1 F            42.2
2 M            43.0
3 NA           64.7
By doing the below, you remove the NA column.
> surveys %>% 
  + filter(!is.na(sex)) %>%
  + group_by(sex) %>%
  + summarize(mean_weight = mean(weight, na.rm = TRUE))
`summarise()` ungrouping output (override with `.groups` argument)
# A tibble: 2 x 2
sex   mean_weight
<chr>       <dbl>
  1 F            42.2
2 M            43.0
Can also filter this way
> surveys %>% 
  + filter(!is.na(sex)) %>% 
  + filter(!is.na(weight)) %>% 
  + group_by(sex) %>% 
  + summarize(mean_weight = mean(weight))
`summarise()` ungrouping output (override with `.groups` argument)
# A tibble: 2 x 2
sex   mean_weight
<chr>       <dbl>
  1 F            42.2
2 M            43.0

Look at recording for when she generates table with sex, species_id, mean_weight

You can stack summaries as well with Shft + Enter

Tip: %>%  then View () to view end of pipe

Sorting data
use arrange(col) to arrange by a certain column such as arrange(min_weight). Will do ascending arrangement.
Use arrange(desc(col)) to arrange largest to smallest AKA descending arrangement.

count(col) gives a quick count of each type in a column
Can count two things at once, e.g.
> surveys %>% 
  + count(sex, species)
# A tibble: 81 x 3
sex   species         n
<chr> <chr>       <int>
  1 F     albigula      675
2 F     baileyi      1646
3 F     eremicus      579
4 F     flavus        757
5 F     fulvescens     57
6 F     fulviventer    17
7 F     hispidus       99
8 F     leucogaster   475
9 F     leucopus       16
10 F     maniculatus   382
# … with 71 more rows

Challenge #3

> surveys %>% 
  + filter(!is.na(hindfoot_length)) %>% 
  + group_by(species_id) %>% 
  + summarize(
    +     mean_hindfoot_length = mean(hindfoot_length),
    +     min_hindfoot_length = min(hindfoot_length),
    +     max_hindfoot_length = max(hindfoot_length),
    +     n = n()
    + )
`summarise()` ungrouping output (override with `.groups` argument)
# A tibble: 25 x 5
species_id mean_hindfoot_length min_hindfoot_length max_hindfoot_length     n
<chr>                     <dbl>               <dbl>               <dbl> <int>
  1 AH                         33                    31                  35     2
2 BA                         13                     6                  16    45
3 DM                         36.0                  16                  50  9972
4 DO                         35.6                  26                  64  2887
5 DS                         49.9                  39                  58  2132
6 NL                         32.3                  21                  70  1074
7 OL                         20.5                  12                  39   920
8 OT                         20.3                  13                  50  2139
9 OX                         19.1                  13                  21     8
10 PB                         26.1                   2                  47  2864
# … with 15 more rows
> surveys %>% 
  + filter(!is.na(weight)) %>% 
  + group_by(year) %>% 
  + filter(weight==max(weight)) %>% 
  + select(year, genus, species, weight) %>% 
  + arrange(year)
# A tibble: 27 x 4
# Groups:   year [26]
year genus     species     weight
<dbl> <chr>     <chr>        <dbl>
  1  1977 Dipodomys spectabilis    149
2  1978 Neotoma   albigula       232
3  1978 Neotoma   albigula       232
4  1979 Neotoma   albigula       274
5  1980 Neotoma   albigula       243
6  1981 Neotoma   albigula       264
7  1982 Neotoma   albigula       252
8  1983 Neotoma   albigula       256
9  1984 Neotoma   albigula       259
10  1985 Neotoma   albigula       225
# … with 17 more rows

n() better than count() here because we don't retain hindfoot_length and also stacking



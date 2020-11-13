Taking data in tables and putting them in different kinds of tables

Creating subset tables from original tables

Also called re-shaping (for a variable of interest)
  - spreading 
  - gathering 

Let's gather some data on weight by species
> surveys_gw <- surveys %>% 
  + filter(!is.na(weight)) %>% 
  + group_by(genus, plot_id) %>% 
  + summarize(mean_weight = mean(weight))
`summarise()` regrouping output by 'genus' (override with `.groups` argument)
> surveys_gw
# A tibble: 196 x 3
# Groups:   genus [10]
genus       plot_id mean_weight
<chr>         <dbl>       <dbl>
  1 Baiomys           1        7   
2 Baiomys           2        6   
3 Baiomys           3        8.61
4 Baiomys           5        7.75
5 Baiomys          18        9.5 
6 Baiomys          19        9.53
7 Baiomys          20        6   
8 Baiomys          21        6.67
9 Chaetodipus       1       22.2 
10 Chaetodipus       2       25.1 
# … with 186 more rows

New function called spread(x)
This will reshape our table. 
spread(key = x, value = y)

the gather(x) function does the reverse of spread but also reshapes data

wide tables:
> surveys_spread
# A tibble: 24 x 11
   plot_id Baiomys Chaetodipus Dipodomys Neotoma Onychomys Perognathus Peromyscus
     <dbl>   <dbl>       <dbl>     <dbl>   <dbl>     <dbl>       <dbl>      <dbl>
 1       1    7           22.2      60.2    156.      27.7        9.62       22.2
 2       2    6           25.1      55.7    169.      26.9        6.95       22.3
 3       3    8.61        24.6      52.0    158.      26.0        7.51       21.4
 4       4   NA           23.0      57.5    164.      28.1        7.82       22.6
 5       5    7.75        18.0      51.1    190.      27.0        8.66       21.2
 6       6   NA           24.9      58.6    180.      25.9        7.81       21.8
 7       7   NA           19.9      57.4    170.      23.6        7          22.2
 8       8   NA           20.5      59.4    134.      25.9        7.06       20.1
 9       9   NA           18.9      57.5    162.      27.5        7.37       21.7
10      10   NA           22.3      51.8    190       28.7       NA          21.1
# … with 14 more rows, and 3 more variables: Reithrodontomys <dbl>, Sigmodon <dbl>,
#   Spermophilus <dbl>

long tables:
> surveys_gather
# A tibble: 240 x 3
   plot_id genus   mean_weight
     <dbl> <chr>         <dbl>
 1       1 Baiomys        7   
 2       2 Baiomys        6   
 3       3 Baiomys        8.61
 4       4 Baiomys       NA   
 5       5 Baiomys        7.75
 6       6 Baiomys       NA   
 7       7 Baiomys       NA   
 8       8 Baiomys       NA   
 9       9 Baiomys       NA   
10      10 Baiomys       NA   
# … with 230 more rows

A : will exclude some data
> surveys_spread %>% 
+ gather (key = genus, value = mean_weight, Onychomys:Sigmodon)
# A tibble: 120 x 8
   plot_id Baiomys Chaetodipus Dipodomys Neotoma Spermophilus genus     mean_weight
     <dbl>   <dbl>       <dbl>     <dbl>   <dbl>        <dbl> <chr>           <dbl>
 1       1    7           22.2      60.2    156.           NA Onychomys        27.7
 2       2    6           25.1      55.7    169.           NA Onychomys        26.9
 3       3    8.61        24.6      52.0    158.           NA Onychomys        26.0
 4       4   NA           23.0      57.5    164.           NA Onychomys        28.1
 5       5    7.75        18.0      51.1    190.           NA Onychomys        27.0
 6       6   NA           24.9      58.6    180.           NA Onychomys        25.9
 7       7   NA           19.9      57.4    170.           NA Onychomys        23.6
 8       8   NA           20.5      59.4    134.           NA Onychomys        25.9
 9       9   NA           18.9      57.5    162.           NA Onychomys        27.5
10      10   NA           22.3      51.8    190            NA Onychomys        28.7
# … with 110 more rows

Challenge #1




Our data should be read only and we should have a separate output folder. 
Data should always be backed up externally, on a drive, or both. 

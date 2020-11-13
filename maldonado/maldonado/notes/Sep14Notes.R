Using ggplot
> ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) + geom_jitter(alpha = 0.3, aes(color = plot_id)) + geom_boxplot(alpha = 0.1)

Time series
Generally like years to be on x-axis
> ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) + geom_jitter(alpha = 0.3, aes(color = plot_id)) + geom_boxplot(alpha = 0.1)
> yearly_counts <- surveys_complete %>% 
  + count(year, genus)
> yearly_counts
# A tibble: 199 x 3
year genus               n
<dbl> <chr>           <int>
  1  1977 Chaetodipus         3
2  1977 Dipodomys         222
3  1977 Onychomys           3
4  1977 Perognathus        22
5  1977 Peromyscus          2
6  1977 Reithrodontomys     2
7  1978 Chaetodipus        23
8  1978 Dipodomys         629
9  1978 Neotoma            23
10  1978 Onychomys          80
# … with 189 more rows
> ggplot(data = yearly_counts, mapping = aes(x = year, y = n))
> ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) + geom_line()
> ggplot(data = yearly_counts, mapping = aes(x = year, y = n, group = genus)) + geom_line()
> ggplot(data = yearly_counts, mapping = aes(x = year, y = n, color = genus)) + geom_line()
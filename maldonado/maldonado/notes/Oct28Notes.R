pal <- colorFactor(
  palette = "YlGnBu",
  domain = mega_df$scientificName
)
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~gdp_md_est,
            title = "Est. GDP (2010)",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )
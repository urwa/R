setwd("YOUR_WORKING_DIRECTORY")
options(device = Cairo::CairoWin, stringsAsFactors=FALSE)
remove(list = ls(all.names = TRUE))

library("geojsonio")
library("leaflet")
library("RColorBrewer")
library("scales")
library("dplyr")
library("data.table")
library("ggplot2")
library("leaflet.extras")
library("plotly")
library("htmlwidgets")

ev_sales_data <- fread("EV Sales Data LL.csv")
cols <- c("YOYSalesIncrease201718", "EVMarketShareWithinState2017", "EVMarketShareWithinState2018", "YOYSharePercentIncrease2018vs2017")
ev_sales_data[ , (cols) := lapply(.SD, "*", 100), .SDcols = cols]
ev_sales_data_51 <- ev_sales_data[ev_sales_data$EVMarketShareWithinState2017!=0,]

top10 <- ev_sales_data_51[order(ev_sales_data_51$EVMarketShareWithinState2018,decreasing = T)[1:10]]
bottom10 <- ev_sales_data_51[order(ev_sales_data_51$EVMarketShareWithinState2018,decreasing = F)[1:10]]
# ev_sales_data$greenest <- 0
# ev_sales_data[top10,"greenest"] <- ev_sales_data[top10,"EVMarketShareWithinState2018"]
# ev_sales_data[bottom10,"greenest"] <- ev_sales_data[bottom10,"EVMarketShareWithinState2018"]

str(ev_sales_data)

states <-
  geojson_read(
    x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
    , what = "sp"
  )

states@data <- merge(states@data, ev_sales_data, by.x="name", by.y="State", sort=FALSE)
states <- states[!is.na(states$EVMarketShareWithinState2017),]


customCols <- c("#ccece6","#99d8c9","#66c2a4","#41ae76","#238b45","#005824")
pal <- colorBin(customCols, domain=states$EVMarketShareWithinState2018)

labels2017 <- sprintf(
  "<strong>%s</strong><br/>%s%%",
  states$name, states$EVMarketShareWithinState2017
) %>% lapply(htmltools::HTML)

labels2018 <- sprintf(
  "<strong>%s</strong><br/>%s%%",
  states$name, states$EVMarketShareWithinState2018
) %>% lapply(htmltools::HTML)

top10icons <- awesomeIcons(icon = "whatever",
                           iconColor = "black",
                           library = "ion",
                           markerColor = "gray")

bottom10icons <- awesomeIcons(icon = "whatever",
                              iconColor = "black",
                              library = "ion",
                              markerColor = "lightgray")


leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions( id = "mapbox.light", accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(EVMarketShareWithinState2017),
    weight = 2,
    opacity = 1,
    group = "2017",
    color = "#ffffff",
    dashArray = "1",
    fillOpacity = 1,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE),
    label = labels2017,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  addPolygons(
    fillColor = ~pal(EVMarketShareWithinState2018),
    weight = 2,
    opacity = 1,
    group = "2018",
    color = "#ffffff",
    dashArray = "1",
    fillOpacity = 1,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE),
    label = labels2018,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  addAwesomeMarkers(data=top10,lng = ~Long, lat = ~Lat, icon = top10icons, group = "top 10 greenest states") %>%
  addAwesomeMarkers(data=bottom10,lng = ~Long, lat = ~Lat, icon = bottom10icons, group = "bottom 10 greenest states") %>%
  addLayersControl( baseGroups = c("2017","2018"), overlayGroups = c("top 10 greenest states","bottom 10 greenest states"), options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(pal = pal, values = ~EVMarketShareWithinState2017, opacity = 1, title = "Percentage of Market Share", position = "bottomright") %>%
  setMapWidgetStyle(list(background= "white")) %>%
  hideGroup("top 10 greenest states") %>%
  hideGroup("bottom 10 greenest states")


str(ev_sales_data)

year_sales <- ev_sales_data[,c(1,2,3,4,9)]

names(year_sales) <- c("State", "2016", "2017", "2018","Abbr")
year_sales <- as.data.frame(year_sales)

str(year_sales)
year_sales <- melt(year_sales, id.vars = c("State","Abbr"), variable.name = "year", value.name = "sales")

gg <- ggplot(year_sales, aes(x=year, y=sales, alpha = 5, label=Abbr,text = paste("Sales:", sales))) + 
  geom_point(aes(size=sales, col=year), stroke = 1,position = position_jitter(seed = 1)) +
  geom_text(aes(label=Abbr),position = position_jitter(seed = 1)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill="white"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "#4B4B4B"),
        axis.line.y =element_blank(),
        axis.line.x = element_line(colour = "#4B4B4B"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(trans = 'log2') +
  scale_color_manual(values = c("2016" = "#66c2a5", "2017" = "#fc8d62", "2018" = "#8da0cb")) +
  scale_size_continuous(range = c(5, 50)) +
  labs(y = "Number of Sales")

gg <- ggplotly(gg, tooltip="text") %>% config(displayModeBar = F)
saveWidget(ggplotly(gg), file = "sales_states_year.html")

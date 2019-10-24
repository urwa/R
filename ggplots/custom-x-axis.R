setwd("YOUR_WORKING_DIRECTORY")
options(device = Cairo::CairoWin, stringsAsFactors=FALSE)
pacman::p_load("data.table","ggplot2", "dplyr","showtext")

font_add("Titillium Regular","C:/Users/Urwa/AppData/Local/Microsoft/Windows/Fonts/TitilliumWeb-Regular.ttf")
showtext_auto()

df <- fread("YOUR_DATA_FILE.csv")
names(df) <- c("Date", "val1","val2")

df$Date <- as.Date(df$Date,format = "%m/%d/%Y")
df <- melt(df, id = "Date")

startDate <- min(df$Date)
endDate <- max(df$Date)

min(df$value)
max(df$value)

p <- ggplot(df) +
  geom_line(aes(x=Date, y=value, color=variable) , alpha = 1, size = 0.5) +
  scale_x_date("Date",
               breaks = c(seq(from=as.Date("2014-12-12"),to=as.Date("2018-12-12"),by="1 year"),
                          as.Date(c(startDate, endDate),origin="1970-01-01")),
               labels= c("2014","2015","2016","2017","2018","12/12/2013","9/30/2019")) +
  scale_y_continuous(breaks = seq(9000, 13000, 500),
                     labels = c("","$9,500","$10,000","$10,500","$11,000","$11,500","$12,000","$12,500","$13,000"),
                     limits = c(9000,13000), expand = c(0,0)) +
  scale_color_manual(values = c("#22807C", "#A6A6A6"),
                     labels = c("DEBIX", "BBG Bar US Agg")) +
  theme_bw() +
  theme(text = element_text(family = "Titillium Regular", size = 40),
        legend.position = "top",
        panel.grid.major.y = element_line(colour = "#d4d4d4"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(colour = "#4B4B4B"),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill="white"),
        axis.title = element_blank(),
        axis.line.y =element_blank(),
        axis.line.x = element_line(colour = "#4B4B4B")) +
  labs(color="")

p
ggsave("YOUR_JPG_FILE.jpg", p, width = 4.5, height = 4.5, unit="in", dpi = 300)

library(ggplot2)
library(ggmap)



preprocess_df <- function(original_df) {
  original_df[original_df$Province.State == "Hong Kong", "Country.Region"] <- "Hong Kong"
  original_df[original_df$Province.State == "Macau", "Country.Region"] <- "Macau"
  
  original_df[original_df$Country.Region == "Diamond Princess", "Lat"] <- 35.4498
  original_df[original_df$Country.Region == "Diamond Princess", "Long"] <- 139.6649
  
  territories <- original_df[original_df$Country.Region %in% c("France", 
                                                                 "United Kingdom", 
                                                                 "Denmark", 
                                                                 "Netherlands") & 
                               original_df$Province.State != "", ]
  
  #Copy province value to their country to split them out from their "mother" country
  territories$Country.Region <- territories$Province.State
  
  #Take the remaining as is - merge them all later
  remaining_countries <- original_df[!(original_df$Country.Region %in% c("France", 
                                                                           "United Kingdom", 
                                                                           "Denmark", 
                                                                           "Netherlands") & 
                                         original_df$Province.State != "") & 
                                       original_df$Lat != 0 &
                                       original_df$Long != 0, ]
  
  processed_covid19 <- rbind(territories, remaining_countries)
  
  return(processed_covid19)
}

fetch_day <- function(confirmed, deaths, recovered, date_col) {
  
  confirmed_grp <- aggregate(list(confirmed = confirmed[, date_col]),
                             by=list(country=confirmed$Country.Region), 
                             FUN = sum)
  
  coord_grp <- aggregate(cbind(lat = confirmed[, "Lat"],
                               lon = confirmed[, "Long"]),
                         by=list(country=confirmed$Country.Region), 
                         FUN = sum)
  
  coord_grp[coord_grp$country == "Canada", "lat"] <- 56.1304
  coord_grp[coord_grp$country == "Canada", "lon"] <- -106.3468
  
  coord_grp[coord_grp$country == "Australia", "lat"] <- -25.2744
  coord_grp[coord_grp$country == "Australia", "lon"] <- 133.7751
  
  coord_grp[coord_grp$country == "China", "lat"] <- 35.8617
  coord_grp[coord_grp$country == "China", "lon"] <- 104.1954
  
  deaths_grp <- aggregate(list(deaths = deaths[, date_col]),
                          by=list(country=deaths$Country.Region), 
                          FUN = sum)
  recovered_grp <- aggregate(list(recovered = recovered[, date_col]),
                             by=list(country=recovered$Country.Region), 
                             FUN = sum)
  
  covid19_cntry_day <- merge(coord_grp, confirmed_grp, by="country")
  covid19_cntry_day <- merge(covid19_cntry_day, deaths_grp, by="country", ALL.x = TRUE)
  covid19_cntry_day <- merge(covid19_cntry_day, recovered_grp, by="country", ALL.x = TRUE)
  covid19_cntry_day[is.na(covid19_cntry_day$deaths), "deaths"] <- 0
  covid19_cntry_day[is.na(covid19_cntry_day$recovered), "recovered"] <- 0
  covid19_cntry_day$active <- covid19_cntry_day$confirmed - covid19_cntry_day$deaths - covid19_cntry_day$recovered
  
  return(covid19_cntry_day)
}

world_map <- map_data("world")

src_dir <- "C:/Users/surasak/Documents/MOOCs/AnalyticsEdge/COVID-19/csse_covid_19_data/csse_covid_19_time_series/"

confirmed_df <- read.csv(paste(src_dir, "time_series_covid19_confirmed_global.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
deaths_df <- read.csv(paste(src_dir, "time_series_covid19_deaths_global.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
recovered_df <- read.csv(paste(src_dir, "time_series_covid19_recovered_global.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)

processed_confirmed <- preprocess_df(confirmed_df)
processed_deaths <- preprocess_df(deaths_df)
processed_recovered <- preprocess_df(recovered_df)

cols <- colnames(processed_confirmed)[5:length(colnames(processed_confirmed))]

for (i in 1:(length(cols) - 1)) {
  previous <- cols[i]
  current <- cols[i + 1]
  
  print(current)
  
  day_previous <- fetch_day(processed_confirmed,
                           deaths = processed_deaths,
                           recovered = processed_recovered,
                           date_col = previous)
  
  day_current <- fetch_day(processed_confirmed,
                           deaths = processed_deaths,
                           recovered = processed_recovered,
                           date_col = current)
  
  threshold = 0.31
  
  active_growth <- (day_current$active - day_previous$active)/day_previous$active
  
  #adjust case where the divisor is zero (giving inf as output)
  active_growth[is.na(active_growth)] <- threshold
  active_growth[is.infinite(active_growth)] <- threshold
  
  print(summary(active_growth))
  active_growth[active_growth > threshold] <- threshold
  active_growth[active_growth < -threshold] <- -threshold
  
  day_current$growth <- active_growth
  
  log_base <- 2.0
  
  #these are the dot size to show on map
  #we play around the number a bit to get rid of -inf value
  #since log retuns negative value if input is less than 1
  #we shift the value a bit so that the size for value 1 is not 0
  sz_confirmed <- log(day_current$confirmed, log_base)
  sz_confirmed[sz_confirmed < 0] <- -1
  sz_confirmed <- sz_confirmed + 1
  
  day_current$sz_confirmed <- sz_confirmed
  
  sz_active <- log(day_current$active, log_base)
  sz_active[sz_active < 0] <- -1
  sz_active <- sz_active + 1
  
  day_current$sz_active <- sz_active
  
  day_current <- day_current[day_current$confirmed > 0,]

  ggplot() + 
    ggtitle(paste("Covid cases on date:", substr(current, 2, 15))) +
    geom_polygon(data = world_map, size = 0.3, color = "darkgrey", fill = "lightgrey", alpha = 0.2, aes(x = long, y = lat, group = group)) +
    geom_point(data = day_current,
               alpha = 0.7, 
               shape = 21, 
               size = day_current$sz_active,
               aes(x = lon, y = lat, color = growth, fill = growth)) +
    geom_point(data = day_current, 
               alpha = 0.8, 
               color= "darkgrey",
               shape = 1, 
               size = day_current$sz_confirmed,
               aes(x = lon, y = lat)) +
    scale_color_gradient2(midpoint = 0.0, 
                          limits=c(-threshold, threshold),
                          low = "green", 
                          mid = "yellow", 
                          high = "red", 
                          name = "Growth of Active Cases") +
    scale_fill_gradient2(midpoint = 0.0, 
                         limits=c(-threshold, threshold),
                         low = "green", 
                         mid = "yellow", 
                         high = "red", 
                         name = "Growth of Active Cases")
  ggsave(paste(src_dir,current,".png", sep = ""), width = 11, height = 6, units = "in")
}

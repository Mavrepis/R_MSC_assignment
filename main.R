# Created by: philip
# Created on: 19/11/20

library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(RColorBrewer)
library(png)
library(gifski)
library(gganimate)
library(wpp2019)

cases <- fread("https://tinyurl.com/tsqkf7y")
deaths <- fread("https://tinyurl.com/rlssflz")


cases <- cases[, c("Province/State", "Lat", "Long", "1/22/20") := NULL]
deaths <- deaths[, c("Province/State", "Lat", "Long", "1/22/20") := NULL]


cases <- pivot_longer(cases, cols = 2:ncol(cases), names_to = "Dates", values_to = "confirmed")
cases <- cases %>% rename("Country" = "Country/Region")

deaths <- pivot_longer(deaths, cols = 2:ncol(deaths), names_to = "Dates", values_to = "deaths")
deaths <- deaths %>% rename("Country" = "Country/Region")


cases <- data.table(cases)
deaths <- data.table(deaths)

cases <- cases[, lapply(.SD, sum), by = .(Country, Dates)]
deaths <- deaths[, lapply(.SD, sum), by = .(Country, Dates)]

cases[, 'Dates'] <- lubridate::mdy(cases$Dates)
deaths[, 'Dates'] <- lubridate::mdy(deaths$Dates)


joined <- cbind(cases, deaths = deaths$deaths)

data(pop)
setDT(pop)
pop2020 <- pop[,c("name","2020")]
pop2020[name=='United States of America',name:='US']

split_by_country <- split(joined, by = "Country")
country_max <- joined %>%
  slice_max(order_by = Dates, n = 1)

world <- apply(country_max[, 3:4], 2, sum)

joined <- joined[order(Country, Dates)]

split_by_country <- lapply(split_by_country, function(x) {
  # Create daily confirmed cases and deaths column
  x$confirmed.ind <- x$confirmed - shift(x$confirmed, fill = 0)
  x$deaths.ind <- x$deaths - shift(x$deaths, fill = 0)
  # Create cases and deaths growth rate column
  x$growth_rate <- (x$confirmed.ind - shift(x$confirmed.ind,fill=0))/shift(x$confirmed.ind,fill=0)*100
  x$death_growth_rate <- (x$deaths.ind - shift(x$deaths.ind,fill=0))/shift(x$deaths.ind,fill=0)*100
  country <- x$Country[1]

  # Create smoothed values for deaths and cases
  x$counts <- list(1:nrow(x))
  span <- 21/nrow(x)
  x$smoothed<- loess(x$confirmed.ind ~ x$counts, degree=1,span=span)$fitted
  x$smoothed_deaths<- loess(x$deaths.ind ~ x$counts, degree=1,span=span)$fitted

  # 7-day rolling mean for deaths and cases
  if(country %in% pop2020[,name]){
    # Get country's population (into a vector)
    pop <- unlist(pop2020[name==country,"2020"],use.names = FALSE)
    x$deaths_07da <- zoo::rollmean(x$deaths.ind,k=7,fill=NA)/pop
    x$cases_07da <- zoo::rollmean(x$confirmed.ind,k=7,fill=NA)/pop
  }
  else{
    x$deaths_07da <- NA
    x$cases_07da <- NA
  }
  return(x)
})

joined <- rbindlist(split_by_country)
joined <- joined[! Country %in% c("Kosovo", "Diamond Princess", "MS Zaandam")]

joined$continent <- countrycode(sourcevar = joined[, Country], origin = "country.name", destination = "continent")
responses <- fread("https://www.ecdc.europa.eu/sites/default/files/documents/response_graphs_data_2.csv")
responses <- responses[Response_measure=="StayHomeOrder",]
new_row <- data.table(Country="New Zealand",Response_measure="StayHomeOrder",
                      date_start=as.IDate("2020-03-26"),date_end=as.IDate("2020-05-14"))
responses <- rbind(responses,new_row)

plot_cases_per_country <- function(countries) {

  cols <- rev(brewer.pal(8, 'RdYlBu'))
  for(country in countries){
    # Keep corresponding records
    Countries <- joined[Country== country]
    Response <- responses[Country==country]

    # Create vector of lockdown dates
    dates <- do.call("c",Response[,.(date_start,date_end)])
    labels <- paste(c("Lockdown starts in ","Lockdown ends in "),country)

    plot <-
    ggplot(data = Countries[confirmed.ind>0], aes(x = Dates, y = confirmed.ind,color=smoothed_deaths)) +
      geom_point(size = 2, alpha = .7) +
      scale_colour_gradientn(colours = cols) +
      geom_line(size=1,aes(Dates,smoothed),color="red",inherit.aes = F)+
      labs(title = paste("Cases per day  in ", country," colored by daily deaths"),
           subtitle = paste(c("From","to"),c(Countries$Dates[1],Countries$Dates[nrow(Countries)]),collapse=" "),
           x = "Date", y = "Cases per day", colour="Daily deaths")

    # If there were lockdowns
    if(length(dates)>0){
        dates <- sort(dates,na.last = TRUE)

      # Add vertical lines at corresponding dates
        plot <- plot + annotate(geom = "vline",
               x = dates,
               xintercept = dates,
               linetype = rep(c("dashed", "solid"),length(dates)/2)) +

          # Annotate each line with corresponding label
        annotate(geom = "text",
           label = paste(labels,dates),
           x = dates,
           y = rep(max(Countries$smoothed)/2,length(dates)),
           angle = 90,
           vjust = 1)
      }
      print(plot)
  }

}

aggregate_pie_chart <- function() {
  ag_sums <- joined[, .(counts=sum(confirmed)), by = continent]

  # Compute percentages
  ag_sums$fraction <- ag_sums$counts / sum(ag_sums$counts)

  # Compute the cumulative percentages (top of each rectangle)
  ag_sums$ymax <- cumsum(ag_sums$fraction)

  # Compute the bottom of each rectangle
  ag_sums$ymin <- c(0, head(ag_sums$ymax, n=-1))
  ag_sums$labelPosition <- (ag_sums$ymax + ag_sums$ymin) / 2
  ag_sums$label <- paste(c("Asia\n","Europe\n","Africa\n","Americas\n","Oceania\n"),round(ag_sums$fraction,digits=3))

  # Make the plot
  plot <- ggplot(ag_sums, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=continent)) +
    geom_rect() +
    geom_text( aes(label=ag_sums$label),x=2,y=ag_sums$labelPosition,size=6)+
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "none")
  print(plot)
}

boxplot <- function(){
  # Data: Mean confirmed cases per Month/Continent
  data<- joined[,.(mean=mean(confirmed)),by=.(month(Dates),continent)]
  # Months to seasons
  seasons <- as.factor(floor((data$month %% 12) /3 +1))
  levels(seasons)<- c("Winter","Spring","Summer","Autumn")
  plot<- ggplot(data=data, aes(x=continent,y=mean,color=continent))+
  geom_boxplot()+
    geom_jitter(width = 0.01,aes(colour=seasons))+
  labs(title = "Average cases per month grouped by continent, colored by season",
   subtitle = paste(c("From","to"),c(Countries$Dates[1],Countries$Dates[nrow(Countries)]),collapse=" "),
       y = "Average cases by month", colour="Continent")
  print(plot)
}

animate_continent_cases<- function(){
  continent_cases <- joined[,.(cumul_cases=sum(confirmed)),by=.(continent,Dates)]
  xists <- ggplot(continent_cases, aes(Dates,cumul_cases,colour = continent))+
   geom_freqpoly(stat='identity')+
   scale_x_date(date_breaks = '30 day',date_labels = "%b")+
   labs(title = "Cumulative cases per day colored by continent",
   subtitle = paste(c("From","to"),c(Countries$Dates[1],Countries$Dates[nrow(Countries)]),collapse=" "),
   x = "Date", y = "Cumulative cases")
  anim <- xists + transition_reveal(Dates)+view_follow(fixed_x = TRUE)
  animate(anim, nframes = 300, fps = 20, end_pause = 20,renderer=gifski_renderer("figures/animate_continent.gif"))
}

growth_rate <- function(countries,start_date=0,end_date=0,period=14){
  if(start_date==0 & end_date==0){
    end_date <- Sys.Date()
    format(end_date,format="%Y-%m-%d")
    start_date <- end_date-period
  }
  else{
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }

  Countries <- joined[Country %in% countries]

  Countries <- Countries[(Dates>start_date) &(Dates<end_date) & (!is.na(growth_rate)) & (!is.infinite(growth_rate))]


  plot <-
  ggplot(data = Countries, aes(x = Dates, y = growth_rate,color=Country)) +
    geom_line(size = 1.3, alpha = .7) +
    labs(title = paste("Cases per day  in ", paste(countries, collapse = ",")," colored by daily deaths"),
         subtitle = paste(c("From","to"),c(Countries$Dates[1],Countries$Dates[nrow(Countries)]),collapse=" "),
         x = "Date", y = "Cases per day", colour="Daily deaths")
  print(plot)
}

cases_rolling_mean_per_million <- function(countries){
  Countries <- joined[Country %in% countries]
  ggplot(Countries,aes(x=Dates,y=cases_07da,color=Country))+
    geom_line()+
    labs(title = paste("New confirmed cases of Covid-19 in", paste(countries, collapse = ","),
                       "seven-day rolling average (per million)"),
     subtitle = paste(c("From","to"),c(Countries$Dates[1],Countries$Dates[nrow(Countries)]),collapse=" "),
     x = "Date", y = "Cases 7-day rolling average (per million)", colour="Country")
}

deaths_rolling_mean_per_million <- function(countries){
  Countries <- joined[Country %in% countries]
  ggplot(Countries,aes(x=Dates,y=deaths_07da,color=Country))+
    geom_line()+
    labs(title = paste("New confirmed deaths of Covid-19 in", paste(countries, collapse = ","),
                       "seven-day rolling average (per million)"),
     subtitle = paste(c("From","to"),c(Countries$Dates[1],Countries$Dates[nrow(Countries)]),collapse=" "),
     x = "Date", y = "Deaths 7-day rolling average (per million)", colour="Country")
}

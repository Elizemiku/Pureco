require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)

# Download some Data, e.g. the CBOE VIX 
getSymbols("^VIX",src="yahoo")

# Make a dataframe
dat<-data.frame(date=index(VIX),VIX)

# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year is simple
dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
# the month too 
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)
# but turn months into ordered facors to control thedat appearance/ordering in the presentation
dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# the day of week is again easily found
dat$weekday = as.POSIXlt(dat$date)$wday
# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order
dat$weekdayf<-factor(dat$weekday,levels=rev(1:7),labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),ordered=TRUE)
# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
dat$yearmonth<-as.yearmon(dat$date)
dat$yearmonthf<-factor(dat$yearmonth)
# then find the "week of year" for each day
dat$week <- as.numeric(format(dat$date,"%W"))
# and now for each monthblock we normalize the week to start at 1 
dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))

# Now for the plot
ggplotly(ggplot(dat %>% filter(year %in% c(2007,2008,2009)), aes(monthweek, weekdayf, fill = VIX.Close)) + 
  geom_tile(colour = "white") + facet_wrap(year~monthf, as.table = TRUE) + scale_fill_gradient(low="red", high="yellow") +  
    theme(
    axis.text.x = element_text(angle = 20, size = 8),
    axis.line = element_line(colour = "black"),
    legend.text = element_text(size = 8),
    strip.background = element_rect(colour = "black", fill = "#99CCFF"),
    panel.background = element_rect(fill = "white", size = 2),
    panel.grid.major = element_line(colour = "gray",
                                    size = 1,
                                    linetype = "solid"),
    panel.grid.minor = element_line(colour = "gray",
                                    size = 1,
                                    linetype = "solid")) 
) 




#import data and functinos
source("toggle_analysis.R")
data<-clean_toggl_data("data/spring2016-spring2017.csv")

#define breaks
#please note that I have put the reading days and finals into winter and summer break
#I only want to look at habits and paterns during the normal semster in this ananlysis
spring_break_2016<-as.Date(c("2016-03-04","2016-03-14"))
summer_2016<-as.Date(c("2016-04-29","2016-09-24"))
fall_break_2016<-as.Date(c("2016-10-07","2016-10-12"))
thanksgiving_2016<-as.Date(c("2016-11-22","2016-11-28"))
winter_break_2016<-as.Date(c("2016-12-16","2017-01-18"))
spring_break_2017<-as.Date(c("2017-03-03","2017-03-13"))
all_breaks<-c(spring_break_2016,summer_2016, fall_break_2016, thanksgiving_2016, winter_break_2016, spring_break_2017)
academic_proj<-c("school", "animal tag")


#totals_worked(data, proj=academic_proj)
#totals_worked(data, proj=academic_proj,skips=all_breaks)
	
totals_worked(data, proj=academic_proj, view_by="month")
totals_worked(data, proj=academic_proj,skips=all_breaks, view_by="month")
#skips<-find_frimon_dates(data$Start.date)
#skips<-skips[2:(length(skips)-1)]
#skips_weakend<-find_weekend_dates(data$Start.date)
#skips_weakend<-skips_weakend[2:(length(skips_weakend)-1)]
#weekday<-time_worked(data, skips=sort(c(skips, spring_break)))
#weekday
#all<-time_worked(data, skips=spring_break)
#all
#weekend<-time_worked(data, skips=skips_weakend, start_date="2017-01-22")
#weekend
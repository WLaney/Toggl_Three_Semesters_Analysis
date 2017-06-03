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
#this excludes the 2days before the break, need to do this for weekly totals
thanksgiving_2016_week<-as.Date(c("2016-11-20","2016-11-28")) 
winter_break_2016<-as.Date(c("2016-12-02","2017-01-18"))
spring_break_2017<-as.Date(c("2017-03-03","2017-03-13"))
all_breaks_week<-c(spring_break_2016,summer_2016, fall_break_2016, thanksgiving_2016_week, winter_break_2016, spring_break_2017)
all_breaks<-c(spring_break_2016,summer_2016, fall_break_2016, thanksgiving_2016, winter_break_2016, spring_break_2017)
academic_proj<-c("school", "animal tag")

#totals worked all semsters by week
#totals_worked(data, proj=academic_proj)
#totals_worked(data, proj=academic_proj,skips=all_breaks_week)

#totals worked all semsters by week
#totals_worked(data, proj=academic_proj, view_by="month")
#totals_worked(data, proj=academic_proj,skips=all_breaks, view_by="month")

#totals worked spring 2016
#totals_worked(data, proj=academic_proj, end_date="2016-04-29", skips=spring_break_2016)

#totals worked fall 2016
#totals_worked(data, proj=academic_proj, start_date="2016-09-24", end_date="2016-12-02", 
#skips=c(fall_break_2016,thanksgiving_2016_week))
	
#totals worked spring 2017
#totals_worked(data, proj=academic_proj, start_date="2017-01-18", skips=spring_break_2017)

#look at weekly totals per class spring 2016
# lat_hist<-totals_worked(data, proj=academic_proj, end_date="2016-04-29", skips=spring_break_2016, plotting=F, desc="latin american hist.")
# dif<-totals_worked(data, proj=academic_proj, end_date="2016-04-29", skips=spring_break_2016, plotting=F, desc="differential equations")
# crit_think<-totals_worked(data, proj=academic_proj, end_date="2016-04-29", skips=spring_break_2016, plotting=F, desc="critical thinking")
# class_mech<-totals_worked(data, proj=academic_proj, end_date="2016-04-29", skips=spring_break_2016, plotting=F, desc="class mech")
# elec1<-totals_worked(data, proj=academic_proj, end_date="2016-04-29", skips=spring_break_2016, plotting=F, desc="electronics")
# spring16_class<-rbind(lat_hist,dif,crit_think,class_mech,elec1)
# rownames(spring16_class)<-c("lat hist","diffy q","crit think","class mech","electronics 1")
#
# #look at weekly totals per class fall 2016
# stat<-totals_worked(data, proj=academic_proj, start_date="2016-09-24", end_date="2016-12-02", skips=c(fall_break_2016,thanksgiving_2016_week), plotting=F, desc="stat")
# quantum1<-totals_worked(data, proj=academic_proj, start_date="2016-09-24", end_date="2016-12-02", skips=c(fall_break_2016,thanksgiving_2016_week), plotting=F, desc="quantum")
# elec2<-totals_worked(data, proj=academic_proj, start_date="2016-09-24", end_date="2016-12-02", skips=c(fall_break_2016,thanksgiving_2016_week), plotting=F, desc="electronics")
# budd<-totals_worked(data, proj=academic_proj, start_date="2016-09-24", end_date="2016-12-02", skips=c(fall_break_2016,thanksgiving_2016_week), plotting=F, desc="buddhism")
# fall16_classes<-rbind(stat,quantum1,elec2,budd)
# rownames(fall16_classes)<-c("stat","quantum 1","electronics 2","buddhism")
#
# #look at weekly totals per class spring 2017
# surr<-totals_worked(data, proj=academic_proj, start_date="2017-01-18", skips=spring_break_2017, plotting=F, desc="surrealism")
# data_an<-totals_worked(data, proj=academic_proj, start_date="2017-01-18", skips=spring_break_2017, plotting=F, desc="data analysis")
# quantum2<-totals_worked(data, proj=academic_proj, start_date="2017-01-18", skips=spring_break_2017, plotting=F, desc="quantum")
# em1<-totals_worked(data, proj=academic_proj, start_date="2017-01-18", skips=spring_break_2017, plotting=F, desc="E&M")
# spring17_classes<-rbind(surr,data_an,quantum2,em1)
# rownames(spring17_classes)<-c("surrealism", "data analysis", "quantum 2", "E&M 1")
#
# all_classes<-rbind(spring16_class, fall16_classes, spring17_classes)
# all_classes
#
# #electroncis 1 looks weired, view by day to see if there is any weird outlyer
# #elec1<-totals_worked(data, proj=academic_proj, end_date="2016-04-29", skips=spring_break_2016, desc="electronics", view_by="day")
#
# #plot the classes data
# par(mar=c(6.1,4.1,4.1,2.1))
# barplot(sort(all_classes[,1], decreasing=T), ylab="Time worked (Hr.)",las=2,
# main="Average Time Worked Per Week Vs. Class")
#
# #add grades to time matrix
# all_classes<-cbind(all_classes, c(3,3.3,3,4,3.7,3.7,4,4,3,NA,4,3.7,3.7))
# colnames(all_classes)[6]<-"Grade"
# all_classes<-cbind(all_classes, c(3,3,3,4,2,3,3,2,3,3,3,3,3))
# colnames(all_classes)[7]<-"Credit hr"
# all_classes<-cbind(all_classes, c(9,9.9,9,16,7.4,11.1,12,8,9,NA,12,11.0,11.0))
# colnames(all_classes)[8]<-"Quality pt"
#
# #look for coralation between ave time and grad realted stuff
# #time and grade
# cor.test(all_classes[,1],all_classes[,6], use="complete.obs")
# #time and credit hours
# cor.test(all_classes[,1],all_classes[,7], use="complete.obs")
# #time and quanlity pt
# cor.test(all_classes[,1],all_classes[,8], use="complete.obs")
#
# #look at work times
# #all data
# event_statstics(data, proj="school")
# #spring 2016
# event_statstics(data, proj="school",end_date="2016-04-29")
# #fall 2016
# event_statstics(data, proj="school",start_date="2016-09-24", end_date="2016-12-02")
# #spring 2017
# event_statstics(data, proj="school", start_date="2017-01-18")
#
#day of week working
#find weekend and week day dates
fri_mon<-find_frimon_dates(data$Start.date)
weekend<-find_weekend_dates(data$Start.date)

#find the weekends for each semester
spring_2016_weekends<-fri_mon[fri_mon<as.Date("2016-04-29")]
fall_2016_weekends<-fri_mon[as.Date("2016-09-24")<fri_mon & fri_mon<"2016-12-02"]
spring_2017_weekends<-fri_mon[fri_mon>as.Date("2017-01-18")]

#spring 2016
#time_worked(data, end_date="2016-04-29", proj=academic_proj, 
#	skips=sort(c(spring_break_2016, spring_2016_weekends)))

#fall 2016
#time_worked(data, start_date="2016-09-24", end_date="2016-12-02", proj=academic_proj,
#	skips=sort(c(fall_break_2016,thanksgiving_2016_week, fall_2016_weekends)))

#spring 2017	
#time_worked(data, start_date="2017-01-18", proj=academic_proj,
#	skips=sort(c(spring_break_2017, spring_2017_weekends)))

#all
time_worked(data, skips=sort(c(all_breaks,fri_mon)), proj=academic_proj)


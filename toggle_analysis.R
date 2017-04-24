data<-read.csv("Toggl_time_entries_2017-01-16_to_2017-04-23.csv", header=T)

# #get rid of unused collums
# data_redus<-data.frame(data$Project, data$Description, data$Start.date, data$Start.time, data$End.date, data$End.time, data$Duration)
# str(data_redus)

#convert durations into fractions of hours 
#EX 00:15:00 -> 0.25 and 01:30:00-> 1.5
data$Duration<-sapply(strsplit(as.character(data$Duration), ":"), function(x) {
  x <- as.numeric(x)
  x[1] + (x[2] / 60)+(x[3]/(60*60))
})
#conver dates to Date type
data$Start.date<-as.Date(data$Start.date)
data$End.date<-as.Date(data$End.date)
str(data)

totals_per_day<-function(data,
	start_date=data$Start.date[1], end_date=data$Start.date[length(data$Start.date)],
	skips=as.Date(x = integer(0), origin = "1970-01-01"),
	proj=data$Project, desc=data$Description){
	
		#this function takes a data set with the optional arguments of a start date,
		#end date, project, desrciption, and skips.
		#It outputs a histogram of the total time 
		#spent on the project and description combinations per day over the time span.
		#it defaluts to all projects and all descriptions over the time span of the data
		start_date<-as.Date(start_date)
		end_date<-as.Date(end_date)
		skips<-as.Date(skips)
		dates<-c(start_date, skips, end_date)
		#find the dates in ranges that we want
		date_range<-as.Date(x = integer(0), origin = "1970-01-01") #predefin variable
		for(i in 1:(length(dates)*.5)){
			#you need to add 1 for the length bc of how seq works
			date_range_par<-seq(dates[(2*i)-1], dates[2*i], length.out = dates[2*i]-dates[(2*i)-1]+1)
			date_range<-c(date_range, date_range_par)
		}
		
		#find the totla duration of each day for the project and description combo used
		day_duration<-rep(0, length(date_range))
		for(i in 1:length(date_range)){
			day_duration[i]<-sum(data$Duration[data$Start.date== date_range[i]
				& data$Project==proj & data$Description==desc])
		}
		print(summary(day_duration))
		print(sd(day_duration))
		plot(date_range, day_duration, type='h', main="total time worked vs day")
}


totals_per_week<-function(data, 
	start_date=data$Start.date[1], end_date=data$Start.date[length(data$Start.date)],
	skips=as.Date(x = integer(0), origin = "1970-01-01"), 
	proj=data$Project, desc=data$Description){
		#this function takes a data set with the optional arguments of a start date,
		#end date, project, desrciption, and skips.
		#It outputs a plot of the total time spent on the project and 
		#description combinations per week over the time span.
		#Weeks start on Monday so this will corrospond to who toggl does weeks
		#It defaluts to all projects and all descriptions over the time span of the data
		#skip gives the ablity to exclude days from the data.
		#They will not be counted in statistics or ploted
		#Skips should be a vector that contain the first and last
		#date included in the average. The dates between these will be exluded
		#ex: skiped=(start date, end date, start date, end date ...)
		start_date<-as.Date(start_date)
		end_date<-as.Date(end_date)
		skips<-as.Date(skips)
		dates<-c(start_date, skips, end_date)
		#find the dates in ranges that we want
		date_range<-as.Date(x = integer(0), origin = "1970-01-01") #predefin variable
		for(i in 1:(length(dates)*.5)){
			#you need to add 1 for the length bc of how seq works
			date_range_par<-seq(dates[(2*i)-1], dates[2*i], length.out = dates[2*i]-dates[(2*i)-1]+1)
			date_range<-c(date_range, date_range_par)
		}
		
		#convert to weeks
		weeks<-format(date_range, format = "%W")
		week_range<-unique(weeks) #find the diffrent weeks we want data for
		start_week<-format(data$Start.date, format = "%W")
		
		#find the totla duration of each day for the project and description combo used
		week_duration<-rep(0, length(week_range))
		for(i in 1:length(week_range)){
			week_duration[i]<-sum(data$Duration[start_week== week_range[i]
				& data$Project==proj & data$Description==desc])
		}
		print(summary(week_duration))
		print(sd(week_duration))
		
		#we need to get the week numbers back to dates so that the data plots in order
		week_dates<-date_range[match(week_range, weeks)]
		plot(week_dates, week_duration, type='b', main="total time worked vs week")
}
	
	
totals_per_day(data)
totals_per_day(data, skips=c("2017-03-05","2017-03-13"))
totals_per_week(data)
c("2017-03-05","2017-03-13")
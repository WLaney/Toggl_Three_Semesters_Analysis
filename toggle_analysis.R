clean_toggl_data<-function(file_path){
	#this function takes the files path as a string to a 
	#CSV exported from toggl and gets the data into a form
	#that can be analysied by R
	
	#import data
	toggl_data<-read.csv(file_path, header=T)
	
	#convert durations into fractions of hours 
	#EX 00:15:00 -> 0.25 and 01:30:00-> 1.5
	toggl_data$Duration<-sapply(strsplit(as.character(toggl_data$Duration), ":"),
		function(x) {
			x <- as.numeric(x)
			x[1] + (x[2] / 60)+(x[3]/(60*60))
		})

	#convert Start.date and End.date to Date type
	toggl_data$Start.date<-as.Date(toggl_data$Start.date)
	toggl_data$End.date<-as.Date(toggl_data$End.date)
	
	#conver Start.time and End.time into POSIXlt
	toggl_data$Start.time<-strptime(toggl_data$Start.time, format="%H:%M:%S")
	toggl_data$End.time<-strptime(toggl_data$End.time, format="%H:%M:%S")
	
	#return clean data
	toggl_data
}

totals_worked<-function(data,
	view_by="week", 
	start_date=data$Start.date[1], end_date=data$Start.date[length(data$Start.date)],
	skips=as.Date(x = integer(0), origin = "1970-01-01"), 
	proj=data$Project, desc=data$Description){
		#This is a function that gives stats and graphs about the amount of time worked
		#in a given time span
		#
		#this function takes a data set with the optional arguments of:
		#view by: defalut week, other option day, month
		#start date, default first date in data set
		#end date, default last date in data set
		#project, default all
		#desrciption, default all
		#skips, defalut none
		#
		#Outputs a plot and stats of the total time spent for the project and 
		#description combinations over the time span.
		#
		#all dates should be in form "%Y-%m-%d" or "%Y/%m/%d"
		#
		#Weeks start on Monday, this corrosponds to how toggl does weeks
		#
		#skip gives the ablity to exclude days from the data.
		#They will not be counted in statistics or ploted
		#Skips should be a vector that contain the first and last
		#date to be included. The dates between these will be exluded
		#ex input: skiped=c(start date, end date, start date, end date ...)
		
		##############################################
		#get vector of enties in the desired project
		proj_included<-rep(0, length(data$Project))
		for(i in 1:length(proj)){
			proj_in<-data$Project==proj[i]
			proj_included<-proj_included | proj_in
		}
		
		#get vector of enties with the desired description
		desc_included<-rep(0, length(data$Description))
		for(i in 1:length(desc)){
			desc_in<-data$Description==desc[i]
			desc_included<-desc_included | desc_in
		}
		
		#combine included projects and descriptions
		include<-(proj_included&desc_included)
		
		#get start, end, and skiped dates
		start_date<-as.Date(start_date)
		end_date<-as.Date(end_date)
		skips<-as.Date(skips)
		dates<-c(start_date, skips, end_date)
		#find the dates in ranges that we want
		date_range<-as.Date(x = integer(0), origin = "1970-01-01") #predefine variable
		for(i in 1:(length(dates)*.5)){
			#you need to add 1 for the length bc of how seq works
			date_range_par<-seq(dates[(2*i)-1], dates[2*i], length.out = dates[2*i]-dates[(2*i)-1]+1)
			date_range<-c(date_range, date_range_par)
		}
		
		#view in terms of weeks
		if (view_by=="week"){
			#convert to weeks
			weeks<-format(date_range, format = "%W-%y")
			range<-unique(weeks) #find the diffrent weeks we want data for
			start_date<-format(data$Start.date, format = "%W-%y")
		}
		
		#view in terms of months
		else if (view_by=="month"){
			#conver to month
			months<-format(date_range, format = "%m-%y")
			range<-unique(months)
			start_date<-format(data$Start.date, format = "%m-%y")
		}
		
		#view in terms of day
		else {
			start_date<-data$Start.date
			range<-date_range
		}
		
		#find the total duration of each time span 
		#for the project and description combo used
		duration<-rep(0, length(range)) #pre allocate memory
		for(i in 1:length(range)){
			duration[i]<-sum(data$Duration[start_date==range[i] & include])
		}
		
		#make diffent plots if week or day data
		#weeks look better with line w/ dot, day looks better with hist
		if (view_by=="week"){
			#we need to get the week numbers to dates so that the data plots in order
			week_dates<-date_range[match(range, weeks)]
			plot(week_dates, duration, type='b', main="total time worked vs week",
			xlab="date", ylab="duration (Hr)", xaxt="n")
			#label the x axis by months
			axis.Date(1, at=seq(date_range[1], date_range[length(date_range)], 
			by="month"), format="%b") 
		}
		else if (view_by=="month"){
			month_dates<-date_range[match(range, months)]
			plot(month_dates, duration, type='b', main="total time worked vs month",
			xlab="date", ylab="duration (Hr)", xaxt="n")
			#label the x axis by months
			axis.Date(1, at=seq(date_range[1], date_range[length(date_range)], 
			by="month"), format="%b")
		}
		else{
			plot(date_range, duration, type='h', main="total time worked vs day",
			xlab="date", ylab="duration (Hr)")
		}
		
		#creat data output tabel with desired stats
		stats_matrix<-matrix(c(mean(duration), median(duration), sd(duration), 
			max(duration), min(duration)), ncol=5, byrow=T)
		colnames(stats_matrix)<-c("Mean", "Median", "Sd", "Max", "Min")
		rownames(stats_matrix)<-c("Totals:")
		stats<-as.table(stats_matrix)
		stats
	}

event_statstics<-function(data, 
	proj=data$Project, desc=data$Description){
		#This is a function that gives stats about the duration of indvidual events
		#
		#this function takes a data set with the optional arguments of:
		#project, default all
		#desrciption, default all
		#
		#It outputs statistacts about the duration of indivudal events
		
		##############################################
		#get vector of enties in the desired project
		proj_included<-rep(0, length(data$Project))
		for(i in 1:length(proj)){
			proj_in<-data$Project==proj[i]
			proj_included<-proj_included | proj_in
		}
		
		#get vector of enties with the desired description
		desc_included<-rep(0, length(data$Description))
		for(i in 1:length(desc)){
			desc_in<-data$Description==desc[i]
			desc_included<-desc_included | desc_in
		}
		
		#get durations of only desired project and descriptions
		duration<-data$Duration[proj_included&desc_included]
		
		#conver to minutes
		duration<-duration*60
		
		#create data output table with desired stats
		stats_matrix<-matrix(c(mean(duration), sd(duration), median(duration), 
			max(duration), min(duration)), ncol=5, byrow=T)
		colnames(stats_matrix)<-c("Mean", "Median", "Sd", "Max", "Min")
		rownames(stats_matrix)<-c("Event Durations:")
		stats<-as.table(stats_matrix)
		round(stats, digits = 2) #round to 2 decimale places
	}

time_worked<-function(data,
	start_date=data$Start.date[1], end_date=data$Start.date[length(data$Start.date)],
	skips=as.Date(x = integer(0), origin = "1970-01-01"),
	proj=data$Project, desc=data$Description){
		#This function gives the average number of minutes worked each hour of the day
		#
		#this function takes a data set with the optional arguments of:
		#start date, default first date in data set
		#end date, default last date in data set
		#project, default all
		#desrciption, default all
		#skips, defalut none
		#
		#all dates should be in form "%Y-%m-%d" or "%Y/%m/%d"
		#
		#skip gives the ablity to exclude days from the data.
		#They will not be counted in statistics or ploted
		#Skips should be a vector that contain the first and last
		#date to be included. The dates between these will be exluded
		#ex input: skiped=c(start date, end date, start date, end date ...)
		##############################################
		
		#get start, end, and skiped dates
		start_date<-as.Date(start_date)
		end_date<-as.Date(end_date)
		skips<-as.Date(skips)
		dates<-c(start_date, skips, end_date)
		#find the dates in ranges that we want
		date_range<-as.Date(x = integer(0), origin = "1970-01-01") #predefine variable
		for(i in 1:(length(dates)*.5)){
			#you need to add 1 for the length bc of how seq works
			date_range_par<-seq(dates[(2*i)-1], dates[2*i], length.out = dates[2*i]-dates[(2*i)-1]+1)
			date_range<-c(date_range, date_range_par)
		}
		
		#get vector of enties in the desired project
		proj_included<-rep(0, length(data$Project))
		for(i in 1:length(proj)){
			proj_in<-data$Project==proj[i]
			proj_included<-proj_included | proj_in
		}
		
		#get vector of enties in the desired description
		desc_included<-rep(0, length(data$Description))
		for(i in 1:length(desc)){
			desc_in<-data$Description==desc[i]
			desc_included<-desc_included | desc_in
		}
		
		#get vector of entries in the desired dates
		date_included<-rep(0, length(data$Start.date))
		for(i in 1:length(date_range)){
			date_in<-data$Start.date==date_range[i]
			date_included<-date_included | date_in
		}
		
		#get vector of what to include
		include<-proj_included&desc_included&date_included
		
		#get dates, start times, and end times that have the 
		#project, descriptions, and dates we want
		start_time<-data$Start.time[include]
		end_time<-data$End.time[include]
		dates_examined<-data$Start.date[include]
		
		#we need to add 0s for the all the hours were no work was done each day
		#this is nessary for the averages to come out okay
		uniuqe_dates<-unique(dates_examined)
		for(i in 1:length(uniuqe_dates)){
			day_start_times<-as.numeric(format(start_time[dates_examined==uniuqe_dates[i]], "%H"))
			day_end_times<-as.numeric(format(end_time[dates_examined==uniuqe_dates[i]], "%H"))
			for(j in 0:23){
				if(!any(c(day_start_times,day_end_times) == j)){
					start_time<-append(start_time, strptime(j, format="%H"))
					end_time<-append(end_time, strptime(j, format="%H"))
					dates_examined<-append(dates_examined, uniuqe_dates[i])
				}
			}
		}
		
		#since it is possible to cross through multiple hours we need to run the loop
		#that spilts data into hour blocks until all the durations are less than an hour
		#we will still end up with hours that have more than 60min worked in them.
		#This is because toggl will let you enter times that over lap, so in raw toggl
		#data you can more time worked then minutes in the time span. This loop does not
		#account for that
		max_dur<-max(data$Duration)*60
		while (max_dur>60) {
			#find entries that cross from one hour to another, and there indecies
			hour_change<-as.numeric(format(start_time, "%H"))!=
				as.numeric(format(end_time, "%H"))
			hour_change_ind<-which(hour_change)
			for(i in 1:length(hour_change_ind)){
				#get the hour time we want to insert
				#please note that we are playing games with indecies in this loop because
				#our vectors get longer each iteration and we need to adjust for that
				hour<-format(end_time[hour_change_ind[i]+(i-1)], format = "%H")
				hour<-strptime(hour, format="%H")
				#insert the new hour time
				#note that we want the end time to be 1sec before
				start_time<-append(start_time, hour, after=hour_change_ind[i]+(i-1))
				end_time<-append(end_time, hour-1, after=hour_change_ind[i]-1+(i-1))
				dates_examined<-append(dates_examined, dates_examined[hour_change_ind[i]+(i-1)], after=hour_change_ind[i]+(i-1))
			}
			#get time durations in minutes
			duration<-(end_time-start_time)/60
			max_dur<-max(duration)
		}
		
		#find how much time was worked each hour for each day
		hour_days<-matrix(0, 24, length(uniuqe_dates)) #pre allocate memory
		for(i in 1:length(uniuqe_dates)){
			#look at each day
			date_durations<-duration[dates_examined==uniuqe_dates[i]]
			start_durations<-start_time[dates_examined==uniuqe_dates[i]]
				for(j in 1:24){
					#look at each hour in that day
					step_dur<-date_durations[as.numeric(format(start_durations, "%H"))==j-1]
					hour_days[j,i]<-sum(step_dur)
				}
		}
		
		#get stats from the time worked each hour each day
		hour_dur_ave<-rowMeans(hour_days)
		hour_dur_sd<-apply(hour_days, 1 ,sd)
		hour_dur_med<-apply(hour_days, 1, median)
		hour_dur_min<-apply(hour_days, 1, min)
		hour_dur_max<-apply(hour_days, 1, max)
		time_24hr<-seq(0,23)
		
		#creat a bar plot showing the avargea amount of time worked for each
		#hour of the day
		barplot(hour_dur_ave, ylim=c(0,60), names.arg=seq(0,23),
			ylab="Average Number of Minutes Worked", xlab="Time of day",
			main="Average Time Worked vs Time of day")
			
		#return stats
		data.frame(time_24hr, hour_dur_ave,hour_dur_sd, 
			hour_dur_med, hour_dur_min, hour_dur_max)
	}

find_frimon_dates<-function(dates){
	#function return unique weekend dates from a list of dates
	weekend_dates<-dates[grepl("Fri|Mon", weekdays(dates))]
	unique(weekend_dates)
}

find_weekend_dates<-function(dates){
	#function return unique weekend dates from a list of dates
	weekend_dates<-dates[grepl("S(at|un)", weekdays(dates))]
	unique(weekend_dates)
}
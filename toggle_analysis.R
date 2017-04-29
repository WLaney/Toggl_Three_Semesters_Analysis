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
		#this function takes a data set with the optional arguments of:
		#view by: defalut week, other option day
		#start date, default first date in data set
		#end date, default last date in data set
		#project, default all
		#desrciption, default all
		#skips, defalut none
		#
		#Outputs a plot of the total time spent for the project and 
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
		#print(proj_included)
		#print(desc_included)
		include<-(proj_included&desc_included)
		#print(include)
		
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
			weeks<-format(date_range, format = "%W")
			range<-unique(weeks) #find the diffrent weeks we want data for
			start_date<-format(data$Start.date, format = "%W")
		}
		
		#view in terms of months
		else if (view_by=="month"){
			#conver to month
			months<-format(date_range, format = "%m")
			range<-unique(months)
			start_date<-format(data$Start.date, format = "%m")
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
		print(summary(duration))
		print(sd(duration))
		
		#make diffent plots if week or day data
		#weeks look better with line w/ dot, day looks better with hist
		if (view_by=="week"){
			#we need to get the week numbers to dates so that the data ploints in order
			week_dates<-date_range[match(range, weeks)]
			plot(week_dates, duration, type='b', main="total time worked vs week",
			xlab="date", ylab="duration (Hr)")
		}
		else if (view_by=="month"){
			month_dates<-date_range[match(range, months)]
			plot(month_dates, duration, type='b', main="total time worked vs month",
			xlab="date", ylab="duration (Hr)")
		}
		else{
			plot(date_range, duration, type='h', main="total time worked vs day",
			xlab="date", ylab="duration (Hr)")
		}
}
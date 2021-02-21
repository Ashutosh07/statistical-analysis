install.packages("tidyverse")
library(tidyverse)
dat = read.csv("govhack3.csv", header = TRUE)
class(dat)
dat[3,1]


#Task 1
class(dat)
view(dat)
length(count.fields("govhack3.csv",skip = 2))
sapply(dat,class)
lapply(dat, class)
difference = difftime(as.Date("30-06-2014"), as.Date("01-07-2013"), units = "weeks")
difference



#Task 2
ed_data_link <- 'govhack3.csv'
top_row <- read_csv(ed_data_link, col_names = FALSE, n_max = 1)
second_row <- read_csv(ed_data_link, n_max = 1)
top_row
i=2
for(i in range(0,64))
{
  print(second_row[i])
  i= i + 2
}
  column_names <- second_row %>% 
  unlist(., use.names=FALSE) %>% 
  make.unique(., sep = "__") # double underscore

column_names[2:8] <- str_c(column_names[2:8], '0', sep='__')
column_names
daily_attendance <- 
  read_csv(ed_data_link, skip = 2, col_names = column_names)

view(daily_attendance)
class(daily_attendance)
head(daily_attendance)
sapply(daily_attendance,class)
lapply(daily_attendance, class)
j=c(2,9,16,23,30,37,44,51,58)
for(i in seq(2,64, by= 7)){
print(colnames(dat,do.NULL = TRUE, prefix = "col")[i])
}
#Replacing all N/A with 0
data <- data.frame(lapply(daily_attendance, function(x) {gsub("N/A",0, x)}))
data
View(data)



#Task3
data %>% 
  dplyr::select(Attendance__0,Admissions__0,Tri_1__0,Tri_2__0,Tri_3__0,Tri_4__0,Tri_5__0)
#Task 3.1
flights %>% 
  summarise(Column_n = colnames(dat,do.NULL = TRUE, prefix = "col")[2], 
            sum_Attendance = sum(daily_attendance$Attendance__0),
            sum_Admissions = sum(daily_attendance$Admissions__0))

#Task 3.2
x= c(mean(as.numeric(data$Admissions__0)),mean(as.numeric(data$Admissions__1)),mean(as.numeric(data$Admissions__2)),mean(as.numeric(data$Admissions__3)),mean(as.numeric(data$Admissions__4)),mean(as.numeric(data$Admissions__5)),mean(as.numeric(data$Admissions__6)),mean(as.numeric(data$Admissions__7)),mean(as.numeric(data$Admissions__8)))
#Barplot for represnting ED demands
arg_list<-c("Royal.Perth.Hospital","Fremantle.Hospital", "Princess.Margaret.Hospital.For.Children","King.Edward.Memorial.Hospital.For.Women"
            ,"Sir.Charles.Gairdner.Hospital","Armadale.Kelmscott.District.Memorial.Hospital","Swan.District.Hospital","Rockingham.General.Hospital","Joondalup.Health.Campus")
barplot(x,names.arg =arg_list,las=2,main = "Barplot for Represnting ED demands per year")

#Task 3.3
y=c(mean((as.numeric(data$Admissions__0)/52.1429)),mean((as.numeric(data$Admissions__1)/52.1429)),mean((as.numeric(data$Admissions__2)/52.1429)),mean((as.numeric(data$Admissions__3)/52.1429)),mean((as.numeric(data$Admissions__4)/52.1429)),mean((as.numeric(data$Admissions__5)/52.1429)),mean((as.numeric(data$Admissions__6)/52.1429)),mean((as.numeric(data$Admissions__7)/52.1429)),mean((as.numeric(data$Admissions__8)/52.1429)))
lines.default(y,main = "Linechart for Representing ED demands per week")

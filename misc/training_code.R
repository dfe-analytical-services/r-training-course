# sec0_1 ------------------------------------------------------------------
library(tidyverse)

# sec0_2 ------------------------------------------------------------------
print("Hello world")

# a0_1 --------------------------------------------------------------------
print("Hello world")

# sec3_1 ------------------------------------------------------------------
my_string <- 'DfE'
my_number <- 2017
my_boolean <- TRUE
my_list <- list(1,2,3,4,5)
my_dataframe <- data.frame(var1 = c(1,2,3,4,5),
                           var2 = c("a","b","c","d","e"))

# a3_1 --------------------------------------------------------------------
#NA - Steps in guidance

# a3_2 --------------------------------------------------------------------
#NA - Steps in guidance

# a3_3 ------------------------------------------------------------------
#Gives you the type of the object

# a3_4 ------------------------------------------------------------------
#NA

# sec3_2 ------------------------------------------------------------------
rm(object_name)

# sec3_3 ------------------------------------------------------------------
rm(list = ls())

# a3_5 --------------------------------------------------------------------
#Creates a list called 'list' of all the items in the environment

# a3_6 --------------------------------------------------------------------
#1+1 - 2 in console
##1+1 - nothing in console, commented out

# sec3_4 ------------------------------------------------------------------
#Remove all objects
rm(list = ls())

# sec4_1 ------------------------------------------------------------------
#Load in data
swfc_16_init <- read.csv("../1_data/swfc_2016_machine_readable.csv")
swfc_16 <- swfc_16_init

# sec4_2 ------------------------------------------------------------------
#Load in data
swfc_16_init <- read.csv("1_data/swfc_2016_machine_readable.csv")
swfc_16 <- swfc_16_init

# sec4_3 ------------------------------------------------------------------
#Basic dataframe exploration functions
summary(swfc_16) #Get a summary of each variable

# sec4_4 ------------------------------------------------------------------
#Basic dataframe exploration functions
summary(swfc_16[,c(1:6)]) #Get a summary of each variable

# sec4_5 ------------------------------------------------------------------
head(swfc_16) #Get the top 6 rows

# sec4_6 -----------------------------------------------------------------
head(swfc_16[,c(1:6)])

# sec4_7 ------------------------------------------------------------------
tail(swfc_16) #Get the bottom 6 rows

# sec4_8 ------------------------------------------------------------------
tail(swfc_16[,c(1:6)])

# sec4_9 ------------------------------------------------------------------
colnames(swfc_16) #Get list of column names

# sec4_10 -----------------------------------------------------------------
#Identify the column number using colnames(swfc_16) and then specify the string to change it to
colnames(swfc_16)[11] <- "Region"

#...and change it back again
colnames(swfc_16)[11] <- "Government_Office_Region_Name"

# sec4_11 -----------------------------------------------------------------
teacher_absences <- swfc_16[,c(2,53:55)]

# sec4_12 -----------------------------------------------------------------
swfc_16 <- swfc_16[,-3]

# sec5_1 ------------------------------------------------------------------
#Conditionally select primary schools
swfc_16_pri <- swfc_16[swfc_16$School_Phase == "Primary",]

# a5_1 --------------------------------------------------------------------
swfc_16_acad <- swfc_16[swfc_16$School_Type == "Academies",]

# sec5_2 ------------------------------------------------------------------
swfc_16_male <- swfc_16[swfc_16$Perc_Male_Teachers > 50,]

# sec5_3 ------------------------------------------------------------------
#Conditionally select schools where Pupil:Teacher Ratios are below 20 and above or equal to 10
swfc_16_ptr <- swfc_16[(swfc_16$Pupil_Teacher_Ratio < 20 & swfc_16$Pupil_Teacher_Ratio >=10),]

#Conditionally select schools where Pupil:Teacher Ratios are below 10 or their LA is in Camden
swfc_16_ptr_camden <- swfc_16[(swfc_16$Pupil_Teacher_Ratio < 10 | swfc_16$LA_Name == "Camden"),]

# a5_2 --------------------------------------------------------------------
swfc_16_statlow5_novac <- swfc_16[(swfc_16$StatutoryLowAge > 5 | swfc_16$FT_Vacant_Posts == 0),]

# sec5_4 ------------------------------------------------------------------
#Turn Religious.Character to binary
swfc_16$Religious_Character <- as.character(swfc_16$Religious_Character)

swfc_16$Religious_Character[swfc_16$Religious_Character == 'Does not apply' |
                              swfc_16$Religious_Character == 'None' |
                              swfc_16$Religious_Character == ""] <- FALSE
swfc_16$Religious_Character[swfc_16$Religious_Character != FALSE] <- TRUE

# sec5_5 ------------------------------------------------------------------
#Calculate percentage of posts which are vacancies
swfc_16$perc_vacancies <- swfc_16$FT_Vacant_Posts/swfc_16$Tot_Teachers_HC

# a5_3 --------------------------------------------------------------------
swfc_16$School_Type <- as.character(swfc_16$School_Type)
swfc_16$School_Type[swfc_16$School_Type != 'LA maintained schools' &
                      swfc_16$School_Type != 'Special schools'] <- 'Not LA maintained'
swfc_16$School_Type <- as.factor(swfc_16$School_Type)

# a5_4 --------------------------------------------------------------------
write.csv(swfc_16,"swfc_16_edited.csv")

# sec6_1 ------------------------------------------------------------------
install.packages("dplyr")

# sec6_2 ------------------------------------------------------------------
#Load required libraries
library(dplyr)

# sec6_3 ------------------------------------------------------------------
swfc_16$School_Name <- as.character(swfc_16$School_Name)

# sec6_4 ------------------------------------------------------------------
swfc_16$School_Name <- swfc_16$School_Name %>% as.character()

# sec6_5 ------------------------------------------------------------------
#Create dataframe on whether region is in London or Rest of England
ldn_roe <- data.frame(Government_Office_Region_Name = c("East Midlands", "East of England", "Inner London", 
                                                        "North East", "North West", "Outer London", "South East", "South West", "West Midlands", "Yorkshire and the Humber"),
                      ldn_roe = c("RoE","RoE","LDN","RoE","RoE","LDN","RoE","RoE","RoE","RoE"))

#Match to SWFC
swfc_16 <- left_join(swfc_16,LDN_RoE,by="Government_Office_Region_Name")

# sec6_6 ------------------------------------------------------------------
#Remove new column
swfc_16 <- select(swfc_16,LA_Number:Perc_FT_Temp_Filled_Posts)

#Or using 'the pipe'

swfc_16 <- swfc_16 %>% 
  select(LA_Number:Perc_FT_Temp_Filled_Posts)

# sec6_7 ------------------------------------------------------------------
#Change name of Government_Office_Region_Name
ldn_roe <- ldn_roe %>% 
  select(Region = Government_Office_Region_Name,
         ldn_roe)

#Match to SWFC
swfc_16 <- left_join(swfc_16,ldn_roe,by=c("Government_Office_Region_Name"="Region"))

# sec6_8 ------------------------------------------------------------------
#Calculate the number of schools by school type
school_type_count <- swfc_16 %>% #Set the name of the output object and feed in 'the ingredients' - swfc_16 in this instance
  group_by(School_Type) %>% #Recipe step 1: Specifying the variable we want to group the data by
  summarise(Count_Schools = n()) #Recipe step 2: Define what function we want to apply to the grouped data. Here we are doing a simple count - n(). We also specify the name of the new column, in this instance 'Count_Schools'

# sec6_9 ------------------------------------------------------------------
#Calculate average percentage of qualified teachers per school, grouped by school type
av_qts_schooltype <- swfc_16 %>% #Set the name of the output object and feed in 'the ingredients' - swfc_16 in this instance
  group_by(School_Type) %>% #Recipe step 1: Specifying the variable we want to group the data by
  summarise(Ave_Perc_QTS = mean(Perc_QTS_Teachers,na.rm=TRUE))
#Recipe step 2: Calculate the average percentage of qualified teachers for each group. The 'na.rm=TRUE' argument does not consider rows where the value is NA.

# a6_1 --------------------------------------------------------------------
vac_posts_region <- swfc_16 %>% 
  group_by(Government_Office_Region_Name) %>% 
  summarise(Total_Vacs = sum(FT_Vacant_Posts, na.rm=TRUE))

# sec6_10 -----------------------------------------------------------------
#Select all schools in Inner and Outer London
swfc_16_lon <- swfc_16 %>% 
  filter(grepl("London",Government_Office_Region_Name)) #The filtering function is unsurprisingly called 'filter()'
#Here, we're using a function called grepl, which finds all rows where the Government_Office_Region_Name column contains the string 'London'.

# a6_2 --------------------------------------------------------------------
swfc16_value_unqual <- swfc_16 %>%
  filter(!is.na(Perc_QTS_Teachers))

# sec6_11 -----------------------------------------------------------------
#Calculate the percentage of schools by school type
school_type_count <- swfc_16 %>%
  group_by(School_Type) %>%
  summarise(Count_Schools = n()) %>% 
  mutate(Perc_Schools = Count_Schools/sum(Count_Schools))

# sec6_12 -----------------------------------------------------------------
#Calculate the percentage of schools by school type
school_type_count <- swfc_16 %>%
  group_by(School_Type) %>%
  summarise(Count_Schools = n()) %>% 
  mutate(Perc_Schools = Count_Schools/sum(Count_Schools)) %>% 
  mutate(Perc_Schools = round(Perc_Schools*100,1)) #Multiply Perc_Schools by 100 to get a percentage, and round to 1 decimal place.

# a6_3 --------------------------------------------------------------------
perc_tas_sec_eoe <- swfc_16 %>%
  filter(School_Phase == 'Secondary') %>% 
  group_by(Government_Office_Region_Name) %>% 
  summarise(Sum_of = sum(Tot_TAs_HC,na.rm=TRUE)) %>% 
  mutate(Sum_of = round(Sum_of/sum(swfc_16 %>%
                               filter(School_Phase == 'Secondary') %>% 
                               select(Tot_TAs_HC),na.rm=TRUE)*100,1)) %>% 
  filter(Government_Office_Region_Name == 'East of England') %>%
  select(Sum_of) %>% 
  as.numeric()


# a7_1 --------------------------------------------------------------------
install.packages("tidyverse")
install.packages("ggplot")

# sec7_1 ------------------------------------------------------------------
ggplot(dataset,aes(x,y)) + 
  geom_*() +
  coords_*() +
  styles() +
  styles()

# sec7_2 ------------------------------------------------------------------
ggplot(swfc_16,aes(Tot_Workforce_HC,Tot_Teachers_HC)) +
  geom_point()

# a7_2 --------------------------------------------------------------------
#swfc_16 - the dataset
#aes(Tot_Workforce_HC,Tot_Teachers_HC) - the data on the x and y axis
#geom_point() - the type of graph to be plotted

# sec7_3 ------------------------------------------------------------------
ggplot(swfc_16,aes(School_Type)) +
  geom_bar()

# a7_3 --------------------------------------------------------------------
#The aes() only has one argument, the x axis, the y axis is a count of each type
#of school by default

# sec7_4 ------------------------------------------------------------------
ggplot(swfc_16 %>% group_by(School_Type) %>% summarise(ave_perc_qts = mean(Perc_QTS_Teachers,na.rm=TRUE)),
       aes(School_Type,ave_perc_qts)) + 
  geom_bar(stat="identity")

# a7_4 --------------------------------------------------------------------
swfc_16 %>% group_by(School_Type) %>% summarise(ave_perc_qts = mean(Perc_QTS_Teachers,na.rm=TRUE))

# a7_5 --------------------------------------------------------------------
ggplot(swfc_16 %>% group_by(Tot_Teachers_HC,School_Type) %>% summarise(ave_ta_teacher_ratio = mean(TA_Teacher_Ratio,na.rm=TRUE)),
       aes(Tot_Teachers_HC,ave_ta_teacher_ratio)) + 
  geom_line()

# sec7_5 ------------------------------------------------------------------
ggplot(swfc_16 %>% group_by(Tot_Teachers_HC,School_Type) %>% summarise(ave_ta_teacher_ratio = mean(TA_Teacher_Ratio,na.rm=TRUE)),
       aes(Tot_Teachers_HC,ave_ta_teacher_ratio)) + 
  geom_line()

# sec7_6 ------------------------------------------------------------------
ggplot(swfc_16,aes(Government_Office_Region_Name,fill=School_Type)) +
  geom_bar()

# a7_6 --------------------------------------------------------------------
ggplot(swfc_16 %>% group_by(Tot_Teachers_HC,School_Type) %>% summarise(ave_ta_teacher_ratio = mean(TA_Teacher_Ratio,na.rm=TRUE)),
       aes(Tot_Teachers_HC,ave_ta_teacher_ratio,col=School_Type)) + 
  geom_line()

# sec7_7 ------------------------------------------------------------------
summarise(var1_name = mean(variable1,na.rm=TRUE),
          var2_name = mean(variable2,na.rm=TRUE),
          var3_name = n())

# a7_7 --------------------------------------------------------------------
regions_data <- swfc_16 %>%
  group_by(Government_Office_Region_Name) %>% 
  summarise(avg_pay = mean(Mean_Gross_Salary_All_Teachers_Sterling,na.rm=TRUE),
            avg_perc_allowance = mean(Perc_Receive_Allowance_Qual_Classroom_Teachers,na.rm=TRUE),
            num_schools = n())

# a7_8 --------------------------------------------------------------------
ggplot(regions_data,aes(avg_pay,
                        avg_perc_allowance,
                        size=num_schools,
                        col=Government_Office_Region_Name)) +
  geom_point()

# a7_9 --------------------------------------------------------------------
ggplot(swfc_16,aes(Government_Office_Region_Name,fill=School_Type)) +
  geom_bar() +
  coord_flip() +
  ggtitle('Schools in each region, split by school type') +
  xlab("Region") +
  ylab("Count") +
  theme_minimal() +
  ylim(0,4000)

# sec8_1 ------------------------------------------------------------------
min(swfc_16$Pupil_Teacher_Ratio)
max(swfc_16$Pupil_Teacher_Ratio)

# a8_1 --------------------------------------------------------------------
#na.rm=TRUE - this removes all NAs

# sec8_2 ------------------------------------------------------------------
swfc_16 %>% 
  select(Pupil_Teacher_Ratio
  ) %>% 
  min(ARGUMENT_IN_HERE
  )

swfc_16 %>%
  select(Pupil_Teacher_Ratio
  ) %>%
  max(ARGUMENT_IN_HERE
  )

# a8_2 --------------------------------------------------------------------
range(swfc_16$Tot_Workforce_FTE,na.rm = TRUE)

swfc_16$Tot_Workforce_FTE %>% 
  range(na.rm = TRUE)

swfc_16 %>% 
  select(Tot_Workforce_FTE) %>% 
  range(na.rm = TRUE)

# sec8_3 ------------------------------------------------------------------
#The mean of the total school workforce for primary schools
swfc_16 %>%
  filter(School_Phase == 'Primary') %>% 
  group_by(School_Phase) %>% 
  summarise(Ave = mean(Tot_Workforce_HC,na.rm=TRUE))

# sec8_4 ------------------------------------------------------------------
#The median of the total school workforce for primary schools
swfc_16 %>%
  filter(School_Type == 'Academies') %>% 
  group_by(School_Type) %>% 
  summarise(Ave = median(Tot_Workforce_HC,na.rm=TRUE))

# sec8_5 ------------------------------------------------------------------
#Base R
cor(swfc_16[,c(15,16)],use="complete.obs")

#Pipes
swfc_16 %>% select(StatutoryLowAge,StatutoryHighAge)%>% cor(use="complete.obs")

# a8_3 --------------------------------------------------------------------
swfc_16 %>% select(StatutoryLowAge:Tot_TAs_HC)%>% cor(use="complete.obs")

# sec8_6 ------------------------------------------------------------------
t.test(swfc_16 %>% filter(School_Phase == "Primary"
) %>% select(Tot_Workforce_HC),
mu = mean(swfc_16$Tot_Workforce_HC,na.rm=TRUE),
alternative = "less")
t.test(swfc_16[swfc_16$School_Phase == "Primary",17],
       mu = mean(swfc_16$Tot_Workforce_HC,na.rm=TRUE),
       alternative = "less")

# sec8_7 ------------------------------------------------------------------
t.test(swfc_16 %>% filter(School_Phase == "Primary") %>% select(Tot_Workforce_HC),
       mu = mean(swfc_16$Tot_Workforce_HC,na.rm=TRUE),
       alternative = "less")

# a8_4 --------------------------------------------------------------------
t.test(swfc_16 %>% filter(LA_District == "Camden") %>% select(FT_Vacant_Posts),
       mu = mean(swfc_16$FT_Vacant_Posts,na.rm=TRUE),
       alternative = "greater")
t.test(swfc_16 %>% filter(LA_District == "Camden") %>% select(FT_Vacant_Posts),
       mu = mean(swfc_16$FT_Vacant_Posts,na.rm=TRUE),
       alternative = "less")

# sec8_8 ------------------------------------------------------------------
t.test(Perc_Male_Teachers ~ School_Phase, 
       data = (swfc_16 %>% filter(School_Phase == "Primary" |
                                    School_Phase == "All Through") %>% 
                 select(School_Phase,
                        Perc_Male_Teachers)))

# sec8_9 ------------------------------------------------------------------
t.test(Perc_Male_Teachers ~ School_Phase, 
       data = (swfc_16 %>% filter(School_Phase == "Primary" |
                                    School_Phase == "All Through") %>% 
                 select(School_Phase,
                        Perc_Male_Teachers)))

# a8_5 --------------------------------------------------------------------
t.test(FT_Vacant_Posts ~ LA_District, 
       data = (swfc_16 %>% filter(LA_District == "Camden" |
                                    LA_District == "Northumberland") %>% 
                 select(LA_District,
                        FT_Vacant_Posts)))

# sec9_1 ------------------------------------------------------------------
function(condition to be met){
  action to be carried out if condition is met #Note how it has to be indented
}

# sec9_2 ------------------------------------------------------------------
prob_male <- swfc_16$Perc_Male_Teachers %>% mean(na.rm = TRUE)/100

# sec9_3 ------------------------------------------------------------------
rand_num <- runif(1)

# sec9_4 ------------------------------------------------------------------
if(rand_num<=prob_male){
  print("M")
}

# sec9_5 ------------------------------------------------------------------
if(rand_num<=prob_male){
  print("M")
}else{
  print("F")
}

# a9_1 --------------------------------------------------------------------
if(runif(1)<=swfc_16$Perc_Male_Teachers %>% mean(na.rm = TRUE)/100){
  print("M")
}else{
  print("F")
}

# sec9_6 ------------------------------------------------------------------
for(i in 1:10){
  if(runif(1)<=prob_male){
    print("M")
  }else{
    print("F")
    
  }
}

# sec9_7 ------------------------------------------------------------------
#For creating an empty object
character(0)

#For joining values to an existing object
c(object,value)

# a9_2 --------------------------------------------------------------------
gender <- character(0)

for(i in 1:10){
  if(runif(1)<=swfc_16$Perc_Male_Teachers %>% mean(na.rm = TRUE)/100){
    gender <- c(gender,"M")
  }else{
    gender <- c(gender,"F")
  }
}

# sec9_8 ------------------------------------------------------------------
while(1<2){
  print("This is going to take a while...")
}

# a9_3 --------------------------------------------------------------------
#NA - sec9_8 to be run

# sec9_9 ------------------------------------------------------------------
object <- starting_value
while(condition_of_object){
  action
  object <- change_value_of_object
}

#For example
i <- 1
while(i<5){
  print(c("This is a while loop, we're at iteration", as.character(i)))
  i<-i+1
}

# a9_4 --------------------------------------------------------------------
gender <- character(0)

for(i in 1:10){
  if(runif(1)<=swfc_16$Perc_Male_Teachers %>% mean(na.rm = TRUE)/100){
    gender <- c(gender,"M")
  }else{
    gender <- c(gender,"F")
  }
}

grade <- character(0)
i <- 1
while(i==1){
  grade <- c(grade,"Headteacher")
  i <- i +1
}
while(i<5){
  grade <- c(grade,"Senior Leader")
  i <- i +1
}

while(i<=10){
  grade <- c(grade,"Classroom Teacher")
  i <- i +1
}

my_school <- data.frame(gender = gender,
                        grade = grade)

# sec10_1 -----------------------------------------------------------------
verb(noun,adverbs)

# sec10_2 -----------------------------------------------------------------
function_name <- function(function_argument1,function_argument2,etc){
  function_actions
}

# sec10_3 -----------------------------------------------------------------
my_func <- function(x){
  x*100
}

# a10_1 -------------------------------------------------------------------
swfc_16 <- swfc_16 %>%
  mutate(perc_pt_test = my_func(Perc_PT_Teaching_Staff_))

# sec10_4 -----------------------------------------------------------------
if(rand_num<=prob_male){
  print("M")
}else{
  print("F")
}

# a10_2 -------------------------------------------------------------------
my_func2 <- function(rand_num){
  if(rand_num<=swfc_16$Perc_Male_Teachers %>% mean(na.rm = TRUE)/100){
    print("M")
  }else{
    print("F")
  }
}

# sec10_5 -----------------------------------------------------------------
ggplot(swfc_16 %>% filter(Government_Office_Region_Name == 'Inner London'),
       aes(Tot_Workforce_HC)) +
  geom_histogram() +
  ggtitle('Distribution of school size in Inner London')

# sec10_6 -----------------------------------------------------------------
region_name <- 'Inner London'

ggplot(swfc_16 %>% filter(Government_Office_Region_Name == region_name),
       aes(Tot_Workforce_HC)) +
  geom_histogram() +
  ggtitle(paste0('Distribution of school size in ',region_name))

# a10_3 --------------------------------------------------------------------
my_graph_func <- function(region_name){
  ggplot(swfc_16 %>% filter(Government_Office_Region_Name == region_name),
       aes(Tot_Workforce_HC)) +
  geom_histogram() +
    ggtitle(paste0('Distribution of school size in the ',x))
}

# sec10_7 -----------------------------------------------------------------
lapply(list_to_apply_function_to_each_entry,function(argument_name) specific_function_to_apply(argument_name))

# a10_4 -------------------------------------------------------------------
lapply(swfc_16$Government_Office_Region_Name %>% levels(),function(x) my_graph_func(x))

# sec10_8 -----------------------------------------------------------------
swfc_16[,grep("Perc", colnames(swfc_16))] <- lapply(swfc_16[,grep("Perc", colnames(swfc_16))],function(x) my_func(x))

# a10_5 -------------------------------------------------------------------
swfc_16[,c(6:10)] <- lapply(swfc_16[,c(6:10)],function(x) as.character(x))

library(rgdal)
library(tidyverse)
library(gpclib)
England <- readOGR("1_Data/SHPs/England_Regions.shp")
library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
str(England)

ave_vacs <- swfc_16_init %>% 
  mutate(Government_Office_Region_Name = Government_Office_Region_Name %>% 
           as.character()) %>% 
  mutate(Government_Office_Region_Name = ifelse(Government_Office_Region_Name == 'Yorkshire and the Humber',
                                                "Yorkshire and The Humber",Government_Office_Region_Name))%>% 
  mutate(Government_Office_Region_Name = 
           ifelse(grepl("London",
                        Government_Office_Region_Name),
                  "London",
                  Government_Office_Region_Name) %>% as.factor()) %>% 
  group_by(Government_Office_Region_Name) %>% 
  summarise(ave_vacs = mean(Perc_FT_Posts_Vacant,na.rm=TRUE)) 
ave_vacs <- swfc_16_init %>% 
  group_by(Government_Office_Region_Name) %>% 
  summarise(ave_vacs = mean(Perc_FT_Posts_Vacant,na.rm=FALSE)) 

england_points <- fortify(England, region="rgn16nm") %>% 
  left_join(ave_vacs,c("id"="Government_Office_Region_Name"))

ggplot() +
  geom_polygon(data=england_points,aes(long,lat,group=group,fill=ave_vacs),col="black")

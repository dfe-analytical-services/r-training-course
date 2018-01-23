# sec0_1 ------------------------------------------------------------------
library(tidyverse)

# sec0_2 ------------------------------------------------------------------
print("Hello world")

# sec3_1 ------------------------------------------------------------------
my_string <- 'DfE'
my_number <- 2017
my_boolean <- TRUE
my_list <- list(1,2,3,4,5)
my_dataframe <- data.frame(var1 = c(1,2,3,4,5),
                           var2 = c("a","b","c","d","e"))

# sec3_2 ------------------------------------------------------------------
rm(OBJECT_NAME)

# sec3_3 ------------------------------------------------------------------
rm(list = ls())

# sec3_4 ------------------------------------------------------------------
#Remove all objects
rm(list = ls())

# sec4_1 ------------------------------------------------------------------
#Load in data
SWFC_16_Init <- read.csv("1_data/SWFC_2016_Machine_Readable.csv")
SWFC_16 <- SWFC_16_Init

# sec4_2 ------------------------------------------------------------------
#Basic dataframe exploration functions
summary(SWFC_16) #Get a summary of each variable

# sec4_3 ------------------------------------------------------------------
#Basic dataframe exploration functions
summary(SWFC_16[,c(1:6)]) #Get a summary of each variable

# sec4_4 ------------------------------------------------------------------
head(SWFC_16) #Get the top 6 rows

# sec4_5 ------------------------------------------------------------------
head(SWFC_16[,c(1:6)])

# sec4_6 -----------------------------------------------------------------
tail(SWFC_16) #Get the bottom 6 rows

# sec4_7 ------------------------------------------------------------------
tail(SWFC_16[,c(1:6)])

# sec4_8 ------------------------------------------------------------------
colnames(SWFC_16) #Get list of column names

# sec4_9 ------------------------------------------------------------------
colnames(SWFC_16)[11] <- "Region"
#Identify the column number using colnames(SWFC_16) and then specify the string to change it to

# sec4_10 -----------------------------------------------------------------
Teacher_Absences <- SWFC_16[,c(2,53:55)]

# sec4_11 -----------------------------------------------------------------
SWFC_16 <- SWFC_16[,-3]

# sec5_1 ------------------------------------------------------------------
#Conditionally select primary schools
SWFC_16_Pri <- SWFC_16[SWFC_16$School_Phase == "Primary",]

# sec5_2 ------------------------------------------------------------------
SWFC_16_Male <- SWFC_16[SWFC_16$Perc_Male_Teachers > 50,]

# sec5_3 ------------------------------------------------------------------
#Conditionally select schools where Pupil:Teacher Ratios are below 20 and above or equal to 10
SWFC_16_PTR <- SWFC_16[(SWFC_16$Pupil_Teacher_Ratio < 20 & SWFC_16$Pupil_Teacher_Ratio >=10),]

#Conditionally select schools where Pupil:Teacher Ratios are below 10 or their LA is in Camden
SWFC_16_PTR_Camden <- SWFC_16[(SWFC_16$Pupil_Teacher_Ratio < 10 | SWFC_16$LA_Name == "Camden"),]

# sec5_4 ------------------------------------------------------------------
#Turn Religious.Character to binary
SWFC_16$Religious_Character <- as.character(SWFC_16$Religious_Character)

SWFC_16$Religious_Character[SWFC_16$Religious_Character == 'Does not apply' |
                              SWFC_16$Religious_Character == 'None' |
                              SWFC_16$Religious_Character == ""] <- FALSE
SWFC_16$Religious_Character[SWFC_16$Religious_Character != FALSE] <- TRUE

# sec5_5 ------------------------------------------------------------------
#Calculate percentage of posts which are vacancies
SWFC_16$Perc_Vacancies <- SWFC_16$FT_Vacant_Posts/SWFC_16$Tot_Teachers_HC

# sec6_1 ------------------------------------------------------------------
install.packages("dplyr")

# sec6_2 ------------------------------------------------------------------
#Load required libraries
library(dplyr)

# sec6_3 ------------------------------------------------------------------
SWFC_16$School_Name <- as.character(SWFC_16$School_Name)

# sec6_4 ------------------------------------------------------------------
SWFC_16$School_Name <- SWFC_16$School_Name %>% as.character()

# sec6_5 ------------------------------------------------------------------
#Create dataframe on whether region is in London or Rest of England
LDN_RoE <- data.frame(Government_Office_Region_Name = c("East Midlands", "East of England", "Inner London", 
                                                        "North East", "North West", "Outer London", "South East", "South West", "West Midlands", "Yorkshire and the Humber"),
                      LDN_RoE = c("RoE","RoE","LDN","RoE","RoE","LDN","RoE","RoE","RoE","RoE"))

#Match to SWFC
SWFC_16 <- left_join(SWFC_16,LDN_RoE,by="Government_Office_Region_Name")

# sec6_6 ------------------------------------------------------------------
#Remove new column
SWFC_16 <- select(SWFC_16,LA_Number:Perc_FT_Temp_Filled_Posts)

#Or using 'the pipe'

SWFC_16 <- SWFC_16 %>% 
  select(LA_Number:Perc_FT_Temp_Filled_Posts)

# sec6_7 ------------------------------------------------------------------
#Change name of Government_Office_Region_Name
LDN_RoE <- LDN_RoE %>% 
  select(Region = Government_Office_Region_Name,
         LDN_RoE)

#Match to SWFC
SWFC_16 <- left_join(SWFC_16,LDN_RoE,by=c("Government.Office.Region.Name"="Region"))

# sec6_8 ------------------------------------------------------------------
#Calculate the number of schools by school type
School_Type_Count <- SWFC_16 %>% #Set the name of the output object and feed in 'the ingredients' - SWFC_16 in this instance
  group_by(School_Type) %>% #Recipe step 1: Specifying the variable we want to group the data by
  summarise(Count_Schools = n()) #Recipe step 2: Define what function we want to apply to the grouped data. Here we are doing a simple count - n(). We also specify the name of the new column, in this instance 'Count_Schools'

# sec6_9 ------------------------------------------------------------------
#Calculate average percentage of qualified teachers per school, grouped by school type
Av_QTS_SchoolType <- SWFC_16 %>% #Set the name of the output object and feed in 'the ingredients' - SWFC_16 in this instance
  group_by(School_Type) %>% #Recipe step 1: Specifying the variable we want to group the data by
  summarise(Ave_Perc_QTS = mean(Perc_QTS_Teachers,na.rm=TRUE))
#Recipe step 2: Calculate the average percentage of qualified teachers for each group. The 'na.rm=TRUE' argument does   not consider rows where the value is NA.

# sec6_10 -----------------------------------------------------------------
#Select all schools in Inner and Outer London
SWFC_16_Lon <- SWFC_16 %>% 
  filter(grepl("London",Government_Office_Region_Name)) #The filtering function is unsurprisingly called 'filter()'
#Here, we're using a function called grepl, which finds all rows where the Government.Office.Region.Name column contains the string 'London'.

# sec6_11 -----------------------------------------------------------------
#Calculate the percentage of schools by school type
School_Type_Count <- SWFC_16 %>%
  group_by(School_Type) %>%
  summarise(Count_Schools = n()) %>% 
  mutate(Perc_Schools = Count_Schools/sum(Count_Schools))

# sec6_12 -----------------------------------------------------------------
#Calculate the percentage of schools by school type
School_Type_Count <- SWFC_16 %>%
  group_by(School_Type) %>%
  summarise(Count_Schools = n()) %>% 
  mutate(Perc_Schools = Count_Schools/sum(Count_Schools)) %>% 
  mutate(Perc_Schools = round(Perc_Schools*100,2)) #Multiply Perc_Schools by 100 to get a percentage, and round 1 decimal place.

# sec7_1 ------------------------------------------------------------------
ggplot(dataset,aes(x,y)) + 
  geom_*() +
  coords_*() +
  styles() +
  styles()

# sec7_2 ------------------------------------------------------------------
ggplot(SWFC_16,aes(Tot_Workforce_HC,Tot_Teachers_HC)) +
  geom_point()

# activity7_4 -------------------------------------------------------------

ggplot(swfc_16 %>% 
         group_by(Government_Office_Region_Name) %>% 
         summarise(ave_pay = mean(Mean_Gross_Salary_All_Teachers_Sterling,na.rm=TRUE),
                   ave_allowance = mean(Perc_Receive_Allowance_Qual_Classroom_Teachers,na.rm=TRUE,),
                   num_schools = n()),
       aes(ave_pay,ave_allowance,size=num_schools,col=Government_Office_Region_Name)) +
  geom_point()

# sec8_1 ------------------------------------------------------------------
min(SWFC_16$Pupil_Teacher_Ratio)
max(SWFC_16$Pupil_Teacher_Ratio)

# sec8_2 ------------------------------------------------------------------
SWFC_16 %>% 
  select(Pupil_Teacher_Ratio
  ) %>% 
  min(ARGUMENT_IN_HERE
  )

SWFC_16 %>%
  select(Pupil_Teacher_Ratio
  ) %>%
  max(ARGUMENT_IN_HERE
  )

# sec8_3 ------------------------------------------------------------------
#The mean of the total school workforce for primary schools
SWFC_16 %>%
  filter(School_Phase == 'Primary') %>% 
  group_by(School_Phase) %>% 
  summarise(Ave = mean(Tot_Workforce_HC,na.rm=TRUE))

# sec8_4 ------------------------------------------------------------------
#The median of the total school workforce for primary schools
SWFC_16 %>%
  filter(School_Type == 'Academies') %>% 
  group_by(School_Type) %>% 
  summarise(Ave = median(Tot_Workforce_HC,na.rm=TRUE))

# sec8_5 ------------------------------------------------------------------
#Base R
cor(SWFC_16[,c(15,16)])

#Pipes
SWFC_16 %>% select(StatutoryLowAge,StatutoryHighAge)%>% cor()

# sec8_6 ------------------------------------------------------------------
t.test(SWFC_16 %>% filter(School_Phase == "Primary"
                          ) %>% select(Tot_Workforce_HC),
       mu = mean(SWFC_16$Tot_Workforce_HC,na.rm=TRUE),
       alternative = "less")
t.test(SWFC_16[SWFC_16$School_Phase == "Primary",17],
       mu = mean(SWFC_16$Tot_Workforce_HC,na.rm=TRUE),
       alternative = "less")

# sec8_7 ------------------------------------------------------------------
t.test(SWFC_16 %>% filter(School_Phase == "Primary") %>% select(Tot_Workforce_HC),
mu = mean(SWFC_16$Tot_Workforce_HC,na.rm=TRUE),
alternative = "less")

# sec8_8 ------------------------------------------------------------------
t.test(Perc_Male_Teachers ~ School_Phase, 
       data = (SWFC_16 %>% filter(School_Phase == "Primary" |
                                    School_Phase == "All Through") %>% 
                 select(School_Phase,
                        Perc_Male_Teachers)))

# sec8_9 ------------------------------------------------------------------
t.test(Perc_Male_Teachers ~ School_Phase, 
       data = (SWFC_16 %>% filter(School_Phase == "Primary" |
                                    School_Phase == "All Through") %>% 
                 select(School_Phase,
                        Perc_Male_Teachers)))

# sec9_1 ------------------------------------------------------------------
my_func <- function(x){
  x*100
}
rm(list = ls())


#Generating the date sequences
date <- as.Date("oct29,2021","%b%d,%Y")
col1_temp <- seq(date,by="day",length.out=70)
col1 <- format(col1_temp,"%b%d,%Y")
str(col1)

# Getting weekdays name
library(lubridate)
col2 <- wday(col1_temp,label = T)
col2
#Generating the Diet(1 ->- 5 )
set.seed(99999)
col3 <- round(runif(70,1,5),0)
col3

#generating meditation
set.seed(999)
col4 <- rbinom(70,1,prob = 0.75)
col4
col4 <- ifelse(col4==1,"Yes","No")

#generating mindset
set.seed(925)
col5 <- rbinom(70,1,prob = 0.75)
col5
col5 <- ifelse(col4==1,"Yes","No")

#genearating visualization
set.seed(745)
col6 <- rbinom(70,1,prob = 0.75)
col6
col6 <- ifelse(col4==1,"Yes","No")


#genearating exercise
set.seed(526)
col7 <- rbinom(70,1,prob = 0.75)
col7
col7 <- ifelse(col4==1,"Yes","No")

#genearating reading
set.seed(900)
col8 <- rbinom(70,1,prob = 0.75)
col8
col8 <- ifelse(col4==1,"Yes","No")

#genearating touch typing
set.seed(458)
col9 <- rbinom(70,1,prob = 0.75)
col9
col9 <- ifelse(col4==1,"Yes","No")

#genearating schedule tommorrow
set.seed(500)
col10 <- rbinom(70,1,prob = 0.75)
col10
col10 <- ifelse(col4==1,"Yes","No")

#generating % of day on schedule
set.seed(421)
col11 <- round(runif(70,0,1),1)
col11

#generating % of planned output completed
set.seed(415)
col12 <- round(runif(70,0,1),1)
col12

#generating hour sleep time
set.seed(856)
col13 <- round(runif(70,10,13),0)
col13

#generating the minutes time sleep
set.seed(78)
col14 <- rbinom(70,80,0.5)
col14

#generating awake hour
set.seed(86)
col15 <- round(runif(70,5,7),0)
col15

#generating the minutes time awake
set.seed(98)
col16 <- rbinom(70,80,0.5)
col16

#generating the health condition (1-<- 5)
set.seed(41)
col17 <- round(runif(70,1,5),0)
col17

#generating the water consumption
set.seed(44)
col18 <- round(runif(70,1.5,3.4),1)
col18

#generating the skin condition (1-<- 5)
set.seed(41)
col19 <- round(runif(70,1,5),0)
col19

#generating the posture & pain condition (1-<- 5)
set.seed(41)
col20 <- round(runif(70,1,5),0)
col20

#generating personal hygiene
set.seed(25)
col21 <- rbinom(70,1,prob = 0.75)
col21
col21 <- ifelse(col4==1,"Yes","No")

#generating database update
set.seed(22)
col22 <- rbinom(70,1,prob = 0.75)
col22
col22 <- ifelse(col4==1,"Yes","No")

#generating the positivity (1-<- 5)
set.seed(456)
col23 <- round(runif(70,1,5),0)
col23

#generating morning prayer
set.seed(20)
col24 <- rbinom(70,1,prob = 0.75)
col24
col24 <- ifelse(col4==1,"Yes","No")


#Generating the data.frame
df2 <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10,
                 col11,col12,col13,col14,col15,col16,col17,col18,col19,
                 col20,col21,col22,col23,col24,check.names = F)
head(df2)
library(readxl)
df1 <- read_excel("C:\\Users\\BIDIT\\OneDrive\\Documents\\Datasets\\Bidit_Life Data & Habit Tracking.xlsx",range="A1:Y44")
head(df1)

dim(df2)

df1 <- df1[,-13]
dim(df1)
colnames(df2)=colnames(df1)

df <- rbind(df1,df2)
dim(df)
set.seed(562653)
df$Phone_Time <- round(runif(113,0,5),1)

head(df)
df$Phone_Time
set.seed(5475)
df$Workout_time <- round(runif(113,0,2),1)
df$Workout_time



df$Focus_Level <- rbinom(113,5,0.6)
df$Focus_Level
df
#write.csv(df,"Bidit_Life_data_tracking.csv")

# Rough work
# Libraries
library(ggplot2)
library(dplyr)

# Dummy data
df$`On This Day` <-as.Date(df$`On This Day`,"%b%d,%Y")
# Most basic bubble plot
p <- ggplot(df, aes(x=`On This Day`, y=Phone_Time)) +
  geom_line() + 
  xlab("")
p
p4 <- table(df$Focus_Level)
p2 <- as.factor(df$Focus_Level)
barplot(p4)
p4

install.packages("margrittr")
ggplot(df)+geom_bar(mapping = aes(x=Focus.Level..1.worst.5.best.),y=Meditation,position ="stack" )

q <- table(df$Focus.Level..1.worst.5.best.,df$Meditation)
q_prop <- round(prop.table(q),2)
p <- addmargins(q_prop)
p 
z <- data.frame(q_prop)
ggplot(z, aes(fill=Var1, y=Freq, x=Var2)) +
  geom_bar(position="dodge", stat="identity")+theme_light() + geom_text(aes(label = Freq), fontface = "bold", vjust = 1.5,
                                                                        position = position_dodge(.9), size = 4)


library(plotly)

fig <- plot_ly()
# Add traces
fig <- fig %>% add_trace(x = df$On.This.Day, y = df$Meditation, name = "yaxis data", mode = "lines+markers", type = "scatter")

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "<b>secondary</b> yaxis title")

fig <- fig %>% add_trace(x = df$On.This.Day, y = df$Focus.Level..1.worst.5.best., name = "yaxis 2 data", yaxis = "y2", mode = "lines+markers", type = "scatter")

# Set figure title, x and y-axes titles
fig <- fig %>% layout(
  title = "Double Y Axis Example", yaxis2 = ay,
  xaxis = list(title="xaxis title "),
  yaxis = list(title="<b>primary</b> yaxis title")
)%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  )

fig

chisq.test(p)$expected

set.seed(90)
df$Focus<- round(sample(rgamma(200,50,rate=2),113),0)
dim(df)
df[which(df$Meditation=="Yes" & df$Exercise=="Yes"),]

p <- ggplot(df, aes(x = On.This.Day, y =Focus, group = Meditation)) +
  geom_point(aes(color = Meditation))+
  geom_line(aes(color = Meditation))+
  ggtitle("Meditation and Focus Time")

ggplotly(p)

table(df$Meditation)
df_cond_2 <- df[df$Meditation=="Yes" & df$Focus >25,]
df_cond_1 <- df[df$Focus >25,]
no_meditation <- df_cond_1[,c(1,27)]
dim(no_meditation)
addmargins(table(df_cond_1$Meditation,df_cond_1$Focus))
set.seed(90)
df$Meditation_time <- rbinom(113,116,0.1)
df$Meditation_time
df$Reading_time <- rbinom(113,125,0.2)
df$Reading_time
plot(df$Reading_time,df$Focus)



df$Meditation
levels(df$Meditation) = c("1","0")
df$Meditation
levels(df$Meditation) = c("Yes","No")


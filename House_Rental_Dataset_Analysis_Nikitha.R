#reading file 

Asn_data = read.csv("C:\\Users\\nikit\\Desktop\\YEAR 2\\Data analysis\\Assignment\\House_Rent_Dataset.csv", header = TRUE, sep= ',')
Asn_data

View(Asn_data)

names(Asn_data) = c("Date_Posted", "rooms", "rent_fee", "sqfeet", "levels", "area", "locale", "city_india", "furnishing", "tenant", "washrooms", "contact_person")
Asn_data

#Installation of the necessary packages
install.packages("ggplot2")
install.packages("plotrix")
install.packages("lessR")
install.packages("tidyverse")
library(ggplot2)
library(plotrix)
library(lessR)
library(tidyverse)


#Data Exploration
#structure
str(Asn_data)


#number of rows and columns
dim(Asn_data)

#Column names
names(Asn_data)

#Summary
summary(Asn_data)


#Data Cleaning 
#check missing value
sum(is.na(Asn_data))

#removing duplicated numbers 
nrow(Asn_data)
Asn_data[!duplicated(Asn_data),]
nrow(Asn_data)


#pre-processing
#print first line
print(head(Asn_data,1))


#to check the class
class(Asn_data$rooms)


#rent fee data
max(Asn_data$rent_fee) #3500000
min(Asn_data$rent_fee) #1200
summary(Asn_data$rent_fee)
plot(Asn_data$rent_fee, type = "l", col = "pink")


#sq feet data
max(Asn_data$sqfeet)#8000
min(Asn_data$sqfeet) #10
summary(Asn_data$sqfeet)
plot(Asn_data$sqfeet, type = "l", col = "pink")


#rooms data
max(Asn_data$rooms) #6
min(Asn_data$rooms) #1
summary(Asn_data$rooms)



## Question 1 - Which city has the cheapest accommodation for families with a preference for carpet type? ########
 
#Analysis 1.1
############################ Average Rent for Furnished Houses with Carpet Type Area for Family Only #####

g1=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
g2=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
g3=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
g4=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
g5=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
g6=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished")&(Asn_data$area=="Carpet Area"),]


r1=mean(g1$rent_fee)
r2=mean(g2$rent_fee)
r3=mean(g3$rent_fee)
r4=mean(g4$rent_fee)
r5=mean(g5$rent_fee)
r6=mean(g6$rent_fee)

city_name=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number1=c(floor(r1),floor(r2),floor(r3),floor(r6),floor(r4),floor(r5))
data <- data.frame( City=city_name ,Houses=number1)
#Graph 
ggplot(data, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="cyan")+
  ylab("Rent in Rs")+geom_text(aes(label=Houses))+
  ggtitle("Comparison of Average Rent for Furnished Houses with Carpet Type Area for Family in Each city")+
  theme_minimal()




#Analysis 1.2
############################  Average Rent for Semi-furnished Houses with Carpet Type Area for Family Only #####

g13=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
g23=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
g33=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
g43=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
g53=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
g63=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished")&(Asn_data$area=="Carpet Area"),]
r13=mean(g13$rent_fee)
r23=mean(g23$rent_fee)
r33=mean(g33$rent_fee)
r43=mean(g43$rent_fee)
r53=mean(g53$rent_fee)
r63=mean(g63$rent_fee)

city_name3=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number13=c(floor(r13),floor(r23),floor(r33),floor(r63),floor(r43),floor(r53))
data <- data.frame( City=city_name3 ,Houses=number13)

#Graph 1
plot(number13,type="o",xlab="City",ylab="Rent Price",main="Comparison of Average Rent for semi-Furnished Houses with Carpet Type Area for Family Only",col="orange")
plot(number13,type="l",xlab="City",ylab="Rent Price",main="Comparison of Average Rent for semi-Furnished Houses with Carpet Type Area for Family Only",col="maroon")
plot(number13,type="p",xlab="City",ylab="Rent Price",main="Comparison of Average Rent for semi-Furnished Houses with Carpet Type Area for Family Only",col="blue")

#Graph 2
ggplot(data, aes(x=City, y=Houses)) + geom_bar(stat = "identity",width=0.5,color="red",fill="orange")+ylab("Rent in RM")+geom_text(aes(label=Houses))+ggtitle("Comparison of Average Rent for semi-Furnished Houses with Carpet Type Area for Family in Each city")+theme_classic()


#Analysis 1.3
###########################################################
##Comparison for cheapest rent option available with carpet types area for family in each city###########
g11=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Carpet Area"),]
g21=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Carpet Area"),]
g31=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Carpet Area"),]
g41=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Carpet Area"),]
g51=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Carpet Area"),]
g61=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Carpet Area"),]
r11=min(g11$rent_fee)
r21=min(g21$rent_fee)
r31=min(g31$rent_fee)
r41=min(g41$rent_fee)
r51=min(g51$rent_fee)
r61=min(g61$rent_fee)

city_name1=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number11=c(floor(r11),floor(r21),floor(r31),floor(r61),floor(r41),floor(r51))
data1 <- data.frame( City=city_name1 ,Houses=number11)


#Graph pie
pie(number11,radius=1,main="Comparison of Cheapest Rent Option Available with Carpet Type Area for Only Family in Each City",
    col=c("green","blue","red","yellow","grey","pink"),
    clockwise=TRUE, labels = paste0(number11))
legend("bottomright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("green","blue","red","yellow","grey","pink"))

#Graph 3D pie
pie3D(number11,main="Comparision Cheapest Rent Option Available with Carpet Type Area for Only Family in Each City",
      labels=city_name1,explode=.19)






ggplot(data1, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "purple", alpha = 0.6)+
  geom_point(shape = 21, color = "navy", fill = "powderblue", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "royalblue4", size = 5)



#Analysis 1.4
###########################################################
##Comparison for most expensive rent option available with carpet types area for family in each city###########
r12=max(g11$rent_fee)
r22=max(g21$rent_fee)
r32=max(g31$rent_fee)
r42=max(g41$rent_fee)
r52=max(g51$rent_fee)
r62=max(g61$rent_fee)

city_name2=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
number12=c(floor(r12),floor(r22),floor(r32),floor(r62),floor(r42),floor(r52))
data2 <- data.frame( City=city_name2 ,Houses=number12)


#Graph 
pie(number12,radius=1,main="Comparison of Most expensive Rent Option Available with Carpet Type Area for Only Family in Each City",
    col=c("lightgreen","lightblue","red","yellow","grey","lightpink"),
    clockwise=TRUE, labels = paste0(number12))
legend("bottomright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("lightgreen","lightblue","red","yellow","grey","lightpink"))
#Graph 
pie3D(number12,main="Comparision most expensive Rent Option Available with Carpet Type Area for Only Family in Each City",
      labels=city_name,explode=.19)




#Question 2 - Which city has the cheapest accommodation for families with a preference for Super area type? ###

#Analysis 2.1
############################ Average Rent for Furnished Houses with Super Type Area for Family Only #####

k1=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
k2=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
k3=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
k4=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
k5=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
k6=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Furnished")&(Asn_data$area=="Super Area"),]
t1=mean(k1$rent_fee)
t2=mean(k2$rent_fee)
t3=mean(k3$rent_fee)
t4=mean(k4$rent_fee)
t5=mean(k5$rent_fee)
t6=mean(k6$rent_fee)

city_nameq=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberq=c(floor(t1),floor(t2),floor(t3),floor(t6),floor(t4),floor(t5))
data <- data.frame( City=city_nameq ,Houses=numberq)
#Graph 2
ggplot(data, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="green")+
  ylab("Rent in RM")+geom_text(aes(label=Houses))+
  ggtitle("Comparison of Average Rent for Furnished Houses with Super Type Area for Family in Each city")+
  theme_dark()





#Analysis 2.2
############################ Average Rent for UnFurnished Houses with Super Type Area for Family Only #####

k11=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
k21=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
k31=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
k41=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
k51=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
k61=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Unfurnished")&(Asn_data$area=="Super Area"),]
t11=mean(k11$rent_fee)
t21=mean(k21$rent_fee)
t31=mean(k31$rent_fee)
t41=mean(k41$rent_fee)
t51=mean(k51$rent_fee)
t61=mean(k61$rent_fee)

city_nameq1=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberq1=c(floor(t11),floor(t21),floor(t31),floor(t61),floor(t41),floor(t51))
data3 <- data.frame( City=city_nameq1 ,Houses=numberq1)
#Graph
ggplot(data3, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "red", alpha = 0.6)+
  geom_point(shape = 21, color = "navy", fill = "powderblue", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "red", size = 5)+
  ggtitle("Comparison of Average Rent for unfurnished Houses with Super Type Area for Family in Each city")+
  theme_grey()



#Analysis 2.3
############################ Average Rent for Semi-Furnished Houses with Super Type Area for Family Only #####
k12=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
k22=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
k32=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
k42=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
k52=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
k62=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")&(Asn_data$furnishing=="Semi-Furnished")&(Asn_data$area=="Super Area"),]
t12=mean(k12$rent_fee)
t22=mean(k22$rent_fee)
t32=mean(k32$rent_fee)
t42=mean(k42$rent_fee)
t52=mean(k52$rent_fee)
t62=mean(k62$rent_fee)

city_nameq2=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberq2=c(floor(t12),floor(t22),floor(t32),floor(t62),floor(t42),floor(t52))
data4 <- data.frame( City=city_nameq2 ,Houses=numberq2)
#Graph
ggplot(data4, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "orange", alpha = 1)+
  geom_point(shape = 21, color = "navy", fill = "powderblue", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "navyblue", size = 5)+
  ggtitle("Comparison of Average Rent for Semi-furnished Houses with Super Type Area for Family in Each city")+
  theme_minimal()





#Analysis 2.4
##Comparison for cheapest rent option available with Super types area for family in each city###########
w1=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Super Area"),]
w2=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Super Area"),]
w3=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Super Area"),]
w4=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Super Area"),]
w5=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Super Area"),]
w6=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")  &(Asn_data$area=="Super Area"),]
e1=min(w1$rent_fee)
e2=min(w2$rent_fee)
e3=min(w3$rent_fee)
e4=min(w4$rent_fee)
e5=min(w5$rent_fee)
e6=min(w6$rent_fee)

city_namew=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberw=c(floor(e1),floor(e2),floor(e3),floor(e6),floor(e4),floor(e5))
data <- data.frame( City=city_namew ,Houses=numberw)

#Graph 
pie(numberw,radius=1,main="Comparison of cheapest Rent Option Available with Super Type Area for Only Family in Each City",
    col=c("lightgreen","lightblue","red","yellow","grey","lightpink"),
    clockwise=TRUE, labels = paste0(numberw))
legend("topright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("lightgreen","lightblue","red","yellow","grey","lightpink"))
#Graph 
pie3D(number12,main="Comparison of cheapest Rent Option Available with Super Type Area for Only Family in Each City",
      labels=city_name,explode=.5)






#Analysis 2.5
##Comparison for most expensive rent option available with Super types area for family in each city###########
e12=max(w1$rent_fee)
e22=max(w2$rent_fee)
e32=max(w3$rent_fee)
e42=max(w4$rent_fee)
e52=max(w5$rent_fee)
e62=max(w6$rent_fee)

city_namee=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbere=c(floor(e12),floor(e22),floor(e32),floor(e62),floor(e42),floor(e52))
data5 <- data.frame( City=city_namee ,Houses=numbere)

#Graph 
pie(numbere,radius=1,main="Comparison of most expensive Rent Option Available with Super Type Area for Only Family in Each City",
    col=c("lightgreen","navyblue","red","orange","purple","lightpink"),
    clockwise=TRUE, labels = paste0(numbere))
legend("bottomright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("lightgreen","navyblue","red","orange","purple","lightpink"))
#Graph 
pie3D(numbere,main="Comparison of most expensive Rent Option Available with Super Type Area for Only Family in Each City",
      labels=city_name,explode=.5)


## Question 3 - What are the different characteristics of  the homes do the families prefer? #######

############## characteristics of houses preferred by families ##########################
#Analysis 3.1
#What type of rooms are the families living in
Asn_data %>%
  select(tenant,rooms,city_india, furnishing) %>%
  filter(tenant == "Family") %>%
  ggplot(aes(city_india,rooms, size = 1, color = rooms)) +
  geom_line() + 
  facet_wrap(~furnishing)
labs(title = "tenants living in unfurnished houses in different cities")



#Analysis 3.2
#how much rent are the families paying for different furnishing of the houses 
Asn_data %>%
  select(tenant,rooms,city_india, furnishing, rent_fee) %>%
  filter(tenant == "Family") %>%
  ggplot(aes(city_india,rent_fee, size = 0.5, color = rooms)) +
  geom_point(type = "o") + 
  facet_wrap(~furnishing)
ggtitle(title = "tenants living in unfurnished houses in different cities")

#Analysis 3.3
#the sq range of the different type of furnished houses families live in
Asn_data %>%
  select(tenant, furnishing, sqfeet) %>%
  filter(tenant == "Family") %>%
  ggplot(aes(sqfeet,furnishing, color = furnishing)) +
  geom_boxplot() 


#Analysis 3.4
#the number of different bhk that each family stays in, in each city
Asn_data %>%
  select(rooms, city_india, tenant) %>%
  filter(tenant == "Family") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "Maroon") +
  facet_wrap(~city_india)


#Analysis 3.5
# the number of rooms families are staying in depending on the furnished type in each city
Asn_data %>%
  select(rooms, city_india, tenant, furnishing) %>%
  filter(tenant == "Family", furnishing == "Furnished") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "pink") +
  facet_wrap(~city_india)  

#Analysis 3.6
#the number of rooms families are staying in depending on the unfurnished type in each city

Asn_data %>%
  select(rooms, city_india, tenant, furnishing) %>%
  filter(tenant == "Family", furnishing == "Unfurnished") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "blue") +
  facet_wrap(~city_india)  

#Analysis 3.7
#the number of rooms families are staying in depending on the semi-furnished type in each city
Asn_data %>%
  select(rooms, city_india, tenant, furnishing) %>%
  filter(tenant == "Family", furnishing == "Semi-Furnished") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "orange") +
  facet_wrap(~city_india) 


### Question 4 - Which city has the lowest Square feet in the houses that families stay in different cities? ###

#Analysis 4.1

######## the Average sq. feet for furnished  that Families stay in, in each city ##############

b1=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Furnished") ,]
b2=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Furnished") ,]
b3=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")  &(Asn_data$furnishing=="Furnished") ,]
b4=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Furnished") ,]
b5=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Furnished") ,]
b6=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Furnished") ,]
x1=mean(b1$sqfeet)
x2=mean(b2$sqfeet)
x3=mean(b3$sqfeet)
x4=mean(b4$sqfeet)
x5=mean(b5$sqfeet)
x6=mean(b6$sqfeet)
city_nameb=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberb=c(floor(x1),floor(x2),floor(x3),floor(x4),floor(x5),floor(x6))
data21 <- data.frame( City=city_nameb ,Houses=numberb)

#Graph
ggplot(data21, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "red", alpha = 0.6)+
  geom_point(shape = 21, color = "navy", fill = "powderblue", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "red", size = 5)+
  ggtitle("Comparison of Average square feet for furnished Houses for Family in Each city")+
  theme_grey()






#Analysis 4.2

######## The Average sq. feet for unfurnished  that Families stay in, in each city ##############
b11=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Unfurnished") ,]
b21=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Unfurnished") ,]
b31=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")  &(Asn_data$furnishing=="Unfurnished") ,]
b41=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Unfurnished") ,]
b51=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Unfurnished") ,]
b61=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Unfurnished") ,]
x11=mean(b11$sqfeet)
x21=mean(b21$sqfeet)
x31=mean(b31$sqfeet)
x41=mean(b41$sqfeet)
x51=mean(b51$sqfeet)
x61=mean(b61$sqfeet)

city_nameb1=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberb1=c(floor(x11),floor(x21),floor(x31),floor(x41),floor(x51),floor(x61))
data22 <- data.frame( City=city_nameb1 ,Houses=numberb1)

#Graph 2
ggplot(data, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="orange")+
  ylab("Sqfeet")+geom_text(aes(label=Houses))+
  ggtitle("Comparing of the Average sqfeet for unFurnished Houses  for Families Only")+
  theme_grey()






#Analysis 4.3
######## The Average sq. feet for semi-furnished  that Families stay in, in each city ##############

b12=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Semi-Furnished") ,]
b22=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Semi-Furnished") ,]
b32=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family")  &(Asn_data$furnishing=="Semi-Furnished") ,]
b42=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Semi-Furnished") ,]
b52=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Semi-Furnished") ,]
b62=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family") &(Asn_data$furnishing=="Semi-Furnished") ,]
x12=mean(b12$sqfeet)
x22=mean(b22$sqfeet)
x32=mean(b32$sqfeet)
x42=mean(b42$sqfeet)
x52=mean(b52$sqfeet)
x62=mean(b62$sqfeet)

city_nameb2=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberb2=c(floor(x12),floor(x22),floor(x32),floor(x42),floor(x52),floor(x62))
data24 <- data.frame( City=city_nameb2 ,Houses=numberb2)
#Graph 
ggplot(data24, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="mistyrose")+
  ylab("Sqfeet")+
  geom_text(aes(label=Houses))+
  ggtitle("Comparing of the Average sqfeet for Semi-Furnished Houses  for Families Only")+
  theme_minimal()






#Analysis 4.4
########### Comparison of the Smallest sqfeet Option Available  for Only families in Each City ####################
b13=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Family")  ,]
b23=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Family") ,]
b33=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Family") ,]
b43=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Family") ,]
b53=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Family")  ,]
b63=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Family")  ,]

x13=min(b13$sqfeet)
x23=min(b23$sqfeet)
x33=min(b33$sqfeet)
x43=min(b43$sqfeet)
x53=min(b53$sqfeet)
x63=min(b63$sqfeet)

city_nameb3=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberb3=c(floor(x13),floor(x23),floor(x33),floor(x43),floor(x53),floor(x63))
data25 <- data.frame( City=city_nameb3 ,Houses=numberb3)


#Graph 
pie(numberb3,radius=1,main="Comparison of the Smallest sqfeet Option Available  for Only families in Each City ",
    col=c("lightgreen","lightblue","red","yellow","grey","lightpink"),
    clockwise=TRUE, labels = paste0(numberb3))
legend("topright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("lightgreen","lightblue","red","yellow","grey","lightpink"))
#Graph 
pie3D(numberb3,main="Comparison of the Smallest sqfeet Option Available  for Only families in Each City",
      labels=city_nameb3,explode=.5)






#Analysis 4.5
########### Comparison of the Largest sqfeet Option Available  for Only families in Each City ####################
x14=max(b13$sqfeet)
x24=max(b23$sqfeet)
x34=max(b33$sqfeet)
x44=max(b43$sqfeet)
x54=max(b53$sqfeet)
x64=max(b63$sqfeet)

city_nameb4=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberb4=c(floor(x14),floor(x24),floor(x34),floor(x44),floor(x54),floor(x64))
data26 <- data.frame( City=city_nameb4 ,Houses=numberb4)


#Graph 
pie(numberb4,radius=1,main="Comparison of the Largest sqfeet Option Available  for Only families in Each City ",
    col= c("green","blue","red","yellow","orange","pink"),
    clockwise=TRUE, labels = paste0(numberb4))
legend("topright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("green","blue","red","yellow","orange","pink"))
#Graph 
pie3D(numberb4,main="Comparison of the Largest sqfeet Option Available  for Only families in Each City",
      labels=city_nameb4,explode=.5)




##Question 5 - What are the total number of postings? ###

#Analysis 5.1
###### what is the total number of postings taking place in each month? ######################
month_4 = Asn_data$Date_Posted
Date_4 = as.POSIXct(month_4, format = "%m/%d/%y")
Date_4 = format(Date_4, format = "%m")
Total_month_4 = as.data.frame(table(Date_4))
names(Total_month_4) = c("Month", "TotalNumbers")
view(Total_month_4)

graph = ggplot(Total_month_4, aes(x = Month, y = TotalNumbers, group = 1)) +
  geom_line(color = "purple", alpha = 0.6) +
  geom_point(shape = 21, color = "navy", fill = "pink", size = 5) +
  geom_text(aes(label = TotalNumbers), hjust = 0, nudge_x = 0.12, fontface = 4, color = "black", size = 5) +
  labs(x = "Month", y = "Total Number of posts", title = "Total Number of post in Each Month") +
  theme_grey()
graph


#Analysis 5.2

###### what is the total number of postings taking place in each day? ######################

day_41 = Asn_data$Date_Posted
Date_41 = as.POSIXct(day_41, format = "%m/%d/%y")
Date_41 = format(Date_41, format = "%d")
Total_day_41 = as.data.frame(table(Date_41))
names(Total_day_41) = c("Day", "TotalNumbers")
view(Total_day_41)

graph1 = ggplot(Total_day_41, aes(x = Day, y = TotalNumbers, group = 1)) +
  geom_line(color = "yellow", alpha = 0.6) +
  geom_point(shape = 21, color = "navy", fill = "pink", size = 5) +
  geom_text(aes(label = TotalNumbers), hjust = 0, nudge_x = 0.12, fontface = 4, color = "white", size = 5) +
  labs(x = "Month", y = "Total Number of posts", title = "Total Number of post in Each Day") +
  theme_dark()
graph1


##Question 6 - Which city has the most expensive accommodation for bachelors with a preference for Super area type? #########

#Analysis 6.1

############################ Average Rent for Furnished Houses with Super Type Area for Bachelors Only #####
o1=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
o2=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
o3=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
o4=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
o5=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Super Area"),]
o6=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished")&(Asn_data$area=="Super Area"),]

u1=mean(o1$rent_fee)
u2=mean(o2$rent_fee)
u3=mean(o3$rent_fee)
u4=mean(o4$rent_fee)
u5=mean(o5$rent_fee)
u6=mean(o6$rent_fee)

city_nameo=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbero=c(floor(u1),floor(u2),floor(u3),floor(u6),floor(u4),floor(u5))
data30 <- data.frame( City=city_nameo ,Houses=numbero)
#Graph 
ggplot(data30, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="green")+
  ylab("Rent in Rs")+
  geom_text(aes(label=Houses))+
  ggtitle("Comparing of the Average Rent for Furnished Houses with Super Type Area for Bachelors Only")+
  theme_dark()





#Analysis 6.2
############################ Average Rent for unFurnished Houses with Super Type Area for Bachelors Only #####
o11=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
o21=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
o31=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
o41=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
o51=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Super Area"),]
o61=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished")&(Asn_data$area=="Super Area"),]

u11=mean(o11$rent_fee)
u21=mean(o21$rent_fee)
u31=mean(o31$rent_fee)
u41=mean(o41$rent_fee)
u51=mean(o51$rent_fee)
u61=mean(o61$rent_fee)

city_nameo1=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbero1=c(floor(u11),floor(u21),floor(u31),floor(u61),floor(u41),floor(u51))
data32 <- data.frame( City=city_nameo1 ,Houses=numbero1)
#Graph
ggplot(data32, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "black", alpha = 1)+
  geom_point(shape = 21, color = "navy", fill = "powderblue", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "navyblue", size = 5)+
  ggtitle("Comparison of Average Rent for unfurnished Houses with Super Type Area for Bachelors in Each city")+
  theme_minimal()






#Analysis 6.3
############################ Average Rent for Semi-Furnished Houses with Super Type Area for Bachelors Only #####
o12=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
o22=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
o32=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
o42=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
o52=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Super Area"),]
o62=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished")&(Asn_data$area=="Super Area"),]
u12=mean(o12$rent_fee)
u22=mean(o22$rent_fee)
u32=mean(o32$rent_fee)
u42=mean(o42$rent_fee)
u52=mean(o52$rent_fee)
u62=mean(o62$rent_fee)

city_nameo2=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbero2=c(floor(u12),floor(u22),floor(u32),floor(u62),floor(u42),floor(u52))
data33 <- data.frame( City=city_nameo2 ,Houses=numbero2)
#Graph
ggplot(data33, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "maroon", alpha = 1)+
  geom_point(shape = 21, color = "navy", fill = "powderblue", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "navyblue", size = 5)+
  ggtitle("Comparison of Average Rent for semi Houses with Super Type Area for Bachelors in Each city")+
  theme_grey()




#Analysis 6.4
##Comparison for cheapest rent option available with Super types area for Bachelors in each city###########
o13=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Super Area"),]
o23=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Super Area"),]
o33=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Super Area"),]
o43=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Super Area"),]
o53=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Super Area"),]
o63=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Super Area"),]

u13=min(o13$rent_fee)
u23=min(o23$rent_fee)
u33=min(o33$rent_fee)
u43=min(o43$rent_fee)
u53=min(o53$rent_fee)
u63=min(o63$rent_fee)

city_nameo3=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbero3=c(floor(u13),floor(u23),floor(u33),floor(u63),floor(u43),floor(u53))
data35 <- data.frame( City=city_nameo3 ,Houses=numbero3)

#Graph 
pie(numbero3,radius=1,main="Comparison of cheapest Rent Option Available with Super Type Area for Only Bachelor in Each City",
    col=c("lightgreen","lightblue","red","yellow","grey","lightpink"),
    clockwise=TRUE, labels = paste0(numbero3))
legend("bottomright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("lightgreen","lightblue","red","yellow","grey","lightpink"))
#Graph 
pie3D(numbero3,main="Comparison of cheapest Rent Option Available with Super Type Area for Only Bachelor in Each City",
      labels=city_nameo3,explode=.5)


#Analysis 6.5
##Comparison for most expensive rent option available with Super types area for Bachelors in each city###########
u14=max(o13$rent_fee)
u24=max(o23$rent_fee)
u34=max(o33$rent_fee)
u44=max(o43$rent_fee)
u54=max(o53$rent_fee)
u64=max(o63$rent_fee)

city_nameo4=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbero4=c(floor(u14),floor(u24),floor(u34),floor(u64),floor(u44),floor(u54))
data44 <- data.frame( City=city_nameo4 ,Houses=numbero4)

#Graph 
pie(numbero4,radius=1,main="Comparison of Most expensive Rent Option Available with Super Type Area for Only Bachelor in Each City",
    col=c("lightgreen","lightblue","red","yellow","grey","lightpink"),
    clockwise=TRUE, labels = paste0(numbero4))
legend("topright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("lightgreen","lightblue","red","yellow","grey","lightpink"))
#Graph 
pie3D(numbero4ain="Comparison of Most expensive Rent Option Available with Super Type Area for Only Bachelor in Each City",
      labels=city_nameo3,explode=.5)


###Question 7 - Which city has the most expensive accommodation for bachelors with a preference for carpet area type? #####

#Analysis 7.1

############################ Average Rent for Furnished Houses with Carpet Type Area for Bachelors Only #####
f1=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
f2=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
f3=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
f4=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
f5=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished") &(Asn_data$area=="Carpet Area"),]
f6=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Furnished")&(Asn_data$area=="Carpet Area"),]

g1=mean(f1$rent_fee)
g2=mean(f2$rent_fee)
g3=mean(f3$rent_fee)
g4=mean(f4$rent_fee)
g5=mean(f5$rent_fee)
g6=mean(f6$rent_fee)

city_namef=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberf=c(floor(g1),floor(g2),floor(g3),floor(g6),floor(g4),floor(g5))
data61 <- data.frame( City=city_namef ,Houses=numberf)

#Graph 
ggplot(data61, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="lightblue")+
  ylab("Rent in Rs")+
  geom_text(aes(label=Houses))+
  ggtitle("Comparing of the Average Rent for Furnished Houses with Carpet Type Area for Bachelors Only")+
  theme_dark()







#Analysis 7.2
############################ Average Rent for unFurnished Houses with Carpet Type Area for Bachelors Only #####
f11=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Carpet Area"),]
f21=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Carpet Area"),]
f31=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Carpet Area"),]
f41=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Carpet Area"),]
f51=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished") &(Asn_data$area=="Carpet Area"),]
f61=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Unfurnished")&(Asn_data$area=="Carpet Area"),]

g11=mean(f11$rent_fee)
g21=mean(f21$rent_fee)
g31=mean(f31$rent_fee)
g41=mean(f41$rent_fee)
g51=mean(f51$rent_fee)
g61=mean(f61$rent_fee)

city_namef1=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberf1=c(floor(g11),floor(g21),floor(g31),floor(g61),floor(g41),floor(g51))
data62 <- data.frame( City=city_namef1 ,Houses=numberf1)

#Graph 2
ggplot(data62, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="brown")+
  ylab("Rent in Rs")+
  geom_text(aes(label=Houses))+
  ggtitle("Comparing of the Average Rent for UNFurnished Houses with Carpet Type Area for Bachelors Only")+
  theme_grey()







#Analysis 7.3
############################ Average Rent for Semi-Furnished Houses with Carpet Type Area for Bachelors Only #####
f12=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
f22=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
f32=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
f42=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
f52=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished") &(Asn_data$area=="Carpet Area"),]
f62=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")&(Asn_data$furnishing=="Semi-Furnished")&(Asn_data$area=="Carpet Area"),]

g12=mean(f12$rent_fee)
g22=mean(f22$rent_fee)
g32=mean(f32$rent_fee)
g42=mean(f42$rent_fee)
g52=mean(f52$rent_fee)
g62=mean(f62$rent_fee)

city_namef2=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberf2=c(floor(g12),floor(g22),floor(g32),floor(g62),floor(g42),floor(g52))
data64 <- data.frame( City=city_namef2 ,Houses=numberf2)

#Graph
ggplot(data64, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "white", alpha = 1)+
  geom_point(shape = 21, color = "navy", fill = "powderblue", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "white", size = 5)+
  ggtitle("Comparison of Average Rent for semi Houses with Super Type Area for Bachelors in Each city")+
  theme_dark()






#Analysis 7.4
##Comparison for most expensive rent option available with Super types area for Bachelors in each city###########
f13=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Carpet Area"),]
f23=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Carpet Area"),]
f33=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Carpet Area"),]
f43=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Carpet Area"),]
f53=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Carpet Area"),]
f63=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors")  &(Asn_data$area=="Carpet Area"),]

g13=max(f13$rent_fee)
g23=max(f23$rent_fee)
g33=max(f33$rent_fee)
g43=max(f43$rent_fee)
g53=max(f53$rent_fee)
g63=max(f63$rent_fee)

city_namef3=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberf3=c(floor(g13),floor(g23),floor(g33),floor(g43),floor(g53),floor(g63))
data67 <- data.frame( City=city_namef3 ,Houses=numberf3)

#Graph 
pie(numberf3,radius=1,main="Comparison of Most expensive Rent Option Available with Super Type Area for Only Bachelor in Each City",
    col=c("lightgreen","lightblue","red","yellow","grey","lightpink"),
    clockwise=TRUE, labels = paste0(numberf3))
legend("topright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("lightgreen","lightblue","red","yellow","grey","lightpink"))
#Graph 
pie3D(numberf3,main="Comparison of Most expensive Rent Option Available with Super Type Area for Only Bachelor in Each City",
      labels=city_nameo3,explode=.5)



#Analysis 7.5

##Comparison for cheapest rent option available with Super types area for Bachelors in each city###########

g14=min(f13$rent_fee)
g24=min(f23$rent_fee)
g34=min(f33$rent_fee)
g44=min(f43$rent_fee)
g54=min(f53$rent_fee)
g64=min(f63$rent_fee)

city_namef4=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numberf4=c(floor(g14),floor(g24),floor(g34),floor(g44),floor(g54),floor(g64))
data <- data.frame( City=city_namef4 ,Houses=numberf4)

#Graph 
pie(numberf4,radius=1,main="Comparison of cheapest Rent Option Available with Super Type Area for Only Bachelor in Each City",
    col=c("purple","cyan","red","yellow","orange","pink"),
    clockwise=TRUE, labels = paste0(numberf4))
legend("topright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("purple","cyan","red","yellow","orange","pink"))
#Graph 
pie3D(numberf4,main="Comparison of cheapest Rent Option Available with Super Type Area for Only Bachelor in Each City",
      labels=city_nameo3,explode=.5)



###Question 8 - What are the different characteristics of the houses do the bachelors prefer? ###########

############## characteristics of houses preferred by Bachelors ##########################
#Analysis 8.1
#What type of rooms are the bachelors living in
Asn_data %>%
  select(tenant,rooms,city_india, furnishing) %>%
  filter(tenant == "Bachelors") %>%
  ggplot(aes(city_india,rooms, size = 1, color = rooms)) +
  geom_line() + 
  facet_wrap(~furnishing)
labs(title = "tenants living in unfurnished houses in different cities")

#Analysis 8.2
#how much rent are the Bachelors paying for different furnishing of the houses 
Asn_data %>%
  select(tenant,rooms,city_india, furnishing, rent_fee) %>%
  filter(tenant == "Bachelors") %>%
  ggplot(aes(city_india,rent_fee, size = 0.5, color = rooms)) +
  geom_point(type = "o") +
  geom_text(aes(label = rooms)) +
  facet_wrap(~furnishing)
labs(title = "rent bachelors are paying for different furnishing")


#Analysis 8.3
#the sq range of the different type of furnished houses Bachelors live in
Asn_data %>%
  select(tenant, furnishing, sqfeet) %>%
  filter(tenant == "Bachelors") %>%
  ggplot(aes(sqfeet,furnishing, color ="white", fill = furnishing)) +
  geom_boxplot() +
  theme_dark()

#Analysis 8.4
#the number of different bhk that each Bachelors stays in, in each city
Asn_data %>%
  select(rooms, city_india, tenant) %>%
  filter(tenant == "Bachelors") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "violet") +
  facet_wrap(~city_india)

#Analysis 8.5
# the number of rooms Bachelors are staying in depending on the furnished type in each city
Asn_data %>%
  select(rooms, city_india, tenant, furnishing) %>%
  filter(tenant == "Bachelors", furnishing == "Furnished") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "red") +
  facet_wrap(~city_india)  

#Analysis 8.6
# the number of rooms Bachelors are staying in depending on the Unfurnished type in each city
Asn_data %>%
  select(rooms, city_india, tenant, furnishing) %>%
  filter(tenant == "Bachelors", furnishing == "Unfurnished") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "brown") +
  facet_wrap(~city_india)  

#Analysis 8.7
# the number of rooms Bachelors are staying in depending on the Semi-furnished type in each city
Asn_data %>%
  select(rooms, city_india, tenant, furnishing) %>%
  filter(tenant == "Bachelors", furnishing == "Semi-Furnished") %>%
  ggplot(aes(rooms)) +
  geom_bar(width = 0.2, color = "yellow", fill = "gold") +
  facet_wrap(~city_india) 


###Question 9 - Which city has the lowest square feet in the houses that bachelors  stay in different cities? #########

#Analysis 9.1

######## the Average sq. feet for furnished  that Bachelors stay in, in each city ##############
y1=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Furnished") ,]
y2=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Furnished") ,]
y3=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")  &(Asn_data$furnishing=="Furnished") ,]
y4=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Furnished") ,]
y5=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Furnished") ,]
y6=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Furnished") ,]

h1=mean(y1$sqfeet)
h2=mean(y2$sqfeet)
h3=mean(y3$sqfeet)
h4=mean(y4$sqfeet)
h5=mean(y5$sqfeet)
h6=mean(y6$sqfeet)

city_namey=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbery=c(floor(h1),floor(h2),floor(h3),floor(h4),floor(h5),floor(h6))
data71 <- data.frame( City=city_namey ,Houses=numbery)

#Graph
ggplot(data71, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "pink", alpha = 1)+
  geom_point(shape = 21, color = "navy", fill = "pink", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "white", size = 5)+
  ggtitle("Comparison of Average square foot for furnished Houses for Bachelors in Each city")+
  theme_dark()







#Analysis 9.2
######## the Average sq. feet for Unfurnished  that Bachelors stay in, in each city ##############
y11=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Unfurnished") ,]
y21=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Unfurnished") ,]
y31=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")  &(Asn_data$furnishing=="Unfurnished") ,]
y41=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Unfurnished") ,]
y51=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Unfurnished") ,]
y61=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Unfurnished") ,]

h11=mean(y11$sqfeet)
h21=mean(y21$sqfeet)
h31=mean(y31$sqfeet)
h41=mean(y41$sqfeet)
h51=mean(y51$sqfeet)
h61=mean(y61$sqfeet)

city_namey1=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbery1=c(floor(h11),floor(h21),floor(h31),floor(h41),floor(h51),floor(h61))
data72 <- data.frame( City=city_namey1 ,Houses=numbery1)
#Graph
ggplot(data72, aes(x=City, y=Houses, group=1)) + 
  geom_line(col = "red", alpha = 1)+
  geom_point(shape = 21, color = "navy", fill = "red", size = 5)+
  geom_text(aes(label = Houses), hjust = 0, nudge_x = 0.12, fontface = 4, color = "white", size = 5)+
  ggtitle("Comparison of Average square foot for unfurnished Houses for Bachelors in Each city")+
  theme_dark()


#Analysis 9.3
######## the Average sq. feet for Semi-furnished  that Bachelors stay in, in each city ##############
y12=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Semi-Furnished") ,]
y22=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Semi-Furnished") ,]
y32=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")  &(Asn_data$furnishing=="Semi-Furnished") ,]
y42=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Semi-Furnished") ,]
y52=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Semi-Furnished") ,]
y62=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors") &(Asn_data$furnishing=="Semi-Furnished") ,]

h12=mean(y12$sqfeet)
h22=mean(y22$sqfeet)
h32=mean(y32$sqfeet)
h42=mean(y42$sqfeet)
h52=mean(y52$sqfeet)
h62=mean(y62$sqfeet)

city_namey2=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbery2=c(floor(h12),floor(h22),floor(h32),floor(h42),floor(h52),floor(h62))
data73 <- data.frame( City=city_namey2 ,Houses=numbery2)

#Graph 
ggplot(data73, aes(x=City, y=Houses)) + 
  geom_bar(stat = "identity",width=0.5,color="red",fill="dark blue")+
  ylab("Sqfeet")+
  geom_text(aes(label=Houses))+
  ggtitle("Comparing of the Average sqfeet for Semi-Furnished Houses  for Bachelors Only")+
  theme_grey()





#Analysis 9.4
########## comparing the smallest sqfeet option available for bachelors ##########################################
y13=Asn_data[Asn_data$city_india=="Hyderabad"&(Asn_data$tenant=="Bachelors") ,]
y23=Asn_data[Asn_data$city_india=="Kolkata"&(Asn_data$tenant=="Bachelors") ,]
y33=Asn_data[Asn_data$city_india=="Mumbai"&(Asn_data$tenant=="Bachelors")   ,]
y43=Asn_data[Asn_data$city_india=="Bangalore"&(Asn_data$tenant=="Bachelors")  ,]
y53=Asn_data[Asn_data$city_india=="Delhi"&(Asn_data$tenant=="Bachelors") ,]
y63=Asn_data[Asn_data$city_india=="Chennai"&(Asn_data$tenant=="Bachelors") ,]

h13=min(y13$sqfeet)
h23=min(y23$sqfeet)
h33=min(y33$sqfeet)
h43=min(y43$sqfeet)
h53=min(y53$sqfeet)
h63=min(y63$sqfeet)

city_namey3=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbery3=c(floor(h13),floor(h23),floor(h33),floor(h43),floor(h53),floor(h63))
data74 <- data.frame( City=city_namey3 ,Houses=numbery3)

#Graph 
pie(numbery3,radius=1,main="Comparison of smallest sqaure foot Option Available for Only Bachelor in Each City",
    col=c("purple","cyan","red","yellow","orange","pink"),
    clockwise=TRUE, labels = paste0(numbery3))
legend("topright", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("purple","cyan","red","yellow","orange","pink"))
#Graph 
pie3D(numbery3,main="Comparison of smallest sqaure foot Option Available for Only Bachelor in Each City",
      labels=city_namey3,explode=.5)



#Analysis 9.5
########## comparing the largest sqfeet option available for bachelors ##########################################
h14=max(y13$sqfeet)
h24=max(y23$sqfeet)
h34=max(y33$sqfeet)
h44=max(y43$sqfeet)
h54=max(y53$sqfeet)
h64=max(y63$sqfeet)


city_namey4=c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi")
numbery4=c(floor(h14),floor(h24),floor(h34),floor(h44),floor(h54),floor(h64))
data <- data.frame( City=city_namey4 ,Houses=numbery4)

#Graph 
pie(numbery4,radius=1,main="Comparison of largest sqaure foot Option Available for Only Bachelor in Each City",
    col=c("purple","brown","red","yellow","orange","pink"),
    clockwise=TRUE, labels = paste0(numbery4))
legend("bottomleft", legend = c("Hyderabad","Kolkata","Mumbai","Chennai","Bangalore","Delhi"), 
       fill = c("purple","brown","red","yellow","orange","pink"))
#Graph 
pie3D(numbery4,main="Comparison of largest sqaure foot Option Available for Only Bachelor in Each City",
      labels=city_namey4,explode=.5)



###Question 10 - What are the different types of characteristic available for renting a house? ###

#Analysis 10.1
#the amount of rental in each state 
ggplot(Asn_data, aes(x = city_india, fill = rooms)) + 
  geom_bar(position = "stack", fill = "yellow")+
  ggtitle("The number of rental")
theme_classic()




#Analysis 10.2
#what are the different areas used based on the number of rooms and sqfeet
Asn_data %>%
  select(sqfeet,rooms,area) %>%
  ggplot(aes(sqfeet,rooms, size = 1, color = rooms)) +
  geom_point() + 
  facet_wrap(~area) +
  labs(title = "areas based on the sqfeet in rooms available")


#Analysis 10.3
#the number of washrooms
qt = nrow(Asn_data[Asn_data$washrooms == 1, ])
wt = nrow(Asn_data[Asn_data$washrooms == 2, ])
et = nrow(Asn_data[Asn_data$washrooms == 3, ])
rt = nrow(Asn_data[Asn_data$washrooms == 4, ])
tt = nrow(Asn_data[Asn_data$washrooms == 5, ])
yt = nrow(Asn_data[Asn_data$washrooms == 6, ])
ut = nrow(Asn_data[Asn_data$washrooms == 7, ])

b = c(qt,wt,et,rt,tt,yt,ut)
l = c(1,2,3,4,5,6,7)
pie_labels_1 <- paste0(round(100*b/sum(b),2), "%")
pie(b,radius=1, main= "Number of washrooms", 
    col = c( "red", "blue", "Yellow", "Pink", "Purple", "Orange", "Grey"), 
    clockwise = TRUE, labels = pie_labels_1)
legend("topleft", legend = c(1,2,3,4,5,6,7), 
       fill = c( "red", "blue", "Yellow", "Pink", "Purple", "Orange", "Grey"))


#Analysis 10.4
#What are the number of rooms in the house?
ggplot(Asn_data, aes(x = rooms)) + 
  geom_histogram(color = "orange", aes(fill = ..count..), binwidth = 0.1) + 
  scale_fill_gradient("count", low = "green", high = "red")


###Question 11-Extra features 

#Analysis 11.1
##density
#most sq feet houses

den <- density(Asn_data$sqfeet)
plot(den, frame = FALSE, col = "blue",main = "Density plot", fill = "cyan")


#Analysis 11.2
######### lollipop ################
#What are the different types of rooms available?
ggplot(Asn_data, aes(x = rooms, y = city_india, label = rooms, color = rooms)) +
  geom_segment(aes(x = 0, y = city_india, xend = rooms, yend = city_india)) +
  geom_point(size = 7)+
  geom_text(nudge_x = 1.5) 


#Analysis 11.3
### strip chart 
## amount of rent paid 
stripchart(Asn_data$rent_fee,
           main="The rent paid",
           xlab="amount paid",
           ylab="rent",
           method="jitter",
           col="orange",
           pch=1)  

#Analysis 11.4
#The amount of carpet areas 
it = nrow(Asn_data[Asn_data$area == "Super Area", ])
is = nrow(Asn_data[Asn_data$area == "Carpet Area", ])
a = c(it,is)
l = c("Super Area", "Carpet Area")
pie_labels <- paste0(round(100*a/sum(a),2), "%")
pie(a,radius=1, main= "Types of areas", col = c( "mistyrose", "lightblue"), clockwise = TRUE, labels = pie_labels)
legend("topleft", legend = c("Super Area", "Carpet Area"), fill = c("mistyrose", "lightblue"))


#Analysis 
#The amount of contact persons 
iy = nrow(Asn_data[Asn_data$contact_person == "Contact Agent", ])
iu = nrow(Asn_data[Asn_data$contact_person == "Contact Owner", ])
a = c(iy,iu)
l = c("Contact Agent", "Contact Owner")
pie_labels <- paste0(round(100*a/sum(a),2), "%")
pie(a,radius=1, main= "Types of contact person", col = c( "lightgreen", "lightblue"), clockwise = TRUE, labels = pie_labels)
legend("topleft", legend = c("Contact Agent", "Contact Owner"), fill = c("lightgreen", "lightblue"))












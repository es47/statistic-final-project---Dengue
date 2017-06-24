# The open data from tainan city.
#ref.url : http://data.tainan.gov.tw/dataset/dengue-dist/resource/bb8d7a88-e038-4cd0-86af-5f13db418928

library(utils)

#data.url = "http://data.tainan.gov.tw/dataset/3ad9da64-0c29-4299-b769-320b57a09be8/resource/7617bfcd-20e2-4f8d-a83b-6f6b479367f9/download/dengue104unicode.csv"

acc_data = read.table(file = "TNdengue104-big5.csv", encoding = "big5", sep = ",", header = TRUE)
head(acc_data)

# Load the libraries we used
library(ggplot2)
library(scales)
library(reshape2)


acc_data.month <- as.data.frame(table(acc_data[,3]))
head(acc_data.month)

acc_data.day <- as.data.frame(table(acc_data[,3]))
head(acc_data.day)


acc_data.loc <- as.data.frame(table(acc_data[,5]))
head(acc_data.loc)


#renamed acc_data.loc's col name to "region" and "sum"
colnames(acc_data.loc) <- c("region", "sum")
#let acc_data.loc sorted by sum and assign to acc_data.loc.sort
acc_data.loc.sort <- acc_data.loc[order(-acc_data.loc$sum),]
#let acc_data.loc.sort leaves only the sum higher than 900
acc_data.loc.sort <- acc_data.loc.sort[acc_data.loc.sort$sum > 900,]
head(acc_data.loc.sort)

#let acc_data.loc.sort's output order be levels lists
acc_data.loc.sort$region = factor(acc_data.loc.sort$region, levels = c("安平區", "安南區", "永康區", "東區", "中西區", "南區", "北區"))

#draw the pie graph
p <- ggplot(acc_data.loc.sort, aes(x=2, y=sum, fill=region)) +
  geom_bar(stat="identity",color="black") +
  ggtitle("2015 Tainan city Dengue  ratio in each region ( >900 )")+
  guides(fill=guide_legend(override.aes=list(colour=NA)))+
  coord_polar(theta = "y", start = 0, direction = -1)+
  theme(axis.ticks=element_blank(),  # the axis ticks
        axis.title=element_blank(),  # the axis labels
        axis.text.y=element_blank(), # the 0.75, 1.00, 1.25 labels
        axis.text.x=element_text(color='black'),
        panel.grid=element_blank())+
  scale_y_continuous(
    breaks = cumsum(acc_data.loc.sort$sum) - acc_data.loc.sort$sum/2,
    label = percent(acc_data.loc.sort$sum/sum(acc_data.loc.sort$sum))
  )
print (p)


####
# Define the function, to process the data in each region.
acc_data.regtime <- function(data){
  ini.df <- data.frame(Date=as.Date(character()),
                       File=character(), 
                       User=character(), 
                       stringsAsFactors=FALSE) 
  
  reg.name <- levels(data[,1])
  for (i in reg.name)
  {
    t8 <- (data[which(data[,1]== i),])
    head(t8)
    t8cm <- cut(t8[,3], breaks=c(1, 3, 6, 9, 12), labels=c("1-3", "3-6", "6-9", "9-12"), right=FALSE)
    t8[,3] <-t8cm

    t8cd <- cut(t8[,2], breaks=c(1, 10, 20, 31), labels=c("1-10", "10-20", "20-31"), right=FALSE)
    t8[,2] <-t8cd
    table(t8[,2])
    
    #classify date
    t8_sub1 <- subset(t8, t8[,2] == "1-10")
    t8_sub2 <- subset(t8, t8[,2] == "10-20")
    t8_sub3 <- subset(t8, t8[,2] == "20-31")
    
    #classify month under date
    t8_mn1 <- subset(t8_sub1, t8_sub1[,3] == "1-3")
    t8_mn2 <- subset(t8_sub1, t8_sub1[,3] == "3-6")
    t8_mn3 <- subset(t8_sub1, t8_sub1[,3] == "6-9")
    t8_mn4 <- subset(t8_sub1, t8_sub1[,3] == "9-12")
    t8_mn5 <- subset(t8_sub2, t8_sub2[,3] == "1-3")
    t8_mn6 <- subset(t8_sub2, t8_sub2[,3] == "3-6")
    t8_mn7 <- subset(t8_sub2, t8_sub2[,3] == "6-9")
    t8_mn8 <- subset(t8_sub2, t8_sub2[,3] == "9-12")
    t8_mn9 <- subset(t8_sub3, t8_sub3[,3] == "1-3")
    t8_mn10 <- subset(t8_sub3, t8_sub3[,3] == "3-6")
    t8_mn11 <- subset(t8_sub3, t8_sub3[,3] == "6-9")
    t8_mn12 <- subset(t8_sub3, t8_sub3[,3] == "9-12")
    
    #combine each classfication
    t8m1 <- c(nrow(t8_mn1), nrow(t8_mn2), nrow(t8_mn3), nrow(t8_mn4))
    t8m2 <- c(nrow(t8_mn5), nrow(t8_mn6), nrow(t8_mn7), nrow(t8_mn8))
    t8m3 <- c(nrow(t8_mn9), nrow(t8_mn10), nrow(t8_mn11), nrow(t8_mn12))
    t8m <- rbind(t8m1, t8m2, t8m3)
    
    colnames(t8m) <- c("1-3", "3-6", "6-9", "9-12")
    rownames(t8m) <- c("1-10", "10-20", "20-31")
    
    #draw the bar plot 
    t8m = as.matrix(t8m)
    is.matrix(t8m)
    barplot(t8m, beside = TRUE, legend = rownames(t8m),
            main = paste("Population of Dengue in ", i), 
            xlab = "Month", ylab = "Population",
            col = c("#FFB3FF", "#FFFF77", "#0066FF"),
            xlim = c(0, 20), ylim = c(0, 1600),
            args.legend=list(x=20, y=1500, bty = "n") 
            )
  }
}

acc_data.hour <- acc_data[,c(5, 4, 3)]

# bar chart
acc_data.regtime(acc_data.hour)



Data = read.table(file = "people104.csv", encoding = "big5", sep = ",", header = TRUE)
head(Data)

Data.sub <- subset(Data, Data[,2] == "臺南市")
head(Data.sub)

Temp <- data.frame(sick)

#function that add new data from acc_data.loc
Data.insert <- function(num)
{
  sick <- c(acc_data.loc[num,2])
  Temp2 <- data.frame(sick)
  Temp <- rbind(Temp, Temp2)
  return(Temp)
}

# Insert the sick people of each region
Temp[1,1] <- c("108")
Temp <- Data.insert(36)
Temp <- Data.insert(12)
Temp <- Data.insert(24)
Temp <- Data.insert(23)
Temp <- Data.insert(19)
Temp <- Data.insert(26)
Temp <- Data.insert(2)
Temp <- Data.insert(7)
Temp <- Data.insert(18)
Temp <- Data.insert(3)
Temp <- Data.insert(17)
Temp <- Data.insert(32)
Temp <- Data.insert(16)
Temp <- Data.insert(1)
Temp <- Data.insert(25)
Temp[17,1] <- c("0")
Temp <- Data.insert(28)
Temp <- Data.insert(27)
Temp <- Data.insert(29)
Temp <- Data.insert(14)
Temp <- Data.insert(4)
Temp <- Data.insert(11)
Temp <- Data.insert(31)
Temp <- Data.insert(21)
Temp <- Data.insert(9)
Temp <- Data.insert(6)
Temp <- Data.insert(34)
Temp <- Data.insert(35)
Temp <- Data.insert(33)
Temp <- Data.insert(10)
Temp <- Data.insert(20)
Temp <- Data.insert(22)
Temp <- Data.insert(8)
Temp <- Data.insert(15)
Temp <- Data.insert(13)
Temp <- Data.insert(5)

head(Temp)

#combine people and dengue
Data.sub <- cbind(Data.sub, Temp)
colnames(Data.sub) <- c("year", "loc", "region", "population", "area", "density", "sick")
rownames(Data.sub) <- c(1 : 37)

#draw the plot
plot (x = Data.sub$density, y = Data.sub$sick, 
      xlab = "density of each region",
      ylab = "population of Dengue in each region",
      main = "Scatter plot about population and density")

#compute correlation between density and Dengue population
Data.sub$sick <- as.numeric(Data.sub$sick)
cor(Data.sub$density, Data.sub$sick)


Acc = read.table(file = "park.csv", encoding = "big5", sep = ",", header = TRUE)
head(Acc)

Acc.region <- as.data.frame(table(Acc[,3]))
head(Acc.region)

acc_data.region <- as.data.frame(table(acc_data[,6]))
head(acc_data.region)

Temp_park <- data.frame(freq = 1 : 146)

#function to add new data from acc_data.region
Data.insert_park <- function(num)
{
  freq <- c(acc_data.region[num, 2])
  return(freq)
}


# Insert the population of dengue in each region
Temp_park[1,] <- C("0")
Temp_park[2,] <- Data.insert_park(6)
Temp_park[3,] <- Data.insert_park(34)
Temp_park[4,] <- Data.insert_park(38)
Temp_park[5,] <- Data.insert_park(41)
Temp_park[6,] <- Data.insert_park(43)
Temp_park[7,] <- Data.insert_park(46)
Temp_park[8,] <- Data.insert_park(48)
Temp_park[9,] <- Data.insert_park(49)
Temp_park[10,] <- Data.insert_park(50)
Temp_park[11,] <- Data.insert_park(53)
Temp_park[12,] <- Data.insert_park(58)
Temp_park[13,] <- Data.insert_park(59)
Temp_park[14,] <- Data.insert_park(61)
Temp_park[15,] <- Data.insert_park(64)
Temp_park[16,] <- Data.insert_park(65)
Temp_park[17,] <- Data.insert_park(68)
Temp_park[18,] <- Data.insert_park(80)
Temp_park[19,] <- Data.insert_park(98)
Temp_park[20,] <- Data.insert_park(103)
Temp_park[21,] <- Data.insert_park(104)
Temp_park[22,] <- Data.insert_park(108)
Temp_park[23,] <- Data.insert_park(112)
Temp_park[24,] <- Data.insert_park(117)
Temp_park[25,] <- Data.insert_park(119)
Temp_park[26,] <- Data.insert_park(123)
Temp_park[27,] <- c("0")
Temp_park[28,] <- Data.insert_park(124)
Temp_park[29,] <- Data.insert_park(125)
Temp_park[30,] <- Data.insert_park(126)
Temp_park[31,] <- Data.insert_park(127)
Temp_park[32,] <- Data.insert_park(145)
Temp_park[33,] <- Data.insert_park(148)
Temp_park[34,] <- Data.insert_park(151)
Temp_park[35,] <- Data.insert_park(159)
Temp_park[36,] <- Data.insert_park(161)
Temp_park[37,] <- Data.insert_park(163)
Temp_park[38,] <- Data.insert_park(173)
Temp_park[39,] <- Data.insert_park(177)
Temp_park[40,] <- Data.insert_park(178)
Temp_park[41,] <- Data.insert_park(183)
Temp_park[42,] <- Data.insert_park(184)
Temp_park[43,] <- c("0")
Temp_park[44,] <- Data.insert_park(187)
Temp_park[45,] <- Data.insert_park(189)
Temp_park[46,] <- Data.insert_park(190)
Temp_park[47,] <- Data.insert_park(194)
Temp_park[48,] <- Data.insert_park(195)
Temp_park[49,] <- Data.insert_park(198)
Temp_park[50,] <- Data.insert_park(201)
Temp_park[51,] <- Data.insert_park(203)
Temp_park[52,] <- Data.insert_park(205)
Temp_park[53,] <- Data.insert_park(206)
Temp_park[54,] <- Data.insert_park(207)
Temp_park[55,] <- Data.insert_park(208)
Temp_park[56,] <- Data.insert_park(209)
Temp_park[57,] <- Data.insert_park(210)
Temp_park[58,] <- Data.insert_park(219)
Temp_park[59,] <- Data.insert_park(224)
Temp_park[60,] <- Data.insert_park(227)
Temp_park[61,] <- Data.insert_park(229)
Temp_park[62,] <- Data.insert_park(230)
Temp_park[63,] <- Data.insert_park(231)
Temp_park[64,] <- Data.insert_park(232)
Temp_park[65,] <- Data.insert_park(238)
Temp_park[66,] <- Data.insert_park(242)
Temp_park[67,] <- Data.insert_park(245)
Temp_park[68,] <- Data.insert_park(246)
Temp_park[69,] <- Data.insert_park(253)
Temp_park[70,] <- Data.insert_park(254)
Temp_park[71,] <- Data.insert_park(256)
Temp_park[72,] <- Data.insert_park(267)
Temp_park[73,] <- c("82.25")
Temp_park[74,] <- Data.insert_park(279)
Temp_park[75,] <- Data.insert_park(280)
Temp_park[76,] <- Data.insert_park(283)
Temp_park[77,] <- Data.insert_park(292)
Temp_park[78,] <- Data.insert_park(293)
Temp_park[79,] <- Data.insert_park(295)
Temp_park[80,] <- Data.insert_park(296)
Temp_park[81,] <- Data.insert_park(298)
Temp_park[82,] <- Data.insert_park(311)
Temp_park[83,] <- Data.insert_park(312)
Temp_park[84,] <- Data.insert_park(315)
Temp_park[85,] <- Data.insert_park(321)
Temp_park[86,] <- Data.insert_park(323)
Temp_park[87,] <- Data.insert_park(324)
Temp_park[88,] <- c("81.25")
Temp_park[89,] <- Data.insert_park(327)
Temp_park[90,] <- Data.insert_park(332)
Temp_park[91,] <- Data.insert_park(338)
Temp_park[92,] <- Data.insert_park(340)
Temp_park[93,] <- Data.insert_park(342)
Temp_park[94,] <- Data.insert_park(344)
Temp_park[95,] <- Data.insert_park(345)
Temp_park[96,] <- Data.insert_park(346)
Temp_park[97,] <- Data.insert_park(350)
Temp_park[98,] <- Data.insert_park(356)
Temp_park[99,] <- Data.insert_park(358)
Temp_park[100,] <- Data.insert_park(360)
Temp_park[101,] <- Data.insert_park(361)
Temp_park[102,] <- Data.insert_park(368)
Temp_park[103,] <- Data.insert_park(370)
Temp_park[104,] <- Data.insert_park(371)
Temp_park[105,] <- Data.insert_park(372)
Temp_park[106,] <- Data.insert_park(373)
Temp_park[107,] <- Data.insert_park(375)
Temp_park[108,] <- Data.insert_park(378)
Temp_park[109,] <- Data.insert_park(382)
Temp_park[110,] <- Data.insert_park(384)
Temp_park[111,] <- Data.insert_park(385)
Temp_park[112,] <- Data.insert_park(388)
Temp_park[113,] <- Data.insert_park(390)
Temp_park[114,] <- Data.insert_park(392)
Temp_park[115,] <- Data.insert_park(394)
Temp_park[116,] <- Data.insert_park(397)
Temp_park[117,] <- Data.insert_park(398)
Temp_park[118,] <- Data.insert_park(399)
Temp_park[119,] <- Data.insert_park(401)
Temp_park[120,] <- Data.insert_park(406)
Temp_park[121,] <- Data.insert_park(412)
Temp_park[122,] <- Data.insert_park(420)
Temp_park[123,] <- Data.insert_park(422)
Temp_park[124,] <- Data.insert_park(427)
Temp_park[125,] <- Data.insert_park(439)
Temp_park[126,] <- Data.insert_park(443)
Temp_park[127,] <- Data.insert_park(445)
Temp_park[128,] <- Data.insert_park(447)
Temp_park[129,] <- Data.insert_park(450)
Temp_park[130,] <- Data.insert_park(455)
Temp_park[131,] <- Data.insert_park(464)
Temp_park[132,] <- Data.insert_park(465)
Temp_park[133,] <- Data.insert_park(467)
Temp_park[134,] <- Data.insert_park(473)
Temp_park[135,] <- Data.insert_park(474)
Temp_park[136,] <- Data.insert_park(478)
Temp_park[137,] <- Data.insert_park(487)
Temp_park[138,] <- Data.insert_park(488)
Temp_park[139,] <- Data.insert_park(496)
Temp_park[140,] <- Data.insert_park(500)
Temp_park[141,] <- Data.insert_park(502)
Temp_park[142,] <- Data.insert_park(511)
Temp_park[143,] <- Data.insert_park(521)
Temp_park[144,] <- Data.insert_park(525)
Temp_park[145,] <- Data.insert_park(532)
Temp_park[146,] <- Data.insert_park(534)

head(Temp_park)

Acc.sub <- cbind(Acc.region[2:146,], Temp_park[2:146,])
colnames(Acc.sub) <- c("region", "park", "population")
rownames(Data.sub) <- c(1 : 145)
head(Acc.sub)


#draw the plot
plot (x = Acc.sub$park, y = Acc.sub$population, 
      xlab = "number of park in each region",
      ylab = "population of Dengue in each region",
      main = "Scatter plot about population and park number")

#compute the correlation between park number and population of dengue
Acc.sub$population <- as.numeric(Acc.sub$population)
cor(Acc.sub$park, Acc.sub$population)


Temp_p <- data.frame(percent = 1 : 37)

Temp_p[1,] <- c("0.132")
Temp_p[2,] <- c("0.058")
Temp_p[3,] <- c("0.031")
Temp_p[4,] <- c("0.074")
Temp_p[5,] <- c("0.029")
Temp_p[6,] <- c("0.014")
Temp_p[7,] <- c("0.118")
Temp_p[8,] <- c("0.065")
Temp_p[9,] <- c("0.062")
Temp_p[10,] <- c("0.097")
Temp_p[11,] <- c("0.099")
Temp_p[12,] <- c("0.116")
Temp_p[13,] <- c("0.101")
Temp_p[14,] <- c("0.076")
Temp_p[15,] <- c("0.089")
Temp_p[16,] <- c("0.034")
Temp_p[17,] <- c("0")
Temp_p[18,] <- c("0.349")
Temp_p[19,] <- c("0.181")
Temp_p[20,] <- c("0.179")
Temp_p[21,] <- c("0.171")
Temp_p[22,] <- c("0.081")
Temp_p[23,] <- c("0.41")
Temp_p[24,] <- c("0.069")
Temp_p[25,] <- c("0.101")
Temp_p[26,] <- c("0.197")
Temp_p[27,] <- c("0.399")
Temp_p[28,] <- c("0.295")
Temp_p[29,] <- c("0.156")
Temp_p[30,] <- c("0.119")
Temp_p[31,] <- c("1.305")
Temp_p[32,] <- c("1.624")
Temp_p[33,] <- c("2.769")
Temp_p[34,] <- c("4.264")
Temp_p[35,] <- c("0.971")
Temp_p[36,] <- c("1.363")
Temp_p[37,] <- c("4.448")

Data.sub <- cbind(Data.sub, Temp_p)
head(Data.sub)

plot (x = Data.sub$density, y = Data.sub$percent, 
      xlab = "density of each region",
      ylab = "percentage of Dengue in each region",
      main = "Scatter plot about percentage and density")

Data.sub$percent <- as.numeric(Data.sub$percent)
cor(Data.sub$density, Data.sub$percent)
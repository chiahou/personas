#Environment Preparation
setwd("~/Desktop/personas/analysis")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(corrplot)

#Import data 
my_data <- read.csv("PersonaData.csv", header=TRUE)
colnames(my_data)
my_data_new <- select(my_data, -c(1,2,3,4))
head(my_data_new)
colnames(my_data_new)

##Correlation coefficent table
my_data_cor <- tbl_df(my_data_new)
colnames(my_data_cor)
my_data_cor <- my_data_cor %>% rename(
BT = Brand.Type,
BS = Business.Size.on.Rakuten,
CS = Company.Size,
TT = Team.Type,
AST = Average.Sales.Per.Transaction,
PP = Preferred..Main..Platform,
MC = Marketing.Channel.Diversity,
MP = Marketing.Planning,
MSL = Marketing.Style,
MST = Marketing.Strategy,
OS = Operational.Strategy,
OM = Operation.Management,
DT = Delivery.Time,
FAQ = FAQ.Type,
CR = Customer.Responsiveness
)
my_cor <- as.matrix(my_data_cor) %>% cor()
my_cor <- corrplot(my_cor, method="number", type="upper")

my_cor <- as.matrix(my_data_cor) %>% 
  cor(method = "spearman") %>% 
  corrplot(method="square", type="upper")

# Draw plots to show the correlation 
par(mfrow=c(4,2))
## plot 1: Preferred Platform VS. Average Sales Per Transaction
p1 <- ggplot(my_data, aes(x = Preferred..Main..Platform, y=Average.Sales.Per.Transaction)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p1 + labs(title="Preferred Platform VS. Average Sales per Transaction")+
  scale_x_continuous(name="Preferred Platform", limits =c(1,5), breaks =c(1:5), label = c("Auction Site"," "," "," ","Shopping Mall"), expand=c(0.1,0))+
  scale_y_continuous(name="Average Sales per Transaction", limits=c(1,4), breaks=c(1,2,3,4), label=c("low","medium","medium-high","high"))
## plot 2: Marketing Channel Diversity VS. Brand Type
p2 <- ggplot(my_data, aes(x = Marketing.Channel.Diversity, y=Brand.Type)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm, color="#333333", linetype="dashed", size=0.5)
p2 + labs(title="Marketing Channel Diversity VS. Brand Type")+
  scale_x_continuous(name="Marketing Channel Diversity", limits=c(1,5), breaks=c(1:5), label=c("Omni-Channel","","","","Multi-Channel"))+
  scale_y_continuous(name="Brand Type", limits=c(1,5), breaks=c(1:5), label=c("Distributor","","","","Manufacturer"))
## plot 3: Marketing Planning VS. Team Type
p3 <- ggplot(my_data, aes(x = Marketing.Planning, y=Team.Type)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p3 + labs(title="Marketing Planning VS. Team Type")+
  scale_x_continuous(name="Marketing Planning", limits=c(1,5), breaks=c(1:5), label=c("spontaneous","","","","organized"))+
  scale_y_continuous(name="Team Type", limits=c(1,5), breaks=c(1:5), label=c("self-managed","","","","functional"))
## plot 4: Marketing Style VS.Marketing Channel
p4 <- ggplot(my_data, aes(x = Marketing.Style, y=Marketing.Channel.Diversity)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p4 + labs(title="Marketing Style VS. Marketing Channel")+
  scale_x_continuous(name="Marketing Planning", limits=c(1,5), breaks=c(1:5), label=c("spontaneous","","","","organized"))+
  scale_y_continuous(name="Marketing Channel", limits=c(1,5), breaks=c(1:5), label=c("Omni","","","","Multi"))
## plot 5: Marketing Strategy VS. Marketing Channel
p5 <- ggplot(my_data, aes(x = Marketing.Strategy, y=Marketing.Channel.Diversity)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p5 + labs(title="Marketing Strategy VS. Marketing Channel")+
  scale_x_continuous(name="Marketing Strategy", limits=c(1,5), breaks=c(1:5), label=c("transactional","","","","content"))+
  scale_y_continuous(name="Marketing Channel", limits=c(1,5), breaks=c(1:5), label=c("Omni","","","","Multi"))
## plot 6: Marketing Strategy VS. Marketing Style
p6 <- ggplot(my_data, aes(x = Marketing.Strategy, y=Marketing.Style)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p6 + labs(title="Marketing Strategy VS. Marketing Style")+
  scale_x_continuous(name="Marketing Strategy", limits=c(1,5), breaks=c(1:5), label=c("transactional","","","","content"))+
  scale_y_continuous(name="Marketing Style", limits=c(1,5), breaks=c(1:5), label=c("spontaneous","","","","organized"))
## plot 7: Operation Management VS. Business Size on Rakuten
p7 <- ggplot(my_data, aes(x = Operation.Management, y=Business.Size.on.Rakuten)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p7 + labs(title="Operation Management VS. Business Size on Rakuten")+
  scale_x_continuous(name="Operation Management", limits=c(1,5), breaks=c(1:5), label=c("Human Labor","","","","Automation"))+
  scale_y_continuous(name="Business Size on Rakuten", limits=c(1,5), breaks=c(1:5), label=c("In the Red","Small","Medium","Large","Super Large"))


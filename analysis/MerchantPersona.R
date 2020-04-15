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

#Line Plot
my_data_new <- as.data.frame(my_data_new)
......

##Correlation coefficent table
my_data_cor <- tbl_df(my_data_new)
my_data_cor <- my_data_cor %>% rename(
BT = Brand.Type,
BS = Business.Size.on.Rakuten,
CS = Company.Size,
TT = Team.Type,
AST = Average.Sales.Per.Transaction,
MC = Marketing.Channel.Diversity,
MP = Marketing.Planning,
MSL = Marketing.Style,
MST = Marketing.Strategy,
DM = Decision.Making.Approach,
OA = Operation.Automation,
DT = Delivery.Time,
FAQ = FAQ,
CR = Customer.Responsiveness
)
par(mfrow=c(1,1))
p <- cor.mtest(my_data_cor, conf.level = .95)
colnew <- colorRampPalette(c("red","white","blue"))
my_cor_1 <- as.matrix(my_data_cor) %>% 
  cor(method = "spearman") %>% 
  corrplot(method="circle", type="upper", tl.col = "#333333", col = colnew(20))

##Exclude insignificance coefficient 
my_cor_2 <-  as.matrix(my_data_cor) %>% 
  cor(method = "spearman") %>% 
  corrplot(method="circle", type="upper", tl.col = "#333333", col = colnew(20), p.mat=p$p, insig = "blank")

##Exclude insignificance coefficient + show the parameters
my_cor_3 <-  as.matrix(my_data_cor) %>% 
  cor(method = "spearman") %>% 
  corrplot(method="number", type="upper", tl.col = "#333333", col = colnew(20), p.mat=p$p, insig = "blank")

# Draw plots to show the correlation 
par(mfrow=c(4,2))
## plot 1: Brand Type VS. Marketing Channel Diversity
p1 <- ggplot(my_data, aes(x=Brand.Type, y=Marketing.Channel.Diversity))+
  geom_point(color="dark blue")+
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p1 + labs(title="Brand Type VS. Marketing Channel Diversity")+
  scale_x_continuous(name="Brand Type", limits=c(1,5), breaks=c(1:5), label=c("Distributor","","","","Manufacturer"))+
  scale_y_continuous(name="Marketing Channel Diversity", limits=c(1,5), breaks=c(1:5), label=c("Omni","","","","Multi"))
## plot 2: Brand Type vs Marketing Style
p2 <- ggplot(my_data, aes(x = Brand.Type, y=Marketing.Style)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p2 + labs(title="Brand Type VS. Marketing Style")+
  scale_x_continuous(name="Brand Type", limits=c(1,5), breaks=c(1:5), label=c("Distributor","","","","Manufacturer"))+
  scale_y_continuous(name="Marketing Style", limits=c(1,5), breaks=c(1:5), label=c("Conservative","","","","Innovative"))
## plot 3: Brand Type VS. Marketing Strategy
p3 <- ggplot(my_data, aes(x = Brand.Type, y=Marketing.Strategy)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p3 + labs(title="Brand Type VS. Marketing Strategy")+
  scale_x_continuous(name="Brand Type", limits=c(1,5), breaks=c(1:5), label=c("Distributor","","","","Manufacturer"))+
  scale_y_continuous(name="Marketing strategy", limits=c(1,5), breaks=c(1:5), label=c("Transactional","","","","Branding"))
## plot 4: Business Size on Rakuten VS. Delivery Time
p4 <- ggplot(my_data, aes(x = Business.Size.on.Rakuten, y=Delivery.Time)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p4 + labs(title="Business Size on Rakuten VS. Delivery Time")+
  scale_x_continuous(name="Business Size", limits=c(1,5), breaks=c(1:5), label=c("In red","Small","Medium","Large","Super Large"))+
  scale_y_continuous(name="Delivery Time", limits=c(1,5), breaks=c(1:5), label=c("< 1 day","1-3days","3-5days","5-10days","> 10 days"))
## plot 5: Business Size on Rakuten VS. Customer Responsiveness
p5 <- ggplot(my_data, aes(x = Business.Size.on.Rakuten, y=Customer.Responsiveness)) +
  geom_point(color="dark blue") +
  geom_smooth(method=lm,color="#333333", linetype="dashed", size=0.5)
p5 + labs(title="Business Size on Rakuten VS. Customer Responsiveness")+
  scale_x_continuous(name="Business Size", limits=c(1,5), breaks=c(1:5), label=c("In red","Small","Medium","Large","Super Large"))+
  scale_y_continuous(name="Customer Responsiveness", limits=c(1,5), breaks=c(1:5), label=c("Instant","","Normal","","Time-Consuming"))
# plot 6: Team Type VS. Marketing Planning
p6 <- ggplot(my_data, aes(x = Team.Type, y = Marketing.Planning))+
  geom_point(color="dark blue")+
  geom_smooth(method=lm, color="#333333", linetype="dashed", size=0.5)
p6 + labs(title="Team Type VS. Marketing Planning")+
  scale_x_continuous(name="Team Type", limits=c(1,5), breaks=c(1:5), label=c("Self-managed","","","","Functional"))+
  scale_y_continuous(name="Marketing Planning", limits=c(1,5), breaks=c(1:5), label=c("Sponatenous","","","","Organized"))

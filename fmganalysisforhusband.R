library(sjPlot)
library(sjmisc)
library(graphics)
library(ggplot2)
library(dplyr)
library(scales)


fmgissue= read.csv("fmgissue1.csv")

summary(fmgissue)

mydat3=fmgissue[5:14]
lab2 = c("Do you think agriculture is lucrative profession?",
         "Do you think men and women have equal opportunities for income generation?",
         "Do you have surplus production of vegetables?",
         "Do you have assured market for your vegetable produces?",
         "Do you have transportation means for your farm produces?",
         "Do you have connectivity problem in your area with rest of the county/ district/ country?",
         "Do you have any cold storage facilities in your area?",
         "Do you have prior business experiences in vegetable marketing?",
         "Do you undertake processing of vegetables to increase the shelf life?",
         "Would you like to take up vegetable marketing business for livelihood?")

names(mydat3)=lab2
plot_likert(
  mydat3,
  title = "Issue",
  grid.range = c(1.2, 1.0),
  expand.grid = FALSE,
  legend.labels = c("No","Yes"),
  catcount = 2,
  geom.colors = "RBu",
  values = "sum.outside",
  show.prc.sign = TRUE
)

fmgimpact= read.csv("fmgimpact1.csv")

summary(fmgimpact)

mydat4=fmgimpact[5:14]


names(mydat4)=lab2
plot_likert(
  mydat4,
  title = "Impact",
  grid.range = c(1.7, 1.7),
  legend.labels = c("No","Yes"),
  expand.grid = FALSE,
  geom.colors = "RBu",
  values = "sum.outside",
  show.prc.sign = TRUE
)


fmgexp= cbind(fmgissue[16], fmgimpact[16]);fmgexp
names(fmgexp)= c("Issue", "Impact")

plot_likert(
  fmgexp,
  title = "Percent expenditure to buy food from other districts",
  grid.range = c(1.0, 1.0),
  expand.grid = FALSE,
  catcount = 5,
  geom.colors = "RBu",
  values = "show",
  show.prc.sign = TRUE
)

exp1= table(fmgexp[1])
exp2= table(fmgexp[2])
exp1=as.data.frame(exp1)

exp1 <- exp1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
exp1
bp4<- ggplot(exp1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Issue")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Percent expenditure to buy food from other districts")+ylab("")+
  labs(fill = "Percent expenditure")
bp4



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(exp1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Percent expenditure to buy food from other districts")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Percent Expenditure")+
  theme_void() 


p<-ggplot(data=exp1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Percent expenditure to buy food from other districts")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Expenditure to buy food from other district")+ylab("%")



exp2=as.data.frame(exp2)

exp2 <- exp2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
exp2

bp5<- ggplot(exp2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Percent expenditure to buy food from other districts")+ylab("")+
  labs(fill = "Percent expenditure")
bp5



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(exp2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Percent expenditure to buy food from other districts")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill = "Percent expenditure")+
  theme_void() 


p<-ggplot(data=exp2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Percent expenditure to buy food from other districts")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Percent Expenditure")+ylab("%")

fmgaoccup= cbind(fmgissue[17], fmgimpact[17]);fmgaoccup
names(fmgaoccup)= c("Issue", "Impact")

plot_likert(
  fmgaoccup,
  title = "Alternate occupation to agriculture",
  grid.range = c(1.0, 1.3),
  expand.grid = FALSE,
  geom.colors = "RBu",
  values = "show",
  show.prc.sign = TRUE
)
aoccup1= table(fmgaoccup[1])
aoccup2= table(fmgaoccup[2])
aoccup1=as.data.frame(aoccup1)

aoccup1 <- aoccup1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
aoccup1

bp6<- ggplot(aoccup1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Issue")+
  xlab("Alternate occupation")+ylab("")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill= "Alternate Occupation")
bp6


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(aoccup1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Alternate occupation")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill= "Alternate occupation")+
  theme_void() 


p<-ggplot(data=aoccup1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Alternate occupation")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Alternate occupation")+ylab("%")+
  scale_x_discrete(limits=c("Moving to city for other oppurtunities", "Floriculture and Livestock farming", "Business (Hotel)", "Agriculture activities", "Farming" ,"Labour works (non-agriculure)"))



aoccup2=as.data.frame(aoccup2)

aoccup2 <- aoccup2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
aoccup2

bp7<- ggplot(aoccup2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill= "Alternate occupation")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Alternate occupation")+ylab("")
bp7



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(aoccup2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Alternate occupation")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill = "Alternate occupation")+
  theme_void() 


p<-ggplot(data=aoccup2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Alternate occupation")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Alternate occupation")+ ylab("%")+
  scale_x_discrete(limits=c( "Agriculture activities", "Farming", "Floriculture and agriculture", "Agricultural marketing"))


fmglb= cbind(fmgissue[24], fmgimpact[24]);fmglb
names(fmglb)= c("Issue", "Impact")

plot_likert(
  fmglb,
  title = "Labour Source",
  grid.range = c(1.0, 1.4),
  expand.grid = FALSE,
  catcount = 2,
  geom.colors = "RBu",
  values = "sum.outside",
  show.prc.sign = TRUE
)
lb1= table(fmglb[1])
lb2= table(fmglb[2])
lb1=as.data.frame(lb1)

lb1 <- lb1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
lb1

bp8<- ggplot(lb1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Issue")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill= "Alternate occupation")+
  xlab("Labour source")+ylab("")
bp8



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(lb1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Labour source")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill= "Labour source")+
  theme_void() 


p<-ggplot(data=lb1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Labour source")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Labour source")+ylab("%")



lb2=as.data.frame(lb2)

lb2 <- lb2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
lb2

bp9<- ggplot(lb2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill="Labour source")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Labour source")+ylab("")
bp9
 


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(lb2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Labour source")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill= "Labour source")+
  theme_void() 


p<-ggplot(data=lb2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Labour source")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Labour source")+ylab("%")


fmginc= cbind(fmgissue[25], fmgimpact[25]);fmginc
names(fmginc)= c("Issue", "Impact")

plot_likert(
  fmginc,
  title = "Income per month",
  grid.range = c(1.2, 1.4),
  expand.grid = FALSE,
  catcount = 2,
  geom.colors = "RBu",
  values = "sum.outside",
  show.prc.sign = TRUE
)
inc1= table(fmginc[1])
inc2= table(fmginc[2])
inc1=as.data.frame(inc1)

inc1 <- inc1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
inc1

bp10<- ggplot(inc1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Issue")+
  labs(fill="Income (Nu.)")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Income per month")+ylab("")
bp10



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(inc1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Income per month")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill= "Income (Nu.)")+
  theme_void() 


p<-ggplot(data=inc1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Income per month")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Income (Nu.)")+ylab("%")+
  scale_x_discrete(limits=c( ">Nu. 6450", "<Nu. 6450"))




inc2=as.data.frame(inc2)

inc2 <- inc2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
inc2

bp11<- ggplot(inc2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill="Income (Nu.)")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Income per month")+ylab("")
bp11


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(inc2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Income per month")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Income (Nu.)")+
  theme_void() 


p<-ggplot(data=inc2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Income per month")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Income (Nu.)")+ylab("%")


fmgainc= cbind(fmgissue[26], fmgimpact[26]);fmgainc
names(fmgainc)= c("Issue", "Impact")

plot_likert(
  fmgainc,
  title = "Income share from agriculture",
  grid.range = c(1.0, 1.4),
  expand.grid = FALSE,
  catcount = 4,
  geom.colors = "RBu",
  values = "hide",
  show.prc.sign = TRUE
)
ainc1= table(fmgainc[1])
ainc2= table(fmgainc[2])
ainc1=as.data.frame(ainc1)

ainc1 <- ainc1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
ainc1

bp10<- ggplot(ainc1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Issue")+
  labs(fill="% Income Share")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Income share from agriculture")+ylab("")
bp10

 

mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(ainc1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Income share from agriculture")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="% Income share")+
  theme_void() 


p<-ggplot(data=ainc1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Income share from agriculture")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("% Income share")+ylab("%")



ainc2=as.data.frame(ainc2)

ainc2 <- ainc2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
ainc2

bp011<- ggplot(ainc2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill="% Income share")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Income share from agriculture")+ylab("")
bp011



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(ainc2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Income share from agriculture")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill= "% Income share")+
  theme_void() 


p<-ggplot(data=ainc2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Income share from agriculture")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("% Income share")+ylab("%")


fmgpsur= cbind(fmgissue[27], fmgimpact[27]);fmgpsur
names(fmgpsur)= c("Issue", "Impact")

plot_likert(
  fmgpsur,
  title = "Percent Surplus",
  grid.range = c(1.0, 1.4),
  expand.grid = FALSE,
  legend.labels = c("<50% of total consumption", "No surplus", ">50% of total consumption"),
  catcount = 3,
  geom.colors = "RBu",
  values = "show",
  show.prc.sign = TRUE
)
psur1= table(fmgpsur[1])
psur2= table(fmgpsur[2])
psur1=as.data.frame(psur1)

psur1 <- psur1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
psur1

bp10<- ggplot(psur1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Issue")+
  labs(fill="% Surplus")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("% Surplus")+ylab("")
bp10



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(psur1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Percent surplus")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="% Surplus")+
  theme_void() 


p<-ggplot(data=psur1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Percent surplus")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("% Surplus")+ylab("%")



psur2=as.data.frame(psur2)

psur2 <- psur2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
psur2

bp11<- ggplot(psur2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill= "% Surplus")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("% surplus")+ylab("")
bp11



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(psur2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Percent surplus")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="% Surplus")+
  theme_void() 


p<-ggplot(data=psur2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Percent surplus")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("% Surplus")+ylab("%")



vegis=read.csv("fmgissueveg.csv")
vegim=read.csv("fmgimpactveg.csv")

v1= table(vegis[3])
v2= table(vegim[3])

v1=as.data.frame(v1)

v1 <- v1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
v1

bp13<- ggplot(v1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Issue")+
  xlab("Vegetables produced")
bp13


#mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(v1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Vegetables Produced")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  #scale_fill_manual(values = mycols) +
  labs(fill= "Vegetables produced")+
  theme_void() 


p<-ggplot(data=v1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Vegetables Produced")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Vegetables Produced")+ylab("%")
# scale_x_discrete(limits=c("Produce unavailable", "Farmers not competitive", "FGM is not aware", "Tender supplier supplies all vegetables", "Budget limitations", "Unfavouralble financial rules and regulations", "Expensive local vegetables"))


v2=as.data.frame(v2)

v2 <- v2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
v2

bp3<- ggplot(v2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  xlab("Vegetables Produced")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill="Vegetables Produced")

bp3



#mycols= c("red3","orange2","green4","deeppink3","olivedrab", "darkslateblue","mediumorchid3" )
ggplot(v2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Vegetables Produced")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  #scale_fill_manual(values = mycols) +
  labs(fill="Vegetables Produced")+
  theme_void()


p<-ggplot(data=v2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Vegetables Produced")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("vegetables Produced")+ylab("%")


vegsuris=read.csv("fmgissuesur.csv")
vegsurim=read.csv("fmgimpactsur.csv")

s1= table(vegsuris[3])
s2= table(vegsurim[3])

s1=as.data.frame(s1)

s1 <- s1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
s1

bp13<- ggplot(s1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Issue")+
  labs(fill="Surplus Vegetables")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Surplus Vegetables")+ylab("")
bp13


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(s1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Surplus Vegetables")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill="Surplus Vegetables")+
  scale_fill_manual(values = mycols) +
  theme_void() 


p<-ggplot(data=s1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Vegetables Surplus")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("surplus Vegetables")+ylab("%")
#scale_x_discrete(limits=c("Produce unavailable", "Farmers not competitive", "FGM is not aware", "Tender supplier supplies all vegetables", "Budget limitations", "Unfavouralble financial rules and regulations", "Expensive local vegetables"))


s2=as.data.frame(s2)

s2 <- s2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
s2

bp3<- ggplot(s2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill="Surplus Vegetables")+
  xlab("Surplus Vegetables")+ylab("")

bp3



mycols= c("red3","orange2","green4","deeppink3","olivedrab", "darkslateblue","mediumorchid3", "yellowgreen", "cadetblue3")
ggplot(s2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Surplus Vegetables")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Surplus Vegetables")+
  theme_void()


p<-ggplot(data=s2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact- Surplus Vegetables")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Surplus Vegetables")+ylab("%")



veglksuris=read.csv("fmgissuelksur.csv")
veglksurim=read.csv("fmgimpactlksur.csv")

l1= table(veglksuris[3])
l2= table(veglksurim[3])


l1=as.data.frame(l1)


l1 <- l1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
l1

bp13<- ggplot(l1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Issue")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill= "Reasons")+
  xlab("Reason for lack of surplus")+ylab("")
bp13

mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(l1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Reason for lack of surplus")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Reasons")+
  theme_void() 


p<-ggplot(data=l1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Reason for lack of surplus")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Reasons")+ylab("%")+
  scale_x_discrete(limits=c("Seed and seedling shortage", "Lack of human resource", "Lack of Knowledge on improved production techniques", "Unpredictable weather conditions", "Susceptibility to various pests and natural disasters", "Low fertility of Soil", "Lack of financial capability", "Lack of assured market for the produce"))


l2=as.data.frame(l2)

l2 <- l2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
l2

bp3<- ggplot(l2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill="Reasons")+
  xlab("Reason for lack of surplus")+ylab("")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)
bp3



mycols= c("red3","orange2","green4","deeppink3","olivedrab", "darkslateblue","mediumorchid3" )
ggplot(l2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Reason for lack of surplus")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Reasons")+
  theme_void()


p<-ggplot(data=l2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Reason for lack of surplus")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Reasons")+ylab("%")+
  scale_x_discrete(limits=c("Unpredictable whether conditions", "Susceptibility to various pests and natural disasters", "Low fertility of soil", "Farm labour shortage", "Lack of financial capability"))


vegwasis=read.csv("fmgissuewas.csv")
vegwasim=read.csv("fmgimpactwas.csv")

w1= table(vegwasis[3])
w2= table(vegwasim[3])


w1=as.data.frame(w1)
w1 <- w1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
w1

bp13<- ggplot(w1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Issue")+
  labs(fill="Farm waste management practices")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Farm waste management practices")+ylab("")
bp13


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(w1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Farm waste management practices")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Farm waste management practices")+
  theme_void() 


p<-ggplot(data=w1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Farm waste management practices")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Farm waste management practices")+ylab("%")
#scale_x_discrete(limits=c("Produce unavailable", "Farmers not competitive", "FGM is not aware", "Tender supplier supplies all vegetables", "Budget limitations", "Unfavouralble financial rules and regulations", "Expensive local vegetables"))


w2=as.data.frame(w2)

w2 <- w2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
w2

bp3<- ggplot(w2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill="Farm waste management practices")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Farm waste management practices")+ylab("")

bp3



mycols= c("red3","orange2","green4","deeppink3","olivedrab", "darkslateblue","mediumorchid3" )
ggplot(w2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Farm waste management practices")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Farm waste management practices")+
  theme_void()


p<-ggplot(data=w2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Farm waste management practices")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Farm waste management practices")+ylab("%")+
  scale_x_discrete(limits=c("Burn them", "Animal feed", "Throw them in field untill it gets rotten", "Composting"))

vegmaris=read.csv("fmgissuemar.csv")
vegmarim=read.csv("fmgimpactmar.csv")
m1= table(vegmaris[3])
m2= table(vegmarim[3])



m1=as.data.frame(m1)

m1 <- m1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
m1

bp13<- ggplot(m1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Issue")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill="Reasons")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Reason for not taking up Agriculture Marketing")+ylab("")
bp13


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(m1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Reason for not taking up Agriculture Marketing")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Reasons")+
  theme_void() 


p<-ggplot(data=m1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Reason for not taking up Agriculture Marketing")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Reasons")+ylab("%")
#scale_x_discrete(limits=c("Produce unavailable", "Farmers not competitive", "FGM is not aware", "Tender supplier supplies all vegetables", "Budget limitations", "Unfavouralble financial rules and regulations", "Expensive local vegetables"))

m2=as.data.frame(m2)

m2 <- m2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
m2

bp130<- ggplot(m2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Impact")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill="Reasons")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Reason for not taking up Agriculture Marketing")+ylab("")
bp130


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(m2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Reason for not taking up Agriculture Marketing")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Reasons")+
  theme_void() 


p<-ggplot(data=m2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Reason for not taking up Agriculture Marketing")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Reasons")+ylab("%")
#scale_x_discrete(limits=c("Produce unavailable", "Farmers not competitive", "FGM is not aware", "Tender supplier supplies all vegetables", "Budget limitations", "Unfavouralble financial rules and regulations", "Expensive local vegetables"))



fmgmeal= cbind(fmgissue[15], fmgimpact[15]);fmgmeal
names(fmgmeal)= c("Issue", "Impact")

exp1= table(fmgmeal[1])
exp2= table(fmgmeal[2])
exp1=as.data.frame(exp1)
exp1 <- exp1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
exp1
bp4<- ggplot(exp1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Issue")+
  xlab("Annual expenditure on meals")+ylab("")+
  labs(fill="Expenditure (Nu.)")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)
  
bp4



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(exp1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Annual expenditure on meals")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Expenditure (Nu.)")+
  theme_void() 


p<-ggplot(data=exp1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Annual expenditure on meals")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Expenditure (Nu.)")+ylab("%")



exp2=as.data.frame(exp2)

exp2 <- exp2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
exp2

bp5<- ggplot(exp2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  labs(fill="Expenditure (Nu.)")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  xlab("Annual expenditure on meals")+ylab("")
bp5



mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(exp2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Annual expenditure on meals")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Expenditure (Nu.)")+
  theme_void() 


p<-ggplot(data=exp2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Annual expenditure on meals")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Expenditure (Nu.)")+ylab("%")


land= read.csv("land.csv")

land1=subset(land, Type=="Dry")

ld<-ggplot(data=land1, aes(x=Village, y=Land.Holding)) +
  geom_bar(stat="identity",fill="cornflowerblue", position=position_dodge())+
  geom_text(aes(label=Land.Holding), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  xlab("FMG")+ylab("Land Holding(acre)")+
  ggtitle("Dry Land")
ld




land2=subset(land, Type=="Wet")

lw<-ggplot(data=land2, aes(x=Village, y=Land.Holding)) +
  geom_bar(stat="identity",fill="darkgreen", position=position_dodge())+
  geom_text(aes(label=Land.Holding), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  xlab("FMG")+ylab("Land Holding(acre)")+
  ggtitle("Wet Land")
lw



land3=subset(land, Type=="Orchard")

lo<-ggplot(data=land3, aes(x=Village, y=Land.Holding)) +
  geom_bar(stat="identity",fill="orange", position=position_dodge())+
  geom_text(aes(label=Land.Holding), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  xlab("FMG")+ylab("Land Holding(acre)")+
  ggtitle("Orchard")
lo

genqual= read.csv("genqualification.csv")
# genqual <- droplevels(group_by(genqual,Gender) %>%
#       mutate(pos = cumsum(Len) - (0.5 * Len)))

gen<-ggplot(data=genqual, aes(x=Gender, y=Len, fill=Qualification, label= Len)) +
  geom_bar(stat="identity")+
  geom_text(size=3.5, position = position_stack(vjust=0.5))+
  ggtitle("Qualification and Gender of FMG respondents")+
  ylab("Number")
gen

# gen1<-ggplot(genqual, aes(x = Gender, y = Len)) +
#   geom_bar(aes(fill = Qualification), stat="identity") +
#   geom_text(aes(label = Len, y = pos), size = 3)
# gen1
# 
# genqual

male=droplevels(subset(genqual,Gender=="Male"))
female=droplevels(subset(genqual,Gender=="Female"))
male=as.data.frame(male)

male <- male %>%
  arrange(desc(Qualification)) %>%
  mutate(lab.ypos = cumsum(Len) - 0.5*Len)
male

bp134<- ggplot(male, aes(x="", y=Len, fill=Qualification))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Qualifcation(Male respondent) ")+
  geom_text(aes(y = lab.ypos, label = percent(Len/sum(Len))), color = "white", size=4.5)+
  labs(fill="Qualification")+
  geom_text(aes(y = lab.ypos, label = percent(Len/sum(Len))), color = "white", size=4.5)+
  xlab("Male Qualifcation")+ylab("%")
bp134

female=as.data.frame(female)

female <-female %>%
  arrange(desc(Qualification)) %>%
  mutate(lab.ypos = cumsum(Len) - 0.5*Len)
female

bp135<- ggplot(female, aes(x="", y=Len, fill=Qualification))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Qualifcation(Female respondent) ")+
  geom_text(aes(y = lab.ypos, label = percent(Len/sum(Len))), color = "white", size=4.5)+
  labs(fill="Qualification")+
  geom_text(aes(y = lab.ypos, label = percent(Len/sum(Len))), color = "white", size=4.5)+
  xlab("Female Qualifcation")+ylab("%")
bp135



library(sjPlot)
library(sjmisc)
library(graphics)
library(ggplot2)
library(dplyr)
library(scales)

schoolissue= read.csv("schoolissue1.csv")

summary(schoolissue)

mydat=schoolissue[5:10]


lab= c("Do you think school contribute in development of rural community particularly agrarians",                                       
       "Do you buy all the school vegetable requirements from farmers",                                                                             
       "Do you trust that the vegetables supplied by quoted suppliers from markets are safe for students health and ensure nutritional requirements",
       "Do you believe that the vegetables supplied by local farmers are safe and ensure nutritional security of students",                         
       "Is buying vegetables from local producers farmers cheaper",                                                                                
       "Do your students think farming as one of the prospective occupations")

names(mydat)= lab
#plot_likert(mydat)

plot_likert(
  mydat,
  title = "Issue",
  grid.range = c(1.4, 1.4),
  expand.grid = FALSE,
  geom.colors = "RBuYl",
  values = "sum.outside",
  show.prc.sign = TRUE
)

schoolimpact= read.csv("schoolimpact1.csv")

summary(schoolimpact)

mydat2=schoolimpact[5:10]


names(mydat2)= lab
#plot_likert(mydat2)

plot_likert(
  mydat2,
  title = "Impact",
  grid.range = c(1.5, 1.2),
  legend.labels = c("No","Yes"),
  expand.grid = FALSE,
  geom.colors = "RBu",
  values = "sum.outside",
  show.prc.sign = TRUE
)

schcontri= cbind(schoolissue[11], schoolimpact[11]);schcontri
names(schcontri)= c("Issue", "Impact");schcontri

plot_likert(
  schcontri,
  title = "Contribution by school to rural agriculture development in terms of",
  grid.range = c(1.2, 1.4),
  expand.grid = FALSE,
  #legend.labels = c("Monetary","Non-monetary", "Both"),
  catcount = 3,
  geom.colors = "RBuG",
  values = "show",
  show.prc.sign = TRUE
)

freq1= table(schcontri[1])
freq2= table(schcontri[2])
freq1=as.data.frame(freq1)

freq1 <- freq1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
freq1

bp1<- ggplot(freq1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Issue") +
  xlab("Contribution to rural community") + ylab(" ")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "Contribution in terms of")
bp1




mycols= c("red3","orange2","green4")
ggplot(freq1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Contribution to rural community")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Contribution in terms of")+
  theme_void() 


p<-ggplot(data=freq1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Contribution to rural community")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Contribution in terms of")+ ylab("%")

freq2=as.data.frame(freq2)
freq2 <- freq2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
freq2

bp01<- ggplot(freq2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Impact") +
  xlab("Contribution to rural community") + ylab(" ")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "Contribution in terms of")
bp01




mycols= c("red3","orange2","green4")
ggplot(freq2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Contribution to rural community")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Contribution in terms of")+
  theme_void() 


p<-ggplot(data=freq2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Contribution to rural community")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Contribution in terms of")+ ylab("%")

buyissue=read.csv("schoolissuebuy.csv")
buyimpact=read.csv("schoolimpactbuy.csv")

f1= table(buyissue[3])
f2= table(buyimpact[3])

f1=as.data.frame(f1)

f1 <- f1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
f1

bp2<- ggplot(f1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 0.5, stat = "identity")+
  labs(title="Issue")+
  xlab("Limitations in buying farmer's produce")+ ylab(" ")+
    geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "Reasons for not buying")
  
bp2


mycols= c("red3","orange2","green4","deeppink3","darkslateblue","red2", "mediumorchid3", "forestgreen")
ggplot(f1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Issue-Limitations in buying farmer's produce")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Reasons for not buying")+
  theme_void() 


p<-ggplot(data=f1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Issue-Limitations in buying farmer's produce")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  scale_x_discrete(limits=c( "Farmers not competitive", "FGM is not aware", "Tender supplier supplies all vegetables", "Produce unavailable","Budget limitations", "Unfavourable financial rules and regulations", "Expensive local vegetables"))+
  xlab("Reasons for not buying")+ ylab("%")


f2=as.data.frame(f2)
f2 <- f2 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
f2

bp3<- ggplot(f2, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  labs(title="Impact")+
  xlab("Limitations in buying farmer's produce")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "Reasons for not buying")

bp3



mycols= c("red3","orange2","green4","deeppink3","olivedrab", "darkslateblue","mediumorchid3" )
ggplot(f2, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact-Limitations in buying farmer's produce")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Reasons for not buying")+
  theme_void()


p<-ggplot(data=f2, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact-Limitations in buying farmer's produce")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Reasons for not buying")+ylab("%")+
  scale_x_discrete(limits=c("Unfavourable financial rules and regulations", "Expensive local vegetables", "Produce unavailable","Budget limitations"))


Sgenqual= read.csv("sgenqualification.csv")

Sgen<-ggplot(data=Sgenqual, aes(x=Gender, y=Len, fill=Qualification)) +
  geom_bar(stat="identity")+
  ggtitle("Qualification and Gender of School respondents")+
  ylab("Number")
Sgen

ggplot(data=Sgenqual, aes(fill=Qualification, y=Len, x=Gender)) + 
  geom_bar( stat="identity", position="fill")+
  ggtitle("Qualification and Gender of School respondents (Percent)")+
  ylab("%")


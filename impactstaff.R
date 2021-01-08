library(sjPlot)
library(sjmisc)
library(graphics)
library(ggplot2)
library(dplyr)
library(scales)

sdat=read.csv("staffimpact.csv")

summary(sdat)

q1=sdat[3]

plot_likert(
  q1,
  title = "Difference between the business as usual school linking program and FMG-based initiative",
  grid.range = c(1.4, 0.6),
  expand.grid = FALSE,
  geom.colors = "RBu",
  values = "sum.outside",
  show.prc.sign = TRUE,
  axis.labels = ""
)
q1=table(q1)
q1=as.data.frame(q1)
names(q1)= c("Var1","Freq")
q1 <- q1 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
q1

bp1<- ggplot(q1, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Impact of FMG") +
  xlab("FMG vs Other School Linking Program") + ylab(" ")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "FMG vs Other School Linking Program")
bp1

amycols= c("red3","orange2","green4")
ggplot(q1, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Impact of FMG")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="FMG vs Other School Linking Program")+
  theme_void() 


p<-ggplot(data=q1, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Impact of FMG")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=0.5, size=3.5)+
  xlab("FMG vs Other School Linking Program")+ ylab("%")



q2=sdat[4:10]
labs= c("Do you think the producers/sellers can build trust with local FMG members while negotiating prices",
        "The FMG model of the RNR produce marketing will bring back the money to the community",
        "Do you think FMG is necessary for communities like in Chhukha",
        "Do you feel that the pilot program for school linking had increased the income level of the FMG members",
        "Do you feel that the FMG to School linking has encouraged the farmers to grow more in the field",
        "Lesser the group member lesser the management issues (Agree or not)",
        "Larger the group members, lesser the risk")

names(q2)= labs
plot_likert(
  q2,
  title = "Impact of FMG",
  grid.range = c(1.5, 1.2),
  expand.grid = FALSE,
  legend.labels = c("No","Yes"),
  geom.colors = "RBu",
  values = "sum.outside",
  show.prc.sign = TRUE
)

q3=sdat[11]

names(q3)= "Members in FMG"

# plot_likert(
#   q3,
#   title = "Members in FMG",
#   grid.range = c(1.4, 0.6),
#   expand.grid = FALSE,
#   catcount = 5,
#   geom.colors = "RBu",
#   values = "sum.outside",
#   show.prc.sign = TRUE,
#   axis.labels = ""
# )
q3=table(q3)
q3=as.data.frame(q3)
names(q3)= c("Var1","Freq")
q3 <- q3 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
q3

bp3<- ggplot(q3, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Members in FMG") +
  xlab("Members in FMG") + ylab(" ")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "Members in FMG")
bp3

mycols= c("red3","orange2","green4")
ggplot(q3, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Members in FMG")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Members in FMG")+
  theme_void() 


p<-ggplot(data=q3, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Members in FMG")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Members in FMG")+ ylab("%")



q4=sdat[12]

names(q4)= "Role of FMG"

# plot_likert(
#   q4,
#   title = "Role of FMG",
#   grid.range = c(1.4, 0.6),
#   expand.grid = FALSE,
#   catcount = 3,
#   geom.colors = "RBu",
#   values = "sum.outside",
#   show.prc.sign = TRUE,
#   axis.labels = ""
# )
q4=table(q4)
q4=as.data.frame(q4)
names(q4)= c("Var1","Freq")
q4 <- q4 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
q4

bp4<- ggplot(q4, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("Role of FMG") +
  xlab("Role of FMG") + ylab(" ")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "Role of FMG")
bp4

mycols= c("red3","orange2","green4")
ggplot(q4, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("Role of FMG")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="Role of FMG")+
  theme_void() 


p<-ggplot(data=q4, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("Role of FMG")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("Role of FMG")+ ylab("%")+
  scale_x_discrete(limits=c("Market imported produces/ products in their gewog", "Both of the above", "Market Collected RNR produces/ products from their gewog"))
  
q5=sdat[13]


names(q5)= "View on FMG"


# plot_likert(
#   q5,
#   title = "View on FMG",
#   grid.range = c(1.4, 0.6),
#   expand.grid = FALSE,
#   geom.colors = "RBu",
#   values = "sum.outside",
#   show.prc.sign = TRUE,
#   axis.labels = ""
# )
q5=table(q5)
q5=as.data.frame(q5)
names(q5)= c("Var1","Freq")
q5 <- q5 %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
q5

bp5<- ggplot(q5, aes(x="", y=Freq, fill=Var1))+
  geom_bar(width = 1, stat = "identity")+
  ggtitle("View on FMG") +
  xlab("View on FMG") + ylab(" ")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  labs(fill = "View on FMG")
bp5

mycols= c("red3","orange2","green4")
ggplot(q5, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  ggtitle("View on FMG")+
  geom_text(aes(y = lab.ypos, label = percent(Freq/sum(Freq))), color = "white", size=4.5)+
  scale_fill_manual(values = mycols) +
  labs(fill="View on FMG")+
  theme_void() 


p<-ggplot(data=q5, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="salmon")
p + coord_flip()+
  ggtitle("View on FMG")+
  geom_text(aes(label=percent(Freq/sum(Freq))), hjust=1, size=3.5)+
  xlab("View on FMG")+ ylab("%")


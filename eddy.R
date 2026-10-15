lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
library(lubridate)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(utils)

setwd("C:/Users/matysekm/Desktop/FastData")
all_files<-list.files()
header_file<-all_files[grep("Header", all_files)]
ec_data<-setdiff(all_files, all_files[grep("Header", all_files)])
#headers<-strsplit(readLines(header_file, n=3), "\t")
headers<-strsplit(readLines(header_file, n=3), ",")
cols<-headers[[1]]
df1<-read.table(all_files[1], sep=",")
colnames(df1)<-cols

str(df1)

df1$Timestamp<- as.POSIXct(df1$Timestamp)
df_all<-do.call(rbind, lapply(ec_data, function(x) read.table(x, sep=",")))
colnames(df_all)<-cols
df_all$Timestamp<- as.POSIXct(df_all$Timestamp)

df_all<-filter(df_all, !is.na(Uz) & CO2!="NaN")
df_all<-filter(df_all, Uz!="NaN" & CO2!="NaN")

min(df_all$CO2)
max(df_all$CO2)

plot1<-ggplot(df_all3, aes(x=Timestamp, y=CO2))+
  geom_point(size=0.3, colour="blue")+
  scale_x_datetime(date_breaks="1 hour", date_labels="%Y-%m-%d %H")+
  #scale_y_continuous(breaks = seq(0, 1600, by =200))+
  labs(title = (expression(paste("CO2"))), x="Time", y = "CO2")+
  stat_summary(fun.y=mean, geom="line", colour="red")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

ggsave(filename=plot1, plot=plot1)


df_all2<-df_all %>%
  mutate(halfhour=ceiling_date(Timestamp, unit="30 minutes"))%>% 
  group_by(halfhour)%>%
summarise(cov_w_co2=cov(Uz, CO2),
       cov_w_h2o=cov(Uz, H2O),
       cov_w_T=cov(Uz, T_sonic))

df_all2<-df_all %>%
  mutate(halfhour=round_date(Timestamp, unit="30 minutes"))%>% 
  group_by(halfhour)%>%
  summarise(cov_w_co2=cov(Uz, CO2),
            cov_w_h2o=cov(Uz, H2O),
            cov_w_T=cov(Uz, T_sonic))
  
plot_cov_co2<-ggplot(df_all2, aes(x=halfhour, y=cov_w_co2))+
  stat_summary(fun.y=mean, geom="line", colour="lawngreen", size=0.8)+
  geom_point(size=1, colour="blue")+
  scale_x_datetime(date_breaks="6 hour", date_labels="%d-%b %H:%M")+
  labs(title = (expression(paste("CO2 covariance"))), x="Time (hour)", y = "CO2 cov")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

plot_cov_h2o<-ggplot(df_all2, aes(x=halfhour, y=cov_w_h2o))+
  stat_summary(fun.y=mean, geom="line", colour="lawngreen", size=0.8)+
  geom_point(size=1, colour="blue")+
  scale_x_datetime(date_breaks="6 hour", date_labels="%d-%b %H:%M")+
  labs(title = (expression(paste("H2O covariance"))), x="Time (hour)", y = "H2O cov")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

plot_cov_T<-ggplot(df_all2, aes(x=halfhour, y=cov_w_T))+
  stat_summary(fun.y=mean, geom="line", colour="lawngreen", size=0.8)+
  geom_point(size=1, colour="blue")+
  scale_x_datetime(date_breaks="6 hour", date_labels="%d-%b \n %H:%M")+
  labs(title = (expression(paste("Temperature covariance"))), x="Time", y = "T cov")+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

grid.arrange(plot_cov_h2o, plot_cov_co2, plot_cov_T, ncol=1)

df_all3<-filter(df_all, as.POSIXct(df_all$Timestamp)<"2022-08-08 23:00:00")

plot_sonic<-ggplot(df_all3, aes(x=Timestamp, y=T_sonic))+
  geom_point(size=0.1, colour="black")+
  stat_smooth(method=loess, colour="red")+
  scale_x_datetime(date_breaks="2 hour", date_labels="%d %H:%M")+
  labs(title = (expression(paste("Sonic tempereture"))), x="Time", y = "T (°C)")+
  scale_y_continuous(breaks = seq(0, 30, by =5))+
  coord_cartesian(ylim = c(0, 30))+
  theme_bw()+
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 10))

##################################################

df_all_rot1 <- df_all3 %>%
  mutate(halfhour=ceiling_date(Timestamp, unit="30 minutes")) %>%
  group_by(halfhour) %>%
  summarise(epsilon = atan2(mean(Uy),mean(Ux))) %>%
  mutate(epsilon_deg = epsilon*180/pi) %>%
  merge(df_all3, all=TRUE) %>%
  mutate(u2 = Ux*cos(epsilon) + Uy*sin(epsilon),
         v2 = -Ux*sin(epsilon) + Uy*cos(epsilon),
         w2 = Uz) %>%
  group_by(halfhour) %>%
  mutate(mean_v2 = mean(v2))

df_all_rot2 <- df_all_rot1 %>%
  group_by(halfhour) %>%
  summarise(teta = atan2(mean(w2),mean(u2))) %>%
  merge(df_all_rot1, all=TRUE) %>%
  mutate(u3 = u2*cos(teta) + w2*sin(teta),
         w3 = -u2*sin(teta) + w2*cos(teta),
         v3 = v2) %>%
  group_by(halfhour) %>%
  mutate(mean_v3 = mean(v3),
         mean_w3 = mean(w3),
         mean_u3 = mean(u3))


df_all_halfhour <- df_all_rot2 %>%group_by(halfhour) %>%
  summarise(cov_w_T = cov(w3,T_sonic),
            cov_w_co2 = cov(w3,CO2),
            cov_w_h2o = cov(w3,H2O))

g_wT <- ggplot(data=df_all_halfhour, aes(x=halfhour, y=cov_w_T))+
  geom_line(size=0.5)+
  geom_point(color="red", size=0.5)+
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %H:%M")+
  #geom_hline(yintercept = mean(df_all$Uz,na.rm = TRUE), color="red")+
  theme_bw()+
  ylab("Cov W_T")+
  #scale_x_datetime(date_breaks = "1 hour", date_labels = "%d %H:%M")+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())


g_wH2O <- ggplot(data=df_all_halfhour, aes(x=halfhour, y=cov_w_h2o))+
  geom_line(size=0.5)+
  geom_point(color="red", size=0.5)+
  #geom_hline(yintercept = mean(df_all$Uz,na.rm = TRUE), color="red")+
  theme_bw()+
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %H:%M")+
  ylab("Cov W_H2O")+
  #scale_x_datetime(date_breaks = "1 hour", date_labels = "%d %H:%M")+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())


g_wCO2 <- ggplot(data=df_all_halfhour, aes(x=halfhour, y=cov_w_co2))+
  geom_line(size=0.5)+
  #geom_hline(yintercept = mean(df_all$Uz,na.rm = TRUE), color="red")+
  geom_point(color="red", size=0.5)+
  theme_bw()+
  ylab("Cov W_CO2")+
  xlab("2022-08-08 to 2022-08-09")+
  scale_x_datetime(date_breaks = "2 hours", date_minor_breaks = "1 hour", date_labels = "%d %H:%M")+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        axis.text.x=element_text(angle = 30, hjust = 1))

grid.arrange(g_wH2O, g_wT, g_wCO2)


############Computing proper wind directions
g1<-ggplot(df_all_rot2, aes(x=epsilon_deg)) +
  geom_histogram(col="grey", alpha=0.5)

df_wd <- df_all_rot2 %>%
  mutate(eps_360 = ifelse(epsilon_deg < 0, epsilon_deg+360, epsilon_deg))

g2 <- ggplot(df_wd, aes(x=eps_360))+
  geom_histogram(col="grey", alpha=0.5)

grid.arrange(g1, g2, ncol=1)





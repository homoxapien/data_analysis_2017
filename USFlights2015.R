#read in data
setwd("~/documents/data_science_R/flight-delays")
f_fl <- file("flights.csv", open="r")
dat <- read.csv(f_fl)
f_al <- file("my.airlines.csv", open="r")
ref_airline <- read.csv(f_al)
close(f_fl)
close(f_al)

#clean missing value
dat <- dat[complete.cases(dat),]
#clean useless features
dat[["FLIGHT_NUMBER"]] <- NULL
dat[["TAIL_NUMBER"]] <- NULL
dat[["YEAR"]] <- NULL

#weekday
ref_weekday <- data.frame(DAY_OF_WEEK=c(1,2,3,4,5,6,7), WEEKDAY=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
dataset <- merge(x=dat, y=ref_weekday, by="DAY_OF_WEEK", all.x=TRUE)

#airline full name
dataset <- merge(x=dataset, y=ref_airline, by="AIRLINE", all.x=TRUE)

#timepoint clearificaiton
timepoint <- c("SCHEDULED_DEPARTURE", "DEPARTURE_TIME", "WHEELS_OFF", "WHEELS_ON", "SCHEDULED_ARRIVAL", "ARRIVAL_TIME")
SCDPmin <- dataset$SCHEDULED_DEPARTURE %% 100
SCDPhr <- (dataset$SCHEDULED_DEPARTURE - SCDPmin)/100
DPTMmin <- dataset$DEPARTURE_TIME %% 100
DPTMhr <- (dataset$DEPARTURE_TIME -DPTMmin)/100
WHOFmin <- dataset$WHEELS_OFF %% 100
WHOFhr <- (dataset$WHEELS_OFF - WHOFmin)/100
WHONmin <- dataset$WHEELS_ON %% 100
WHONhr <- (dataset$WHEELS_ON -WHONmin)/100
SCARmin <- dataset$SCHEDULED_ARRIVAL %% 100
SCARhr <- (dataset$SCHEDULED_ARRIVAL - SCARmin)/100
ARTMmin <- dataset$ARRIVAL_TIME %% 100
ARTMhr <- (dataset$ARRIVAL_TIME - ARTMmin)/100
#join the data
dataset <- cbind(dataset, SCDPhr, SCDPmin, DPTMhr, DPTMmin, WHOFhr, WHOFmin, WHONhr, WHONmin, SCARhr, SCARmin, ARTMhr, ARTMmin)
#set those six timepoints to null
for(tp in timepoint){
	dataset[[tp]] <- NULL
}
head(dataset)

#[AIRLINE]_ver2
##summary(dataset$AIRLINE_FN)
##rec <- 0
##for (a in names(summary(dataset$AIRLINE_FN))){
##	rec <- c(rec, summary(dat$AIRLINE)[a])
##}
##count <- rec[-1]
df_airline_count <- data.frame(AIRLINE=names(summary(dataset$AIRLINE)), COUNT=summary(dataset$AIRLINE))
df_airline_count$AIRLINE <- factor(df_airline_count$AIRLINE, levels=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN")) 
df_airline_count <- df_airline_count[order(-summary(dataset$AIRLINE)),]
#df_airline_count <- df_airline_count[order(df_airline_count$COUNT, decreasing=TRUE), ]
require(ggplot2)
p2 <- ggplot(data=df_airline_count, aes(x=AIRLINE,y=COUNT))+
    geom_bar(stat="identity", fill="steelblue")
p2 <- p2 + coord_flip()
p2

#p1
plot(dataset$ARRIVAL_DELAY, dataset$DEPARTURE_DELAY, pch=16, xlab="ARRIVAL DELAY(min)", ylab="DEPARTURE DELAY(min)", main="DEPARTURE DELAY vs ARRIVAL DELAY",col="darkgrey")
lm_DP_delay <- lm(dataset$ARRIVAL_DELAY ~ dataset$DEPARTURE_DELAY)
abline(coef(lm_DP_delay), lwd=2, linetype="dashed")
text(1500,1000,paste("(k,m)=(",round(lm_DP_delay$coefficients[1],3),",",round(lm_DP_delay$coefficients[2],3),")"))

#p2
p2 <-ggplot(dataset, aes(x=AIRLINE), color="black")+
 geom_bar(stat="count", width=0.7, fill="darkgrey")+
 ggtitle("FLIGHTS by AIRLINE")+
 labs(x="AIRLINE", y="# of FLIGHT")+
 #geom_text(aes(label))+
 theme_classic()+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))
p2
#new p2
p2 <- ggplot(df_airline_count, aes(x=AIRLINE, y=COUNT, fill=AIRLINE))+
 geom_bar(stat="identity", width=0.7)+
 ggtitle("FLIGHTS by AIRLINE")+
 labs(x="AIRLINE", y="# of FLIGHT")+
 geom_text(aes(label=COUNT), vjest=-10, size=4)+
 theme_classic()+
 coord_flip()+
 scale_fill_grey()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))
p2

#p3
p3 <- ggplot(dataset, aes(x=AIRLINE, y=DEPARTURE_DELAY))+
 geom_boxplot(notch=TRUE, outlier.size=.5, outlier.shape=23)+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))+
 ylim(0,120)
p3
#p3(<120)
p3 <- ggplot(dataset[dataset$DEPARTURE_DELAY<120,], aes(x=AIRLINE, y=DEPARTURE_DELAY))+
 geom_boxplot(notch=TRUE, outlier.size=.5, outlier.shape=23)+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))
p3

#p4
p4 <- ggplot(dataset, aes(x=AIRLINE, y=ARRIVAL_DELAY))+
 geom_boxplot(notch=TRUE, outlier.size=.5, outlier.sha
 pe=23)+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))+
 ylim(0,80)+
 geom_hline(yintercept=37, color="red", linetype="dashed")
p4
#p4(<120)
p4 <- ggplot(dataset[dataset$ARRIVAL_DELAY<120,], aes(x=AIRLINE, y=ARRIVAL_DELAY))+
 geom_boxplot(notch=TRUE, outlier.size=.5, outlier.shape=23)+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))
p4

#p5
dataset$MONTH <- factor(dataset$MONTH, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df_month_count <- data.frame(MONTH=names(summary(dataset$MONTH)), COUNT=as.numeric(summary(dataset$MONTH)))
dataset_MT <- split(dataset, dataset$MONTH)
count.DP.delay <- function(x_DELAY, period=0){
	sum(x_DELAY[["DEPARTURE_DELAY"]]>period)
}
count.AR.delay <- function(x_DELAY, period=0){
	sum(x_DELAY[["ARRIVAL_DELAY"]]>period)
}
count_DP15_delay <- sapply(dataset_MT, count.DP.delay, period=15)
count_AR15_delay <- sapply(dataset_MT, count.AR.delay, period=15)
rate_DP15_delay <- count_DP15_delay/df_month_count$COUNT
rate_AR15_delay <- count_AR15_delay/df_month_count$COUNT
df_month_rate_DP15 <- data.frame(df_month_count, RATE=round(rate_DP15_delay,2), IDE=rep("DP15", 12))
df_month_rate_AR15 <- data.frame(df_month_count, RATE=round(rate_AR15_delay,2), IDE=rep("AR15", 12))
df_month_rate_DP15AR15 <- rbind(df_month_rate_DP15, df_month_rate_AR15)
#p5 <- ggplot(dataset, aes(x=MONTH))+
# geom_bar(stat="count", width=0.5, fill="#999999")
#p5 
p5 <- ggplot(df_month_rate_DP15AR15)+
 ggtitle("DELAY RATE vs MONTH")+
 labs(x="MONTH", y="DELAY RATE")+
 geom_bar(aes(x=MONTH, y=COUNT/2), stat="identity", width=0.5, fill="gold")+
 geom_line(aes(x=MONTH, y=RATE*max(df_month_rate_DP15AR15$COUNT), group=IDE, color=IDE), stat="identity")+
 geom_point(aes(x=MONTH, y=RATE*max(df_month_rate_DP15AR15$COUNT), group=IDE))+
 geom_text(aes(label=RATE, x=MONTH, y=RATE*max(df_month_rate_DP15AR15$COUNT)), vjust=2)+
 #geom_text(aes(label=COUNT, x=MONTH, y=0.95*COUNT))+
 scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
 scale_y_continuous(sec.axis=sec_axis(~./max(df_month_rate_DP15AR15$COUNT)))+
 scale_color_manual(values=c("#999999","#E69F00"))+
 theme(plot.title=element_text(size=12))
p5

#p6,p7
dataset$SCDPhr_f <- factor(dataset$SCDPhr, labels=c("0hr","1hr","2hr","3hr","4hr","5hr","6hr","7hr","8hr","9hr","10hr","11hr","12hr","13hr","14hr","15hr","16hr","17hr","18hr","19hr","20hr","21hr","22hr","23hr"))
p6 <- ggplot(dataset, aes(x=SCDPhr_f, y=DEPARTURE_DELAY, fill=SCDPhr_f))+
 geom_boxplot(notch=TRUE, outlier.size=.5, outlier.shape=23)+
 ggtitle("DEPARTURE DELAY vs SCHEDULED DEPARTURE TIME")+
 labs(x="SCHEDULED DEPARTURE TIME",y="DEPARTURE DELAY")+
 ylim(-30,120)+
 scale_fill_grey()+
 stat_summary(fun.y=mean, geom="point", shape=4, size=2)+
 stat_summary(fun.y=mean, geom="line", aes(group=1))+
 #theme(legend.position="none")
 theme_classic()
p6
p7 <- ggplot(dataset, aes(x=SCDPhr_f, y=ARRIVAL_DELAY, fill=SCDPhr_f))+
 geom_boxplot(notch=TRUE, outlier.size=.5, outlier.shape=23)+
 ggtitle("ARRIVAL DELAY vs SCHEDULED DEPARTURE TIME")+
 labs(x="SCHEDULED DEPARTURE TIME",y="ARRIVAL DELAY")+
 ylim(0,120)+
 scale_fill_grey()+
 stat_summary(fun.y=mean, geom="point", shape=4, size=2)+
 stat_summary(fun.y=mean, geom="line", aes(group=1))+
 #theme(legend.position="none")
 theme_classic()
p7

#p8
dataset_DP <- data.frame(NAME=dataset$AIRLINE, TIME=dataset$DEPARTURE_DELAY, IDE=rep("DEPARTURE",length(dataset$DEPARTURE_DELAY)))
dataset_AR <- data.frame(NAME=dataset$AIRLINE, TIME=dataset$ARRIVAL_DELAY, IDE=rep("ARRIVAL",length(dataset$ARRIVAL_DELAY)))
dataset_DPAR <- rbind(dataset_DP, dataset_AR)
p8 <- ggplot(dataset_DPAR, aes(x=NAME, y=TIME, fill=IDE))+
 geom_boxplot(notch=TRUE, outlier.size=.5, outlier.shape=23)+
 ggtitle("DELAY TIME vs AIRLINE")+
 labs(x="AIRLINE", y="DELAY TIME(min)")+
 ylim(-30,120)+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))+
 scale_fill_manual(values=c("#999999","gold","#56B4E9"))+
 #theme_classic()
 theme(plot.title=element_text(size=12))
p8
#E69F00

#p9
dataset_AL <- split(dataset, dataset$AIRLINE)
##count_delay <- sum(dataset_AL$DL[["DEPARTURE_DELAY"]]>0)
# function to count departure delay
count.DP.delay <- function(x_DELAY, period=0){
	sum(x_DELAY[["DEPARTURE_DELAY"]]>period)
}
count_DP_delay <- sapply(dataset_AL, count.DP.delay)
#function to count arrival delay
count.AR.delay <- function(x_DELAY, period=0){
	sum(x_DELAY[["ARRIVAL_DELAY"]]>period)
}
count_AR_delay <- sapply(dataset_AL, count.AR.delay)
#rate of DP and AR delay
rate_DP_delay <- count_DP_delay/df_airline_count$COUNT
rate_AR_delay <- count_AR_delay/df_airline_count$COUNT
#>15min upset me
count_DP15_delay <- sapply(dataset_AL, count.DP.delay, period=15)
count_AR15_delay <- sapply(dataset_AL, count.AR.delay, period=15)
rate_DP15_delay <- count_DP15_delay/df_airline_count$COUNT
rate_AR15_delay <- count_AR15_delay/df_airline_count$COUNT
##df_airline_rate <- cbind(df_airline_count, rate_DP_delay, rate_AR_delay, rate_DP15_delay, rate_AR15_delay)
er95_rate_DP15_delay <- 1.96*sqrt(rate_DP15_delay * (1-rate_DP15_delay) / df_airline_count$COUNT)
er95_rate_AR15_delay <- 1.96*sqrt(rate_AR15_delay * (1-rate_AR15_delay) / df_airline_count$COUNT)
df_airline_rate_DP15 <- data.frame(NAME=df_airline_count$AIRLINE, RATE=rate_DP15_delay, ER95=er95_rate_DP15_delay, IDE=rep("DP15",length(rate_DP15_delay)))
df_airline_rate_AR15 <- data.frame(NAME=df_airline_count$AIRLINE, RATE=rate_AR15_delay, ER95=er95_rate_AR15_delay, IDE=rep("AR15",length(rate_AR15_delay)))
df_airline_rate_DP15AR15 <- rbind(df_airline_rate_DP15, df_airline_rate_AR15)
p9 <- ggplot(df_airline_rate_DP15AR15, aes(x=NAME, y=RATE, group=IDE, color=IDE))+
 geom_line(aes(color=IDE))+
 geom_point(aes(color=IDE))+
 geom_errorbar(aes(ymin=RATE-ER95, ymax=RATE+ER95), width=.5, position=position_dodge(0.05))+
 geom_text(aes(label=round(RATE,2)), vjust=-1, hjust=1, size=3.5, color="black")+
 ggtitle("DELAY RATE vs AIRLINE")+
 labs(x="AIRLINE", y="DELAY RATE")+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))+
 scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
 #theme_classic()
 theme(plot.title=element_text(size=12))
p9


#P10
dataset_taxi_count_OT <- data.frame(TAXI=dataset$TAXI_OUT, IDE=rep("OUT", length(dataset$TAXI_OUT)))
dataset_taxi_count_IN <- data.frame(TAXI=dataset$TAXI_IN, IDE=rep("IN", length(dataset$TAXI_IN)))
dataset_taxi_count_OTIN <- rbind(dataset_taxi_count_OT, dataset_taxi_count_IN)
require(plyr)
mu <- ddply(dataset_taxi_count_OTIN, "IDE", summarise, grp.mean=mean(TAXI))
p10 <- ggplot(dataset_taxi_count_OTIN, aes(x=TAXI, color=IDE, fill=IDE))+
 geom_histogram(position="identity", bins=100, alpha=0.5)+
 #geom_histogram(aes(y=..density..), position="identity", bins=30, alpha=0.5)+
 #geom_density(alpha=0.6)+
 geom_vline(data=mu, aes(xintercept=grp.mean, color=IDE), linetype="dashed")+
 ggtitle("TAXI TIME DISTRIBUTION")+
 labs(x="TAXI TIME(min)", y="COUNT")+
 annotate("text", x=18, y=1.4e05, label=paste("mean=", round(mean(dataset_taxi_count_IN$TAXI),2),"\nmedian=", median(dataset_taxi_count_IN$TAXI),"\nstd=",round(sd(dataset_taxi_count_IN$TAXI),2)))+
 annotate("text", x=30, y=4.8e04, label=paste("mean=", round(mean(dataset_taxi_count_OT$TAXI),2),"\nmedian=", median(dataset_taxi_count_OT$TAXI),"\nstd=",round(sd(dataset_taxi_count_OT$TAXI),2)))+
 xlim(0,100)+
 scale_color_manual(values=c("#999999","gold","#56B4E9"))+
 scale_fill_manual(values=c("#999999","gold","#56B4E9"))+
 theme(plot.title=element_text(size=12))
 #theme_classic() 
p10

#p11
df_weekday_count <- data.frame(WEEKDAY=names(summary(dataset$WEEKDAY)), COUNT=as.numeric(summary(dataset$WEEKDAY)))
dataset_WD <- split(dataset, dataset$WEEKDAY)
count.DP.delay <- function(x_DELAY, period=0){
	sum(x_DELAY[["DEPARTURE_DELAY"]]>period)
}
count.AR.delay <- function(x_DELAY, period=0){
	sum(x_DELAY[["ARRIVAL_DELAY"]]>period)
}
count_DP15_delay <- sapply(dataset_WD, count.DP.delay, period=15)
count_AR15_delay <- sapply(dataset_WD, count.AR.delay, period=15)
rate_DP15_delay <- count_DP15_delay/df_weekday_count$COUNT
rate_AR15_delay <- count_AR15_delay/df_weekday_count$COUNT
df_weekday_rate_DP15 <- data.frame(df_weekday_count, RATE=round(rate_DP15_delay,2), IDE=rep("DP15", 7))
df_weekday_rate_AR15 <- data.frame(df_weekday_count, RATE=round(rate_AR15_delay,2), IDE=rep("AR15", 7))
df_weekday_rate_DP15AR15 <- rbind(df_weekday_rate_DP15, df_weekday_rate_AR15)

p11 <- ggplot(df_weekday_rate_DP15AR15)+
 ggtitle("DELAY RATE vs WEEKDAY")+
 labs(x="WEEKDAY", y="DELAY RATE")+
 geom_bar(aes(x=WEEKDAY, y=COUNT/2), stat="identity", width=0.5, fill="gold")+
 geom_line(aes(x=WEEKDAY, y=RATE*max(df_weekday_rate_DP15AR15$COUNT), group=IDE, color=IDE), stat="identity")+
 geom_point(aes(x=WEEKDAY, y=RATE*max(df_weekday_rate_DP15AR15$COUNT), group=IDE))+
 geom_text(aes(label=RATE, x=WEEKDAY, y=RATE*max(df_weekday_rate_DP15AR15$COUNT)), vjust=2)+
 #geom_text(aes(label=COUNT, x=WEEKDAY, y=0.95*COUNT))+
 scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
 scale_y_continuous(sec.axis=sec_axis(~./max(df_weekday_rate_DP15AR15$COUNT)))+
 scale_color_manual(values=c("#999999","#E69F00"))+
 #theme_classic()
 theme(plot.title=element_text(size=11))
p11

#p12
dataset$SPEED <- dataset$DISTANCE/dataset$AIR_TIME
dataset$AIRLINE <- factor(dataset$AIRLINE, levels=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN")) 
my.summary <- function(x){
	m <- mean(x)
	ymin <- m-sd(x)
	ymax <- m+sd(x)
	return(c(y=m,ymin=ymin,ymax=ymax))
}
p12 <- ggplot(dataset, aes(x=AIRLINE, y=SPEED*60, fill=AIRLINE))+
 ggtitle("SPEED vs AIRLINE")+
 labs(x="AIRLINE", y="SPEED(mph)")+
 geom_violin(trim=FALSE)+
 #geom_boxplot(width=0.5)+
 stat_summary(fun.y=mean, geom="point", size=0.5)+
 stat_summary(fun.data=my.summary)+
 coord_flip()+
 scale_x_discrete(limits=c("HA","VX","AS","F9","NK","US","B6","MQ","UA","OO","EV","DL","AA","WN"))+
 scale_fill_grey()+
 #scale_color_grey()+
 theme_classic()
 #theme_minimal()
p12

#p13
dataset_scatter_MX <- data.frame(DP_DELAY=dataset$DEPARTURE_DELAY, TAXI_OUT=dataset$TAXI_OUT, AIR_TIME=dataset$AIR_TIME, TAXI_IN=dataset$TAXI_IN, AR_DELAY=dataset$ARRIVAL_DELAY)
pairs(dataset_scatter_MX)


#Try plotting
plot(dataset$DEPARTURE_DELAY)


#Export csv file
write.csv(dataset, "my.flights.csv")




#[AIRLINE]_ver1
i_AIRLINE_AA <- dataset1[,"AIRLINE"]=="AA"
i_AIRLINE_UA <- dataset1[,"AIRLINE"]=="UA"
i_AIRLINE_US <- dataset1[,"AIRLINE"]=="US"
i_AIRLINE_F9 <- dataset1[,"AIRLINE"]=="F9"
i_AIRLINE_B6 <- dataset1[,"AIRLINE"]=="B6"
i_AIRLINE_OO <- dataset1[,"AIRLINE"]=="OO"
i_AIRLINE_AS <- dataset1[,"AIRLINE"]=="AS"
i_AIRLINE_NK <- dataset1[,"AIRLINE"]=="NK"
i_AIRLINE_WN <- dataset1[,"AIRLINE"]=="WN"
i_AIRLINE_DL <- dataset1[,"AIRLINE"]=="DL"
i_AIRLINE_EV <- dataset1[,"AIRLINE"]=="EV"
i_AIRLINE_HA <- dataset1[,"AIRLINE"]=="HA"
i_AIRLINE_MQ <- dataset1[,"AIRLINE"]=="MQ"
i_AIRLINE_VX <- dataset1[,"AIRLINE"]=="VX"

ALL_AIRLINE <- c("AA", "UA", "US", "F9", "B6", "OO", "AS", "NK", "WN", "DL", "EV", "HA", "MQ", "VX")
idf_AIRLINE <- data.frame(i_AIRLINE_AA,i_AIRLINE_UA,i_AIRLINE_US,i_AIRLINE_F9,i_AIRLINE_B6,i_AIRLINE_OO,
                          i_AIRLINE_AS,i_AIRLINE_NK,i_AIRLINE_WN,i_AIRLINE_DL,i_AIRLINE_EV,i_AIRLINE_HA,
                          i_AIRLINE_MQ,i_AIRLINE_VX)
colnames(idf_AIRLINE) <- ALL_AIRLINE

count <- sapply(idf_AIRLINE,sum)

df <- data.frame(ALL_AIRLINE,count)
df[order(-count),]

require(ggplot2)
install.packages("gridExtra")
require(gridExtra)
require(grid)
require(lattice)
p2 <- ggplot(data=df, aes(x=reorder(ALL_AIRLINE,count),y=count))+
    geom_bar(stat="identity", fill="steelblue")
p2 <- p2 + coord_flip()
p2


#graffiti
p <- ggplot(test1)+
 geom_bar(aes(x=NAME,y=COUNT/2), stat="identity")+
 geom_line(aes(x=NAME, y=COST*max(test1$COUNT), group=PART, color=PART), stat="identity")+
 geom_text(aes(label=COST, x=NAME, y=COST*max(test1$COUNT)))+
 geom_text(aes(label=COUNT, x=NAME, y=0.95*COUNT))+
 scale_y_continuous(sec.axis=sec_axis(~./max(test1$COUNT)))
p


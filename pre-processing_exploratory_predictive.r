df<-read.csv("path/df_only_rep.csv",stringsAsFactors=FALSE)
str(df)
length(unique(df$BP))
#3515 customers are unique in here.
t1<-sort(table(df$BP))
tail(t1)
#63 maximum iterations per customer.
df[df$BP==147622053,"COMBINE"]
library(arules)
install.packages("arules")
library(arules)
#now:
#convert the variables into factor ones. also subset the variables into just the ones that you need
str(df)

df_min<-df[,c("DNP_TYPE","DNP_STATUS",
              "BPKIND_CURRENT","COMPCODE","STATE",
              "NETWORK","CUSTOMER_TYPE")]

str(df_min)
df_min[[1]]<-as.factor(df_min[[1]])
str(df_min)
i=1
while (i<7){
  df_min[[i]]<-as.factor(df_min[[i]])
  i=i+1
}

str(df_min)
df_min$CUSTOMER_TYPE<-as.factor(df_min$CUSTOMER_TYPE)
str(df_min)
df_min$COMBINE<-as.factor(df$COMBINE)
str(df_min)


#now time to apply the association rule. lets see if it gives any sensible rules:
a1<-apriori(df_min,parameter = list(minlen=3, supp=0.3,conf=0.5));inspect(a1)
str(df_min)
df_min$DNP_TYPE<-NULL
a1<-apriori(df_min,parameter = list(minlen=3, supp=0.2,conf=0.5))
ares<-print(inspect(a1))
class(ares)
ares

inspect(sort(a1,by="support"))



inspect(sort(a1,by="support"))


#now going to observe changes in the combine status codes if possible.
str(df)
head(df)

#first add the codes dummy variables by customer BPs.

#first create the dummy variables:
library(dummies)
df_min2<-df[,c("BP","COMBINE","STATE",
               "NETWORK","CUSTOMER_TYPE")]

dfdum_min2<-dummy.data.frame(df_min2,name="COMBINE")
str(dfdum_min2)
#now group by BP and add each variable or atleast the most common ones:
names(dfdum_min2)<-gsub("COMBINE","",names(dfdum_min2))
names(dfdum_min2)<-gsub("COMBINE ","",names(dfdum_min2))
names(dfdum_min2)<-trimws(names(dfdum_min2))
names(dfdum_min2)<-gsub(" ","_",names(dfdum_min2))
names(dfdum_min2)<-gsub("-","_",names(dfdum_min2))
str(dfdum_min2)
"BP" %in% names(dfdum_min2)
"N-CO ACCE" %in% names(dfdum_min2)
library(plyr)
df_sum_comb_bp<-ddply(dfdum_min2,~BP,summarise,S1=sum(N_CO_ACCE),S2=sum(COMP_ACCE),
                      S3=sum(N_CO_ACCE_C_SE_C_RE_CANC))
str(df_sum_comb_bp)

"N_CO_ACCE_C_SE_C_RE_CANC" %in% names(dfdum_min2)
nrow(df_sum_comb_bp)
length(unique(df$BP))
sink("comb_accum.txt")
print(df_sum_comb_bp)
sink()
colSums(df_sum_comb_bp[,-1],na.rm=TRUE)
tail(df_sum_comb_bp[order(df_sum_comb_bp$S1),])


summary(df_sum_comb_bp)
df_sum_comb_bp[df_sum_comb_bp$S1==47,]

nrow(df[df$BP==147622053,])
head(df)

prev=1
for (current in df$COMBINE[-1, ]){
  cur_num=2
  df$schange<-ifelse(df$COMBINE[prev]==current,0,1)
  df$tchange<-difftime(df$CREATED_DATE[cur_num],df$CREATED_DATE[prev])
  cur_num=cur_num+1
  prev=prev+1
  }
}


#tapply(df$COMBINE,df$BP,)
s2<-split(df$COMBINE,as.factor(df$BP))
length(s2)
head(s2)
table(s2[['147622053']])
st<-split(df$CREATED_DATE,as.factor(df$BP))
df$C_DATE<-as.Date(df$CREATED_DATE,"%d/%m/%Y")
s_cdate<-split(df$C_DATE,as.factor(df$BP))
class(s_cdate[['147622053']][1])

i=1
l_diffdate<-c()
while (i<length(s_cdate[['147622053']])){
  l_diffdate<-c(l_diffdate,difftime(s_cdate[['147622053']][i+1],s_cdate[['147622053']][i],
                                    units = 'days'))
  i=i+1
}

l_diffdate
split(st[['147622053']],as.factor(s2[['147622053']]))
s_by_time<-split(s2[['147622053']],as.factor(st[['147622053']]))
names()
df_split<-split(df[,c("CREATED_DATE","ATTEMPTED_DATE","COMBINE")],as.factor(df$BP))
#Now you have a list of dataframe where each member is a dataframe.
str(df)
length(df_split)
df_split[1]

lapply(df_split[1:5],function(x){
  i=1
  while(i<=nrow(x)){
    x$schange[i]<-ifelse(x$COMBINE[i+1]==x$COMBINE[i],0,1)
    x$tchange[i]<-difftime(x$CREATED_DATE[i+1],x$CREATED_DATE[i],units = 'days')
    i=i+1
    print(x)
  }
})

df_split[1]

difftime(as.Date(21/07/2012),5/10/2012,units='days')

#^ problem in created date. its not date. its character.
df_split_2<-split(df[,c("C_DATE","COMBINE")],as.factor(df$BP))


lapply(df_split_2[1:5],function(x){
  i=1
  while(i<=nrow(x)){
    x$schange[i]<-ifelse(x$COMBINE[i+1]==x$COMBINE[i],0,1)
    x$tchange[i]<-difftime(x$C_DATE[i+1],x$C_DATE[i],units = 'days')
    i=i+1
    print(x)
  }
})

#repetition is happening. don't know why.

length(unique(df$BP))




df<-read.csv("path/df_only_rep1.csv",stringsAsFactors=FALSE)
length(unique(df$BP))

split_bp2<-split(df,f = as.factor(df$BP))
length(split_bp2)
str(df)
split_bp2[[1]][1,]
split_bp2[['147622053']][,c("CREATED_DATE","COMBINE")]
split_bp2[['147622053']
df[df$BP==147622053,c("CREATED_DATE","COMBINE")]
l2<-lapply(split_bp2[1:5],function(x) x[nrow(x),])

dtemp<-data.frame()

lapply(l2,function(x){
  dtemp<-rbind(dtemp,x)
  print(dtemp)
  }
)
dtemp
dtemp
str(df)
df2<-df
r1rbind(df2,df2[nrow(df2),])
d2<-as.data.frame(l2)
str(d2)

d3<-as.data.frame(do.call(rbind, l2))
d3[,c("BP","C_DATE","COMBINE")]
df[df$BP %in% c(120012050,120012329),c("BP","C_DATE","COMBINE")]

length(split_bp2)
#going to run the following list which will grab the latest record of the repeating customers:
list_by_bp<-lapply(split_bp2,function(x) x[nrow(x),])
length(list_by_bp)
df_rep_latest<-as.data.frame(do.call(rbind, list_by_bp))
str(df_rep_latest)

#now rbind the two dataframes:
#the ones which aren't repeating and the ones which are.
df_not_rep<-read.csv("path/df_not_rep_dates.csv",stringsAsFactors=FALSE)
str(df_not_rep)
length(names(df_not_rep))
length(names(df_rep_latest))
names(df_not_rep)
names(df_rep_latest)

df_not_rep<-df_not_rep[,!names(df_not_rep) %in% c("X.1","X","B_DATE","C_WDYAS","A_DATE","C_A_DIFF","S_A_DIFF")]
str(df_not_rep)
df_rep_latest<-df_rep_latest[,!names(df_rep_latest) %in% c("X.1","X","A_DATE","CA_DIFF","SA_DIFF")]
str(df_rep_latest)
names(df_not_rep)
names(df_rep_latest)
df_not_rep$C_WDAYS<-NULL
length(names(df_not_rep))


#now rbind them both
final_df_no_rep<-rbind(df_not_rep,df_rep_latest)
str(final_df_no_rep)
write.csv(final_df_no_rep,file="final_df_no_rep.csv")

t1<-sort(table(final_df_no_rep$BP))
tail(t1)
length(unique(final_df_no_rep$BP))==nrow(final_df_no_rep)

print ('r')
df<-read.csv("D:/Designing snippets/MSc Logs/AGL/DNP Data Anonymised - Copy.csv",stringsAsFactors=FALSE)
str(df)
names(df)
df_min<-df[,c("CREATED_DATE","SCHEDULED_DATE","ATTEMPTED_DATE","YEAR","DNP_TYPE","DNP_STATUS",
              "BPKIND_CURRENT","COMPCODE","STATE",
              "NETWORK","CUSTOMER_TYPE","CitPostCode","City")]
df_min<-df[,c(6,7,9,11,12,13,14,17,18,19,20,22,23,24)]
str(df_min)
#for the time being, also ignore the dates and just focus on the remaining
#and also group the combine codes.
comb_t<-sort(table(df_min$COMBINE))
comb_t
#length(comb_t[comb_t>118])
frequent_comb_codes<-comb_t[comb_t>118]
length(frequent_comb_codes)
length(unique(df_min$COMBINE))
frequent_comb_names<-names(frequent_comb_codes)
frequent_comb_names
#create dummy columns and then remove the ones which aren't frequent.
library(dummies)
get.dummy
#now trying to create the dummy dataframes. #can also think of grouping the columns.
dumdf_comb<-dummy.data.frame(df_min,name='COMBINE')
names(dumdf_comb)
head(dumdf_comb)
#its done. now time to remove the columns. #to delete all together: use df$col_name<-NULL
#its better to combine all the less frequent columns as:
less_freq_comb<-comb_t[comb_t<118]
less_freq_comb_names<-names(less_freq_comb)
less_freq_comb_names #got the names of the least frequent comb codes. need to either add COMBINE 
#before these or need to remove
#COMBINE before the column names :
names(dumdf_comb)
names(dumdf_comb)<-sub("COMBINE ","",names(dumdf_comb))
names(dumdf_comb)<-sub("COMBINE","",names(dumdf_comb))
names(dumdf_comb)<-trimws(names(dumdf_comb))
names(dumdf_comb)
dum2<-dumdf_comb

library(dplyr)
select(dum2,-frequent_comb_names)
cd<-c(1,2,3)
head(select(dum2,cd))
d3<-head(select(dum2,one_of(frequent_comb_names)))
length(less_freq_comb_names)

#form a subset of columns from the main dataframe and rowsum that. and then combine with the main dataframe:

less_freq_comb_names %in% names(dum2)
names(dum2)
less_freq_comb_names<-trimws(less_freq_comb_names)
sum(!less_freq_comb_names %in% names(dum2))

dum_less_freq<-dum2[,less_freq_comb_names]
str(dum_less_freq)
length(names(dum_less_freq))
length(less_freq_comb_names)

#finally got the dataframe which consists of only those codes which are less frequent. rowSum them all. 
less_freq_codes_sum<-rowSums(dum_less_freq,na.rm=TRUE)
length(less_freq_codes_sum)
head(less_freq_codes_sum)
summary(less_freq_codes_sum)
sum(which(less_freq_codes_sum>0)) #you got the sum of all such codes which are less frequent.
#add this to the main dataframe:
dumdf_comb$LESS_FREQ_COMB<-less_freq_codes_sum
str(dumdf_comb)
length(names(dumdf_comb))
head(dumdf_comb$LESS_FREQ_COMB)
colSums(dumdf_comb[,c("LESS_FREQ_COMB","N-CO")],na.rm=TRUE)
#good going. now remove those columns which are less frequent.

dumdf_comb_rem<-dumdf_comb[,!names(dumdf_comb)%in% less_freq_comb_names]

length(names(dumdf_comb_rem))
length(names(dumdf_comb))
length(less_freq_comb_names)
length(names(dum_less_freq))
str(dumdf_comb_rem)

#summary of what you did:
#created dummy variables for each code of the combine. then combined all less frequent codes in one and added
#that to the 
#main dataframe.

#going to do the same for city and state
names(df)

#lets do that for network first.
summary(df$NETWORK)
unique(df$NETWORK)
network_t<-sort(table(df$NETWORK))
tail(network_t)
dumdf_comb[dumdf_comb_rem$NETWORK=="?","NETWORK"]
dumdf_comb_rem$NETWORK[which(dumdf_comb_rem$NETWORK=="?")]<-"UNKNOWN"

net_t<-sort(table(dumdf_comb_rem$NETWORK))
net_t
dumdf_comb_net<-dummy.data.frame(dumdf_comb_rem,name="NETWORK")
length(names(dumdf_comb_net))

#^created the dataframe with all levels of network. now going to do the same for State.

table(dumdf_comb_net$STATE)
dumdf_comb_net_state<-dummy.data.frame(dumdf_comb_net,name="STATE")
length(names(dumdf_comb_net_state))

dumdf_comb_net_state_type<-dummy.data.frame(dumdf_comb_net_state,name='CUSTOMER_TYPE')
dumdf_comb_net_state_type_dnpstatus<-dummy.data.frame(dumdf_comb_net_state_type,name='DNP_STATUS')
dumdf_comb_net_state_type_dnpstatus_year<-dummy.data.frame(dumdf_comb_net_state_type_dnpstatus,name='YEAR')
dumdf_comb_net_state_type_dnpstatus_year_comp<-dummy.data.frame(dumdf_comb_net_state_type_dnpstatus_year,name='COMPCODE')


write.csv(dumdf_comb_net_state,file="dumdf_comb_net_state.csv")
getwd()
rcol_clustering<-c("City","CityPostCode","CREATED_DATE","SCHEDULED_DATE","ATTEMPTED_DATE","DNP_TYPE","KPKIND_CURRENT")

#skip cities and postal code for a moment and now apply k-means clustering.
#shorten dataframe for clustering
names(dumdf_comb_net_state_type_dnpstatus_year)
final_dumdf<-dumdf_comb_net_state_type_dnpstatus_year_comp
final_dumdf_rem<-final_dumdf[,!names(final_dumdf) %in% rcol_clustering]
names(final_dumdf_rem)
final_dumdf_rem2<-na.omit(final_dumdf_rem)

#apply kmeans:

kclust1
sum(is.na(final_dumdf_rem2))
final_dumdf_rem2$BPKIND_CURRENT<-NULL
str(final_dumdf_rem2)
kclust1<-kmeans(final_dumdf_rem2,5)
kclust1 #created five clusters and membership in each of these are:
table(kclust1$cluster)
kclust1$totss
kclust1$size
agg_result<-aggregate(final_dumdf_rem2,by=list(kclust1$cluster),FUN=sum)
agg_result[,c("STATEQLD")]
class(agg_result)


kclust2<-kmeans(final_dumdf_rem2,10)
kclust2$totss
kclust1$totss
kclust1$tot.withinss;kclust2$tot.withinss
kclust1$betweenss;kclust2$betweenss

#clusplot(final_dumdf_rem2, kclust1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) didnt work.


names(final_dumdf_rem2)
plot(final_dumdf_rem2[,"STATENSW","STATEQLD"],col=kclust1$cluster)
kclust20<-kmeans(final_dumdf_rem2,20)

mydata <- final_dumdf_rem2
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 10:30) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#according to that, 13 clusters would be optimal thus:

kclust13<-kmeans(final_dumdf_rem2,13)
kclust13
str(df)
df$CLUSTER<-kclust13$cluster
str(df)
head(kclust13$cluster)
head(df)
write.csv(df,file="df_cluster.csv")


#moving on to applying decision trees on it.
str(df)
df$COMBINE<-trimws(df$COMBINE)
str(df)

library(caTools)
set.seed(3000)
spl=sample.split(df$CUSTOMER_TYPE,SplitRatio = 0.7)
train_set<-subset(df,spl==TRUE)
test_set<-subset(df,spl==FALSE)
library(rpart)
library(rpart.plot)
df_tree<-rpart(CUSTOMER_TYPE~YEAR+COMBINE+DNP_TYPE+DNP_STATUS+COMPCODE+STATE+NETWORK+STATE, data = train_set,method='class')

prp(df_tree)
table(df$CUSTOMER_TYPE)
df$typebin<-ifelse(df$CUSTOMER_TYPE=='NORMAL',1,0)

str(df)
table(df$typebin)
set.seed(3001)
spl=sample.split(df$CUSTOMER_TYPE,SplitRatio = 0.7)
train_set<-subset(df,spl==TRUE)
test_set<-subset(df,spl==FALSE)
df_tree<-rpart(typebin~YEAR+DNP_STATUS+STATE+NETWORK, data = train_set,method='class')
prp(df_tree)
df_tree
printcp(df_tree)
summary(df_tree)
str(final_dumdf_rem2)
final_dumdf_rem2$CUSTOMERTYPE<-df$CUSTOMER_TYPE
final_dumdf_rem3<-final_dumdf_rem2
final_dumdf_rem3['CUSTOMER_TYPEDEAR CUSTOMER']<-NULL
final_dumdf_rem3['CUSTOMER_TYPENORMAL']<-NULL
final_dumdf_rem3$CUSTOMERTYPE<-as.factor(final_dumdf_rem3$CUSTOMERTYPE)

str(final_dumdf_rem3)
df_tree2<-rpart(CUSTOMERTYPE~.,data=final_dumdf_rem3,method='class',cp=0.00001)
prp(df_tree2) #still the same bullshit
printcp(df_tree2)

df_tree_4<-rpart(CUSTOMER_TYPE~YEAR+COMBINE+DNP_TYPE+DNP_STATUS+COMPCODE+STATE+NETWORK, data = train_set,method='class',cp=0.0001)
prp(df_tree_4)
df_tree5<-rpart(CUSTOMER_TYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK, data = train_set,method='class',cp=0.00001)
prp(df_tree5)
printcp(df_tree5)
df_tree6<-rpart(CUSTOMER_TYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK, data = train_set,
                control=rpart.control(maxdepth = 10),cp=0.00001)

prp(df_tree5)
install.packages("party")
library(party)
plot(as.par)
install.packages("partykit")
library(partykit)
plot(as.party(df_tree5))
print(df_tree5)

str(df)
df$CTYPE<-ifelse(df$typebin==1,"N","D")
df$CTYPE<-as.factor(df$CTYPE)
str(df)
df_tree7<-rpart(CTYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK, data = df,method='class',cp=0.00001)
plot(as.party(df_tree7))
df$YEAR<-as.factor(df$YEAR)

df_tree8<-rpart(CTYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK, data = df,method='class',cp=0.00001)
plot(as.party(df_tree8))
print(df_tree8)
table(df$CTYPE)
df_tree9<-rpart(CTYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK, data = df,method='class',cp=0.000001)

plot(as.party(df_tree9))
print(df_tree9)


#now build logistic regressiont to see which variables are significant and which aren't.
str(final_dumdf_rem3)
log_model<-glm(CUSTOMERTYPE~.,family=binomial(link='logit'),data=final_dumdf_rem3)
summary(log_model)
sapply(final_dumdf_rem3,function(x) sum(is.na(x)))
str(df_min)
rcol2<-c("CREATED_DATE","SCHEDULED_DATE","ATTEMPTED_DATE")
df_min2<-df_min[,!names(df_min)%in%rcol2]
str(df_min2)
df_min2$CTYPE<-ifelse(df_min2$CUSTOMER_TYPE=='NORMAL',1,0)
str(df_min2)
log_model_raw<-glm(CTYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK,family=binomial,data=df_min2)
summary(log_model_raw)

str(df_min2)
df_min2$YEAR<-as.factor(df_min2$YEAR)
df_min2$COMBINE<-trimws(df_min2$COMBINE)
df_min2$COMBINE<-as.factor(df_min2$COMBINE)
str(df_min2)
df_min2[3]<-as.factor(df_min2[3])
str(df_min2)
df_min2$DNP_STATUS<-as.factor(df_min2$DNP_STATUS)
str(df_min2)

table(df_min2$YEAR)
head(df_min2$YEAR)
df_min2$DNP_STATUS<-as.factor(df_min2$DNP_STATUS)
df_min2$BPKIND_CURRENT<-as.factor(df_min2$BPKIND_CURRENT)
df_min2$COMPCODE<-as.factor(df_min2$COMPCODE)
df_min2$STATE<-as.factor(df_min2$STATE)
df_min2$NETWORK<-as.factor(df_min2$NETWORK)
df_min2$CityPostCode<-as.factor(df_min2$CityPostCode)
df_min2$City<-as.factor(df_min2$City)

write.csv(df_min2,file="df_min_factors.csv")
log_model_raw_2<-glm(CTYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK,family=binomial,data=df_min2)
summary(log_model_raw_2)
sink("glm_output.txt")
sink()
unlink
str(df_min2)
df_min2$RCTYPE<-ifelse(df_min2$CUSTOMER_TYPE=='DEAR CUSTOMER',1,0)
str(df_min2)
log_model_raw_3<-glm(RCTYPE~YEAR+DNP_STATUS+COMPCODE+STATE+NETWORK,family=binomial,data=df_min2)
sink("glm_output_rev.txt")
summary(log_model_raw_3)
sink()
unlink

anova(log_model_raw_3,test='Chisq')

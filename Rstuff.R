## NOTE: This is my journal of R commands. It decends chronologically.
## My R-comprehension (and thus style) evolves quite a bit thoughout.
## scroll all the way down to see what I've been up to lately.


##---- General Use ----##

## functions:
source("../almond/R/testFunctions.R")

######## 10.2.14 ########

db = sapply(unique(SF$TrapSite),function(x){

    eggs = as.numeric(levels(SF$Eggs))[SF$Eggs]
    females = as.numeric(levels(SF$Females))[SF$Females]
    males = as.numeric(levels(SF$Males))[SF$Males]

    av_eggs =
        mean(eggs[!is.na(eggs[which(LHR$TrapSite == x)])])

    av_fem = 
        mean(females[which(LHR$TrapSite == x)])

    av_mal = 
        mean(males[which(LHR$TrapSite == x)])

    av_adults =
        mean((males+females)[which(LHR$TrapSite
                                   == x)])

    ans = c(av_eggs,av_fem,av_mal,av_adults,x)

    return(ans)
})

### Let's assume eggs are a good response for MD:

 pre = which(LHR$PreMD=="Pre")
 post = which(LHR$PreMD="Post")

 par(mfrow=c(2,1))
 plot(eggs[pre]~Week[pre],data=LHR)
 plot(eggs[post]~Week[post],data=LHR)

 summary(eggs[post])
 summary(eggs[pre])

# ok, so according to the summary MD has had some effect (granted the
# sample sizes differ significantly).
 

# Here I look at predicting eggs based on week and mating disruption
#  (among others)

# actually, all of the puffer data is already contained in TrapSite - right?

m.eggs.week = gam(eggs ~ s(Week) + s(PreMD,bs="re") +
s(Puffers,bs="re") + s(TrapSite,bs="re") + s(Crop,bs="re") +
s(Block,bs="re"), data = LHR)

# let's look at just 'Week' and at 'Week' plus some other hierarchical
  factors:

m.post.week = gam(eggs[post] ~ s(Week),data=LHR[post,])

m.post.week.re = gam(eggs[post] ~ s(Week)+s(Loc,bs="re") +
s(Block,bs="re"),data=LHR[post,])

summary(m.post.week)

# Family: gaussian 
# Link function: identity 

# Formula:
# eggs[post] ~ s(Week)



summary(m.post.week.re)

# Family: gaussian 
# Link function: identity 

# Formula:
# eggs[post] ~ s(Week) + s(Loc, bs = "re") + s(Block, bs = "re")


# Let's look at comparisons:
m.males.week = gam(Males~s(Week),data=LHR[post,])
m.females.week = gam(Females~s(Week),data=LHR[post,])

par(mfrow=c(1,3))
plot(m.post.week,main="Eggs")
plot(m.males.week,main="Males")
plot(m.females.week,main="Females")


########### 10.3.14 #############

# Now I have all the data ***REDO: 10.14.14***

lh09 = read.csv("NOW MD AWI Lost Hills Traps Raw 2009.csv")
lh10 = read.csv("NOW MD AWI Lost Hills Traps Raw 2010.csv")
lh11 = read.csv("NOW MD AWI Lost Hills Traps Raw 2011.csv")
lh12 = read.csv("NOW MD AWI Lost Hills Traps Raw 2012.csv")
lh13 = read.csv("NOW MD AWI Lost Hills Traps Raw 2013.csv")


# lost hills trap counts (they don't all have the same columns) #

### **** wrong = as.numeric(lh09$Males)
### **** right = as.numeric(levels(lh09$Males))[lh09$Males]

males12 = as.numeric(levels(lh12$Males))[lh12$Males]
eggs12 = as.numeric(levels(lh12$Eggs))[lh12$Eggs]

males13 = as.numeric(levels(lh13$Males))[lh13$Males]
eggs13 = as.numeric(levels(lh13$Eggs))[lh13$Eggs]

eggs09 = as.numeric(levels(lh09$Eggs))[lh09$Eggs]

lh09$Eggs = eggs09
lh13$Eggs = eggs13 
lh12$Eggs = eggs12

lh12$Males = males12
lh13$Males = males13

lh10$Rep = rep(NA,dim(lh10)[1])
lh11$Rep = rep(NA,dim(lh11)[1])
lh12$Rep = rep(NA,dim(lh12)[1])
lh13$Rep = rep(NA,dim(lh13)[1])

lh09$Card = rep(NA,dim(lh09)[1])
lh09$MD = rep(NA,dim(lh09)[1])

lh09$X = rep(NA,dim(lh09)[1])
lh10$X = rep(NA,dim(lh10)[1])
lh11$X = rep(NA,dim(lh11)[1])
lh12$X = rep(NA,dim(lh12)[1]) 

lh09$Delta_Totmoths = rep(NA,dim(lh09)[1])
lh10$Delta_Totmoths = rep(NA,dim(lh10)[1])

l = rbind(lh09[,c(1:12,19,13:14,20,16:18,22,21,15)],lh10[,c(1:19,22,21,20)],lh11[,c(1:20,22,21)],lh12[,c(1:20,22,21)],lh13)

#and lets call the harvest damage df h:

hl = read.csv("Lost Hills")

# and :

p = read.csv("../Pesticide Applications")
p = p[,-c(15,16)]     #no need for the urls make it ugly


# now for SF:

s09 = read.csv("../Santa_Fe/Santa Fe  2009.csv")
s10 = read.csv("../Santa_Fe/Santa Fe  2010.csv")
s11 = read.csv("../Santa_Fe/Santa Fe  2011.csv")
s12 = read.csv("../Santa_Fe/Santa Fe  2012.csv")
s13 = read.csv("../Santa_Fe/Santa Fe  2013.csv")


males11 = as.numeric(levels(s11$Males))[s11$Males]
eggs09 = as.numeric(levels(s09$Eggs))[s09$Eggs]

s11$Males = males11
s09$Eggs = eggs09

s09$Moths = rep(NA,dim(s09)[1]) 
s09$PreMD = rep(NA,dim(s09)[1]) 

s = rbind(s09[,c(1:14,16,15,17)],s10,s11,s12)

hs = read.csv("../Santa_Fe/Santa Fe  2009-2012.csv")

# now all the data is in (though it's not necessarily formatted
# properly), so I'm going to start the function that will take
# a spray date, use that date to find trap data within some time
# difference of that date, perform a linear regression, and return the
# slope of that regression. 

# the following was to test whether or not any intersection of dates
# occurred. Thanks to it I learned && was supposed to be & and that
# was my problem all along.

# dummy = sapply(p_NOW$PUR.Applied.Date, function(x){

#       		date = tojd(x)
# 		#j = which((l$julian.dates-date <= 7 & l$julian.dates - date >= 0 ))
		
# 		j = which((date-l$julian.dates <= 7 & date-l$julian.dates >= 0 ))
# 		return(length(j))

# }
# )




# On second thought: I could spend hours looking for rows from p_NOW
# that will align with l/s, so first I should build a list of rows
# guaranteed to give me something to look at. Since this is going to
# be pretty temporary, I'll do away with the shell,set,test stuff
# (which could be a pretty silly convention to adopt, anyways):

rows = apply(p_NOW,1,function(x){

       ranch = x[1]
       date = x[4]
       date.jd = tojd(date)
       blocks = unlist(strsplit(as.character(x[6]),","))

       #init empty df:
       loc = data.frame()

       test.0 = ranch %in% l$Ranch

       # Previously I used ifelse(test, yes, no), but BEWARE! this only returns
       # Things in the shape of test.

       if(test.0){loc = l}else{loc = s}	       

       # this time we're doing for loops (since there won't be many plots): 
       # we want to see if there is any data whatsoever. If there is -
       # for any block - return true. If each block leaves an empty
       # df, return false after the loop resolves:

       for(block in blocks){ 
       		 
		 i = which(loc$Block == block & abs(loc$julian.dates -
		 date.jd) <= 14) 
		 
		 if(length(i) > 0){return(TRUE)}	 
	
	}
	

	return(FALSE)	
		
})

# p_NOW_test = p_NOW[which(rows == TRUE),]
# Ok, now I've got all of the plot-viable rows of p_NOW collated into
# p_NOW_test! It has 196 rows. Let's see if my functions work:

shell.0(p_NOW_test[1,])  #NOPE

# I'm getting a lot of repeates. Here's why (*see Quantity*):

p_NOW_test[1:7,]


#Ok, I need to look at TrapSite
# OK OK OK: first things first. That quantity column is messing me
# up. So let's consolidate! 

p_NOW_test[which(p_NOW_test$Block.s. == "14-1" &p_NOW_test$PUR.Applied.Date == "4/15/2009"),]



# What is this PUR.Number?

# I've created 'nn' - this has only the unique rows of p_NOW_test (I
# discarded PUR.Number and used unique().

# to create nn: p->p_NOW->p_NOW_test->kill PUR.Number & unique()


# Some plots makes sense, like this one:
shell.1(nn[15,])  
shell.1(nn[22,])
shell.1(nn[23,])
shell.1(nn[24,])
shell.1(nn[29,])
shell.1(nn[37,])
shell.1(nn[57,])
shell.1(nn[58,])

# and some don't:

shell.1(nn[18,])
shell.1(nn[32,])
shell.1(nn[36,])
shell.1(nn[37,])
shell.1(nn[43,])

# especially when considering (note, spray was on 15469!):

j = which(abs(l$julian.dates - 15440)<= 30 & l$Block == "24-3")
test = l[j,]
ggplot(test,aes(julian.dates,Males,color=TrapType,shape=TrapSite))+geom_point(size=5)


# Check this out!:

 j = which(abs(tojd(p$PUR.Applied.Date)-15440)<=11 & p$Ranch == 344)
 length(j)

 p[j,]

# PUFFERS! applied within a month of a spray. No wonder that spray had
# no effect!

# Another puffer within a month of a spray:

j = which(abs(p$julian.dates - 14710) <= 10 & p$Ranch == 345)
 length(j)

 p[j,]

# a look at population regrowth:

shell.1(nn[68,])
      Ranch
10694   371
PUR.Applied.Date 
           15540 
[1] "25-3" "25-2"


j = which(abs(s$julian.dates - 15540) <=80 & s$Block =="25-2")
test = s[j,]

d.f = data.frame()
for( i in 1:70){
     row = nn[i,]
     blocks = unlist(strsplit(as.character(row[6][1,1]),","))
     for( j in 1:length(blocks)){
     	  block = blocks[j]
     	  new_row = row
     	  new_row[6] = block
     	  d.f = rbind(d.f,new_row)
	  
     	  }
      }

# It will be easier to coerce d.f into a data frame...

#Here's how we sort by day in the month:

dm = sapply(d.f$PUR.Applied.Date,function(x){

	mdy = unlist(strsplit(x,"/"))
		
	dm = paste(mdy[2],mdy[1],sep="/")

	return(dm)
	
	}
	
)

d.f[order(as.Date(d.f$Day.Month, format="%d/%m")),] 

# and then to set these as factors and fix their order:

x = factor(x,levels(x)[c(4,5,1:3)]) #change x to whatever

# without puffers: 

no_puff = which(d.f$Block.s. %in% c("2-2","2-1","1-2","2-3","2-4","1-3","1-4"))

# Let's look at how the fields without puffers faired, because the
#values I'm getting in d.f don't look RIGHT!!


j = which((abs(l$julian.dates - 15467) <= 20 | abs(l$julian.dates -
15526) <= 20) & l$Block %in% c("2-2","2-1","1-2","2-3","2-4","1-3","1-4"))
ggplot(l[j,],aes(julian.dates,Males,color=TrapSite,shape=TrapType)) +
geom_point(size=5)

# now I'm thoroughly confused because of the lack of years 09-11...

#Let's go back to p and give each block it's own row:

dd.f = data.frame()
for( i in 1:12246){
     row = p[i,]
     blocks = unlist(strsplit(as.character(row[6][1,1]),","))
     for( j in 1:length(blocks)){
     	  block = blocks[j]
     	  new_row = row
     	  new_row[6] = block
     	  dd.f = rbind(dd.f,new_row)
	  
     	  }
      }
dd.f$PUR.Number = rep(NA,15278)
dd.f= unique(dd.f)

j = which(dd.f$Block.s. %in% c("2-2","2-1","1-2","2-3","2-4","1-3","1-4"))

# The MAJORITY of sprays targeting these blocks has NA as it's targe
# pest... Ok these are all pesticide aides and nitrogen supplements...

# Wrapping up, here are the things I want to keep:

p,dd.f,d.f,s,l,hl,hs


############## 10.13.14 ##############

# Oh Gosh, I have no puffer data. This is because puffers are
# applied to Sites, not Blocks. So now I've got to figure out which
# blocks line up with which sites and which sites line up with which
# ranches. I could do this by hand, but...

sites = unique(dd.f$Site)
dict = vector(mode="list",length=length(sites))
names(dict) = sites
for(site in sites){

	 blocks = unique(dd.f$Block.s.[which(dd.f$Site == site)])
	 dict[[which(names(dict)==site)]] = blocks
}		   


# Ok this was only important for the visualization bit, some I'm not
# going to stress. Whatever the case, I now have a dictionary of sites
# and the blocks they contain: dict.

# For reference: p-->dd.f and nn-->d.f (former is NOW centric, the latter
# is spray data for all pests).


which(d.f$aft.bef >= 1)
 [1]   1   2   4   5   6   7   9  10  13  15  21  23  24  26  27  28  29  42  55
[20]  56  57  58  59  60  61  63  64  65  66  67  68  71  72  73  74  75  76  78
[39]  79  80  82  83  84  86  87  89  90  91  92  93  94  95  98  99 100 101 102
[58] 103 104 105 106 107 108 109 110 111 114 115 140 141


shell.1(d.f[140,])
shell.1(d.f[141,])

# OK, I need to figure out first how to deal with the case where the
# spray occurs between two trap sampling dates:

set.3 = apply(d.f,1,function(y){shell.3(y,"p")})


set.4 = apply(d.f,1,function(y){shell.3(y)}) # this has the exclude methbod


# Sooooooooooooooo lost hills 2009 is broken....


######### 10.14.14 ############

# ALRIGHT! I think I've fixed the issue! The problem was when I
# imported my original data frames (lost hills and santa fe) I just
# blithely coerced factors into numeric without really knowing what I
# was doing. If you take a factor variable X, however and do
# as.numeric(X) you're not going to get the literal values of X, but
# their level values as factors. so the factor "1.2" might sit among
# other factors "1" "1.1" "1.2" "10" "13" so it's level will be 3. And
# that is what as.numeric will give you. to get 1.2 out of "1.2" you
# need to use this command: 

### **** wrong = as.numeric(X)
### **** right = as.numeric(levels(X))[X]

# so now I have new s and l data frames, and I'll have to run the data
# through again!

set.5 = apply(d.f,1,function(y){shell.3(y)})
ggplot(d.f,aes(Day.Month,log(set.5),color=Product..Agrian.,shape=as.factor(Ranch))) + geom_point(size=5)


######## 10.15.14 ############

# Now I'll try with the t-test method:

set.6 = apply(d.f,1,function(y){shell.3(y)})
ggplot(d.f,aes(Day.Month,set.6,color=Product..Agrian.,shape=as.factor(Ranch))) + geom_point(size=5)

# Puffers often don't aren't given blocks in d.f/dd.f. This means they
# aren't showing up when I'm calculating percent changes. I created
# dict so that I could assign them blocks based on site.

# Also, something weird is going on, so I'm sort of starting over:
# dd.f, so far as I can tell, is clean.

j = which(dd.f$Target.Pest == "Navel Orangeworm")
> length(j)
[1] 400
> d = dd.f[j,]


# wait a minute: dict changes with each year, so we're going to need
# different dictionaries for each year. I think I have everything I need in dd.f...
# ok, let's take a minute to collect ourselves:

# what do I need: dd.f, (correct) l & s, hl & hs.

# First I'll add Day.Month to dd.f (now d)

dm = sapply(d$PUR.Applied.Date,function(x){

	mdy = unlist(strsplit(x,"/"))
		
	dm = paste(mdy[2],mdy[1],sep="/")

	return(dm)
	
	}
	
)

d$Day.Month = dm

# Now all I need is d, l, s, hl, and hs.

# saved them all in almond101514.rda


# now for our dictionaries:
 

dict_years = vector(mode="list",length=length(unique(d$Year)))
names(dict_years) = unique(d$Year)

for(year in unique(d$Year)){
	 
	 dict = vector(mode="list",length=length(unique(d$Site[which(d$Year == year)])))
	 sites = unique(d$Site[which(d$Year == year)])
	 names(dict) = sites
	 for(site in sites){
	 	 blocks = unique(d$Block.s.[which(d$Site == site & d$Year == year)])
	 	 dict[[which(names(dict)==site)]] = blocks
	 }

	 dict_years[[which(names(dict_years)==year)]] = dict
}		   


##### 10.16.14 #####


d[order(as.Date(d$Day.Month, format="%d/%m")),] 
dm = factor(d$Day.Month,levels=unique(d$Day.Month),ordered=TRUE)
d$Day.Month = dm

d1 = d[which(d$Target.Pest == "Navel Orangeworm"),]

j = which(d1$Block.s. != " ")
set.7 = apply(d1[j,],1,function(y){shell.3(y,"p")})
ggplot(d1[j,],aes(Day.Month,log(set.7),color=Product..Agrian.,shape=Year)) + geom_point(size=3)

# ok now this looks right

temp = d1[j,]
temp = temp[which(temp$Day.Month > "9/7"),]

shell.4(temp[8,])
s$Males[which(abs(s$JD-15898)<=21 & s$Block == "30-2")]


shell.4(temp[19,])   # sharp measurement (excluded)


shell.4(temp[28,])   # shart measurement (included)



###### 10.17.14 ######

# I've created a new file with all the functions, functions.R, in this same directory. 


shell.4(temp[28,])


sites = unique(d$Site)
dict = vector(mode="list",length=length(sites))
names(dict) = sites
for(site in sites){
    
	 blocks = unique(d$Block.s.[which(d$Site == site)])
	 dict[[which(names(dict)==site)]] = blocks
         
}

#OK the site->block dictionary is complete and saved as sitedict.rda
# Also, I have a new method in shell.3 to return the number of insects before and
# after a spray. This we can use to build a binomial model - essentially a survival
# analysis - which will be pretty cool. 


########## 10.19.14 #############


surv = apply(d1,1,function(x){shell.3(x,"s")})
surv = rbind(surv)
dim(surv)

# Jay doesn't want the binomial model afterall.


#Checking prps with threshold.
prop = apply(d1,1,function(x){shell.3(x,"p",0)})
ggplot(d1,aes(Day.Month,log(prop),shape=Year)) + geom_point(size=3)

sub =  apply(d1,1,function(x){shell.3(x,"ss",0)})
ggplot(d1,aes(Day.Month,sub,shape=Year)) + geom_point(size=3)

j = which(d1$Block.s. != " ")


######### 10.21.14 ###########

## with the threshold set to 5, the relationship between spray date and efficacy
## looks linear (though, sparse):

prop = apply(d1,1,function(x){shell.3(x,"p",5)})
m1 = lmer(log(prop)~as.numeric(Day.Month) + (1|Year),data=d1, familiy=gaussian)
m2 = lm(log(prop)~as.numeric(Day.Month),data=d1)
plot(log(prop)~as.numeric(Day.Month),data=d1)
abline(-5.654,.03)


#########################
## ggplot exploration  ##
#########################

z = rep(c(TRUE,FALSE),length(l$Day.Month))
i = which(z == TRUE)
i = seq(1,length(l$Day.Month),by=2)
d.m.axis = l$Day.Month[i]

q = ggplot(d1,aes(Day.Month,sub,shape=Year)) + geom_point(size=3)
q.lab = q + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works

q = ggplot(d1,aes(Day.Month,prop,shape=Year)) + geom_point(size=3)
q.lab = q + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works


#########################
#########################


## Let's ice this momentarily until I can confer with Jay. I'll proceed instead
## with looking at the duration of a spray's effect:


## Initial thoughts on things to consider:
## - the big one is "significant amount"
## - what should be unit of time? It has to be sample periods - weeks - right?
## - again, percent recovery depends on "significant amount"

## let's be adults and merge s and l...


## first things first: make columns coincide.

s$MD = rep(NA,dim(s)[1])
s$Delta_Totmoths = rep(NA,dim(s)[1])
s$Puffers = rep(NA,dim(s)[1])
s$Rep = rep(NA,dim(s)[1])
s$X = rep(NA,dim(s)[1])
s$Type = rep(NA,dim(s)[1])

l$Moths = rep(NA,dim(l)[1])

c = rbind(s[,c(1:12,17,13,14,19,24,21,16,20,23,22,18,15)],l)

## if any class conflicts come up I'll fix them then...

year = sapply(c$Date,function(x){ unlist(strsplit(as.character(x),"/"))[3] })
 
##### 10.22.14 ######

## continuing today with shell.5

## I'm concerned that some of the traps literally never catch any
## insects and thus needlessly bring our means down:

sapply(unique(c$TrapType),function(x){mean(c$Males[which(c$TrapType == x & !is.na(c$Males))])})


## I need to start using switch statements when possible. Ifelse is slow.
## check.

## this is the one problematic site:

shell.4(d1[272,])

intervals = apply(d1,1,function(x){shell.5(x,10,.4)})

## the regex I wanted was \s=\s, btw

## d1[272,]
##      Ranch REC.Number PUR.Number PUR.Applied.Date  Site Block.s. Treated.Area
## 3772   345     949798         NA         7/5/2012 3450B      1-2        278.2
##      Product..Agrian.      Target.Pest Spray.Volume..gal.ac. Rate Rate.Units
## 3772      Intrepid 2F Navel Orangeworm                   200   18   floz / A
##      Quantity Quantity.Unit    JD Year Day.Month
## 3772   5007.6          floz 15526 2012       5/7

shell.5(d1[272,],1,.4)

##-------10.23.14-------##

## to discuss with Jay 10.22

## Delta/Ovip traps are dreadful at catching insects - this brings our means down. Cant I just ignore them?

## The way I’m constructing the mean when the original mean is 0.

## If we set the threshold to 5, things become much more sparse. Does this just call for a simple linear analysis? How can I proceed?

## Take a closer look at the traptype discrepancies (over year and season) - drop d/o and egg

## put threshold on the mean

## Jay wanted me to check the male catches. He thought the data I have
## looked suspicious, but now I have checked and they are correct. The Delta/Ovip
## traps need to be removed from the analysis - they are abominable and distort our
## means. From there I need to examine average male densities. This will allow me
## to make an educated guess for the sig_amnt parameter in the shell.5 function. 


## the mean of males not including Delta/Ovip:
mean(c$Males[which(!is.na(c$Males) & c$TrapType %in% c("Phero","Flight","LurePhero"))])

## the mean of males including delta/ovip:
mean(c$Males[which(!is.na(c$Males))])


## not so different...

## Delta/Ovip        Egg      Phero     Flight  LurePhero 
##      16633      17346      14928       1980       1980

## that's why...

mean(c$Males[which(!is.na(c$Males) & c$TrapType %in% c("Flight","LurePhero"))])

mean(c$Males[which(!is.na(c$Males) & c$TrapType %in% c("Flight","LurePhero"))])


## That is some significant shit.

## ok, then I want to know what one, two, three, and four week per-block
## averages are:
bin = 7
## including phero:
one_week = blockav(7)
mean(one_week)

    
two_week = blockav(14)
three_week = blockav(21)

## not including phero:

test = blockav(7)
> mean(test[which(!is.na(test))])


## The tentative conclusion: a mean of 9 without phero, a mean of 4 with phero.
## K.

## This means we can look at a couple of things:

## (1) re-run shell.3 (now excluding delta/ovip) with a threshold of 4 (or 9, if we
## want to exclude phero as well).
##
## (2) re-run shell.5 (also excluding delta/ovip) with a sig amount of 4 (or 9) and
## change the sig_prcnt to just be sig_amnt.

## also:
d2 = d1[which(d1$Product..Agrian. != "Puffer NOW"),]
prop = apply(d2,1,function(x){shell.3(x)})

intervals = apply(d2,1,function(x){shell.5(x)})

q = ggplot(d2,aes(Day.Month,log(prop), color=Year)) + geom_point(size=3)
q.lab = q + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works

w = ggplot(d2,aes(Day.Month,intervals, color=Year)) + geom_point(size=3)
w.lab = w + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works

prop = apply(d2,1,function(x){shell.3(x,thresh = 2)})

w = ggplot(prop,aes(Day.Month, proportion, color=Year,shape=as.factor(weight))) + geom_point(size=3)
w.lab = w + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works


##--------- 10.24.14 ----------#

## It really looks like the mean as a threshold is too strong; it is pretty rare
## (only 16!) that a 2-week mean breeches the 3 insect threshold. So, what to do?
##

## Let's look at the average number of insects each block sees every 2 weeks:
 "25-2" "25-2" "25-3" "36-2" "36-3" "36-3"
days <- unique(c$JD)[which(!is.na(unique(c$JD)))]
days <- sort(days)
bin <- 14
av_sums = sapply(unique(c$Block),function(x){ 
    sums <- sapply(days,
        function(y){

            i <- which(
                (c$JD - y <= bin & c$JD - y >= 0) &
                c$TrapType %in% c("Phero","Flight","LurePhero") &
                !is.na(c$Males) &
                c$Block == x
                )

            if(length(i) == 0){ return(NA) }
            else{ return(sum(c$Males[i])) }
        }
        )

    av_sum <- mean(sums[which(!is.na(sums))])
    return(av_sum)
})

ss = sapply(unique(c$Block),function(x){ 
    sums <- sapply(days,
        function(y){

            i <- which(
                (c$JD - y <= bin & c$JD - y >= 0) &
                c$TrapType %in% c("Phero","Flight","LurePhero") &
                !is.na(c$Males) &
                c$Block == x
                )

            if(length(i) == 0){ return(NA) }
            else{ return(sum(c$Males[i])) }
        }
        )
    return(sums)
})

## traps: phero, flight, lurephero
summary(av_sums)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  0.6477   3.9890  11.3200  22.5400  26.6000 130.0000

## ok this is good, but I have to be careful because one block might have more
## traps than another, etc. (so now look at only phero)

## traps: phero
summary(av_sums)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.4496  2.7950  6.7250 14.4800 20.1200 73.2900


## George suggested to look at the correlation between the different trap types -
## - great idea!!

o <-  which(c$TrapType == "Delta/Ovip")
ph <-  which(c$TrapType == "Phero")
fl <-  which(c$TrapType == "Flight")
lp <-  which(c$TrapType == "LurePhero")

o = data.frame(c$Males[o],c$Block[o],c$JD[o],stringsAsFactors = F  )  
ph = data.frame(c$Males[ph],c$Block[ph],c$JD[ph],stringsAsFactors = F)           
fl = data.frame(c$Males[fl],c$Block[fl],c$JD[fl],stringsAsFactors = F)
lp = data.frame(c$Males[lp],c$Block[lp],c$JD[lp],stringsAsFactors = F) 

colnames(o) = c("Males","Block","JD")
colnames(ph) = c("Males","Block","JD")
colnames(fl) = c("Males","Block","JD")
colnames(lp) = c("Males","Block","JD")

findcor = function(df1,df2){

    i = which(df1$Block == df2$Block & df1$JD == df2$JD)
    print(length(i))
    if(length(i) == 0){return(NA)}
    c_i = complete.cases(df1$Males[i],df2$Males[i])
    cor = cor(df1$Males[c_i],df2$Males[c_i])
    return(cor)
}
    
## Not much success - there's no overlap...

## Let's see if having better weights than 1 & 0 will help...
bins = seq(min(),max(),length.out=5)
i = which(bins == max(bins[which(bins < n_males)]))
## later...


dm = sapply(c$Date,function(x){

    if(x==""){return(NA)}else{
        mdy = unlist(strsplit(as.character(x),"/"))
        
        dm = paste(mdy[2],mdy[1],sep="/")
    
        return(dm)
    }
    
})

c$Day.Month = dm

c = c[order(as.Date(c$Day.Month, format="%d/%m")),] 
dm = factor(c$Day.Month,levels=unique(c$Day.Month),ordered=TRUE)
c$Day.Month = dm

## Potential realization: there are loads of trap counts, and loads that are above
## the "significant" threshold... BUT there aren't many significant counts
## near spray dates



# and then to set these as factors and fix their order:


## ----------- 10.27.14 ------------- ##


## today looking at func vis_blockpop, trying to highlight dates of sprays/puffers
## on the x axis

## forms of anamalous cases I need to deal with: "01-1","3-Jan"  **ASK**

[1] " "     "02-1"x  "02-2"x  "02-3"x  "02-4"x  "01-1"x  "01-2"x  "14-1"  "14-2" 
[10] "13-1"  "13-2"  "13-3"  "13-4"  "24-1"  "24-2"  "24-3"  "24-4"  "11-1" 
[19] "11-2"  "11-4"  "12-1"  "12-2"  "12-3"  "12-4"  "3-Jan"x "4-Nov"x "2-Jan"x
[28] "25-1"  "25-3"  "30-1"  "30-2"  "30-4"  "30-3"  "25-4"  "14-4"  "2-1"  
[37] "2-2"   "2-3"   "2-4"   "1-2"   "1-3"   "2-Feb"x "36-2"  "36-3"  "25-2" 
[46] "31-1"  "31-2"  "36-1"  "36-4" 

d1$Block.s.[which(d1$Block.s. == "3-Jan")] = "1-3"
d1$Block.s.[which(d1$Block.s. == "2-Jan")] = "1-2"
d1$Block.s.[which(d1$Block.s. == "02-1")] = "2-1"
d1$Block.s.[which(d1$Block.s. == "02-2")] = "2-2"
d1$Block.s.[which(d1$Block.s. == "02-3")] = "2-3"
d1$Block.s.[which(d1$Block.s. == "02-4")] = "2-4"
d1$Block.s.[which(d1$Block.s. == "01-1")] = "1-1"
d1$Block.s.[which(d1$Block.s. == "01-2")] = "1-2"
d1$Block.s.[which(d1$Block.s. == "2-Feb")] = "2-2"
d1$Block.s.[which(d1$Block.s. == "4-Nov")] = "11-4"


## Do all of my important blocks have sites?
test = unlist(dict)
sapply(unique(c$Block),function(x){
    return( x %in% test)})

## YES


dim(c)
[1] 52867    26
> colnames(c)
 [1] "Crop"           "Ranch"          "Trtmnt"         "Block"         
 [5] "Date"           "Week"           "Flight"         "TrapType"      
 [9] "TrapID"         "Eggs"           "Females"        "Males"         
[13] "PreMD"          "Loc"            "Card"           "MD"            
[17] "Type"           "Puffers"        "TrapSite"       "Delta_Totmoths"
[21] "X"              "Rep"            "JD"             "Moths"         
[25] "Year"           "Day.Month"   




## ok my stupid vis_blockpop function has to take a ranch too, because there's
## absolutely unecssary overlap in the blocks between sites.

br = apply(c,1,function(x){
    return(c(x[4],x[2]))
})
br = t(br)
br = unique(br)

##--------- 10.28.14 ----------##


## ok the viewing begins:

## ----- MALES ----- ##
 vis_blockpop( 1 ) 
 vis_blockpop( 2 ) 
 vis_blockpop( 3 ) 
 vis_blockpop( 4 )  
 vis_blockpop( 5 )  
 vis_blockpop( 6 ) 
 vis_blockpop( 7 ) 
 vis_blockpop( 8 ) ## 31-1; error of shared plot (single ranch)
vis_blockpop(9) ## 30-3;error of shared plot (single ranch)
vis_blockpop(11)
vis_blockpop(12)
vis_blockpop(13)
vis_blockpop(14) ## 25-1;error of shared plot (multi ranch)
vis_blockpop(15)
vis_blockpop(16)
vis_blockpop(17)
vis_blockpop(18) ## 29-4; error of split plot
vis_blockpop(19)## 31-1; error of shared plot (single ranch)
vis_blockpop(20)## 30-3;error of shared plot (single ranch)
vis_blockpop(21)
vis_blockpop(22)
vis_blockpop(23)
vis_blockpop(24)
vis_blockpop( 25 ) ## good look
 vis_blockpop( 26 ) ## good
 vis_blockpop( 27 ) ## interesting
 vis_blockpop( 28 ) ## interesting
 vis_blockpop( 29 ) ## good
 vis_blockpop( 30 ) 
 vis_blockpop( 31 ) 
 vis_blockpop( 32 ) ## No Puffer Plot
 vis_blockpop( 33 ) ## No Puffer Plot
 vis_blockpop( 34 ) ## No Puffer Plot
 vis_blockpop( 35 ) ## 12-1; error of shared plot (single ranch)
 vis_blockpop( 36 ) 
 vis_blockpop( 37 ) ## 11-3; error of ghost plot
 vis_blockpop( 38 ) 
 vis_blockpop( 39 ) ## No Puffer Plot
 vis_blockpop( 40 ) ## No Puffer Plot
 vis_blockpop( 41 ) ## No Puffer Plot
 vis_blockpop( 42 ) 
 vis_blockpop( 43 ) ## No Puffer Plot
 vis_blockpop( 44 ) 
 vis_blockpop( 45 ) 
 vis_blockpop( 46 ) ## 12-1; error of shared plot (single ranch)
 vis_blockpop( 47 ) 
 vis_blockpop( 48 ) 
 vis_blockpop( 49 ) 
 vis_blockpop( 50 ) 
 vis_blockpop( 51 ) ## 25-4; error of shared plot (multi ranch) 
 vis_blockpop( 52 ) 
 vis_blockpop( 53 ) ## Error of ranch/block out of scope 
 vis_blockpop( 54 )  ## Error of ranch/block out of scope 
 vis_blockpop( 55 )  ## Error of ranch/block out of scope 
 vis_blockpop( 56 )   ## Error of ranch/block out of scope 
 vis_blockpop( 57 ) ## Error of ranch/block out of scope 
 vis_blockpop( 58 )  ## Error of ranch/block out of scope 
 vis_blockpop( 59 )  ## Error of ranch/block out of scope 
 vis_blockpop( 60 )  ## Error of ranch/block out of scope 
 vis_blockpop( 61 )   ## Error of ranch/block out of scope 
 vis_blockpop( 62 )   ## Error of ranch/block out of scope 
 vis_blockpop( 63 )   ## Error of ranch/block out of scope 
 vis_blockpop( 64 )   ## Error of ranch/block out of scope 
 vis_blockpop( 65 )   ## Error of ranch/block out of scope 
 vis_blockpop( 66 )   ## Error of ranch/block out of scope 
 vis_blockpop( 67 )   ## Error of ranch/block out of scope 


## These visualizations yield a couple of points:
## - The vast majority of sprays take place when no insects were there
##   to begin with.
## - Puffers are responsible for the large drop in trap counts at the
##   beginning of the season, and could easily be leading to the
##   correlation of early season sprays with high efficacy.
## - In the presence of Puffers, we have to question how effective pheromone
##   traps are in the first place.

## So now we need to look at egg counts and female counts:

## ---- EGGS ---- ##

test = sapply(1:52,function(x){print(paste("vis_blockpop(",as.character(x),",e)",""))})

 vis_blockpop( 1 ,"e") 
 vis_blockpop( 2 ,"e") 
 vis_blockpop( 3 ,"e") ## still lots of eggs after puffer
 vis_blockpop( 4 ,"e") ## some puffer efficacy
 vis_blockpop( 5 ,"e") 
 vis_blockpop( 6 ,"e") 
 vis_blockpop( 7 ,"e") 
 vis_blockpop( 8 ,"e") ## error
 vis_blockpop( 9 ,"e") ## error
 vis_blockpop( 10 ,"e")  ## some puffer efficacy
 vis_blockpop( 11 ,"e") 
 vis_blockpop( 12 ,"e") ## no eggs?
 vis_blockpop( 13 ,"e")  ## no eggs
 vis_blockpop( 14 ,"e") ## error
 vis_blockpop( 15 ,"e") 
 vis_blockpop( 16 ,"e") ## still lots of eggs after puffer
 vis_blockpop( 17 ,"e") 
 vis_blockpop( 18 ,"e") ## error
 vis_blockpop( 19 ,"e") ## error
 vis_blockpop( 20 ,"e") ## error
 vis_blockpop( 21 ,"e") ## interesting
 vis_blockpop( 22 ,"e") 
 vis_blockpop( 23 ,"e") 
 vis_blockpop( 24 ,"e") 
 vis_blockpop( 25 ,"e") ## interesting
 vis_blockpop( 26 ,"e") ## interesting
 vis_blockpop( 27 ,"e") ## interesting
 vis_blockpop( 28 ,"e") ## interesting
 vis_blockpop( 29 ,"e") ## interesting - problematic?
 vis_blockpop( 30 ,"e") 
 vis_blockpop( 31 ,"e") 
 vis_blockpop( 32 ,"e") ## no puff
 vis_blockpop( 33 ,"e") ## no puff
 vis_blockpop( 34 ,"e") ## no puff
 vis_blockpop( 35 ,"e") ## error
 vis_blockpop( 36 ,"e") 
 vis_blockpop( 37 ,"e") ## error
 vis_blockpop( 38 ,"e") ## interesting
 vis_blockpop( 39 ,"e") ## no puff, interesting
 vis_blockpop( 40 ,"e") ## no puff, interesting
 vis_blockpop( 41 ,"e") ## no puff, interesting
 vis_blockpop( 42 ,"e") 
 vis_blockpop( 43 ,"e") 
 vis_blockpop( 44 ,"e") ## interesting
 vis_blockpop( 45 ,"e") ## interesting
 vis_blockpop( 46 ,"e") ## error
 vis_blockpop( 47 ,"e") 
 vis_blockpop( 48 ,"e") ## interesting
 vis_blockpop( 49 ,"e") ## interesting, efficacious?
 vis_blockpop( 50 ,"e") ## interesting, efficacious?
 vis_blockpop( 51 ,"e") ## error
 vis_blockpop( 52 ,"e") 

## -------------- 10.29.14 --------------- ##


## Now the images are in individual pdfs:
plots = sapply(1:52,function(x){vis_blockpop(x,"m")})
plots = sapply(1:52,function(x){vis_blockpop(x,"f")})
plots = sapply(1:52,function(x){vis_blockpop(x,"e")})

## Ok, that was an ordeeeeeal. Now we've seen that MD probably distorts
## phero traps, though, so it was useful. Given this new piece of info
## we've got to check out pre and post puffer densities for each of the m,f,e
## counts and see if anything correlates. If m doesn't covary with f and e, then
## we really can't expect males counts to hold a reliable signal... bummer.
## let's go back to shell.3:

## I'm going to need dict to be correct...


dict[[1]] = c("14-4", "14-1", "14-2" )

dict[[2]] = c("13-3", "13-1", "13-2", "13-4" )

dict[[3]] = c( "24-1", "24-2", "24-3", "24-4" )

dict[[4]] = c( "11-1", "11-2", "11-4" )

dict[[5]] = c( "12-4", "12-3", "12-1", "12-2" )

dict[[6]] = c( "2-1", "2-2", "2-3", "2-4" )

dict[[7]] = c( "1-2", "1-3", "01-1", "01-2" )

dict[[8]] = c( "25-1", "25-3" ,"25-4" )

dict[[9]] = c( "30-1", "30-2", "30-4", "30-3" )

dict[[10]] = c( "29-3", "29-4", "29-4" )

dict[[11]] = c( "31-1" )

dict[[12]] = c( "31-1", "31-2", "31-3", "31-4" )

dict[[13]] = c( "25-1", "25-2", "25-3", "25-4" )

dict[[14]] = c( "36-3", "36-1", "36-2", "36-4" )

dict[[15]] = c( "30-3", "30-4", "30-1", "30-2" )

dict[[16]] = c( "25-1" )

dict[[17]] = c( "30-3" )

dict[[18]] = c( "1-4" )

dict[[19]] = c( "12-1" )


## OK far as I can tell, dict is now correct - only, be mindful that some of
## blocks are shared, split, or ghosted.

site.date.pairs <- unique(apply(dp,1,function(x){return(c(x[5],x[15]))}))


## ------- 10.30.14 ---------##

## I've cleaned up the plots so that the spray/puffer lines are right. Now
## I need to take care of those errors.

##Broken:
 vis_blockpop( 8 ) ## 31-1; error of shared plot (single ranch)
vis_blockpop(9) ## 30-3;error of shared plot (single ranch)
vis_blockpop(14) ## 25-1;error of shared plot (multi ranch)
vis_blockpop(18) ## 29-4; error of split plot
vis_blockpop(19)## 31-1; error of shared plot (single ranch)
vis_blockpop(20)## 30-3;error of shared plot (single ranch)
 vis_blockpop( 35 ) ## 12-1; error of shared plot (single ranch)
 vis_blockpop( 37 ) ## 11-3; error of ghost plot
 vis_blockpop( 46 ) ## 12-1; error of shared plot (single ranch)

##Fixed:
 vis_blockpop( 51 ) ## 25-4; error of shared plot (multi ranch) 


## Before I go through that, I'll make a trap site dictionary:

for (val in 1:length(dict)) {

    site = names(dict)[val]
    ranch = unique(d1$Ranch[which(d1$Site == site)])[1] 
    blocks = dict[[val]]
    block.dict = vector(mode="list",length=length(blocks))
    names(block.dict) = blocks
    for (block in blocks){

        trap.sites = unique(c$TrapSite[which(c$Block == block & c$Ranch == ranch)])
        block.dict[[which(names(block.dict)==block)]] = trap.sites

    }

    dict[[val]] = block.dict

}
        
## Ok this is great, because now I can just pull out records by trap site!!

sapply(1:length(br),function(x){
    sapply(c("m","f","e"),function(y){
        vis_blockpop(x,y)
    })
})

## Trap sites 59 and 60 float ...
## 11-3 doesn't exist and should be 11-4

i = which(c$Block == "11-3")
c$Block[i] = rep("11-4",length(i))

sapply(1:length(br),function(x){vis_blockpop(x,"m")})

## ------- 10.31.14 ----- ##

## 25-3 should be  25-4 (but only in ranch 346)

j = which(d1$Block.s. == "25-3" & d1$Ranch == 346)
d$Block.s.[j] = rep("25-4",length(j))

## Data for Santa Fe 2009 doesn't align with the 2009 Maps:

 j = which(d1$Ranch %in% c(370,371) & d1$Year == 2009)
 d1[j,]

j = which(d1$Ranch %in% c(344,439,345,371,370) & d1$Year == 2009)

## Problems in:

## j = which(d1$Site == "3540B" & d1$Year == 2009)
## j = which(d1$Site == "4390A" & d1$Year == 2009)
## j = which(d1$Site == "3540A" & d1$Year == 2009)
## j = which(d1$Site == "3540C" & d1$Year == 2009)
## j = which(d1$Site == "3540D" & d1$Year == 2009)
## j = which(d1$Site == "3440B" & d1$Year == 2009)
## j = which(d1$Site == "3440A" & d1$Year == 2009)
## j = which(d1$Site == "3440C" & d1$Year == 2009)

##j = which(d1$Ranch %in% c(345,439,344) & d1$Year == 2009)

## it looks like only the the MD+Pesticide blocks got any recording, though
## it was only when the sprays occurred:



## I see no way around this problem.
## On to other matters: "e" and "f" need saving.
## How to select blocks/years that underwent to MD?

## ---------- 11.3.14 ----------- ##

mean(c$Males[which(c$Year == 2013 & c$TrapType == "LurePhero" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Males))])

28.85498

mean(c$Males[which(c$Year == 2013 & c$TrapType == "Flight" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Males))])

17.50547

mean(c$Males[which(c$Year == 2012 & c$TrapType == "Phero" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Males))])

8.783158

mean(c$Males[which(c$Year == 2011 & c$TrapType == "Phero" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Males))])

6.504926

mean(c$Males[which(c$Year == 2010 & c$TrapType == "Phero" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Males))])

7.463203

c("25-4","30-3","30-4","24-3","24-4","14-4","13-3","13-4","11-4","12-3","12-4")


## let's look just at 2013 graphs:

mean(c$Males[which(c$Year == 2013 & c$TrapType == "LurePhero" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("25-4","30-3","30-4","24-3","24-4","14-4","13-3","13-4","11-4","12-3","12-4") & !is.na(c$Males))])

##6.231405


mean(c$Eggs[which(c$Year == 2013 & c$TrapType == "Egg" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Eggs))])

##6.691974

mean(c$Eggs[which(c$Year == 2012 & c$TrapType == "Egg" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Eggs))])

##4.202174

mean(c$Eggs[which(c$Year == 2011 & c$TrapType == "Egg" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Eggs))])

##2.57619

mean(c$Eggs[which(c$Year == 2010 & c$TrapType == "Egg" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Eggs))])

##3.720779

mean(c$Eggs[which(c$Year == 2009 & c$TrapType == "Egg" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Eggs))])

##3.268456

sapply(1:length(br),function(x){vis_blockpop(x,"m")})


mean(c$Females[which(c$Year == 2013 & c$TrapType == "Delta/Ovip" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Females))])

mean(c$Females[which(c$Year == 2012 & c$TrapType == "Delta/Ovip" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Females))])

mean(c$Females[which(c$Year == 2011 & c$TrapType == "Delta/Ovip" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Females))])

mean(c$Females[which(c$Year == 2010 & c$TrapType == "Delta/Ovip" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Females))])

mean(c$Females[which(c$Year == 2009 & c$TrapType == "Delta/Ovip" & c$Ranch %in% c(344,345,346,439) & c$Block %in% c("2-1","2-2","2-3","2-4","1-2","1-3","1-4") & !is.na(c$Females))])

### Enough of that

### as per discussion 11.3.14, my new task is to sort out the treatments each
## site/year pair received and make sure it matches

## 1] "Ranch"                 "REC.Number"            "PUR.Number"           
##  [4] "PUR.Applied.Date"      "Site"                  "Block.s."             
##  [7] "Treated.Area"          "Product..Agrian."      "Target.Pest"          
## [10] "Spray.Volume..gal.ac." "Rate"                  "Rate.Units"           
## [13] "Quantity"              "Quantity.Unit"         "JD"                   
## [16] "Year"                  "Day.Month" 


temp = d1
temp$Block.s. = rep(NA,dim(d1)[1])
temp = unique(temp)

temp1 = apply(temp,1,function(row){

    site = row[5]
    date = row[4]
    product = row[8]
    blocks = d1$Block.s.[which(
        d1$Site == site &
        d1$PUR.Applied.Date == date &
        d1$Product..Agrian. == product
        )]
    
    if( " " %in% blocks){
        blocks = names(dict[[site]])
    }

    row[6] = paste(blocks,collapse="/")
    return(row)
})
     
                
##--------11.4.14--------##

## My new task is to double that the integrity of the spray data is Navy-Seal-pin-
## point-bin-laden-hoo-rah quality. Then I've got to sort out this who got what
## what not with the # puffers/acre.
##
## PROBLEM DISCOVERED: Whenever two blocks had the same name I was palimpsesting.

d1[which(d1$Ranch %in% c(439,345,344,346) & d1$Year == 2009),]
temp[which(temp$Ranch %ni% c(439,345,344,346) & temp$Year == 2009),]
temp[which(temp$Ranch %in% c(439,345,344,346) & temp$Year == 2009),]

## Let's take one last quick look at each block plot by year (I'm try this thang
## on emacs so it might break):

sapply(1:length(br),function(x){vis_blockpop(x,"m",2009)})
sapply(1:length(br),function(x){vis_blockpop(x,"m",2010)}) 
sapply(1:length(br),function(x){vis_blockpop(x,"m",2011)})
sapply(1:length(br),function(x){vis_blockpop(x,"m",2012)})
sapply(1:length(br),function(x){vis_blockpop(x,"m",2013)})

## This look has resulted in these observations:

###############
## ELISIONS  ##
###############

## There is no puffer information for 2009 from either location.
p[which(p$Target.Pest == "Navel Orangeworm" & p$Year == "13" & p$Site == "3450D"),]

## There is no 2011 pdf for Santa Fe sites.

## Why did 3710D - 36-3 & 36-2 get pesticide sprays in 2012?
temp[which(temp$Ranch %ni% c(439,345,344,346) & temp$Year == 2012),]
temp[which(temp$Site == "3710D" & temp$Year == 2012),]

## Why are there pesticide sprays in the MD-only blocks in 2013 (e.g.):
temp[which(temp$Site == "3450D" & temp$Year == 2013),]
temp[which(temp$Site == "3440A" & temp$Year == 2013),] 

###############
###############





##-------11.5.14--------##

## Today look at pesticide efficacy in plots w/o MD:

## first lets assemble the pesticide df:

d2 = d1[which(
    (d1$Site %in% c('3460A','3460B') & d1$Year == 2009) |
    (d1$Ranch == 371 & d1$Block.s. %in% c('25-2','25-3') & d1$Year == 2012) |
    (d1$Site %in% c('3450A','3450B','4390A') & d1$Year %in% c(2010,2011,2012,2013))
    ),] 

s.d = d2
s.d$Block.s. = rep(NA,dim(d2)[1])
s.d = unique(s.d)
d3 = d2[which(is.na(d2$Day.Month)),] 
for (i in 1:dim(s.d)[1]){
    
    site = s.d$Site[i]
    
    d.m = s.d$Day.Month[i]
    
    blocks = d2$Block.s.[which(d2$Site == site & d2$Day.Month == d.m)]
    print(blocks)
    if ( " " %in% blocks){
        blocks = names(dict[[site]])
        for (block in blocks){
            row = s.d[i,]
            row$Block.s. = block
            d3 = rbind(d3,row)
        }
    }else{
        for(block in blocks){
            row = s.d[i,]
            row$Block.s. = block
            d3 = rbind(d3,row)
        }
    }
}

## Some serious housekeeping needed to occur. my d df's weren't cutting it. weird
## stuff was going on. now I have p.n - this is a cleaned up p constrained to only
## navel orangeworm treatments. So now let's get on with it:

## wait. Need Day.Month...
dm = sapply(p.n$PUR.Applied.Date,function(x){
    
    mdy = unlist(strsplit(as.character(x),"/"))
    
    dm = paste(mdy[2],mdy[1],sep="/")
    
    return(dm)
})

p.n$Day.Month = dm

p.n = p.n[order(as.Date(p.n$Day.Month, format="%d/%m")),] 
dm = factor(p.n$Day.Month,levels=unique(p.n$Day.Month),ordered=TRUE)
p.n$Day.Month = dm

dates = sapply(p.n$PUR.Applied.Date,function(x){
    mdy = unlist(strsplit(as.character(x),"/"))
    y = paste0("20",mdy[3])
    mdy = paste(mdy[1],mdy[2],y,sep="/")
    return(mdy)
})

## ------ 11.6.14 ------- ##

## I don't understand. Why did p.n revert back to having bad blocks?

## I'm just going to make a function, just in case this happens again.

## now I need to pare down p.n so that it is just the pesticide-only blocks.

p.n.p = p.n[which(
    (p.n$Site %in% c('3460A','3460B') & p.n$Year == 2009) |
    (p.n$Ranch == "371" & p.n$Block.s. %in% c("25-2,25-3","25-3,25-2") & p.n$Year == 2012) |
    (p.n$Site %in% c('3450A','3450B','4390A') & p.n$Year %in% c(2010,2011,2012,2013))
    ),] 

## eval.efficacy() works fine.
ef.data = eval.efficacy()


## opacity:
ggplot(ef.dat,aes(x=Day.Month,y=log(Proportion))) + geom_point(size = 5,aes(alpha=Weight)) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy w/o MD")

## size
ggplot(ef.dat,aes(x=Day.Month,y=log(Proportion))) + geom_point(aes(size=Weight)) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy w/o MD")

## lm
ggplot(ef.dat,aes(x=Day.Month,y=log(Proportion))) + geom_point(size = 5,aes(alpha=Weight)) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy w/o MD") + stat_smooth(method="lm")


## some models on the output:
i  = complete.cases(ef.dat$Proportion,ef.dat$Weights)

m1 = gam(log(Proportion) ~ s(as.numeric(Day.Month)), data = ef.dat[i,], weights = ef.dat$Weights[i])

m2 = gam(log(Proportion) ~ s(as.numeric(Day.Month)) + s(as.factor(Year),bs="re") + s(as.factor(Ranch),bs="re") + s(Site, bs="re"), data = ef.dat[i,], weights = ef.dat$Weights[i])


## -------- 11.7.14 --------- ##

## Just looking at the data in different ways today.
## maybe I'll make some models.


###########################
## BEGIN EFFICACY DATA 1 ##
###########################


## this is the pesticide w/o MD set:
p.n.p = p.n[which(
    (p.n$Site %in% c('3460A','3460B') & p.n$Year == 2009) |
    (p.n$Ranch == "371" & p.n$Block.s. %in% c("25-2,25-3","25-3,25-2") & p.n$Year == 2012) |
    (p.n$Site %in% c('3450A','3450B','4390A') & p.n$Year %in% c(2010,2011,2012,2013))&
    p.n$Product..Agrian. == "Intrepid 2F"
    ),] 

## this is the pesticide in presence of MD set:
p.n.pu = p.n[-which(
    (p.n$Site %in% c('3460A','3460B') & p.n$Year == 2009) |
    (p.n$Ranch == "371" & p.n$Block.s. %in% c("25-2,25-3","25-3,25-2") & p.n$Year == 2012) |
    (p.n$Site %in% c('3450A','3450B','4390A') & p.n$Year %in% c(2010,2011,2012,2013))
    ),] 
p.n.pu = p.n.pu[which(p.n.pu$Product..Agrian. = "Intrepid 2F"),] 


## This is all the pesticides:
p.n.pall = p.n[which(p.n$Product..Agrian. = "Intrepid 2F"),]

ef.data = eval.efficacy()
ef.data.MD = eval.efficacy(df = p.n.pu)
ef.data.all = eval.efficacy(df = p.n.pall)

ef.data$Year = as.numeric(ef.data$Year)
ef.data.MD$Year = as.numeric(ef.data.MD$Year)
ef.data.all$Year = as.numeric(ef.data.all$Year)


#################
## PLOT BASES  ##
#################

## proportion vs day of season
q = ggplot(ef.data,aes(x=Day.Month,y=log(Proportion))) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy w/o MD")

q.MD = ggplot(ef.data.MD,aes(x=Day.Month,y=log(Proportion))) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy w/ MD")

q.all = ggplot(ef.data.all,aes(x=Day.Month,y=log(Proportion),shape=Location)) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy")

## proportion vs year
q.all.year = ggplot(ef.data.all,aes(x=Year,y=log(Proportion))) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy")

q.year = ggplot(ef.data,aes(x=Year,y=log(Proportion),color=Month)) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy w/o MD") 

q.MD.year = ggplot(ef.data.MD,aes(x=Year,y=log(Proportion),color=Month)) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy w/ MD")

q.all.year = ggplot(ef.data.all,aes(x=Year,y=log(Proportion),color=Month)) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("All Pesticide Efficacy Data by Year")

## proportion vs product
q.product = ggplot(ef.data,aes(x=Product..Agrian.,y=log(Proportion),color=as.factor(Year))) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy by product")

q.MD.product = ggplot(ef.data.MD,aes(x=Product..Agrian.,y=log(Proportion),color=as.factor(Year))) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy by product") 

q.product.date = ggplot(ef.data,aes(x=Day.Month,y=Product..Agrian.,color=as.factor(Year))) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("product use by date")

## bifenture is less effective because it is used late in the season

## weight bs day of the season
q.weight.date = ggplot(ef.data,aes(x=Day.Month,y=Weight,color=as.factor(Year))) + geom_point(aes(size=ceiling(Weight))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("weight by date")

## weight vs proportion
q.weight.proportion = ggplot(ef.data,aes(x=Weight,y=log(Proportion),color=as.factor(Month))) + geom_point(size = 4) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("proportion by weight")

#################
#################


### m2 ###
## gam log(proportion) vs Day.Month with random effects:

m2a = gam(log(Proportion) ~ s(as.numeric(Day.Month)) + s(Year,bs="re") + s(Ranch,bs="re") + s(Site, bs="re"), data = ef.data, weights = ef.data$Weight) 

m2b = gam(log(Proportion) ~ s(as.numeric(Day.Month)) + s(Year,bs="re") + s(Ranch,bs="re") + s(Site, bs="re"), data = ef.data) ## weights don't work?

##------##

### m3 ###
## linear mixed effect log(prop) vs day.month:

m3a = glmer(log(Proportion) ~ as.numeric(Day.Month) + (1|Year) + (1|Ranch) + (1|Site),data = ef.data, family = "gaussian", weights = ef.data$Weight)

m3b = glmer(log(Proportion) ~ as.numeric(Day.Month) + (1|Year) + (1|Ranch) + (1|Site),data = ef.data, family = "gaussian")

## vis:
vals = predict(m3)
i = which(!is.na(ef.data$Proportion))
ggplot(ef.data[i,],aes(x=Day.Month, y = vals)) + geom_point(size = 5)



##-------##

### m4 ###
## gam log(prop) vs year & day.month w/o re:
K = length(unique(ef.data$Year))-1
m4 = gam(log(Proportion) ~ s(as.numeric(Day.Month)) + s(Year,k=K) , data = ef.data, weights = ef.data$Weight)

##------##

### m5 ###
## gam log(prop) vs year w/o re:

m5 = gam(log(Proportion) ~ s(Year,k=K),data=ef.data,weights = ef.data$Weights)
## not sure about this

##########

### m6 ###
## linear mixed effect proportion vs year:

m6a1 = glmer(log(Proportion) ~ Year + (1|Ranch) + (1|Site) + (1|Day.Month),data = ef.data,family="gaussian",weights = ef.data$Weight)

m6a2 = glmer(log(Proportion) ~ Year + (1|Ranch) + (1|Site),data = ef.data,family="gaussian",weights = ef.data$Weight)

m6b = glmer(log(Proportion) ~ Year + (1|Ranch) + (1|Site),data = ef.data.MD,family="gaussian",weights = ef.data.MD$Weight)

## summary:

## I'm not happy with this. The weights really upset the results.

## vis:

vals = predict(m6a)
i = which(!is.na(ef.data$Proportion))
ggplot(ef.data[i,],aes(x=Year, y = vals)) + geom_point(size = 5) 

### m7 ###
##

m7a = glmer(log(Proportion) ~ (1|Year),data=ef.data,family="gaussian",weights=ef.data$Weight)


m7b  = glmer(log(Proportion) ~ (1|Year),data=ef.data.MD,family="gaussian",weights=ef.data.MD$Weight)


m7c = glmer(log(Proportion) ~ (1|Year),data=ef.data.all,family="gaussian",weights=ef.data.all$Weight)


## Brief update:
## It looked like there were pesticides being used in MD-only plots for 2013.
## There weren't; the block in these cases is listed as " " (which led me to
## believe all the blocks were sprayed). The Treated.Area is indicated as
## (usually) 1/2 the full area of that block. So along with the maps this
## makes it clear that the correct plots were sprayed:

p.n[which(p.n$Year == "13" & p.n$Ranch %in% c(3440,3450,3460) & p.n$Product..Agrian. != "Puffer NOW" & p.n$Block.s. == " "),c("PUR.Applied.Date","Product..Agrian.","Site","Treated.Area")]  



unique(p.n$[which(p.n$Site == "3440A" & p.n$Block.s. == " "),])

unique(p.n$Treated.Area[which(p.n$Site == "3440C" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3440B" & p.n$Block.s. == " ")])


unique(p.n[which(p.n$Site == "3440B" & p.n$Block.s. == " "),])

unique(p.n[which(p.n$Site == "3440B" & p.n$Block.s. == " "),])

unique(p.n[which(p.n$Site == "3440B" & p.n$Block.s. != " "),])

ef.data$Month = sapply(ef.data$Day.Month,function(x){unlist(strsplit(as.character(x),"/"))[2]})
ef.data.MD$Month = sapply(ef.data.MD$Day.Month,function(x){unlist(strsplit(as.character(x),"/"))[2]}) 
ef.data.all$Month = sapply(ef.data.all$Day.Month,function(x){unlist(strsplit(as.character(x),"/"))[2]})



## ------ 11.10.14 ------ ##

## to do: fix the section of the spray database above so the blocks are correct.

## also: add in 2009 puffer data: For Santa Fe, puffers deployed on 4/22 and 6/13
## in 2009. All acreage at 2 puffers/ac and 100% pheromone load (about 4 g ai).
## For Lost Hills in 2009, puffers were deployed on 4/15 at 1 or 2 puffers/ ac as
## depicted on map (attached).

unique(p.n[which(p.n$Year == "13" & p.n$Ranch %in% c(3440,3450,3460) & p.n$Product..Agrian. != "Puffer NOW" & p.n$Block.s. == " "),c("Site")])

## 3450C 3450D 3450A 3450B 3440A 3440B 3440C 3460A 3460B

unique(p.n$Treated.Area[which(p.n$Site == "3440A" & p.n$Block.s. == " ")])

p.n$Block.s.[which(p.n$Site == "3440A" & p.n$Treated.Area == 85)] = "14-1"

unique(p.n$Treated.Area[which(p.n$Site == "3440C" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3440B" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3450A" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3450B" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3450C" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3450D" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3460B" & p.n$Block.s. == " ")])

unique(p.n$Treated.Area[which(p.n$Site == "3460A" & p.n$Block.s. == " ")])





## also: add in 2009 puffer data: For Santa Fe, puffers deployed on 4/22 and 6/13
## in 2009. All acreage at 2 puffers/ac and 100% pheromone load (about 4 g ai).
## For Lost Hills in 2009, puffers were deployed on 4/15 at 1 or 2 puffers/ ac as
## depicted on map (attached).

unique(p.n[which(p.n$Site == "3450C" & p.n$Block.s. == " "),])

## these are the problematic rows:

p[c(3960 ,3962 ,4142,4147, 1419 ,1424 ,1429,5685,5687,4413,4415,1554,1556,1558,5734,5736 ),]

## now trying to incorporate the 2009 puffers:



sf.3710 <- do.call(rbind,lapply(unique(p.n$Site[which(p.n$Ranch == 3710)]),function(site){

    row = p.n[which(p.n$Site == site & p.n$Product..Agrian. == "Puffer NOW"),][1,]
    row$Block.s. = " "
    row$PUR.Applied.Date = "XXXXX"
    row$Day.Month = "XXXX"
    row$JD = 14356
    row$Year = "09"
    row$Treated.Area = NA
    row$REC.Number = NA
    row$Quantity = NA

    return(row)
}))
    
sf.3700 <- do.call(rbind,lapply(unique(p.n$Site[which(p.n$Ranch == 3700)]),function(site){

    row = p.n[which(p.n$Site == site & p.n$Product..Agrian. == "Puffer NOW"),][1,]
    row$Block.s. = " "
    row$PUR.Applied.Date = "XXXXXX"
    row$Day.Month = "XXXXXXX"
    row$JD = 14408
    row$Year = "09"
    row$Treated.Area = NA
    row$REC.Number = NA
    row$Quantity = NA

    return(row)
}))


sites <- unique(p.n$Site[which(p.n$Ranch %in% c(3440,3450,4390))])
OnepA <- c("2-2", "2-1", "1-2", "2-3", "2-4", "1-3", "1-4", "24-2", "24-1", "24-3", "24-4")

TwopA <- c("11-2", "11-1", "12-2", "12-1", "11-4", "12-3", "12-4", "14-1", "13-2", "13-1", "14-4", "13-3", "13-4")



lh.all <- do.call(rbind,lapply(sites,function(site){
    
    
    blocks = names(dict[[as.character(site)]])
    
    rows = do.call(rbind,lapply(blocks,function(block){
        row = p.n[which(
            p.n$Site == site &
            p.n$Product..Agrian. == "Puffer NOW"),][1,]
        row$Rate = NA
        row$Block.s. = block
        row$PUR.Applied.Date = "4/15/09"
        row$Day.Month = "15/4"
        row$JD = 14349   ## WHOOPS, 14408
        row$Year = "09"
        row$Treated.Area = NA
        row$REC.Number = NA
        row$Quantity = NA
        if( block %in% OnepA){
            row$Rate = 1
        }
        if( block %in% TwopA){
            row$Rate = 2
        }
        if ( is.na(row$Rate) ){stop(print(paste(block,site,sep=" ")))}
        
        return(row)
    }))
   
    return(rows)
    
}))

new.rows = rbind(sf.3700,sf.3710,lh.all)

## I still want the Day.Month value as a factor... or do I?
## ... Yes I do because this is going to become the new,
## fully furnished (except for some errant sprays?) p.n .
temp = p.n
temp$Day.Month = as.character(temp$Day.Month)
temp = rbind(temp,new.rows)
temp = temp[order(as.Date(temp$Day.Month, format="%d/%m")),] 
dm = unique(temp$Day.Month)
temp$Day.Month = factor(temp$Day.Month,levels=dm,ordered=TRUE) #change x to whatever
p.n=temp
p.n = p.n[order(as.Date(p.n$PUR.Applied.Date, format="%m/%d/%y")),] 

## now to merge p.n and new.rows


##--------12.11.14--------##

## Once all the puffer stuff above is entered, this will
## give us the puff-only db arranged by Block
puf.db <- p.n[which(p.n$Product..Agrian. == "Puffer NOW"),]
puf.db.by.block = do.call(rbind,lapply(1:dim(puf.db)[1],function(i){

    row = puf.db[i,]
    site = row$Site
    
    blocks = row$Block.s.
    
    if ( row$Block.s. == " " ){
        blocks = names(dict[[as.character(site)]])
    }else{
        blocks = unlist(strsplit(row$Block.s.,","))
    }
    
    rows = do.call(rbind,lapply(blocks,function(block){
        new.row = row[,c(6,1,5,4,17,18,19,8,11,12,13,14)]
        new.row$Block.s. = block
        return(new.row)
    }))
    
    return(rows)

}))
puf.db = puf.db.by.block

#### SUPER IMPORTANT ####
## dict[[factor]] yeilds dict[[levels(factor)]] !!
## dict[[as.character(factor)]] is what you want

##-----11.13.14------##

## Treatment (c$Trtmnt) needs to be put into the puf.db:        
            
temp = puf.db
temp$Treatment = NA
            
temp1 = do.call(rbind, lapply(1:dim(puf.db)[1],function(i){

    new.row = temp[i,]
    block = new.row$Block.s.
    ranch = new.row$Ranch
    year = new.row$Year
    
    treatment = c$Trtmnt[which(
        c$Block == block &
        c$Ranch == as.character(ranch / 10) &
        c$Year == as.numeric(year) + 2000 ## Hack
        )]

    treatment = unique(treatment)
    if(length(treatment) !=1){
        new.row$Treatment = NA

    }
    else{
        new.row$Treatment = treatment
        
    }

    return(new.row)
    
}))

puf.db = temp1


## this is just to verify the data in puf.db are correct:


sapply(1:dim(puf.db)[1],function(i){

    row = puf.db[i,]
    j = which(
        as.character(p$Site) == as.character(row$Site) &
        (grepl(row$Block.s.,p$Block.s.) |
         p$Block.s. == " ") &
        as.character(row$PUR.Applied.Date) == as.character(p$PUR.Applied.Date) &
        as.character(p$Product..Agrian.) == "Puffer NOW"
        )
    print(length(j))
})


i = which(
    as.character(p.n$Site) == "3450C" &
    (grepl("11-1",p.n$Block.s.) |
     p.n$Block.s. == " ") &
    "4/15/09" == as.character(p.n$PUR.Applied.Date) &
    as.character(p.n$Product..Agrian.) == "Puffer NOW"
    )
print(length(i))


## That's done, so now Jay would like a quick analysis of the efficacy
## data I've been looking at. He wanted a few other variables, so I've
## added them to eval.efficacy()

######################################
## BEGIN EFFICACY DATA 2 (11.13.14) ##
######################################


## this is the pesticide w/o MD set:

p.n.p = p.n[which(
    (p.n$Site %in% c('3460A','3460B') & p.n$Year == 2009) |
    (p.n$Ranch == "371" & p.n$Block.s. %in% c("25-2,25-3","25-3,25-2") & p.n$Year == 2012) |
    (p.n$Site %in% c('3450A','3450B','4390A') & p.n$Year %in% c(2010,2011,2012,2013)) &
    p.n$Product.Agrian. == "Intrepid 2F"
    ),] 

## this is the pesticide in presence of MD set:
p.n.pu = p.n[-which(
    (p.n$Site %in% c('3460A','3460B') & p.n$Year == 2009) |
    (p.n$Ranch == "371" & p.n$Block.s. %in% c("25-2,25-3","25-3,25-2") & p.n$Year == 2012) |
    (p.n$Site %in% c('3450A','3450B','4390A') & p.n$Year %in% c(2010,2011,2012,2013))
    ),] 
p.n.pu = p.n.pu[which(p.n.pu$Product..Agrian. == "Intrepid 2F"),] 


ef.data = eval.efficacy()
ef.data.MD = eval.efficacy(df = p.n.pu)
ef.data.all = eval.efficacy(df = p.n.pall)

ef.data$Year = as.numeric(ef.data$Year)
ef.data.MD$Year = as.numeric(ef.data.MD$Year)
ef.data.all$Year = as.numeric(ef.data.all$Year)

ef.data.all$Treatment2 = sapply(ef.data.all$Treatment,function(x){

    if(is.na(x)){return(x)}

    if(x %in% c("Control","Conv")){return("C")}
    else{return("CMD")}
})


q.all = ggplot(ef.data.all,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )") 

q = ggplot(ef.data,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )") 


m1 = gam(log(Proportion) ~ s(DayOfYear) + Year + s(MeanPopBeforeSpray) + Edges + Treatment2 + TotalPriorSprays + PriorSprays,data=ef.data.all,family="gaussian")

m1a = gam(log(Proportion) ~ s(as.numeric(Day.Month)) + Year + Edges + TotalPriorSprays + PriorSprays + Treatment2,data=ef.data.all,weights=MeanPopBeforeSpray,family="gaussian")


i = complete.cases(ef.data.all[,c('Weight','Proportion','PriorSprays')])
cor(ef.data.all[i,c('Weight','Proportion','PriorSprays')])


##--------17.11.14--------##

ef.data.MD = eval.efficacy(df = p.n.pu)

q.MD = ggplot(ef.data.MD,aes(x=Day.Month,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )") 


## Drawing the smooths from a gam:
## m <- gam(blah)
## pdf('~/Documents/test.pdf')
## plot(m, select = 1)
## dev.off()

## pdf('~/Documents/test2.pdf')
## plot(m, select = 2)
## dev.off()


2013:3440B:13-2
Spray:15818 Puffers:15812
Spray:15818 Puffers:15818

puf.db[which(puf.db$Year=="13" & puf.db$Site == "3440B" & puf.db$Block.s. == "13-2"),]

p.n[which(p.n$Site == "3440B" & p.n$Year == "13"),]


## eval trap shutdow ##

temp = eval.trap.shutdown()
i = which(temp$Proportion > 100)
> temp[i,]
      Block.s. Ranch  Site PUR.Applied.Date Year Day.Month    JD
11909     12-1  4390 4390B           4/6/11   11       6/4 15070
      Product..Agrian. Rate Rate.Units Quantity Quantity.Unit Treatment
11909       Puffer NOW    1   unit / A    78.15          unit      1CMD
      Proportion MeanPopBeforeMD Edges
11909        133            0.25     1
> 

## -------- 18.11.14 -------- ##

## this is nice:
## strptime("01/01/2011","%m/%d/%Y")$yday + 1
## This will give us the day of the year!

d.o.y = sapply(p.n$PUR.Applied.Date,function(x){
    strptime(as.character(x),"%m/%d/%Y")$yday + 1
})

d.o.y = sapply(c$Date,function(x){
    strptime(as.character(x),"%m/%d/%Y")$yday + 1
})

d.o.y = sapply(puf.db$PUR.Applied.Date,function(x){
    strptime(as.character(x),"%m/%d/%Y")$yday + 1
})


## --- OK back to trap shutdown --- ##

w = ggplot(temp,aes(x=DayOfYear,y=log(Proportion),color=MeanPopBeforeMD)) + geom_point(aes(size=Edges))

w1 = ggplot(temp,aes(x=Edges,y=log(Proportion))) + geom_point(aes(size=MeanPopBeforeMD))

w2 = ggplot(temp,aes(x=MeanPopBeforeMD,y=log(Proportion))) + geom_point(size=5)

w3 = ggplot(temp,aes(x=Year,y=log(Proportion))) + geom_point(size=5)

w4 = ggplot(temp,aes(x=Treatment,y=log(Proportion))) + geom_point(size=5)

w5 = ggplot(temp,aes(x=Rate,y=log(Proportion),color=Treatment)) + geom_point(size=5)

m2 = gam(log(Proportion) ~ Edges + Rate + Year + s(DayOfYear), data = temp[i,], weight = MeanPopBeforeMD)

m2a = gam(log(Proportion) ~ Edges + Rate + Year + s(DayOfYear) + s(MeanPopBeforeMD), data = temp[i,])

m2d = lm(log(Proportion) ~ Edges + Rate + Year, data = tempi)

q = ggplot(ef.data,aes(x=Day.Month,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )") 


## I should ask about temp$Rate - whether I can change 1.943 to 2.
                                             
##--- 19.11.14 ---##

## Alright! I know a little bit about plyr now!


## Call:
## lm(formula = log(Proportion) ~ Year, data = tempi, weights = MeanPopBeforeMD)




## Linear mixed model fit by REML ['lmerMod']
## Formula: log(Proportion) ~ (1 | Year)
##    Data: tempi
## Weights: MeanPopBeforeMD



## 11.20.14 ##

## More trap shutdown ##


w1a = ggplot(ts.db.13,aes(x=DayOfYear,y=log(Proportion),color=Edges)) + geom_point(aes(size=MeanPopBeforeMD))

w1b = ggplot(ts.db.else,aes(x=DayOfYear,y=log(Proportion),color=Edges)) + geom_point(aes(size=MeanPopBeforeMD))

w1 = ggplot(ts.db,aes(x=DayOfYear,y=log(Proportion),color=Edges)) + geom_point(aes(size=MeanPopBeforeMD))

w1 = ggplot(ts.db,aes(x=Edges,y=log(Proportion))) + geom_point(aes(size=MeanPopBeforeMD))

w2 = ggplot(ts.db,aes(x=Year,y=log(Proportion))) + geom_point(aes(size=MeanPopBeforeMD))

w3 = ggplot(ts.db,aes(x=Edges,y=MeanPopBeforeMD)) + geom_point(aes(size=MeanPopBeforeMD))

## 11.21.14 ##

## I was curious about why the number of edges seemed
## to be positively correllated to the degree of trap shutdown,
## especially as I was expecting the opposite. See:

m2d = lm(log(Proportion) ~ Edges + Rate + as.numeric(Year), data = ts.db)


## but I was curious about how the edges may be related to the
## mean pop before the spray:

##(a)
t2 = lm(MeanPopBeforeMD ~ Edges, data=ts.db)


temp = ddply(ts.db, .(Edges), summarize, max = max(MeanPopBeforeMD))


temp = ddply(ts.db, .(Edges), summarize, median = median(MeanPopBeforeMD))


## So it's hard to say... There are just so many more plots with 1 edge.
## The problem is visible in the graph:
w2 = ggplot(ts.db,aes(x=Edges,y=MeanPopBeforeMD)) + geom_point(aes(size=MeanPopBeforeMD))

## Can we account for this statistically?

m2a = gam(log(Proportion) ~ Edges + Rate + as.numeric(Year) + s(MeanPopBeforeMD), data = ts.db)

m2a = gam(log(Proportion) ~ Edges + Rate + Year + s(MeanPopBeforeMD), data = ts.db)

ts.db13 = subset(ts.db,Year == "13")
ts.dbnot13 = subset(ts.db,Year != "13")

m = gam( log(Proportion) ~ Edges + Rate,data = ts.dbnot13, weights = MeanPopBeforeMD)
m2 = gam( log(Proportion) ~ Edges + Treatment + s(MeanPopBeforeMD),data = ts.db13)

## Now it looks like only Edges and the smooth are "Significant"

temp13 = ddply(ts.db13,.(Edges),summarize,mean = mean(MeanPopBeforeMD))

temp13 = ddply(ts.db13,.(Edges),summarize,length = length(MeanPopBeforeMD))

tempElse = ddply(ts.dbnot13,.(Edges),summarize,mean = mean(MeanPopBeforeMD))

tempElse = ddply(ts.dbnot13,.(Edges),summarize,length = length(MeanPopBeforeMD))

temp = ddply(ts.db, .(Year), summarize, mean = mean(Proportion))



mean(ts.db13$Proportion)


mean(ts.dbnot13$Proportion)


## 11.24.14 ##

## I think my focus now is going to be on making these models as good as
## they can be. Jay and I talked on friday about encoding a little more
## 'biology' into them, as well as make some changes for statistical
## purposes. As far as biology goes, the only thing we came up with was
## trying to account for life cycles. Since Intrepid actually targets pupae/cater-
## pillars (or something), looking at male counts before and after sprays doesn't
## make much sense; we'd expect to see no immediate change in the male counts
## followed by sharp decrease in males when the next youngest generation reaches
## maturity. Computationally this is more difficult to do. We could count off
## the time it takes for a NOW to reach maturity, using the spray date as D1,
## and see what trap counts look like then - but what do we compare it to? Unclear.
## We'll have to think about it some more. One thing Jay definitely wanted me to
## try was making smooth interaction terms to account for different NOW flight
## patterns between years. It looks like s(x,fac,bs="fs") should do the trick.
##
## Statistically, there are a couple other things to ponder. First off, ln isn't
## working for us. It forces us to alter data, which means our model is really
## quite off. Just how off it was didn't become clear until recently. Instead
## we need to figure out a way to work with raw proportions. I'm not sure if
## there was a second off. 

## just a sec, I want to look at eggs:

plots = sapply(1:52,function(x){vis_blockpop(x,"e",year=2013)})
plots = sapply(1:52,function(x){vis_blockpop(x,"e",year=2012)})
plots = sapply(1:52,function(x){vis_blockpop(x,"e",year=2011)})
plots = sapply(1:52,function(x){vis_blockpop(x,"e",year=2010)})
plots = sapply(1:52,function(x){vis_blockpop(x,"e",year=2009)})

## Well now I want to look at males again:

plots = sapply(1:52,function(x){vis_blockpop(x,"m",year=2013)})
plots = sapply(1:52,function(x){vis_blockpop(x,"m",year=2012)})
plots = sapply(1:52,function(x){vis_blockpop(x,"m",year=2011)})
plots = sapply(1:52,function(x){vis_blockpop(x,"m",year=2010)})
plots = sapply(1:52,function(x){vis_blockpop(x,"m",year=2009)})


## Can we see MD efficacy in egg traps? (later)
## What is the average number of males / the max number of males a block
## will see before it gets any treatment:

## OK eval.efficacy and eval.trap.shutdown are still broken

## 11.25.14 ##

## eval.efficacy is fixed. The problem was with p.n.p - I hadn't converted
## everyting to sane-person classes. Now... What now? Oh right. I want to make
## my models, yo:

m.ef = gam(Proportion ~ s(DayOfYear,as.factor(Year),bs="fs") + Year + Edges + , data=ef.data.all, family = "poisson")

ef.data.09 = subset(ef.data,Year == 2009)
ef.data.10 = subset(ef.data,Year == 2010)
ef.data.11 = subset(ef.data,Year == 2011)
ef.data.12 = subset(ef.data,Year == 2012)
ef.data.13 = subset(ef.data,Year == 2013)

q.all = ggplot(ef.data.all,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")

q.13 = ggplot(ef.data.13,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")

q.09 = ggplot(ef.data.09,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")

q.10 = ggplot(ef.data.10,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")

q.11 = ggplot(ef.data.11,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")

q.12 = ggplot(ef.data.12,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")


ggsave("prop13.pdf",q.13)
ggsave("prop09.pdf",q.09)
ggsave("prop12.pdf",q.12)
ggsave("prop11.pdf",q.11)
ggsave("prop10.pdf",q.10)

ef.data.all$Ranch2 = sapply(ef.data.all$Ranch,function(x) if (x %in% c("344","345", "346", "439")) return("L") else return("S"))


m.ef.yearlySmooth = gam(Proportion ~ s(DayOfYear,as.factor(Year),bs="fs") + Edges + Treatment2 + TotalPriorSprays + PriorSprays + Ranch2, data=ef.data.all, weights = MeanPopBeforeSpray, family = "poisson")

m.ef = gam(Proportion ~ s(DayOfYear) + Year + Edges + Treatment2 + TotalPriorSprays + PriorSprays + Ranch2, data=ef.data.all, weights = MeanPopBeforeSpray, family = "poisson")


## this is the pesticide w/o MD set:
p.n.p = p.n[which(
    (p.n$Site %in% c('3460A','3460B') & p.n$Year == 2009) |
    (p.n$Ranch == "371" & p.n$Block.s. %in% c("25-2,25-3","25-3,25-2") & p.n$Year == 2012) |
    (p.n$Site %in% c('3450A','3450B','4390A') & p.n$Year %in% c(2010,2011,2012,2013))
    ),] 

p.n.p.intrepid = subset(p.n.p, Product..Agrian. == "Intrepid 2F")


## this is the pesticide in presence of MD set:
p.n.md = p.n[-which(
    (p.n$Site %in% c('3460A','3460B') & p.n$Year == 2009) |
    (p.n$Ranch == "371" & p.n$Block.s. %in% c("25-2,25-3","25-3,25-2") & p.n$Year == 2012) |
    (p.n$Site %in% c('3450A','3450B','4390A') & p.n$Year %in% c(2010,2011,2012,2013))
    ),] 
p.n.md = p.n.md[which(p.n.pu$Product..Agrian. != "Puffer NOW"),] 
p.n.md.intrepid = subset(p.n.md, Product..Agrian. == "Intrepid 2F")


## This is all the pesticides:
p.n.all = p.n[which(p.n$Product..Agrian. != "Puffer NOW"),]
p.n.all.intrepid = subset(p.n.all,Product..Agrian. == "Intrepid 2F")

ef.data = eval.efficacy()
ef.data.MD = eval.efficacy(df = p.n.pu)
ef.data.all = eval.efficacy(df = p.n.pall)

ef.data.intrepid = eval.efficacy(df = p.n.p.intrepid)
ef.data.MD.intrepid = eval.efficacy(df = p.n.md.intrepid)
ef.data.all.intrepid = eval.efficacy(df = p.n.all.intrepid)

q = ggplot(ef.data,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")

q.in = ggplot(ef.data.intrepid,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")

q.md = ggplot(ef.data.MD,aes(x=DayOfYear,y=log(Proportion))) + geom_point(aes(size=ceiling(MeanPopBeforeSpray))) + theme(axis.text.x = element_text(angle=90,hjust=0)) + ggtitle("Pesticide Efficacy") + xlab("Day in Growing Season") + ylab("ln( (mean male NOW pop month after spray) / (mean male NOW pop month before spray) )")


ggplot(c,aes(x=JD,y=Males))+geom_point(size=5)

ggplot(c,aes(x=DayOfYear,y=Males))+geom_point(size=5) + geom_vline(xintercept = unique(puf.db$DayOfYear), colour = "blue", linetype = "longdash") + geom_vline(xintercept = unique(p.n$DayOfYear[which(p.n$Product..Agrian. == "Intrepid 2F")]), colour = "red", lintype = "longdash")

plot1 = ggplot(subset(c,Trtmnt %in% c("Conv","Control")),aes(x=DayOfYear,y=Males))+geom_point(size=5) + geom_vline(xintercept = unique(p.n.p$DayOfYear), colour = "red", lintype = "longdash") + ggtitle("Male Now Densities vs. Day of the Year")

plot2 = ggplot(subset(c,Trtmnt %ni% c("Conv","Control")),aes(x=DayOfYear,y=Males))+geom_point(size=5) + geom_vline(xintercept = unique(puf.db$DayOfYear), colour = "blue", linetype = "longdash") + geom_vline(xintercept = unique(p.n.md.intrepid$DayOfYear), colour = "red", lintype = "longdash")

plot3 = ggplot(subset(c,Trtmnt %in% c("1CMD","2CMD","100%C","50%C")),aes(x=DayOfYear,y=Males))+geom_point(size=5) + geom_vline(xintercept = unique(subset(puf.db, Treatment %in% c("1CMD","2CMD","100%C","50%C"))$DayOfYear), colour = "blue", linetype = "longdash") + geom_vline(xintercept = unique(p.n.md.intrepid$DayOfYear), colour = "red", lintype = "longdash")

plot4 = ggplot(subset(c,Trtmnt %ni% c("Conv","Control","1CMD","2CMD","100%C","50%C")),aes(x=DayOfYear,y=Males))+geom_point(size=5) + geom_vline(xintercept = unique(subset(puf.db, Treatment %ni% c("1CMD","2CMD","100%C","50%C"))$DayOfYear), colour = "blue", linetype = "longdash") 



ggplot(ef.data,aes(DayOfYear,MeanPopBeforeSpray)) + geom_point(size=5)
ggplot(ef.data.MD,aes(DayOfYear,MeanPopBeforeSpray)) + geom_point(size=5)
ggplot(ef.data.all,aes(DayOfYear,MeanPopBeforeSpray)) + geom_point(size=5)

## 11.25.14 ##

## So I had this feeling that the results we were seeing were not, as we hoped, a
## signal from the spray efficacy, but were in fact a vestige of some larger
## biological pattern... Given the above plots I'm fairly convinced. 

## So now looking at trap shutdown again:

ts.db.14 = eval.trap.shutdown()

length(subset(ts.db.14, !is.na(Proportion))$Proportion)
## [1] 141

ggplot(ts.db.14,aes(Edges,log(Proportion))) + geom_point(aes(size=MeanPopBeforeMD))
ggplot(ts.db.14,aes(Edges,MeanPopBeforeMD)) + geom_point(size = 5)

ts.db.21 = eval.trap.shutdown(bin = 21)

length(subset(ts.db.21, !is.na(Proportion))$Proportion)
##[1] 147

ggplot(ts.db.21,aes(Edges,log(Proportion))) + geom_point(aes(size=MeanPopBeforeMD))
ggplot(ts.db.21,aes(Edges,MeanPopBeforeMD)) + geom_point(size = 5)

ddply(ts.db.14,.(Edges),summarize, mean = mean(Proportion))
ddply(ts.db.21,.(Edges),summarize, mean = mean(Proportion))

ddply(ts.db.14,.(Edges),summarize, mean = mean(MeanPopBeforeMD))
ddply(ts.db.21,.(Edges),summarize, mean = mean(MeanPopBeforeMD))

mean(ts.db.14$Proportion, na.rm = TRUE)
mean(ts.db.21$Proportion, na.rm = TRUE)

## I suspect that the degree of trap shutdown is determined by the number
## of insects present before the spray. Since more Edges -> higher MeanPreSpray,
## it looks like more edges -> better shutdown.

ddply(ts.db.14,.(Year),summarize,mean=mean(Proportion,na.rm=TRUE))



ddply(ts.db.14,.(Year),summarize,mean=mean(MeanPopBeforeMD,na.rm=TRUE))


## The 2013 Plots of Male counts under Conv and MDonly are telling:
plot1 = ggplot(subset(c,Trtmnt %in% c("Conv","Control") & Year == 2013),aes(x=DayOfYear,y=Males))+geom_point(size=5) + geom_vline(xintercept = unique(subset(p.n.p.intrepid, Year == 2013)$DayOfYear), colour = "red", lintype = "longdash")

plot4 = ggplot(subset(c,Trtmnt %ni% c("Conv","Control","1CMD","2CMD","100%C","50%C") & Year == 2013),aes(x=DayOfYear,y=Males))+geom_point(size=5) + geom_vline(xintercept = unique(subset(puf.db, Treatment %ni% c("1CMD","2CMD","100%C","50%C") & Year == 2013)$DayOfYear), colour = "blue", linetype = "longdash")

## the traps used in 2013 are MUCH more effective at catching insects, so long as
## there is no MD. Introduce MD and they are the least effective.

ts.db.14$Treatment2 <- sapply(ts.db.14$Treatment,function(x){

    if(is.na(x)){return(x)}
    if(as.character(x) %in% c("EMD","LMD","2MD","1MD","100%MD","50%MD") ) { return("MD") }
    else{ return("CMD") }
})

ts.db.14$Treatment3 <- sapply(ts.db.14$Treatment,function(x){

    if( is.na(x) ) { return(NA) }
    if ( grepl("50%",x) ){ return( "50%" ) }
    else { return( "100%" ) }

})

m.ts = gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs="fs") + Rate + Edges + Treatment2, data = ts.db.14, family = "gaussian")

ggplot(ts.db.14,aes(Treatment2,log(Proportion))) + geom_point(aes(size = MeanPopBeforeMD))

ggplot(ts.db.14,aes(Rate,log(Proportion))) + geom_point(aes(size = MeanPopBeforeMD))

rate = sapply(ts.db.14$Rate,function(x){
    if ( x %ni% c(1,2) ) {
        
        return(2)
    }else{return(x)}
})

ddply(ts.db.14,.(Rate),summarize,mean=mean(Proportion, na.rm = TRUE))

## Rate Matters!! wait, what?

## Before change     ## What change?
## Family: gaussian 
## Link function: identity 

## Formula:
## log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + 
##     Rate + Edges + Treatment2



## after change
##  summary(m.ts)

## Family: gaussian 
## Link function: identity 

## Formula:
## log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + 
##     Rate + Edges + Treatment2




##   12/2/14   ##

## Looking at trap shutdown

## Before I forget, I want to look at:
## - Year effects (different trap types)
## - Edge effects (expect MeanPopBeforeMD to account for these)
## - Rate:Percentage effects
## - Does MeanPopBefore MD need to be smooth?

## (unrelated to trap shutdown)
## - Look at the other pesticides in males-DayOfSeason Plots
## - Look at female and egg traps
## - reference:

ddply(p.n,.(Product..Agrian.),summarize, number.of.sprays = length(DayOfYear) )


p.n.other <- subset(p.n, Product..Agrian. %ni% c("Puffer NOW", "Intrepid 2F"))

## I've add a Site column to c.

plots <- lapply( unique(p.n.other$Site), function(site) { Make.Site.Plot(site, "other") })

plot1e = ggplot(subset(c,Trtmnt %in% c("Conv","Control")),aes(x=DayOfYear,y=Eggs))+geom_point(size=5) + geom_vline(xintercept = unique(p.n.p.intrepid$DayOfYear), colour = "red", lintype = "longdash")

plot4e = ggplot(subset(c,Trtmnt %ni% c("Conv","Control","1CMD","2CMD","100%C","50%C")),aes(x=DayOfYear,y=Eggs))+geom_point(size=5) + geom_vline(xintercept = unique(subset(puf.db, Treatment %ni% c("1CMD","2CMD","100%C","50%C"))$DayOfYear), colour = "blue", linetype = "longdash")

plot3e = ggplot(subset(c,Trtmnt %in% c("1CMD","2CMD","100%C","50%C")),aes(x=DayOfYear,y=Eggs))+geom_point(size=5) + geom_vline(xintercept = unique(subset(puf.db, Treatment %in% c("1CMD","2CMD","100%C","50%C"))$DayOfYear), colour = "blue", linetype = "longdash") + geom_vline(xintercept = unique(p.n.md.intrepid$DayOfYear), colour = "red", lintype = "longdash")

plot2e = ggplot(subset(c,Trtmnt %ni% c("Conv","Control")),aes(x=DayOfYear,y=Eggs))+geom_point(size=5) + geom_vline(xintercept = unique(puf.db$DayOfYear), colour = "blue", linetype = "longdash") + geom_vline(xintercept = unique(p.n.md.intrepid$DayOfYear), colour = "red", lintype = "longdash")

av.males.by.doy <- ddply(subset(c, Trtmnt %in% c("Conv", "Control")),.(DayOfYear),summarize,mean = mean(Males, na.rm = TRUE))

###########
###########
## TRAP SHUTDOWN:

## Starting model:
m.ts <- gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + Rate:Treatment3 + Edges, family = "gaussian", data = ts.db.14)

summary(m.ts)

## Family: gaussian 
## Link function: identity 

## Formula:
## log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + 
##     Rate:Treatment3 + Edges

## Parametric coefficients:
##                      Estimate Std. Error t value Pr(>|t|)  
## (Intercept)          0.004737   2.517862   0.002   0.9985  
## Edges               -0.006156   0.086386  -0.071   0.9433  
## Rate:Treatment3100% -0.317533   0.189904  -1.672   0.0973 .
## Rate:Treatment350%  -0.152413   0.225105  -0.677   0.4998  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Approximate significance of smooth terms:
##                                      edf Ref.df     F p-value    
## s(MeanPopBeforeMD,as.factor(Year)) 26.74     45 11.09  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## R-sq.(adj) =  0.786   Deviance explained = 83.1%
## GCV score = 0.55797  Scale est. = 0.43634   n = 141

ggplot(ts.db.14,aes(Ranch,log(Proportion))) + geom_point(aes(size = MeanPopBeforeMD))


## How the interation of rate:treamtent relate to the mean proportion:
ddply(ts.db.14,.(Rate,Treatment3),summarize, mean = mean(Proportion,na.rm=TRUE))
##   Rate Treatment3       mean
## 1    1       100% 0.14108696
## 2    2       100% 0.07375758
## 3    2        50% 0.09815789
## 4    2       <NA>        NaN

## How the number of edges relate to the mean proportion:
ddply(ts.db.14,.(Edges), summarize, mean = mean(Proportion, na.rm = TRUE))
##   Edges       mean
## 1     0 0.11788889
## 2     1 0.09623377
## 3     2 0.05178788
## 4     3 0.02750000
## 5    NA        NaN

## How edges relate to mean before md:
ddply(ts.db.14,.(Edges), summarize, mean = mean(MeanPopBeforeMD, na.rm = TRUE))
##   Edges      mean
## 1     0  7.398148
## 2     1  9.157251
## 3     2 12.481061
## 4     3  4.187500
## 5    NA       NaN

m.ts.1 <- gam(log(Proportion) ~ MeanPopBeforeMD:Year + Rate:Treatment3 + Edges + Year, family = "gaussian", data = ts.db.14)

m.ts.2 <- gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + Rate:Treatment3 + Edges + Year, family = "gaussian", data = ts.db.14)

m.ts.3 <- gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + s(DayOfYear, as.factor(Year), bs = "fs") + Rate:Treatment3 + Edges + Year, family = "gaussian", data = ts.db.14)

m.ts.4 <- gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + Rate:Treatment3 + Edges + Year, family = "gaussian", data = ts.db.14)

m.ts.5 <- gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + Rate:Treatment3 + Edges + as.factor(Year), family = "gaussian", data = ts.db.14)

m.ts.6 <- gam(log(Proportion) ~ s(MeanPopBeforeMD) + Rate:Treatment3 + Edges + as.factor(Year), family = "gaussian", data = ts.db.14)

m.ts.7 <- gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + Treatment1:Rate + Edges, family = "gaussian", data = ts.db.14) ## ***


## 12/4/14 ##

## MELT! from library(reshape)!! so awesome.

temp <- melt(c[,c(27,10,11,12)], id = c("DayOfYear"))
ggplot(temp,aes(DayOfYear,value,colour = variable)) + geom_point(size = 5, alpha = .8)
ggplot(temp,aes(DayOfYear,value,colour = variable, group = variable)) + stat_smooth()


plots <- lapply( unique(p.n.other$Site), function(site) { Make.Site.Plot(site, "other") })

plots <- lapply( unique(puf.db$Site), function(site) { Make.Site.Plot(site, "puffer", "m") })

temp <- do.call(arrangeGrob, plots)
ggsave(filename = "MalesUnderMD.pdf", temp)

plot4 = ggplot(subset(c,Trtmnt %ni% c("Conv","Control","1CMD","2CMD","100%C","50%C")),aes(x=DayOfYear,y=Males,colour = Year, group = Year))+stat_smooth() + geom_vline(aes( xintercept = DayOfYear, colour = as.factor(Year), linetype = "longdash"), subset(puf.db, Treatment %ni% c("1CMD","2CMD","100%C","50%C")), show_guide = TRUE)

plots <- lapply(unique(puf.db$Year), function(year) {
   plot <- ggplot(subset(c,Trtmnt %ni% c("Conv","Control","1CMD","2CMD","100%C","50%C") & Year == year),aes(x=DayOfYear,y=Males))+stat_smooth() + geom_vline(xintercept = unique(subset(puf.db, Treatment %ni% c("1CMD","2CMD","100%C","50%C") & Year == year)$DayOfYear), colour = "red", linetype = "longdash")
   return(plot)
})

## 12/5/14 ##

## just working on the reports today.

## I'll need a few things:


gam(log(Proportion) ~ s(DayOfYear, as.factor(Year), bs = "fs") + s(MeanPopBeforeSpray, as.factor(Year), bs = "fs") + Edges + PriorSprays + TotalPriorSprays, data = ef.dat)

m1 = gam(log(Proportion) ~ s(DayOfYear, as.factor(Year), bs = "fs") + s(MeanPopBeforeSpray, as.factor(Year)) + Edges + TotalPriorSprays + PriorSprays,data=ef.dat,family="gaussian")

m1a = gam(log(Proportion) ~   s(DayOfYear, as.factor(Year), bs = "fs") + Edges + TotalPriorSprays + PriorSprays,data=ef.dat,weights=MeanPopBeforeSpray,family="gaussian")


m.ts.7 <- gam(Proportion ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + Treatment1:Rate + Edges, family = "gaussian", data = ts.db) ## ***

ts.db.1$PufPerAcre <- sapply(ts.db.1$Rate,function(x){
    if ( x %ni% c(1,2) ) {
        
        return(2)
    }else{return(x)}
})

ggplot(ts.db,aes(MeanPopBeforeMD,Proportion, group = as.factor(Year), colour = as.factor(Year))) + stat_smooth()


m.ts.7 <- gam( log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + RateOfRelease + PufPerAcre + Edges, family = "gaussian", data = ts.db.e) ## ***

m.ts.6 <- gam( log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Treatment2), bs = "fs") + as.factor(Year) + Treatment1 + Rate + Edges + Treatment2, family = "gaussian", data = ts.db) ## ***

m.ts.6a <- gam( log(Proportion) ~ s(MeanPopBeforeMD) + as.factor(Year) + Treatment1 + Rate + Edges + Treatment2, family = "gaussian", data = ts.db) ## ***

m.ts.6b <- gam( log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Treatment2), bs = "fs") + as.factor(Year) + Treatment1 + Rate + Edges, family = "gaussian", data = ts.db) ## ***

m.ts.5 <- gam( log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs") + Treatment1 + Rate + Edges, family = "gaussian", data = ts.db.e) ## ***



TimeOfMD = sapply(ts.db$DayOfYear,function(x){
    if( x < 130 ) { return("E") }
    if( x > 130 ) { return("L") }
    if( is.na(x) ) {return( x )}
}
   )

## 12/8/14  ##

test <- gam(log(Proportion) ~ DayOfYear, data = ts.db)
m.ts.resid <- glmer( residuals(test) ~ as.factor(Year) + (1|Block.s.) )

loc <- sapply(ts.db$Ranch,function(ranch){

    if( ranch %in% c("371", "370") ){ return("SF") }
    if( ranch %in% c("344", "345", "346", "439") ) {return("LH") }
    else{ return(NA) }
    
})


test <- gam(log(Proportion) ~ DayOfYear, data = ts.db)
m.ts.resid <- glmer( residuals(test) ~ as.factor(Year) + (1|Loc/Block.s.)  + Treatment1 + Rate + Edges, data = subset(ts.db, !is.na(Proportion)))




test <- gam(log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs = "fs"), data = ts.db)

m.ts.8 <- gam( residuals(test) ~ as.factor(Year) + Edges + Treatment1 + Rate, data = subset(ts.db, !is.na(Proportion)))

m.ts.7 <- gam( log(Proportion) ~ s(MeanPopBeforeMD, as.factor(Year), bs="fs") + Treatment1 + Rate + Edges, family = "gaussian", data = ts.db) ## ***

m.ts.e <- gam( log(Proportion) ~ s(DayOfYear, as.factor(Year), bs = "fs") + s(MeanPopBeforeMD) + Edges + Treatment1 + Rate, data = ts.db.e)

plots <- lapply(unique(puf.db$Site), function(site){ Make.Site.Plot2(site) })

## BINOMIAL MODEL ##

ts.db.binom <- subset(ts.db.1, Proportion =< 1)

m.ts.binom <- gam( cbind(MeanPopAfterMD,MeanPopBeforeMD-MeanPopAfterMD) ~ s(DayOfYear) + as.factor(Year) + PufPerAcre + RateOfRelease + Edges, family = "binomial", data = ts.db.binom)

m.ts.binom.2 <- glm( cbind(MeanPopAfterMD,MeanPopBeforeMD-MeanPopAfterMD) ~ as.factor(Year) + PufPerAcre + RateOfRelease + Edges, family = "binomial", data = ts.db.binom)


## 12/9/14 ##

ddply(ts.db.binom, .(Edges), summarize, MeanProportion = mean(Proportion, na.rm = TRUE))

ddply(ts.db, .(Edges), summarize, MeanProportion = mean(Proportion, na.rm = TRUE))

m.ts.test1 <- gam(cbind(MeanPopAfterMD, MeanPopBeforeMD - MeanPopAfterMD) ~ s(DayOfYear) + as.factor(Year) + RateOfRelease + PufPerAcre + Edges, family = "binomial", data = ts.db.binom)

m.ts.test2 <- gam(cbind(MeanPopAfterMD, MeanPopBeforeMD - MeanPopAfterMD) ~ as.factor(Year) +  as.factor(PufPerAcre):RateOfRelease + Edges, family = "binomial", data = ts.db.binom)

m.ts.test2 <- gam(cbind(MeanPopAfterMD, MeanPopBeforeMD - MeanPopAfterMD) ~ s(DayOfYear) + as.factor(Year) +  as.factor(PufPerAcre) + RateOfRelease + Edges, family = "binomial", data = ts.db.binom)

m.ts.test3 <- gam(cbind(MeanPopAfterMD, MeanPopBeforeMD - MeanPopAfterMD) ~  as.factor(Year) + as.factor(PufPerAcre) + RateOfRelease + as.factor(PufPerAcre):RateOfRelease + Edges, family = "binomial", data = ts.db.binom)

m.ts.test4 <- gam(cbind(MeanPopAfterMD, MeanPopBeforeMD - MeanPopAfterMD) ~ s(DayOfYear) + as.factor(Year):as.factor(PufPerAcre):RateOfRelease + Edges, family = "binomial", data = ts.db.binom)

## Let's start looking at damage

sites <- sapply(1:dim(damage.df)[1],function(i){

    site <- GetSite(damage.df[i,]$Block,damage.df[i,]$Ranch)
    return(site)

})

blocks.ranches <- apply(damage.df, 1, function(row) {
    block = row[9]
    ranch = row[8]
    return(paste0(block,"/",ranch))
}) 

blocks.ranches <- unique(blocks.ranches)

sites <- sapply(blocks.ranches, function(pair){
    block.ranch <- unlist(strsplit(pair, "/"))
    site <- GetSite(block.ranch[1],block.ranch[2])
    return(site)
})



## 12/10/14 ##

## expand.grid(), nnet

## Jay suggested looking at which of Males, Females, Eggs is the best predictor for
## damage. To do this we'll need:

## - a function that takes a ranch/block/date and gives out the average
## densities of Male,Female,Egg during predetermined periods throughout the
## focal year.

## given a ranch/block/date:



## put that on hold there's some other business to attend to:



models <- llply(seq(5,15,by=1), function(x){

    d.f <- subset(ts.db, MeanPopBeforeMD > x)
    model <- failwith(NULL,gam)(log(ProportionAdj) ~  s(MeanPopBeforeMD) + as.factor(Year)  + RateOfRelease + PufPerAcre + Edges, data = d.f)
    return(model)
})

sapply(seq(10,30,by=10),function(x){ dim(subset(ts.db, MeanPopBeforeMD > x))[1]})

## Jay was concerned that our samples were not independant - i.e. our binomial
## was broken. He directed me to Matt's report to see solutions:

ts.db <- eval.trap.shutdown()
ts.db.trap <- eval.trap.shutdown.by.trap()

ts.db.binom <- subset(ts.db, ProportionRaw <= 1)
ts.db.trap.binom <- subset(ts.db.trap, ProportionRaw <= 1)

m.ts.binom1 <- glmer(cbind(MeanPopAfterMD,MeanPopBeforeMD-MeanPopAfterMD) ~ as.factor(Year) + PufPerAcre + RateOfRelease + Edges + (1|Block.s.), family = "binomial", data = ts.db.binom)

m.ts.binom2 <- glmer(cbind(MeanPopAfterMD,MeanPopBeforeMD-MeanPopAfterMD) ~ as.factor(Year) + PufPerAcre + RateOfRelease + Edges +(1|SampleID), family = "binomial", data = ts.db.trap.binom)

######################

ts.db.10 <- subset(ts.db, MeanPopBeforeMD >= 10)

tableEdge <- ddply(ts.db.10, .(Edges), summarize, MeanProportion = mean(ProportionRaw, na.rm = TRUE), StdDev = sd(ProportionRaw, na.rm = TRUE), NumSamples = length(subset(ProportionRaw, !is.na(ProportionRaw))))

tableYear <- ddply(ts.db.10, .(Year), summarize, MeanProportion = mean(ProportionRaw, na.rm = TRUE), StdDev = sd(ProportionRaw, na.rm = TRUE), NumSamples = length(subset(ProportionRaw, !is.na(ProportionRaw))))

tableRate <- ddply(ts.db.10, .(RateOfRelease), summarize, MeanProportion = mean(ProportionRaw, na.rm = TRUE), StdDev = sd(ProportionRaw, na.rm = TRUE), NumSamples = length(subset(ProportionRaw, !is.na(ProportionRaw))))

tablePuf <-  ddply(ts.db.10, .(PufPerAcre), summarize, MeanProportion = mean(ProportionRaw, na.rm = TRUE), StdDev = sd(ProportionRaw, na.rm = TRUE), NumSamples = length(subset(ProportionRaw, !is.na(ProportionRaw))))


m.ts.binom <- glmer(cbind(MeanPopAfterMD,MeanPopBeforeMD-MeanPopAfterMD) ~ as.factor(Year) + PufPerAcre + RateOfRelease + Edges +(1|Trap), family = "binomial", data = ts.db.trap.binom)


m.ts.binom <- glmer(cbind(MeanPopAfterMD,MeanPopBeforeMD-MeanPopAfterMD) ~ as.factor(Year) + PufPerAcre + RateOfRelease + Edges + (1|Trap), family = "binomial", data = subset(ts.db.trap, MeanPopBeforeMD >= 10) )


## 12/12/14 ##

## test <- ddply(damage.df,.(Year,Ranch,Block), transform,
##               Tot.Nuts.Block = sum(Tot_Nuts, na.rm = TRUE),
##               Tot.Inf.Now.Block = sum(Tot



## 12/15/14 ##

## great. this segments seasonal trap counts (M,F, & E) into bins:

seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 10)

d.o.y = sapply(damage.df$Date,function(x){
    strptime(as.character(x),"%m/%d/%Y")$yday + 1
})

## now combine damage.df with seas.bins:

## 12/16/14 ##

## Model structure:

## for each treatment:

## for each type (m,f,e):

## (potentially for each bin size:)

## response: binom( nuts damaged, nuts not damaged)
## variables: (1|SampleID) + Bin1...BinN + (1|Year) + (1|Block/Ranch) +
##            loc + Edge/Interior

## lets just do conventional for now:

## first we merge the data frames:

damage.counts <- merge(damage.df, seas.bins, c("Year","Ranch", "Block"))

## let's break the treatments into proper parts:

##tmnt.for.damage <- dlply(p.n, .(Year, Site, Block.s.), BuildTreatment)

## the above doesn't work because there is overlap between Block.s. values
## so I need to split

p.n.by.block <- do.call(rbind, lapply( 1:dim(p.n)[1], function(i){

    row = p.n[i,]
    blocks = row$Block.s.
    if (blocks == " "){
        blocks = names(dict[[as.character(row$Site)]])
    } else {
        blocks = unlist(strsplit(blocks, ","))
    }

    rows <- do.call(rbind, lapply(blocks, function(block){
        new.row <- row
        new.row$Block.s. <- block
        return(new.row)
    }))

    return(rows)
}))

                                  
tmnt.for.damage <- ddply(p.n.by.block, .(Year, Site, Block.s.), BuildTreatment)

## now I have damage.counts & tmnt.for.damage - I think that's all I need.
damage.df <- merge( damage.df, tmnt.for.damage,  c("Year", "Site", "Block"))

test <- merge(damage.counts, tmnt.for.damage, c("Year", "Site", "Block"))

## ok it's all in damage.counts

## lets do a test with just males and just conventional:

test <- subset(damage.counts, Tmnt == "C")

model.m <- glmer( cbind(DmgNOW, Tot_Nuts - DmgNOW) ~ (1|ID) + (1|Year) + (1|Ranch/Block) + loc + M1 + M2 + M3 + M4 + M5 + M6, data = test, family = "binomial")

model.f <- glmer( cbind(DmgNOW, Tot_Nuts - DmgNOW) ~ (1|ID) + (1|Year) + (1|Ranch/Block) + loc + F1 + F2 + F3 + F4 + F5 + F6, data = test, family = "binomial")

model.e <- glmer( cbind(DmgNOW, Tot_Nuts - DmgNOW) ~ (1|ID) + (1|Year) + (1|Ranch/Block) + loc + E1 + E2 + E3 + E4 + E5 + E6, data = test, family = "binomial")


## 12/17/14 ##

## now for all the bins, treatments, etc:

## initial list for grid
val.grid <- list( trtmnt = unique(damage.df$Tmnt), bins = c(2, 5, 10), type = c("M", "F", "E"))

## more thorough grid for list
val.grid <- list( trtmnt = unique(damage.df$Tmnt), bins = seq(2, 20, by = 2), type = c("M", "F", "E"))

## 12/18/14 ##


val.grid <- expand.grid(val.grid)

results <- mdply(val.grid, RunTrialWithOpts, .progress = "text")

results <- mdply(val.grid, failwith(NA, RunTrialWithOpts), .progress = "text")

## lets look at some of the models individually:

val.grid <- list( trtmnt = unique(damage.df$Tmnt), bins = c(10), type = c("M", "F", "E"))

val.grid <- expand.grid(val.grid)

models <- mlply(val.grid, failwith(NA, RunTrialWithOpts), mod = TRUE, .progress = "text")

ggplot(results, aes(x = bins, y = AIC)) + geom_point(size = 3) + facet_grid(trtmnt ~ type)

## ok... I'm interested in seeing what Jay says about the results.

## 12/19/14 ##

val.grid <- list( trtmnt = unique(damage.df$Tmnt), bins = seq(2, 20, by = 2), type = c("M", "F", "E"))
val.grid <- expand.grid(val.grid)
results <- mdply(val.grid, failwith(NA, RunTrialWithOpts), resp = "ID", .progress = "text")


## 12/22/14 ##

ggplot(damage.df, aes(x = Year, y = (DmgNOW/InfNOW))) + geom_point(size = 2) + facet_wrap( ~ Site)

ddply(damage.df, .(Plot), summarize, perc = mean(DmgNOW/InfNOW, na.rm = TRUE))

## Boosting (for all blocks):
library(gbm)
test <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 4)
test <- merge(damage.df, test, c("Year", "Ranch", "Block"))

train <- sample(1:nrow(test), nrow(test) / 2)
boost.dmg <- gbm( DmgNOW ~ M1 + M2 + M3 + M4  + F1 + F2 + F3 + F4 + E1 + E2 + E3 + E4 + as.factor(Year) + as.factor(loc) + as.factor(Site), data = test[train,], distribution = "poisson", n.trees = 5000, interaction.depth = 4)

yhat.boost <- predict(boost.dmg, newdata = test[-train,], n.trees = 5000)
mean(( yhat.boost - boost.dmg)^2)


## Random Forest (for all blocks)

library(randomForest)
rf.dmg <- randomForest(DmgNOW ~ M1 + M2 + M3 + M4  + F1 + F2 + F3 + F4 + E1 + E2 + E3 + E4, data = test, mtry = 3, importance = TRUE)

## ok, now a litte more sophistication?
## what do I want? I think going into the site level will be interesting. I don't
## know why Year, loc, Site don't work with the random forest function... Can I
## use matt's model, but mess around with the population variable he used? further-
## more, is my binning strategy silly? Jay didn't think so, but matt apparently has
## problems with it. So let's go one step at a time:

## I want to apply a (boost, rf) model to each Tmnt/Year/Site, combination, output
## the variable's' importances into a new df and display them nicely.

results <- ddply(test, .(Tmnt,Year,Site), failwith(NA,RunBoost), .progress = "test")
results <- ddply(test, .(Tmnt,Year,Site), failwith(NA,RunForest), .progress = "test")

## then I want to generate a grid of trap-types and bin-schemes and apply matt's
## model with each of them. Then I can output the aic and the correlation of
## actual vs predicted. That sounds good for now.


## 12/29/14 ##

## I'm going to take a step back because I'm feeling a little over whelmed, plus I do
## not appear to have the complete data set (missing tree age...). I need to
## reorganize my thoughts. (1) I'd like to look at which bin (8 of 10? 4 of 10?)
## produces the lowest AIC when used in the model. I am just interested. Then, what?
## We wand to develop a profile of recommended management practices when a grower
## is using MD. So the question I'm looking at is: is one trap-type, on average, a
## better predictor of damage than the others? SO, questions we can ask: (2) First
## (as mentioned above) is there a period in the season which is MOST predictive of
## damage? What is this period (if it exists) for each trap type? Then, (3) can we
## compare predictive-efficacy across trap-types using the optimal period?

## (1); Outline:

## we want to split on Tmnt. Then, drawing on a list of the possible periods,
## regress on each split, varying the model by the current period. Output the AIC.
## the resulting dataframe should have a column for each possible period that
## contains the AIC of the resulting model. RunTrialWithOpts already has nearly
## everything I need. Since I won't be varying the number of bins, this should run
## much faster.

val.grid <- list(c("M","F", "E"), c("EMD","ECMD","C","LMD","LCMD"), c(1:10))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 10)
## we can gradually adjust the number of bins

results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .progress = "text")

ddply(damage.df, .(Tmnt), summarize, num = length(unique(Block)))
ddply(damage.df, .(Tmnt2), summarize, num = length(unique(Block)))

Tmnt <- sapply(1:nrow(damage.df), function(i){

    row <- damage.df[i,]

    if( row$Treatment %in% c("Conv", "Control_Conv", "Control")) {
        return("C")
    } else {

        if( row$Treatment %in% c("2CMD", "1CMD", "100%C", "50%C") ){

            tmnt <- "CMD"
            
        } else {
            tmnt <- "MD"
        }

    }

    puf.subset <- subset(puf.db,
                         Block.s. == row$Block &
                         Ranch == row$Ranch &
                         Year == row$Year)

    if(nrow(puf.subset) > 0){

        dates = unique(puf.subset$DayOfYear)

        if( max(dates) > 119 ){
            tmnt <- paste0("L", tmnt)
        } else {
            tmnt <- paste0("E", tmnt)
        }
        
    }

    return(tmnt)
    
})


## 12/30/14 ##

val.grid <- list(c("M","F", "E"), c("EMD","ECMD","C","LMD","LCMD"), c(1:3))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 3)
results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .progress = "text")
ggplot(results, aes(x = bin, y = V1)) + geom_point( size = 5)  + facet_grid(trtmnt ~ type, scales = "free")
## I'd like to look at these AIC values with a boot:

results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), boot.bool = TRUE, .progress = "text", .parallel = TRUE)

## 1/2/15 ##

## I just realized that trying to determine the variance of AIC with a Bootstrap
## will return completely nonsensical results. I think it will still be useful to
## explore AWS EC2, but perhaps not at this moment.

## instead, let's continue to look at the bins:
                   
val.grid <- list(c("M","F", "E"), c("EMD","ECMD","C","LMD","LCMD"), c(1:10))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 10)
## Check it: parallel.
results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .parallel = TRUE)
ggplot(results, aes(x = bin, y = V1)) + geom_point( size = 2)  + facet_grid(trtmnt ~ type, scales = "free")

## For .parallel:

## library(plyr) 
## library(doMC) 
## registerDoMC(cores=2) 

## results <- mdply(val.grid, .fun = failwith(NA, RunTrialWithOpts2), .parallel = TRUE)

## Compare with system.time if you like.

test <- subset(results, Tmnt = "C" & V1 > 800)
ggplot(test, aes(x = bin, y = V1)) + geom_point( size = 2)  + facet_grid(trtmnt ~ type, scales = "free")

val.grid <- list(c("M","F", "E"), c("EMD","ECMD","C","LMD","LCMD"), c(1:5))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 5)
results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .progress = "text")
ggplot(results, aes(x = bin, y = COR)) + geom_point( size = 2)  + facet_grid(trtmnt ~ type, scales = "free")

## I'm getting crazy results... correlations between actual and predicted values
## are ~.99 just from the random effects! Is this possible? OK I'm going to
## make sure the damage.df is correct...
ddply(damage.df, .(Year, Site), treatment = unique(Tmnt))

## Arg, well it all looks pretty correct... it just also happens to be missing
## A LOT of stuff.

## 1/5/15 ##

## let's look at the AICc of the model without an insect-pop var
## as well!! now bins go from 0-5.

## C 5 ##
val.grid <- list(c("M","F", "E"), c("E","CONV","L"), c(0:5))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 5)
results.c.5 <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), grain = "coarse", .progress = "text")
ggplot(results.c.5, aes(x = bin, y = AICc)) + geom_point( size = 2)  + facet_grid(trtmnt ~ type, scale = "free")

## F 5 ##
val.grid <- list(c("M","F", "E"), c("EMD","ECMD","CONV","LMD","LCMD"), c(0:5))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 5)
results.f.5 <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .progress = "text")
ggplot(results.f.5, aes(x = bin, y = AICc)) + geom_point( size = 2)  + facet_grid(trtmnt ~ type, scale = "free")

## F 2 ##
val.grid <- list(c("M","F", "E"), c("EMD","ECMD","CONV","LMD","LCMD"), c(0:2))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 2)
results.f.2 <- mdply(val.grid, .fun = failwith(NA, RunTrialWithOpts2), .progress = "text")
ggplot(results.f.2, aes(x = bin, y = AICc)) + geom_point( size = 2)  + facet_grid(trtmnt ~ type, scale = "free")

## C 2 ##
val.grid <- list(c("M","F", "E"), c("E","CONV","L"), c(0:2))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 2)
results.c.2 <- mdply(val.grid, .fun = failwith(NA, RunTrialWithOpts2), grain = "coarse", .parallel = TRUE)
gplot(results.c.2, aes(x = bin, y = AICc)) + geom_point( size = 2)  + facet_grid(trtmnt ~ type, scale = "free")


##  1/6/15 ##

## Recap: I've saved AICc images as AICcBy[2/5]Bin_[Coarse/Fine].pdf.
## These are interesting - do they make any sense? If the AICc is
## robust and I haven't made any grievous (coding) errors, then I suppose so.
## In any case, I feel a bit like I'm using a black box, which makes me VERY WARY.
## Wariness aside, the natural thing to do now is to compare the 'best' bins
## across the 5/2 categories:

## Switching over to Matt's damage df because mine is fouled up:

ranch <- sapply(dmg$Ranch, function(x){
    if (identical(x, NA)) { return(x) } else { return(gsub("0$","",x)) }
})
dmg$Ranch <- ranch

block <- gsub("[.]", "-", dmg$Block)
dmg$Block <- block

## some sort of error:
block <- gsub("11-3", "11-4", dmg$Block)

blocks.ranches <- dmg[,c('Block','Ranch')]
blocks.ranches <- unique(blocks.ranches)

sites <- mdply(blocks.ranches, GetSite)
colnames(sites) <- c("Block", "Ranch", "Site")
test <- merge(dmg, sites, c("Ranch", "Block"))
dmg <- test

## now my pred_analysis stuff is ready
save(dmg, dict, p, p.n, p.n.by.block, c, file = "predictor_analysis.rda")

## tomorrow re-run [c/f] [2/5] ...

## 1/7/15 ## 

#####################
## Av AICc summarize:

ddply(results.c.2, .(type), summarize, av = mean(AICc, na.rm = TRUE))


ddply(results.c.5, .(type), summarize, av = mean(AICc, na.rm = TRUE))

ddply(results.f.5, .(type), summarize, av = mean(AICc, na.rm = TRUE))


ddply(results.f.2, .(type), summarize, av = mean(AICc, na.rm = TRUE))


#########################
## Best AICc f.e. trtmnt:

ddply(results.c.5, .(trtmnt), summarize, minAICc = min(AICc), type = type[which(AICc == min(AICc))])


ddply(results.f.5, .(trtmnt), summarize, minAICc = min(AICc), type = type[which(AICc == min(AICc))])


ddply(results.f.2, .(trtmnt), summarize, minAICc = min(AICc), type = type[which(AICc == min(AICc))])


ddply(results.c.2, .(trtmnt), summarize, minAICc = min(AICc), type = type[which(AICc == min(AICc))])


## 1/9/15 ##
"/Library/Frameworks/R.framework/Versions/3.1/Resources/library"

## plyr told me this:
## The downloaded source packages are in:
## '/private/var/folders/qp/7c56lr_s1yd9fglpvst2lz640000gn/T/RtmpCsJcbK/downloaded_packages'

## oh god, I'm trying the bootstrap again... does it make sense? IDK

val.grid <- list(c("M","F", "E"), c("EMD","ECMD","CONV","LMD","LCMD"), c(0:1))
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 1)
results.f.5 <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), boot.bool = TRUE, .progress = "text")

## Struck me: the bootstrapped dfs need to be shared across treatments (otherwise
## the comparison doesn't exist). I think the best way to do this will be to
## precompile bootstrapped indicies, then load them within each call of
## RunTrialWithOpts2. This calls for a 5x[#boots] matrix...

## replicate

objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function & grepl("<-",names), objs)
rep.funs <- funs[which(grepl("<-", names(funs)))]
prim.rep.funs <- Filter(is.primitive, rep.funs)

`%XOR%` <- function(A,B){
    setdiff(union(A,B), intersect(A,B))
}

`%\\/%` <-  function(A,B){
    union(A,B)
}

`%/\\%` <- function(A,B){
    intersect(A,B)
}

`%-%` <- function(A,B){
    setdiff(A,B)
}

`replace.random<-` <- function(x, value){
    x[ceiling(runif(1, min = 0, max = length(x)))] <- value
    force(x)
}
## Don't use . in function names - it can cause confusion over what object is
## being used.

## hash library for dictionaries...

## 1-14-15 ##

x <- 10
exists("x", envir = e)

exists("x", envir = e, inherits = FALSE)

## recurse over environments:

search_prac <- function( e = environment()){

    if(identical(e, emptyenv())){
        print(e)
    } else {
        print(e)
        parent = parent.env(e)
        Recurse(parent)
    }
}

## CHECK THIS OUT!

library(pryr)
## active assignment
x %<a-% runif(1)

## delayed assignment
library(pryr)
system.time(b %<d-% {Sys.sleep(1); 1})
#>    user  system elapsed 
#>       0       0       0
system.time(b)

## <<- rebinds a value in the parent environment

## using environments to pass information:
## (set empty envir as parent)
e2 <- new.env(parent = emptyenv())
get("x", envir = e2)


## Back to AICc:


boot_matrix <- llply(na.omit(unique(dmg$trt2)), MakeIndices, num = 10)
names(index_matrix) <- na.omit(unique(dmg$trt2))
val.grid <- list(c("M", "F", "E"), c("EMD", "ECMD", "CONV", "LMD", "LCMD"),
                 0:5, 0:10)
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin", "boot_i")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 5)
results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), 
    .progress = "text") 

## test:

system.time(RunTrialWithOpts2("M", "EMD", 4, 4))
 ##   user  system elapsed 
 ## 12.725   0.022  12.812 

## if each row takes 13 sec, then if I run 40 boot strap calls
## it will take me ~four hours computation time with all of my processors
## ... I guess I'll start with 10 calls (1 hour) and see if it looks interesting
## The next thing to do will be to CV it (data sets will be smaller...) and
## see what pops out of that. Given my MakeIndices function, I can do CV as
## an almost (change rep = TRUE and # of draws) direct application.

## For .parallel:

## library(plyr) 
library(doMC) 
registerDoMC(cores=4) 
results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .parallel = TRUE)
## the original version of the above had errors whenever mat_0 == 0, I think
## I fixed it.

## we can visualize, but it isn't terribly helpful:
ggplot(results, aes(x = bin, y = AIC, color = color)) + geom_point(size = 3) +facet_grid(trtmnt ~ type, scale = "free")

## if we look at the average AIC?
results_summary <-  ddply(results, .(type, trtmnt, bin), summarize, meanAIC = mean(AIC, na.rm = TRUE), varAIC = var(AIC, na.rm = TRUE))
ggplot(results_summary, aes(x = bin, y = meanAIC, group = bin)) + geom_boxplot() + facet_grid(trtmnt ~ type, scale = "free") 

test <- rbind(test,results_0) ## there was some massaging in this step.
ggplot(test, aes(x = bin, y = meanAIC, color = color)) + geom_point(size = 2) + facet_grid(trtmnt ~ type, scale = "free")

## emacs crashed and I lost a bunch of stuff...

## CV:

## test: FoldData:

test <- FoldData(dmg) ## works

## test: RunTrialWithOpts2:
cv_list <- dlply(dmg, .(trt2), FoldData, k = 5, seed = 10)
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 5)
RunTrialWithOpts2("M", "CONV", 4, fold = 3)


val.grid <- list(c("M", "F", "E"), c("EMD", "ECMD", "CONV", "LMD", "LCMD"),
                 0:5, 1:5)
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin", "fold")
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 5)


results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .progress = "text") 

## 1/20/15 ##

## just input gps data. Merging:

gpsTest <- merge(c,gps, by("Ranch", "TrapSite"))

## good stuff is happening:
## I'm making a reproducible example:

## randomizeVector <- function(vector) {

##     if (!is.vector) stop("must be a vector") 

##     len <- length(vector)
##     vals <- unique(vector)

##     if (is.numeric(vector)){ }
##     if (is.character(vector)){}
##     if (is.factor(vector)){}
##     else{}
## }


## tomorrow... see test and train dataframes.x

## 1/21/15 ##
## I'm going to make that repex once I'm really sure I don't know what's going
## on:

## individual testing:
RunTrialWithOpts2("M", "EMD", 2, fold = 3)

cv_list <- dlply(dmg, .(trt2), FoldData, k = 5, seed = 10)
seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = 5)
dmg_sets <-  dlply(dmg, .(trt2), merge, y = seas.bins,
                   by = c("Year", "Ranch", "Block")) 
val.grid <- list(c("M", "F", "E"), c("EMD", "ECMD", "CONV", "LMD", "LCMD"),
                 0:5, 1:5)
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin", "fold")
registerDoMC(cores=4) 
results <- mdply(val.grid, failwith(NA, RunTrialWithOpts2), .progress = "text") 


val.grid <- list(c("M"), c("CONV"), 2, 3)
val.grid <- expand.grid(val.grid)
colnames(val.grid) <- c("type", "trtmnt", "bin", "fold")

## ok so that worked-ish. Now let's see about that repex:
library(lme4)
randomData <- data.frame( "Year" = as.factor(
                              sample(c(2009, 2010, 2011, 2012, 2013), 488,
                              replace = TRUE)),
                         "Block" = sample(c("13-3", "13-4", "14-4", "24-3",
                             "24-4", "1-3", "11-4", "12-3", "12-4", "2-3",
                             "2-4", "13-1", "13-2", "14-1", "24-1", "24-2",
                             "11-1", "11-2", "12-1", "12-2", "25-1", "30-1",
                             "30-2", "29-3", "29-4", "30-3", "30-4", "31-1",
                             "25-4", "31-2", "31-3", "31-4", "36-1", "36-4"),
                             488, replace = TRUE),
                         "Plot" = sample(c("Interior", "Edge"), 488,
                             replace = TRUE),
                         "loc" = sample(c("LH", "SF"), 488, replace = TRUE),
                         "DmgNOW" = sample(1:200, 488, replace = TRUE),
                         "Tot_Nuts" = sample(300:700, 488, replace = TRUE),
                         "Variety" = as.factor(sample(c("N", "F", "C", "M", "W",
                             "B", "P", "Mi", "R", "S"), 488, replace = TRUE)),
                         "tree_age" = sample(10:25, 488, replace = TRUE),
                         "M4" = sample(0:10, 488, replace = TRUE),
                         stringsAsFactors = FALSE)

test <- sample(1:488, 98)
train <- randomData[-test,]
test <- randomData[test,]

response <- "cbind(DmgNOW, Tot_Nuts - DmgNOW) ~ "
predictors <- "(1|Year) + (1|Block) + Plot + as.factor(Variety) + tree_age + loc"
f <- as.formula(paste0(response, predictors))
m <- glmer(f, data = train, family = "binomial")
preds <- predict(m, newdata = test, re.form = NULL, type = "response")


## Ok, I'm going to have to dig deep:
## I'll go one by one and see at which predictor it breaks:
error_data <- RunTrialWithOpts2("M", "EMD", 3, fold = 1)

randomData1 <- data.frame( "Year" = error_data$Year,
                         "Block" = error_data$Block,
                         "Plot" = error_data$Plot,
                         "loc" = error_data$loc,
                         "DmgNOW" = error_data$DmgNOW,
                         "Tot_Nuts" = error_data$Tot_Nuts,
                         "Variety" = sample(c("N", "F", "C", "M", "W",
                              "B", "P", "Mi", "R", "S"), 488, replace = TRUE),
                         "tree_age" = error_data$tree_age,
                         "M3" = error_data$M3, 
                         stringsAsFactors = FALSE)


randomData2 <- data.frame( "Year" = error_data$Year,
                         "Block" = error_data$Block,
                         "Plot" = error_data$Plot,
                         "loc" = error_data$loc,
                         "DmgNOW" = error_data$DmgNOW,
                         "Tot_Nuts" = error_data$Tot_Nuts,
                         "Variety" = error_data$Variety,
                         "tree_age" = error_data$tree_age,
                         "M3" = error_data$M3, 
                         stringsAsFactors = FALSE)


test <- cv_list[["EMD"]][[1]]
test <- sample(1:nrow(randomData2), 98)
train <- randomData2[-test,]
test <- randomData2[test,]

m <- glmer(f, data = train, family = "binomial")
preds <- predict(m, newdata = test, re.form = NULL, type = "response")

## 1/22/15
## FACTORS? I NEEDED TO MAKE THEM FACTORS?!

## ET TU, expand.grid !?

## let's try with 10-fold:
## also, let's see if we can make it parallel


## Ok, so once I cross validate for 'variable utility' I can pick the top performers
## and add them to a model to run on the full data set with trt2 as a factor.
## Then I'd like to try to remove year and see what happens... HAPPENS?!

## the Error in X %*% fixef(object) : non-conformable arguments I've been
## getting is partially due to small samples of certain factors. Sometimes the
## model gets trained on a dataset that does not include all the factors (though
## setting as a factor should deal this this... I need to do so in the original
## data set itself, not in the model...) because they were randomly excluded by
## the folding process. NAILED IT.

practice <- data.frame( A = c("a","b","c"), B = c("m", "n", "o"), C = c(1,2,3))

practice[,c("A","B")] <- lapply(practice[,c("A","B")], as.factor)

dmg[, c("loc", "Plot", "Variety")] <- lapply(dmg[, c("loc", "Plot", "Variety")], as.factor)

## parallel works, cv works. woo!

test1 <- ddply(test1, .(type, trtmnt, bin), transform, varCOR = var(COR, na.rm = TRUE))

test1 <- ddply(test1, .(type, trtmnt, bin), transform, sdCOR = sd(COR, na.rm = TRUE))


ggplot(res, aes(x = bin, y = meanCOR)) + geom_point(size = 2) + facet_grid(trtmnt ~ type) + geom_errorbar(aes(ymax = meanCOR + sdCOR, ymin = meanCOR - sdCOR))

## 1/26/15 ##

res <- testRunTrialWithOpts2(K = 10, bins = 2, parallel = TRUE)

## Just showed Jay the results. He wasn't stoked. I was excited about the tiny
## improvement trap catches appear to contribute. He said they are nowhere enough:
## pesticide applications have simply become so cheap, that unless we can prove
## that it is more efficient to sample insect traps and spray selectively, farmers
## will simply spray more and forget about the insect traps. That is simply the
## more economical decision. Yikes.

## so now I'm going to do a couple of things. I'm going to look at the block effect
## to see if fixed vs random makes a difference to my predictions.

## then I'm going to build a real predictive model, one without a year effect.
## that way we will be able to see how useful trap catch counts are.

## finally I'm going to do run exactly the same experiment I've been doing, but
## this time run  model without trap catch at all, then use the residuals in
## another model where trap catch is the only predictor. I don't know enough
## about statistics to understand the difference, but we'll see.


## 1/28/15 ##
ggplot(dmg_sets[["EMD"]], aes(x = parse(text = "M1"), y = DmgNOW/Tot_Nuts)) + geom_point(size = 2)

binterms <- paste0(c("M", "F", "E"), rep(1:2, each = 3))
vals <- expand.grid(
    list(trt = c("EMD", "CONV", "ECMD", "LMD", "LCMD"), bin = binterms),
    stringsAsFactors = FALSE
    )
colnames(vals) <- c("trt", "bin")

plotstest <- mlply(vals, .fun = DrawPlot)

DrawPlot <- function(trt = NULL, bin = NULL) {

    if (is.null(trt) || is.null(bin)) return(NA)

    p <- ggplot(dmg_sets[[trt]], aes(x = eval(parse(text = bin)),
                                     y = DmgNOW/Tot_Nuts))
    p <- p + geom_point(size = 2)
}

residuals_set <- llply(dmg_sets, .fun = GetResiduals)

## 1/29/15 ##

## I've made some adjustments to RunParametricCV.

## 1/30/15 ##

## The models we've been using have started to show cracks.
## The insect variable doesn't appear to have any predictive
## value against the backdrop of the other predictors...
## What to do? Culprits might be the Year and Block variables.
## But I was under the impression that including them
## was what would allow use to tease out the insect effects in
## first place. So back to exploring models, I suppose...

ggplot(dmg, aes(x = as.factor(Block), y = DmgNOW / Tot_Nuts)) + geom_point(aes(size = Tot_Nuts)) + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works

ggplot(dmg, aes(x = as.factor(Block), y = DmgNOW / Tot_Nuts)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works

## 2/2/15 ##

## BinSeason works...

## teachable moment: don't change your raw data files. Write functions
## that make all the necessary adjustments so that you can start fresh
## instances of your data in every session. DON'T depend on rda's! They
## get messy, and leave vestiges of mistakes in every new session you
## start.

FindMissingInt <- function(v){

    if(!is.numeric(v)) v <- as.numeric(v); warning("v has been altered")

    Identical(1:length(v), names(v))
}

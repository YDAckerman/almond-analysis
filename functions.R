#----- General use -----##

## 10.22.14
`%ni%` <- Negate(`%in%`)

####### 10.1.14 #########

tojd <- function(d, cent = 2000){
    ## require(chron)
     julian.dates <- sapply(d,function(x){ 

         if( !grepl("/",x) ){ return(NA) }
         mdy <- unlist(strsplit(as.character(x),"/"))
         mdy <- as.numeric(mdy)
         if(mdy[3] < 100){ mdy[3] = mdy[3] + cent }
         jd <- julian(mdy[1], mdy[2], mdy[3])
         ## 2009 != 09 !!! 
         return(jd)
     })
    return(julian.dates)
    }  


##------------------------ 10.7.14 ------------------------###


## Update: I spoke with Jay and he pointed out
## a problem with using regression as I have done, and with my analysis
## in general. First, I will need to look at a bunch of plots showing
## the individual effects of sprays on block populations, but his
## reasoning is clear: what we expect to see in such a plot is a
## plateau over some period of time, followed by a severe drop in the
## population (at the time of the spray) proceded by a gradual growth
## back to capacity. One problem with my analysis is that if the
## spray occurred in the middle of a 7 day trap sampling cycle (on the plateau), it
## would appear that no change in the population occurred (I feel like
## pulling counts from 10 days before and after addresses this, but I
## need to check regardless). As for regression, Jay didn't think it was
## the best idea for a number of reasons: the inconsistency of the data
## could distort the slope considerably; the risk of being on a plateau
## could do the same; and ultimately, he says farmers really don't
## think in those terms (fascinating!), but instead in percent change.

## So, obejectives now: look at some plots! write a function that will
## take a line from p_NOW and return a time series of counts before
## and after that date (The work is pretty much done!). Then pick out
## as many rows as you can handle and peruse the outputs!!

##-------------------------------------------------------------------##
##
## @Function shell.4: (removed, see vis.blockpop)

## I'm going to take a line from
## p_NOW, pull out ranch, spray date, and blocks affected. Then I'm
## going to determine the location (l or s) using the ranch, and loop
## through the blocks (I've set up the database so that there is only ever on block
## now, but I'll just keep the functionality around in case I ever want to reuse it
## somewhere). With each block I'll build a pared down version of l or s
## containing count data from within that block that was collected
## within 30 days of the spray date. I'll then plot the counts vs the
## date and hope to see some cool results.


# @Helper-Function shell.4a
# This function returns row values in which sample dates are within 30 days of
# the spray date and which were also taken from the correct block.




##------------------------ 10.13.14 ------------------------##
#
# @Function shell.3 (removed, see eval.efficacy)
#
## This function takes a row from the pesticide database and a method value 'm'. It
## uses the information in that row to find the trap counts within three weeks of that
## particular spray's date. Then, depending on the choice of m ('p' or 't') the
## finds either the proportion change or the t statistic using before and after
#counts. It then outputs that number. 
## 
## A few things of note: trap count dates are included iff they occur at least seven
## days after the spray or one day before the spray. We decided that including any
## counts that occurred within a week of the spray would be hard to justify. Also,
## in the 'p' context, if a count is 0, we replace it with 1/length(opposite count).
## This ensures the ratio has the proper magnitude. 



## @Function proportion
## finds the quotient of the two values entered with some tweaks
## i.e. adjusting means of 0 so that we get an informative (if not exactly
## accurate ? ? ? ? ) result.

proportion.spray <- function(n_before,n_after){
        
    b <- mean(n_before)
    a <- mean(n_after)
        
    proportion <- (a + .1) / (b + .1) ## ad hoc
    p <- round(proportion,3)	
    return(p)
    
}

proportion.md <- function(n_before,n_after){
        
    b <- mean(n_before)
    a <- mean(n_after)
        
    proportion <- (a) / (b) ## ad hoc
    p <- round(proportion,3)	
    return(p)

}


##------------------------ 10.21.14 ------------------------##
    
## @Function shell.5:
## Starting with a spray date, look at the pre-spray insect levels. If they are
## above some significant amount, chart the "amount of time" for their post-spray
## to return to (50%? 100%?) of the pre-spray levels.

## shell.5 <- function(row,sig_amnt=4){

##     row <- unlist(row)
    
##     ranch <- row[1]
##     date.jd <- as.numeric(row[15])
##     block <- row[6]

    
##     ## the indices of rows containing count values within the
##     ## appropriate range:

##     i_before <- which(
##         c$Block == block &
##         c$Ranch == ranch &
##         (date.jd - c$JD <= 14 & date.jd - c$JD >= 1 ) &
##         !is.na(c$Males) &
##         c$TrapType %in% c("Phero","Flight","LurePhero")
##         )
    
##     ## if this vector has length zero, return NA:
##     if(length(i_before)==0){
##         return(NA)
##     }

##     ## otherwise, continue with computation:
##     else{

##         pop_before <- c$Males[i_before]
                            
        
##         ## Determine if mean is greater than some significant amount
##         pre_mean <-  mean(pop_before)
        
##         interval <-  NA
##         if(pre_mean > sig_amnt){
            
##             ## if so
##             bin <- 7
##             interval <- shell.5a(
##                 date.jd,
##                 c[which(c$Ranch==ranch & !is.na(c$Males)),],
##                 sig_amnt,
##                 block,
##                 bin
##                 )
##         }

##         return(interval)
##     }

## }


## @Function shell.5a:
## Report amount of time for population to achieve sig_prcnt.

## shell.5a <- function(j.day, loc, sig_amnt, block, bin){
    
##     ## knowing the year is important:
##     require("chron")
##     year <- month.day.year(j.day)[[3]]
    
##     indices <- which(
##         loc$JD > j.day &
##         loc$Year == year &
##         loc$Block == block
##         )
    
##     day_max <- max(loc$JD[indices])
##     day_min <- min(loc$JD[indices])

##     dates = seq(day_min,day_max, by = bin)
##     intervals = sapply(dates,
##         function(x){

##             day1 = x
##             day2 = x + bin
            
##             i <- which(
##                 day1 <= loc$JD &
##                 loc$JD <= day2 &
##                 loc$Block == block &
##                 loc$TrapType %in% c("Phero","Flight","LurePhero")
##                 )

##             if(length(i)==0){return(NA)}

##             mean <-  mean(loc$Males[i])

##             if(mean >= sig_amnt){return(day2 - j.day)}
##             else{return(day_max-j.day)}
##         })

##     return(min(intervals[!is.na(intervals)]))  

## }

##------------ 10.23.14 -------------##

## @Function blockav.
## function takes bin - a time period (7 days, 14 days, etc)
## and returns a vector of the average number of insects
## per insect-infested block over that period of time, sorted by
## the period when it took place - nice. 

## blockav <- function(bin,phero = "t"){

##     days <- unique(c$JD)[which(!is.na(unique(c$JD)))]
##     days <- sort(days)
    
##     bav <- sapply(days,
##         function(x){
            
##             i <- switch(phero,
##                 "f" = which(
##                     (c$JD - x <= bin & c$JD - x >= 0) &
##                     c$TrapType %in% c("Flight","LurePhero") &
##                     !is.na(c$Males)
##                     ),
##                 "t" = which(
##                     (c$JD - x <= bin & c$JD - x >= 0) &
##                     c$TrapType %in% c("Phero","Flight","LurePhero") &
##                     !is.na(c$Males)
##                     )
##                 )
##             if(length(i)==0){
##                 return(NA)
##             }
##             ## Then I want to know what blocks had measurements:
##             blocks <- c$Block[i]
##             ## and I want to know what the densities were:
##             insects <- c$Males[i]

##             ## Then I'll return the sum of the insects divided
##             ## by the number of blocks

##             return(sum(insects)/length(blocks))
##         })
##     return(bav)
## }

##------------- 10.24.14 --------------##

## @function view_proportion:
## Quick little wrapper to view proportion graph given a threshold
## view_proportion <-  function(t = 3,r = "mean", b = 7){
    
##     prop = apply(d2,1,function(x){shell.3(x,thresh = t,route = r, bin = b)})
##     prop = t(prop)
##     prop = data.frame(prop,d2$Day.Month)
##     colnames(prop) = c("proportion","weight","Day.Month")
##     wi = as.factor(prop$weight)
##     prop$weight = wi
##     prop$Year = d2$Year
##     w = ggplot(prop,aes(Day.Month, log(proportion), color=Year,shape=weight)) + geom_point(size=3)
##     w.lab = w + theme(axis.text.x = element_text(angle = 90, hjust = 0)) ## works
##     w.lab

## }


GetSite <- function(Block, Ranch){

    ## @Function GetSite
    ## @Params
    ##    Block: the Block in question (must be from either SF or LH location)
    ##    Ranch: the Ranch in question ("")
    ##    *note*: the pair (Block, Ranch) uniquely determines a site
    
    if ( Ranch %in% c("Rosed", "SV", "", NA) ) { return(NA) } else {
        Ranch_sites = unique(p.n$Site[which(p.n$Ranch == Ranch)])

        ## What site is this Block in?
        site_bool = sapply(1:length(dict),
            function(y){
                return(Block %in% names(dict[[y]]) & names(dict)[y] %in% Ranch_sites)
            }
            )

        if (length(which(site_bool == TRUE)) == 0) {return(NA)} else {
            site = names(dict)[which(site_bool == TRUE)]
        }

    return(site)
    }
}

## @function vis_blockpop:
## takes a block and spits out a time series of trap counts for each year
## Dependencies: d1 and c
vis_blockpop <- function(index,t="m",year=NA){

    ##require(ggplot2)
    ##require(gridExtra)
    
    ## Gather the unique block/ranch pairs:
    br = apply(c,1,
        function(x){
            return(c(x[4],x[2]))
        })
    br = t(br)
    br = unique(br)

    ## set the current block/ranch by index requested:
    block = br[index,1]
    ranch = br[index,2]
    
    ## What years did this block appear in?
    if (is.na(year)){
        years = c$Year[which(c$Block == block & c$Ranch == ranch)]
        years = sort(unique(years[which(!is.na(years))]))
    } else {
        years = c(year)
    }

    site = GetSite(block, ranch)

    trap.sites = dict[[site]][[block]]
    ##print(paste0("trap sites: ", trap.sites))
    plots <- lapply(years,function(x){

            
        ## What row indices (c) hold insect counts for this block/year?
        ## we have to include block because some of the sites migrate from
        ## year to year (59 & 60)
        i = which(c$Block == block & c$TrapSite %in% trap.sites & c$Year == year) 
        
        c_pop = c[i,]

        puff.data = c_pop[1,c('MD','Puffers','Rep')]
        puff.data = paste("MD: ",puff.data[1], ", Puffers: ",puff.data[2],", Rep :",puff.data[3],collapse="")
        ##print(puff.data)
        
        ## What row indices (d1) hold puffer sprays for this block/year?
        i_pufs = which(
            puf.db$Site == site &
            puf.db$Year == year &
            puf.db$Block.s. == block      
            )

        
        ## What row indices (d1) hold non-puffer sprays for this block/year?
        i_sprays = which(
            p.n$Site == site &
            p.n$Year == year &
            p.n$Product..Agrian. != "Puffer NOW" &
            (grepl(block,p.n$Block.s.) | p.n$Block.s. == " ")      
            )

        ## create plots
        q =  switch(t,
            m = ggplot(c_pop,aes(DayOfYear,Males,color=TrapType,shape=Loc)),
            f = ggplot(c_pop,aes(DayOfYear,Females,color=TrapType,shape=Loc)),
            e = ggplot(c_pop,aes(DayOfYear,Eggs,color=TrapType,shape=Loc))
            )
        q = q + geom_point(size=5) + ggtitle(paste(block,site,x,sep=", "))
        ## rotate x_labels:
        q = q + theme(axis.text.x = element_text(angle = 90, hjust = 0))


        if(length(i_pufs) != 0){
            q = q + geom_vline(
                xintercept= puf.db$DayOfYear[i_pufs],
                colour = "blue",
                linetype = "longdash")
            
        }
        if(length(i_sprays) != 0){
            q = q + geom_vline(
                xintercept= p.n$DayOfYear[i_sprays],
                colour="red",
                linetype="longdash")
            
        }
        
        file = switch(t,
            m = paste0("../almond/males/",block,"_",ranch,"_",year,".pdf"),
            f = paste0("../almond/females/",block,"_",ranch,"_",year,".pdf"),
            e = paste0("../almond/eggs/",block,"_",ranch,"_",year,".pdf")
            )

        ggsave(filename = file, plot = q)
        ##return(q)
    })

    ## print plots (this requires gridExtra):
    ##do.call(grid.arrange,plots)

}

## @Function get.number.edges - Will write later
get.number.edges <- function(ranch,block){

    two.Edges.lh = c("2-2","1-2","1-4","25-4","30-3","30-4","30-1")
    zero.Edges.lh = c("2-4","1-3","11-1","12-2","12-3","13-2","13-3")
    two.Edges.sf = c("25-2","36-3","31-4","29-3","30-1")
    zero.Edges.sf = c("25-4","30-3","30-4","36-1","31-2")
    three.Edges = c("29-4")
    
    edges = 1

    if(ranch %in% c("344","345","346","439")){
        if(block %in% two.Edges.lh){ edges = 2 }
        if(block %in% zero.Edges.lh){ edges = 0 }
    }else{
        if(block %in% two.Edges.sf){ edges = 2}
        if(block %in% zero.Edges.sf){ edges = 0}
        if(block %in% three.Edges){edges = 3}
    }

    return(edges)
}


## @Function get.treatment
get.treatment <- function(ranch,block,year){
    
      treatment = c$Trtmnt[which(
        c$Block == block &
        c$Ranch == ranch &
        c$Year == year
        )]

      treatment = unique(treatment)
      return(treatment)
}

## @Function get.prior.sprays during the focal year
get.prior.sprays <- function(df,date, year, site, block){
    
    spray.dates = df$JD[which(
        df$Site == site &
        (grepl(block,df$Block.s.) |
         df$Block.s. == " ") &
        df$Year == year
        )]
    number = length(which(spray.dates < date))
    return(number)
}

## @Function get.total.prior.sprays
## get # of sprays that block has seen throughout the whole experiment
get.total.prior.sprays <- function(df,date,site,block){

    spray.dates = df$JD[which(
        df$Site == site &
        (grepl(block,df$Block.s.) |
         df$Block.s. == " ")
        )]
    number = length(which(spray.dates < date))
    return(number)
}


## @Function Make.Site.Plot
## takes: a site
## returns: a plot of the (smoothed) male populations
## in that site for each year with vertical lines where
## non-intrepid, non-puffer sprays occurred (for each year).
Make.Site.Plot1 <- function(site, treatment = "puffer", trap = "m" ) {

    
    df <- switch(treatment,
                 puffer = puf.db,
                 intrepid = p.n.p.intrepid,
                 all = p.n.p.all,
                 other = p.n.p.other,
                 conv = p.n.p
                 )

    years <- unique(subset(df,Site == site)$Year)
    c.site <- subset(c,Site == site & Year %in% years)

    
    plot <- switch(trap,
                   m = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Males,
                                group = Year,
                                colour = Year)),
                   f = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Females,
                                group = Year,
                                colour = Year)),
                   e = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Eggs,
                                group = Year,
                                colour = Year))
                   )
    
    plot <- plot + ggtitle(site)
    plot <- plot + stat_smooth()
    plot <- plot + geom_vline( aes(
        xintercept = DayOfYear,
        colour = as.factor(Year),
        lintype = "longdash"
        ), subset(df, Site == site), show_guide = TRUE)

    return(plot)

}


Make.Site.Plot1 <- function(site, treatment = "puffer", trap = "m" ) {

    
    df <- switch(treatment,
                 puffer = puf.db,
                 intrepid = p.n.p.intrepid,
                 all = p.n.p.all,
                 other = p.n.p.other,
                 conv = p.n.p
                 )

    years <- unique(subset(df,Site == site)$Year)
    c.site <- subset(c,Site == site & Year %in% years)

    
    plot <- switch(trap,
                   m = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Males,
                                group = Year,
                                colour = Year)),
                   f = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Females,
                                group = Year,
                                colour = Year)),
                   e = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Eggs,
                                group = Year,
                                colour = Year))
                   )
    
    plot <- plot + ggtitle(site)
    plot <- plot + stat_smooth()
    plot <- plot + geom_vline( aes(
        xintercept = DayOfYear,
        colour = as.factor(Year),
        lintype = "longdash"
        ), subset(df, Site == site), show_guide = TRUE)

    return(plot)

}

Make.Site.Plot2 <- function(site, treatment = "puffer", trap = "m" ) {

    
    df <- switch(treatment,
                 puffer = puf.db,
                 intrepid = p.n.p.intrepid,
                 all = p.n.p.all,
                 other = p.n.p.other,
                 conv = p.n.p
                 )

    years <- unique(subset(df,Site == site)$Year)
    c.site <- subset(c,Site == site & Year %in% years)

    
    plot <- switch(trap,
                   m = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Males,
                                group = Year,
                                colour = Year)),
                   f = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Females,
                                group = Year,
                                colour = Year)),
                   e = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Eggs,
                                group = Year,
                                colour = Year))
                   )
    
    plot <- plot + ggtitle(site)
    plot <- plot + stat_smooth()
    plot <- plot + geom_vline( aes(
        xintercept = DayOfYear,
        colour = as.factor(Year),
        lintype = "longdash"
        ), subset(df, Site == site), show_guide = TRUE)

    return(plot)

}


## @Function get.blocks
get.blocks <- function(block_text,site) {
    if (block_text == " "){
        blocks = names(dict[[as.character(site)]])
    }else{
        blocks = unlist(strsplit(block_text,","))
    }

    return(blocks)
}

## @Function get.trapsites
get.trapsites <- function(block,site) dict[[as.character(site)]][[block]]


## @Function get.prespray.count.index
get.postspray.count.index <- function(ranch,trapsites,date,bin, type) {

    traps <- switch(type,
                    m = c("Phero","Flight","LurePhero"),
                    e = c("Egg"),
                    f = c("Delta/Ovip")
                    )
    
    ## could do something like abs( (date + bin + 7) - c$JD ) <= bin
    which(
        c$Ranch == ranch &
        (c$JD - date <= (14 + bin) & c$JD >= date + 7 ) &
        c$TrapType %in% trap &
        c$TrapSite %in% trapsites
        )
}

## @Function get.postspray.count.index
get.prespray.count.index <- function(ranch,trapsites,date,bin, type) {

    traps <- switch(type,
                    m = c("Phero","Flight","LurePhero"),
                    e = c("Egg"),
                    f = c("Delta/Ovip")
                    )
    
    ## could do something like abs( (date - bin - 1) - c$JD ) <= bin
    which(
        c$Ranch == ranch &
        (date - (8 + bin) <= c$JD & c$JD < date ) &
        c$TrapType %in%  traps &
        c$TrapSite %in% trapsites
        )
}

## @Function get.count
get.count <- function(type,indices) {
    switch(type,
           m = c$Males[indices],
           f = c$Females[indices],
           e = c$Eggs[indices]
           )
    
}


## @Function puffer.dates.for.block
puffer.dates.for.block <- function(site,block,year) {

    new.set = subset(puf.db,
        Site == site &
        Block.s. == block &
        Year == year
        )

    return(new.set$JD)

}

## @Function spray.dates.for.block
spray.dates.for.block <- function(site,block,year){

    new.set = subset(p.n,
        Site == site &
        (grepl(block,Block.s.) | Block.s. == " ") &
        Year == year &
        Product..Agrian. != "Puffer NOW"
        )

    return(new.set$JD)
}

## @Function eval.efficacy. 
## Uses the data contained in p.n.p (or its siblings), c, and puf.db
## to measure spray efficacy. This is done by selecting out each unique
## block-spray combination, finding the pre & post spray insect densities
## for that combination, then finding the proportion of the post to pre
## spray means. This information is augmented with: the # of exterior edges
## that block has; # of prior sprays on that block for the given year;
## # of prior sprays on that block througout the entire experiment; the
## mean insect population prior to the spray; and the treatment the block
## is undergoing that year.

eval.efficacy <- function(df = p.n.p, type="m", route = "mean", bin = 7){

    ##require(plyr)
    
    if ( type %ni% c("m", "f", "e") ) {
        stop("Please choose a valid type")
    }

    temp = df
    temp$MeanPopBeforeSpray <- NA
    temp$Proportion <- NA
    temp$Edges <-  NA
    temp$PriorSprays <- NA
    temp$Treatment <- NA
    temp$TotalPriorSprays <- NA
    
    ef.dat = do.call(rbind, lapply(1:dim(temp)[1],function(i){

        site <- temp$Site[i]
        blocks <- temp$Block.s.[i]
        year <- temp$Year[i] 
        
        ## gather up the relevant blocks:

        blocks = get.blocks(blocks,site)

        ## yes, we need this because of block overlap:
        ranch <- temp$Ranch[i]
        date.jd <- temp$JD[i]

        ## now go through each block and look at
        ## trap catches before/after the spray date.
        ## Use these to produce "proportion" and "weight"
        
        site.rows <- do.call(rbind,lapply(blocks,function(block){

            ## first let's check if a puffer was turned on within
            ## the time-bin of the spray:
            puf.dates = puffer.dates.for.block(site, block, year)

            test = length(which(abs(date.jd - puf.dates) <= (14+bin)))

            if(test != 0){
                ##print("block skipped due to puffer prox")
                row = temp[i,]
                row$Block.s. = block
                return(row)
            }

            ## Get the trap sites for the block:
            trapsites = get.trapsites(block,site)
            
            ## Now,
            ## Select the range of insect-sample dates based on
            ## the julian date of the pesticide spray:
            i_before <- get.prespray.count.index(ranch,
                                                trapsites,
                                                date.jd,
                                                bin, type)
            
            i_after <- get.postspray.count.index(ranch,
                                                  trapsites,
                                                  date.jd,
                                                  bin, type)
                
            ## Select the actual catches:
            pop_before = get.count(type, i_before)
            pop_after = get.count(type, i_after)
            
            ## if we allow na's to hang around then we can't take
            ## a mean
            pop_before = subset(pop_before, !is.na(pop_before))
            pop_after = subset(pop_after, !is.na(pop_after))
            
            ## if at this point there's no data, regur some na's:
            if( length(pop_after) == 0 | length(pop_before) == 0 ){

                ##print("row skipped due to lack of data")
                row = temp[i,]
                row$Block.s. = block
                return(row)
            } else {
            
                ## this will tell our regression whether our proportion value
                ## is worth listening to (right?):
                weight <-  mean(pop_before)
                
                ## find proportion:
                proportion <- proportion.spray(pop_before,pop_after)

                ## create row:
                row = temp[i,]
                row$MeanPopBeforeSpray = weight
                row$Proportion = proportion
                row$Block.s. = block
                row$Edges = get.number.edges(ranch,block)
                row$Treatment = get.treatment(ranch,block,year)
                row$PriorSprays = get.prior.sprays(
                    df,
                    date.jd,
                    year,
                    site,
                    block)
                row$TotalPriorSprays = get.total.prior.sprays(
                    df,
                    date.jd,
                    site,
                    block)
                return(row)

            }
        }))
        return(site.rows) 
    }))    

    return(ef.dat)
}    
    
## This was the worst:
##

clean.blocks <-  function(blocks){
    if ("3-Jan" == blocks){return("1-3")}
    if ("4-Nov" == blocks){return("11-4")}
    if ("2-Jan" == blocks){return("1-2")}
    if ("02-3,02-4" == blocks){return("2-3,2-4")}
    if ("02-1,02-2,02-3,02-4" == blocks){return("2-1,2-2,2-3,2-4")}
    if ("02-1,02-2" == blocks){return("2-1,2-2")}
    if ("02-1,02-3,02-4" == blocks){return("2-1,2-3,2-4")}
    if ("2-Feb" == blocks){return("2-2")}
    if ("01-1,01-2" == blocks){return("1-1,1-2")}
    return(blocks)
}


## Then the other task for today is to start pinning down
## a measure of trap shutdown. That will take a function
## quite similar to eval.efficacy(). Things I need to watch
## out for: (1) my first pop measurment MUST be before any sprays
## or puffers are applied and (2) my second measurement must
## be after the FIRST puffer of the season (if there are multiple)
## but BEFORE the first spray of the season.

## this function will be much easier once puf.db.by.block is complete.
## This

eval.trap.shutdown <- function(bin = 14){

    ## Make a temporary DF with the columns we want
    temp = puf.db
    temp$ProportionAdj <-  NA
    temp$ProportionRaw <- NA
    temp$MeanPopBeforeMD <- NA
    temp$MeanPopAfterMD <- NA
    temp$Edges <- NA
    temp$SampleID <- NA
    temp$TimeOfMD <- NA

    ## pull a lapply do-call combo on the rows of temp
    ts.db = do.call(rbind,lapply(1:dim(temp)[1],function(i){

        ## pull out the info we need, convert as necessary
        block = temp$Block.s.[i]
        ranch = temp$Ranch[i]
        site = temp$Site[i]
        year = temp$Year[i]
        date = temp$JD[i]

        ## find all puffer dates for that year,block,site
        puf.dates = puffer.dates.for.block(site,block,year)

        ## find all non-puffer spray dates for that year,block,site
        spray.dates = spray.dates.for.block(site,block,year)
       
        if (length(spray.dates) == 0 ) {
            earliest.spray.date = date + bin + 7
        } else {

            earliest.spray.date = min(spray.dates)
            
            ## Check for non-MD applications that came before
            ## the MD was turned on:
            if ( earliest.spray.date < date){
                if (date - earliest.spray.date < 2 * bin){
                    row = temp[i,]
                    return(row)
                } else {
                    earliest.spray.date = date + bin + 7
                }            
            }
            
        }
        
        ## ## Make sure no other puffers are too close by:
        puf.dists <- abs(date - puf.dates)
        puf.logical <- (puf.dists < bin & puf.dists > 0)
        nearby.puffers <- puf.dates[which( puf.logical == TRUE )]

        if (length(nearby.puffers) != 0){

            earliest.spray.date = min(puf.dates[which( puf.logical == TRUE )])
            
        }
        ## if this isn't the first puffer return a row with $Proportion as NA        
        if (date != min(puf.dates) ) {
            row = temp[i,]
            return(row)
        }else{

            ## Since this was the fist puffer and no spray came
            ## before it, let's look at its effect on trap shutdown:
            
            ## grab the relevant trapsites
            trapsites = dict[[as.character(site)]][[block]]
            
            ## determine the cutoffs for the trap sample dates we want
            cutoffmin =  date - bin
            cutoffmax = min(earliest.spray.date, date + 7 + bin)
            #trap.rows <- do.call(rbind, lapply(trapsites, function(trap){
                
                ## find the pre and post spray samples according to
                ## the cutoffs,trapsites,traptypes, etc we want
                pre.spray = c$Males[which(
                    c$Ranch == ranch &
                    c$Block == block & ##redundant
                    (c$JD < date & c$JD >= cutoffmin) &
                    c$TrapType %in% c("Phero","Flight","LurePhero") &
                    c$TrapSite %in% trapsites
                )]

            
            
            post.spray = c$Males[which(
                c$Ranch == ranch &
                c$Block == block & ##redundant
                (c$JD >= date + 7  & c$JD <= cutoffmax) &
                c$TrapType %in% c("Phero","Flight","LurePhero") &
                c$TrapSite %in% trapsites
                )]

      
            ## if either vector has length 0 we can't calculate
            ## anything
            if(length(post.spray) == 0 | length(pre.spray) == 0){
                row = temp[i,]
                return(row)
            }else{
                row = temp[i,]
                mean.before.spray = mean(pre.spray, na.rm = TRUE)
                mean.after.spray = mean(post.spray, na.rm = TRUE)
                ## if there are no bugs then we can't measure
                ## the effect of the MD...
                if (mean.before.spray == 0) {
                    row = temp[i,]
                    return(row)
                }
                
                row$MeanPopBeforeMD = mean.before.spray
                row$MeanPopAfterMD = mean.after.spray
                row$ProportionRaw = proportion.md(pre.spray,post.spray)
                row$ProportionAdj = proportion.spray(pre.spray,post.spray)
                row$Edges = get.number.edges(ranch,block)
                row$SampleID = RandomString()
                row$TimeOfMD = getTimeOfMD(row$DayOfYear)
                return(row)
            }  
            
        }
        
    }))

    ts.db$RateOfRelease <- sapply(ts.db$Treatment,function(x){

        if( is.na(x) ) { return(NA) }
        if ( grepl("50%",x) ){ return( "50%" ) }
        else { return( "100%" ) }
        
    })

    ts.db$PufPerAcre <- sapply(ts.db$Rate,function(x){
        if ( x %ni% c(1,2) ) {
            
            return(2)
        }else{return(x)}
    })

    return(ts.db)
}


## @Function GetYear
GetYear <- function(date){

    if( date == " " | date == "" ){ return(NA) }
    year <- unlist(strsplit(date,"/"))[3]
    year <- as.numeric(year) + 2000
    return(year)


}

## the same as make.site.plot1, but instead of smoothing
## for each year it makes a facet for each year. Still shows
## smooth, but addes geom_point
Make.Site.Plot2 <- function(site, treatment = "puffer", trap = "m" ) {

    
    df <- switch(treatment,
                 puffer = puf.db,
                 intrepid = p.n.p.intrepid,
                 all = p.n.p.all,
                 other = p.n.p.other,
                 conv = p.n.p
                 )

    years <- unique(subset(df,Site == site)$Year)
    c.site <- subset(c,Site == site & Year %in% years)

    
    plot <- switch(trap,
                   m = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Males,
                       )),
                   f = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Females,
                       )),
                   e = ggplot(c.site, aes(
                                x = DayOfYear,
                                y = Eggs,
                       ))
                   )
    
    plot <- plot + ggtitle(site)
    plot <- plot + stat_smooth(size = 2)
    plot <- plot + geom_point(size = 4, alpha = .7)
    plot <- plot + facet_wrap( ~ Year)
    plot <- plot + geom_vline( aes(
        xintercept = DayOfYear,
        lintype = "longdash"
        ), subset(df, Site == site))

    return(plot)

}


eval_trap_shutdown_by_trap <- function(bin = 14){

    ## Make a temporary DF with the columns we want
    temp = puf.db
    temp$ProportionAdj <-  NA
    temp$ProportionRaw <- NA
    temp$MeanPopBeforeMD <- NA
    temp$MeanPopAfterMD <- NA
    temp$Trap <- NA
    temp$Edges <- NA
    temp$SampleID <- NA
    temp$TimeOfMD <- NA

    ## pull a lapply do-call combo on the rows of temp
    ts.db = do.call(rbind,lapply(1:dim(temp)[1],function(i){

        ## pull out the info we need, convert as necessary
        block = temp$Block.s.[i]
        ranch = temp$Ranch[i]
        site = temp$Site[i]
        year = temp$Year[i]
        date = temp$JD[i]

        ## find all puffer dates for that year,block,site
        puf.dates = puffer.dates.for.block(site,block,year)

        ## find all non-puffer spray dates for that year,block,site
        spray.dates = spray.dates.for.block(site,block,year)
       
        if (length(spray.dates) == 0 ) {
            earliest.spray.date = date + bin + 7
        } else {

            earliest.spray.date = min(spray.dates)
            
            ## Check for non-MD applications that came before
            ## the MD was turned on:
            if ( earliest.spray.date < date){
                if (date - earliest.spray.date < 2 * bin){
                    row = temp[i,]
                    return(row)
                } else {
                    earliest.spray.date = date + bin + 7
                }            
            }
            
        }
        
        ## ## Make sure no other puffers are too close by:
        puf.dists <- abs(date - puf.dates)
        puf.logical <- (puf.dists < bin & puf.dists > 0)
        nearby.puffers <- puf.dates[which( puf.logical == TRUE )]

        if (length(nearby.puffers) != 0){

            earliest.spray.date = min(puf.dates[which( puf.logical == TRUE )])
            
        }
        ## if this isn't the first puffer return a row with $Proportion as NA        
        if (date != min(puf.dates) ) {
            row = temp[i,]
            return(row)
        }else{

            ## Since this was the fist puffer and no spray came
            ## before it, let's look at its effect on trap shutdown:
            
            ## grab the relevant trapsites
            trapsites = dict[[as.character(site)]][[block]]
            
            ## determine the cutoffs for the trap sample dates we want
            cutoffmin =  date - bin
            cutoffmax = min(earliest.spray.date, date + 7 + bin)
            trap.rows <- do.call(rbind, lapply(trapsites, function(trap){
              
                ## find the pre and post spray samples according to
                ## the cutoffs,trapsites,traptypes, etc we want
                pre.spray = c$Males[which(
                    c$Ranch == ranch &
                    c$TrapSite == trap & 
                    (c$JD < date & c$JD >= cutoffmin) &
                    c$TrapType %in% c("Phero","Flight","LurePhero")
                )]

            
            
                post.spray = c$Males[which(
                    c$Ranch == ranch &
                    c$TrapSite == trap & 
                    (c$JD >= date + 7  & c$JD <= cutoffmax) &
                    c$TrapType %in% c("Phero","Flight","LurePhero")
                    )]
                
      
                ## if either vector has length 0 we can't calculate
                ## anything
                if(length(post.spray) == 0 | length(pre.spray) == 0){
                    row = temp[i,]
                    return(row)
                }else{
                    row = temp[i,]
                    mean.before.spray = mean(pre.spray, na.rm = TRUE)
                    mean.after.spray = mean(post.spray, na.rm = TRUE)
                    ## if there are no bugs then we can't measure
                    ## the effect of the MD...
                    if (mean.before.spray == 0) {
                        row = temp[i,]
                        return(row)
                    }
                    
                    row$MeanPopBeforeMD = mean.before.spray
                    row$MeanPopAfterMD = mean.after.spray
                    row$ProportionRaw = proportion.md(pre.spray,post.spray)
                    row$ProportionAdj = proportion.spray(pre.spray,post.spray)
                    row$Edges = get.number.edges(ranch,block)
                    row$Trap = trap
                    row$SampleID = RandomString()
                    row$TimeOfMD = getTimeOfMD(row$DayOfYear)
                    return(row)
                }  
            }))
            
        
            return(trap.rows)
        }
    }))

    ts.db$RateOfRelease <- sapply(ts.db$Treatment,function(x){

        if( is.na(x) ) { return(NA) }
        if ( grepl("50%",x) ){ return( "50%" ) }
        else { return( "100%" ) }
        
    })

    ts.db$PufPerAcre <- sapply(ts.db$Rate,function(x){
        if ( x %ni% c(1,2) ) {
            
            return(2)
        }else{return(x)}
    })

    return(ts.db)
}

RandomString <- function(length = 4){

    str <- paste(sample(c(0:9, letters, LETTERS),
                         length, replace=TRUE),collapse="")
    return(str)

}

getTimeOfMD <- function(day.of.year){

    if( day.of.year < 130 ) { return("E") }
    if( day.of.year > 130 ) { return("L") }
    if( is.na(day.of.year) ) {return( day.of.year )}
}


BinSeason <- function(df, num.bins = 8){

    ##require(plyr)
    
    doy.segs <- SegmentVec(unique(df$DayOfYear), num.bins)
    means <- llply(doy.segs, function(seg){
        
        males <- subset(df, DayOfYear %in% seg)$Males
        eggs <- subset(df, DayOfYear %in% seg)$Eggs
        females <- subset(df, DayOfYear %in% seg)$Females

        males <- mean(males, na.rm = TRUE)
        eggs <- mean(eggs, na.rm = TRUE)
        females <- mean(females, na.rm = TRUE)

        means.list <- list(males,females,eggs)
        return(means.list)
    })

    means = unlist(means)

    c.names <- paste0(c("M", "F", "E"), rep(1:num.bins, each = 3))

    names(means) <- c.names
    
    return(means)
}

SegmentVec <- function(vec, num.segs, ordered = TRUE){

    ##require(plyr)
    v <- vec

    if(ordered){
        v <- sort(v)
    }

    segs <- sort(rep(1:num.segs,length = length(v)))
    segments <- llply(1:num.segs,function(i){ v[which(segs == i)] })
    return(segments)
}

ToCentury <- function(date, cent){

    if( date %in% c(" ", "") ){ return(NA) }

    mdy <- unlist(strsplit(date,"/"))
    year <- as.numeric( mdy[3] )
    if( year < 100 ){
        mdy[3] = as.character(year + cent)
    }
    mdy <- paste(mdy, collapse = "/")
    return(mdy)

}


BuildTreatment <- function(df){

    kLatestMD <- 119

    products <- unique(df$Product..Agrian.)

    trtmnt <- NA

    if( "Puffer NOW" %ni% products){
            trtmnt <- "C"
        } else {

            dates <- unique(subset(df, Product..Agrian. == "Puffer NOW")$DayOfYear)
            max.date <- max(dates)
            if( max.date < kLatestMD ){
                if( length(products) == 1 ) {
                    trtmnt <- "EMD"
                } else {
                    trtmnt <- "ECMD"
                }
            } else {
                if( length(products) == 1 ) {
                    trtmnt <- "LMD"
                } else {
                    trtmnt <- "LCMD"
                }
            }
        }

    return(trtmnt)
}


RunTrialWithOpts <- function(type, trtmnt, bins, mod = FALSE, resp = "D"){

    ##require(lme4)

    ## bin the m, f, e count data:
    seas.bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = bins)

    ## looks like:
    ##cnames:        M1 F1 E1 ... Mn Fn En
    ##data:           . .  .  .    .  .  .
    ##                . .  .   .   .  .  .
    ## etc..          . .  .    .  .  .  .

    ## subsetting response var df by what treatment blocks received
    dc.subset <- subset(dmg, trt2 == trtmnt)

    ## merging the subset with the season bins:
    dc <- merge(dc.subset, seas.bins, c("Year", "Ranch", "Block"))

    ## create formula:
    bin.terms <-  paste(paste0(type, seq(bins)), collapse = " + ")

    switch( resp,
           D = response.term <- "cbind(DmgNOW, Tot_Nuts - DmgNOW)",
           I = response.term <- "cbind(InfNOW, Tot_Nuts - DmgNOW)",
           ID = response.term <- "cbind(DmgNOW, InfNOW - DmgNOW)",
           )

    if( trtmnt == "LMD" ){
        other.terms <- " ~ (1|ID) + (1|Block) +  Plot + Variety"
    } else {

        other.terms <- " ~ (1|ID) + (1|Year) + (1|Ranch/Block) + loc + Plot + Variety"

        if( trtmnt == "LCMD" ){
            other.terms <- " ~ (1|ID) + (1|Year) + (1|Ranch/Block) + Plot + Variety"
        }

    }

    f <- as.formula( paste0(response.term, other.terms, bin.terms) )

    ## make model:
    m <- glmer(f, data = dc, family = "binomial")

    if(mod == TRUE){
        return(m)
    } else {
        return( AIC(m) )
    }
}




RunBoost <- function(df){

    require(gbm)
    boost.dmg <- gbm( DmgNOW ~ M1 + M2 + M3 + M4  + F1 + F2 + F3 + F4 + E1 + E2 + E3 + E4, data = df, distribution = "poisson", n.trees = 5000, interaction.depth = 4)
    result <- summary(boost.dmg)
    #result <- t(as.data.frame(t(results)[2,]))
    return(result)
}

RunForest <- function(df){

    require(randomForest)
    rf.dmg <- randomForest(DmgNOW ~ M1 + M2 + M3 + M4  + F1 + F2 + F3 + F4 + E1 + E2 + E3 + E4, data = df, mtry = 3, importance = TRUE)
    results <- t(as.data.frame(t(importance(rf.dmg))[1,]))
    return(results)
}


RunTrialWithOpts2 <- function(type,
                              trtmnt,
                              bin,
                              fold = NULL,
                              boot_i = NULL,
                              resp = "D",
                              dmg_sets = NULL,
                              cv_list = NULL
                              ) {


    ## @Function RunTrialWithOpts2
    ## use: runs models based on different type, trtmnt, bin combinations
    ## @Params:
    ##    (type, trtmnt, bin) - used to subset the damage dataframe
    ##    type - "M", "F", "E"; the desired trap type
    ##    trtmnt - the treatment type (depends also on grain):
    ##         1) grain == "fine" --> trtmnt in EMD, ECMD, C, LMD, LCMD
    ##         2) grain != "fine" --> trtmnt in L, C, E
    ##    bin - which bin of the season you'll be using
    ##    mat_i - which column of index_matrix to use for bootstrap
    ##    fold - which fold to use for CV
    ##    resp - sets the response variable for the models
    ##    grain - set the granularity of the trtmnt parameter
    ##            (pretty hacky, sorry)
    ## @Out:
    ##    The AIC of the model, or the boostrapped AIC of the model
    ##    *note* the bootstrapped AIC doesn't really have any formal
    ##           meaning; I'd just like to see what happens, i.e. do
    ##           strong models stay strong?
    ## @Depends:
    ##    dmg - the damage dataframe
    ##    seas.bins - the season pop averages dataframe

    ## require(lme4)
    ## require(AICcmodavg)
    if (is.null(dmg_sets) | is.null(cv_list)){stop("We need complete data")}
    
    ## Selecting the right dataset to use
    dc <- dmg_sets[[trtmnt]]

    ## select the proper rows:
    if (!is.null(boot_i)) {
        dc <- try(dc[boot_matrix[[trtmnt]][, boot_i], ])
        if (identical(class(dc), "try-error")) {stop("boot_i must be an int")}
    }

    ## response term
    switch( resp,
           D = response.term <- "cbind(DmgNOW, Tot_Nuts - DmgNOW)",
           I = response.term <- "cbind(InfNOW, Tot_Nuts - InfNOW)",
           ID = response.term <- "cbind(DmgNOW, InfNOW - DmgNOW)",
           )

    ## general predictors
    if (is.null(fold)) {
        other.terms <- " ~ (1|SampleID) + "
    } else {
        other.terms <- " ~ "
    }

    other.terms <- paste0(other.terms,"(1|Year) + (1|Block)")
    other.terms <- paste0(other.terms," + Plot + Variety + tree_age")
    if (!grepl("L", trtmnt)) { other.terms <- paste0(other.terms," + loc") }

    ## specific predictors
    if (bin == 0) {
        bin.terms <- NULL
    } else {
        bin.terms <- paste0(" + ", type, bin)
    }

    ## create formula
    f <- as.formula(paste0(response.term, other.terms, bin.terms))

    ## Go CV route, or AIC route. 
    if (!is.null(fold)) {
        ## CV

        ## set training and testing sets
        dc_train <- dc[-cv_list[[trtmnt]][[fold]], ]
        dc_test <- dc[cv_list[[trtmnt]][[fold]], ]

        ## build model
        m <- try(glmer(f, data = dc_train, family = "binomial"), silent = FALSE)
        if (identical(class(m), "try-error")) {
            ##warning(paste(type, trtmnt, bin, fold, sep =" "))
            data.frame("COR" = NA)
        } else {

            preds <- try(predict(m,
                                 newdata = dc_test,
                                 type = "response",
                                     re.form = NULL), silent = FALSE)
            if (identical(class(preds), "try-error")) {
                warning(paste(type, trtmnt, bin, fold, sep = " "))
                data.frame("COR" = NA)
            } else {
                actual <- na.omit(dc_test$DmgNOW / dc_test$Tot_Nuts)
                data.frame("COR" = cor(preds, actual))
            }
        }
    } else {
        ## AIC

        ## build model
        m <- try(glmer(f, data = dc, family = "binomial"), silent = FALSE)
        if (identical(class(m), "try-error")) {
            warning(paste(type, trtmnt, bin, sep =" "))
            data.frame("AIC" = NA)
        } else {
            data.frame("AIC" = AIC(m))
        }
    }
}


testRunTrialWithOpts2 <- function(K = 5, bins = 5, parallel = FALSE) {

    cv_list <- dlply(dmg, .(trt2), FoldData, k = K, seed = 10)
    seas_bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = bins)
    dmg_sets <-  dlply(dmg, .(trt2), merge, y = seas_bins,
                       by = c("Year", "Ranch", "Block"))

    val_grid <- list(c("M", "F", "E"), c("EMD", "ECMD", "CONV", "LMD", "LCMD"),
                     0:bins, 1:K)
    val_grid <- expand.grid(val_grid, stringsAsFactors = FALSE)
    colnames(val_grid) <- c("type", "trtmnt", "bin", "fold")

    registerDoMC(cores = 4)

    par_opts = list(.export = c("dmg_sets", "cv_list"), .packages = c("lme4"))

    results <- mdply(val_grid,
                     RunTrialWithOpts2,
                     dmg_sets = dmg_sets,
                     cv_list = cv_list,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE)
}



aic.boot.fn <- function(data, index, f){
    ## @Function aic.boot.fn
    ## use: helper function for (pseudo) boot call
    ## @Param:
    ##  -data - the dataset
    ##  -index - indices of the subset
    ##  -f - formula to use in glmer call
    ## @Out: NA if model fails, AIC if model succeeds

    new_data = data[index,]
    m <- try(glmer(formula = f, data = new_data, family = "binomial"),
             silent = TRUE)
    if (identical(m, "try-error") ) { return(NA) }
    AIC(m)
}


MakeIndices <- function(tmnt, num = 10){
    df <- subset(dmg, trt2 == tmnt)
    i <- replicate(num, sample(1:nrow(df), nrow(df), rep = TRUE))
}

FoldData <- function(df, k = 5, random = TRUE, seed = NULL){
    ## @Function FoldData
    ## @Params:
    ##   - df dataframe: data to be used for CV
    ##   - k integer: number of folds desired
    ##   - random logical: if true, randomizes rows before folding
    ##   - seed integer vector: sets a seed for reproducability
    ## @Output: a list of integer vector folds

    if (!is.null(seed)) { set.seed(seed) }

    if (!is.logical(random)) {
        warning("random must be logical; setting to TRUE")
        random <- TRUE
    }

    rows <- 1:nrow(df)
    random && (rows <- sample(1:nrow(df), nrow(df)))

    folds <- SegmentVec(rows, k, ordered = FALSE)
}


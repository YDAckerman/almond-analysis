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


eval.efficacy <- function(df = p.n.p, type="m", route = "mean", bin = 7){

    ##require(plyr)
    
    if ( type %ni% c("m", "f", "e") ) {
        stop("Please choose a valid type")
    }

    temp < df
    temp$MeanPopBeforeSpray <- NA
    temp$Proportion <- NA
    temp$Edges <-  NA
    temp$PriorSprays <- NA
    temp$Treatment <- NA
    temp$TotalPriorSprays <- NA
    
    ef.dat <- do.call(rbind, lapply(1:dim(temp)[1],function(i){

        site <- temp$Site[i]
        blocks <- temp$Block.s.[i]
        year <- temp$Year[i] 
        
        ## gather up the relevant blocks:

        blocks <- get.blocks(blocks,site)

        ## yes, we need this because of block overlap:
        ranch <- temp$Ranch[i]
        date.jd <- temp$JD[i]

        ## now go through each block and look at
        ## trap catches before/after the spray date.
        ## Use these to produce "proportion" and "weight"
        
        site.rows <- do.call(rbind,lapply(blocks,function(block){

            ## first let's check if a puffer was turned on within
            ## the time-bin of the spray:
            puf.dates <- puffer.dates.for.block(site, block, year)

            test <- length(which(abs(date.jd - puf.dates) <= (14+bin)))

            if(test != 0){
                ##print("block skipped due to puffer prox")
                row = temp[i,]
                row$Block.s. = block
                return(row)
            }

            ## Get the trap sites for the block:
            trapsites <- get.trapsites(block,site)
            
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
            pop_before <- get.count(type, i_before)
            pop_after <- get.count(type, i_after)
            
            ## if we allow na's to hang around then we can't take
            ## a mean
            pop_before <- subset(pop_before, !is.na(pop_before))
            pop_after <- subset(pop_after, !is.na(pop_after))
            
            ## if at this point there's no data, regur some na's:
            if( length(pop_after) == 0 | length(pop_before) == 0 ){

                ##print("row skipped due to lack of data")
                row <- temp[i,]
                row$Block.s. <- block
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



## previously RunTrialWithOpts
RunParametricAIC <- function(type, trtmnt, bins, mod = FALSE, resp = "D"){

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

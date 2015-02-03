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



## @Function GetYear
GetYear <- function(date){

    if( date == " " | date == "" ){ return(NA) }
    year <- unlist(strsplit(date,"/"))[3]
    year <- as.numeric(year) + 2000
    return(year)


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

        bin_df <- subset(df, DayOfYear %in% seg)

        males <- mean(bin_df$Males, na.rm = TRUE)
        eggs <- mean(bin_df$Eggs, na.rm = TRUE)
        females <- mean(bin_df$Females, na.rm = TRUE)

        means.list <- list(males,females,eggs)
        return(means.list)
    })

    means = unlist(means)
    names(means) <- paste0(c("M", "F", "E"), rep(1:num.bins, each = 3))

    means
}

SegmentVec <- function(vec, num.segs, ordered = TRUE){

    ##require(plyr)
    if(ordered){
        vec <- sort(vec)
    }

    segs <- sort(rep(1:num.segs,length = length(vec)))
    segments <- llply(1:num.segs,function(i){ vec[which(segs == i)] })
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

GetResiduals <- function(.subset = NULL,
                         .response = "D",
                         .family = "binomial"
                         ) {

    if (is.null(subset)) return(NA)

    f <- switch(.response,
                D = "cbind(DmgNOW, Tot_Nuts - DmgNOW)",
                I = "cbind(InfNOW, Tot_Nuts - InfNOW)",
                ID = "cbind(DmgNOW, InfNOW - DmgNOW)",
                LD = "DmgNOW / Tot_Nuts",
                LI = "InfNOW / Tot_Nuts",
                LID = "DmgNOW / InfNOW"
                )

    f <- paste0(f, " ~ (1|Variety) + tree_age")

    ## create formula
    f <- as.formula(f)

    ## model
    m1 <- try(glmer(f,
                    data = .subset,
                    family = .family,
                    na.action = na.exclude
                    ))
    if (identical(class(m1), "try-error")) {
        return(NA) ##TODO make a better catch
    }

    residuals(m1)
}

ToDayOfYear <- function(date, format){
    strptime(as.character(date), format)$yday + 1
}

rescale <- function(x) {

    listBool <- is.list(x)
    numBool <- is.numeric(x)

    if (listBool) {
        warning("coercing list to vector")
        names <- names(x)
        x <- unlist(x)
    }

    if (!is.numeric(x)) {

        if(listBool){
            return(list(names = x))
        }
        return(x)
    }

    x <- 100 * (x - min(x))/(max(x) - min(x))

    list(names = x)
}

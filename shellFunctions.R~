
## previously RunTrialWithOpts2
RunParametricCV <- function(type,
                              trtmnt,
                              bin,
                              fold = NULL,
                              boot_i = NULL,
                              resp = "D",
                              dmg_sets = NULL,
                              cv_list = NULL
                              ) {


    ## @Function RunParametricCV
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

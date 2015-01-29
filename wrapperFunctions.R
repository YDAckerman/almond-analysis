## source functions & packages
source("../almond/R/helperFunctions.R")
source("../almond/R/libs.R")

## previously RunTrialWithOpts2
RunParametricCV <- function(type,
                            trtmnt,
                            bin,
                            fold = NULL,
                            boot_i = NULL,
                            resp = "D",
                            subset_sets = NULL,
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
    ##    subset_sets & cv_list - pushing these into the function as
    ##                         parameters was the only way I could
    ##                         figure out to make parallel work.
    ## @Out:
    ##    -The AIC of the model, or the boostrapped AIC of the model
    ##        *note* does the bootstrapped AIC really have any formal
    ##        meaning? I'd just like to see what happens, i.e. do
    ##        strong models stay strong?
    ##    -Or the correllation coefficient of the predicted vs. acutal
    ##        response values.

    ## require(lme4)
    ## require(AICcmodavg)

    if (is.null(subset_sets) | is.null(cv_list)) {stop("We need complete data")}

    ## Selecting the right dataset to use
    dc <- subset_sets[[trtmnt]]

    ## select the proper rows:
    if (!is.null(boot_i)) {
        dc <- try(dc[boot_matrix[[trtmnt]][, boot_i], ])
        if (identical(class(dc), "try-error")) {stop("boot_i must be an int")}
    }

    ## build text form
    response.term <- switch(resp,
                            D = "cbind(DmgNOW, Tot_Nuts - DmgNOW) ~ ",
                            I = "cbind(InfNOW, Tot_Nuts - InfNOW) ~ ",
                            ID = "cbind(DmgNOW, InfNOW - DmgNOW) ~ ",
                            )

    other.terms <- paste0(other.terms," (1|Year) + (1|Block/Ranch)")
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

    ## preset outputs to na and change them when necessary:
    COR <- NA
    MSE <- NA
    AIC <- NA

    ## Go CV route, or AIC route.
    if (!is.null(fold)) {
        ## CV

        ## set training and testing sets
        dc_train <- dc[-cv_list[[trtmnt]][[fold]], ]
        dc_test <- dc[cv_list[[trtmnt]][[fold]], ]

        ## build model
        m <- try(glmer(f, data = dc_train, family = "binomial"), silent = FALSE)
        if (identical(class(m), "try-error")) {
            warning(paste(type, trtmnt, bin, fold, sep =" "))
        } else {

            preds <- try(predict(m,
                                 newdata = dc_test,
                                 type = "response",
                                     re.form = NULL), silent = FALSE)
            if (identical(class(preds), "try-error")) {
                warning(paste(type, trtmnt, bin, fold, sep = " "))
            } else {
                actual <- dc_test$DmgNOW / dc_test$Tot_Nuts
                COR <- cor(preds, actual, na.rm = TRUE)
                MSE <- mean((actual - preds)^2, na.rm = TRUE)
            }
        }

    } else {
        ## AIC

        ## build model
        m <- try(glmer(f, data = dc, family = "binomial"), silent = FALSE)
        if (identical(class(m), "try-error")) {
            warning(paste(type, trtmnt, bin, sep =" "))
        } else {
            actual <- dc$DmgNOW / dc$Tot_Nuts
            AIC <- AIC(m)
            COR <- cor(fitted(m), actual, na.rm = TRUE)
            MSE <- mean(residuals(m)^2)
        }
    }

    data.frame("COR" = COR, "MSE" = MSE, "AIC" = AIC)
}



RunParametricCVagainstResiduals <- function(type,
                                         trtmnt,
                                         bin,
                                         fold = NULL,
                                         resp = "D",
                                         res_sets = NULL,
                                         dmg_sets = NULL,
                                         cv_list = NULL
                                         ) {


    ## @Function RunParametricCVwithResiduals
    ## use: uses the residuals from a base glmer model
    ##      to fit a CV linear regression against seasonal trap-counts.
    ##      Then computes the correlation between test response and
    ##      actual response
    ## @Params:
    ##    type - "M", "F", "E"; the desired trap type
    ##    trtmnt - the treatment type (depends also on grain):
    ##    bin - which bin of the season you'll be using
    ##    fold - which fold to use for CV
    ##    resp - sets the response variable for the models
    ##    res_sets - list of pre-assembled residuals
    ##               (this was how I got parallel to work)
    ##    cv_list - list of pre-assembled folds to use for cv
    ## @Out:
    ##    The COR of the model

    ## require(lme4)
    ## require(AICcmodavg)

    if (is.null(cv_list)  || is.null(dmg_sets) || is.null(res_sets)){
        stop("Please add cv_list & dmg_sets & res_sets")
    }

    if (bin == 0) { stop("bin = 0; this is an issue")}

    res_df <- data.frame(
        RES = res_sets[[trtmnt]],
        BIN = dmg_sets[[trtmnt]][, paste0(type, bin)]
        )

    rtrain <- res_df[-cv_list[[trtmnt]][[fold]], ]
    rtest <- res_df[cv_list[[trtmnt]][[fold]], ]

    ## model
    m <- lm(RES ~ BIN, data = rtrain)

    ## Make predictions:
    preds <- stats::predict(
        m,
        newdata = rtest
        )

    data.frame(COR = cor(preds, rtest$RES, na.rm = TRUE))

}

## source functions & packages
source("~/Documents/RosenheimLab/almond/R/helperFunctions.R")
source("~/Documents/RosenheimLab/almond/R/libs.R")

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



RunParametricCVagainstResiduals <- function(V1, V2, V3,
                                            trtmnt,
                                            fold = NULL,
                                            .res_sets = NULL,
                                            .dmg_sets = NULL,
                                            .cv_list = NULL
                                         ) {


    ## @Function RunParametricCVwithResiduals
    ## TODO: write function summary

    ## require(lme4)
    ## require(AICcmodavg)

    COR <- NA
    MSE <- NA
    R2 <- NA
    predR2 <- NA

    if (sum(is.na(c(V1,V2,V3))) != 3)  {
        if (is.null(.cv_list)  || is.null(.dmg_sets) || is.null(.res_sets)){
            stop("Please add .cv_list & dmg_sets & .res_sets")
        }

        vars <- as.vector(na.omit(c(V1, V2, V3)))

        ## I'll d.f instead of merge so I can access by name?
        res_df <- data.frame(RES = .res_sets[[trtmnt]],
                             (.dmg_sets[[trtmnt]])[, vars]
                             )
        if (fold == 0) {

            m <- lm(RES ~ ., data = res_df, na.action = na.exclude)
            fit <- fitted(m, na.action = na.exclude)

            MSE <- mean((residuals(m))^2, na.rm = TRUE)
            COR <- cor(fit, res_df$RES, use = "pairwise.complete.obs")
            R2 <- summary(m)$adj.r.squared
            predR2 <- pred_r_squared(m)

        } else {

            ## split for CV (is this cv even legit?)
            rtrain <- res_df[-.cv_list[[trtmnt]][[fold]], ]
            rtest <- res_df[.cv_list[[trtmnt]][[fold]], ]

            ## formula
            m <- lm(RES ~ ., data = rtrain)

            ## Make predictions:
            preds <- predict(m, newdata = rtest)

            MSE <- mean((preds - rtest$RES)^2, na.rm = TRUE)
            COR <- cor(preds, rtest$RES, use = "pairwise.complete.obs")
        }
    }

    data.frame('COR' = COR, 'MSE' = MSE, 'R2' = R2, 'predR2' = predR2)
}


RunPredictiveModelWithCV <- function(.bin,
                                     .type,
                                     fold = NULL,
                                     subset_sets = NULL,
                                     cv_list = NULL,
                                     .subset = NULL,
                                     .formula = NULL,
                                     .family = "binomial"
                                     ) {


    ## @Function RunPredictiveModelWithCV
    ## @Param  type
    ## @Param  bin
    ## @Param  fold
    ## @Param  subset_sets
    ## @Param  cv_list
    ## @Param  subset
    ## @Out: COR & MSE of predicted vs actual responses. AIC of model if
    ##       CV is turned off.

    ## require(lme4)
    ## require(AICcmodavg)

    if (is.null(subset_sets) | is.null(cv_list)) stop("We need complete data")
    if (is.null(.formula)) stop("Please provide a formula and subset")

    ## Selecting the right dataset to use
    dc <- subset_sets[[.subset]]

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
        f <- as.formula(paste0(.formula, .type, .bin))
        m <- try(glmer(f,
                       data = dc_train,
                       family = .family
                       ),
                 silent = FALSE
                 )

        if (identical(class(m), "try-error")) {
            warning(paste(.type, .bin, fold, sep =" "))
        } else {

            preds <- try(predict(m,
                                 newdata = dc_test,
                                 type = "response",
                                     re.form = NULL), silent = FALSE)
            if (identical(class(preds), "try-error")) {
                warning(paste(.type, .bin, fold, sep = " "))
            } else {
                actual <- dc_test$DmgNOW / dc_test$Tot_Nuts
                COR <- cor(preds, actual, na.rm = TRUE)
                MSE <- mean((actual - preds)^2, na.rm = TRUE)
            }
        }
    } else {
        ## AIC

        ## build model
        f <- as.formula(paste0(.formula, .type, .bin))
        m <- try(glmer(f, data = dc, family = .family), silent = FALSE)
        if (identical(class(m), "try-error")) {
            warning(paste(.type, .bin, sep =" "))
        } else {
            actual <- dc$DmgNOW / dc$Tot_Nuts
            AIC <- AIC(m)
            COR <- cor(fitted(m), actual, na.rm = TRUE)
            MSE <- mean(residuals(m)^2)
        }
    }
    data.frame("COR" = COR, "MSE" = MSE, "AIC" = AIC)
}

RunSimplePredModel <- function(rhs,
                               trtmnt,
                               fold = NULL,
                               .dmg_sets = NULL,
                               .cv_list = NULL,
                               .lhs = "PercentDamaged",
                               .loocv = FALSE
                               ) {


    ## @Function RunSimplePredModel
    ## TODO: write function summary

    if (is.null(.cv_list)  || is.null(.dmg_sets)){
        stop("Please add .cv_list & dmg_sets & .res_sets")
    }

    f <- paste0(.lhs, "~", rhs)
    f <- as.formula(f)

    ## Pull out the desired dataset:
    reg_df <- .dmg_sets[[trtmnt]]

    COR <- NA
    MSE <- NA
    VAR <- NA
    MEAN <- NA
    meanPercError <- NA
    FreqError <- NA
    numPreds <- NA
    
    if (fold == 0) {

        m <- glm2(f,
                  data = reg_df,
                  family = "poisson",
                  na.action = na.exclude
                  )

        fit <- predict(m, type = "response", na.action = na.exclude)

        results <- data.frame('predicted' = fit,
                              'actual' = reg_df[, .lhs],
                              'totalNuts' = reg_df[, 'Tot_Nuts']
                              )

        ## dplyr experimentation
        tmp1 <- results %>%
            dplyr::filter(predicted < .01 & actual >= .01 ) %>%
                dplyr::mutate(percError = actual - predicted) %>%
                        dplyr::summarise(
                            meanPercError = mean(percError, na.rm = TRUE)
                            )
        
        tmp2 <- results %>%
            dplyr::filter(predicted < .01) %>%
                dplyr::summarise(
                    numUnacceptable = sum(actual >= .01, na.rm = TRUE),
                    numPreds = sum(predicted < .01, na.rm = TRUE)
                    ) %>%
                        dplyr::mutate(
                            freqError = numUnacceptable /numPreds
                            )
        
        MSE <- mean((residuals(m))^2, na.rm = TRUE)
        COR <- cor(results$predicted,
                   results$actual,
                   use = "pairwise.complete.obs"
                   )
        VARMSE <- var((residuals(m))^2, na.rm = TRUE)
        meanPercError <- tmp1$meanPercError
        FreqError <- tmp2$freqError
        numPreds <- tmp2$numPreds
        
    } else {

        ## split for CV (is this cv even legit?)
        rtrain <- reg_df[-.cv_list[[trtmnt]][[fold]], ]
        rtest <- reg_df[.cv_list[[trtmnt]][[fold]], ]

        ## formula
        m <- glm2(f, data = rtrain, family = "poisson")

        ## Make predictions:
        preds <- predict(m, newdata = rtest)

        MSE <- mean((preds - rtest[, .lhs])^2, na.rm = TRUE)
        COR <- cor(preds, rtest[, .lhs], use = "pairwise.complete.obs")
        VARMSE <- var((preds - rtest[, .lhs])^2, na.rm = TRUE)
        
    }

    data.frame('COR' = COR,
               'MSE' = MSE,
               'VARMSE' = VARMSE,
               'meanPercError' = meanPercError,
               'FreqError' = FreqError,
               'NumPreds' = numPreds
               )
}

DrawModel <- function(trtmnt = "CONV",
                      v1 = "F1", v2 = "F2", v3 = "F3",
                      individual = FALSE,
                      residuals = FALSE
                      ){

    ## @Function DrawModel
    ## TODO: write function summary

    AssembleData(test = "testRunSimplePredModel",
                 K = 0,
                 rescale = FALSE,
                 bins = 3,
                 seed = 10
             )

    set <- dmgNP_sets[[trtmnt]]
    
    if (individual) {
        forPlot <- melt(set,
                        id = setdiff(colnames(set),
                            na.omit(c(v1, v2, v3)))
                        )
        ggplot(forPlot, aes( x = value, y = PercentDamaged)) +
            geom_point(size = 2) +
                facet_wrap(~variable, scale = "free") +
                    labs(title = paste(
                             trtmnt,
                             " with model ",
                             paste(na.omit(c(v1, v2, v3)), collapse = "+")))
    } else {
        vars <- paste(na.omit(c(v1, v2, v3)), collapse = "+")
        f <- as.formula(paste(c("PercentDamaged", vars), collapse = "~"))
        m <- glm2(f,
                  family = "poisson",
                  data = set,
                  na.action = na.exclude
                  )

        set$Predictions <- predict(m, type = "response")

        if (residuals) {
            set$Residuals <- residuals(m)
            ggplot(set, aes(y = Residuals, x = 1:length(Residuals))) +
                geom_point(aes(size = Tot_Nuts)) +
                    labs(title = paste(
                             trtmnt,
                             " with model ",
                             paste(na.omit(c(v1, v2, v3)), collapse = "+")))

        } else {

            tmp <- set %>%
                select(Predictions, PercentDamaged) %>%
                    filter( Predictions <= .01 & PercentDamaged >= .01)
            
            danger_zone <- with(tmp,
                                data.frame(
                                    x = c(0, 0, .01, .01),
                                    y = c(.01, max(PercentDamaged, na.rm = TRUE),
                                         max(PercentDamaged, na.rm = TRUE), .01
                                    )))

            ggplot(set, aes(x = Predictions, y = PercentDamaged)) +
                geom_point(aes(size = Tot_Nuts)) +
                    geom_polygon(data=danger_zone, aes(x, y), fill="#d8161688") +
                    labs(title = paste(
                             trtmnt,
                             " with model ",
                             paste(na.omit(c(v1, v2, v3)), collapse = "+")))
        }
    }

}

RunModelLOOCV <- function(rhs,
                          trtmnt,
                          fold = NULL,
                          .dmg_sets = NULL,
                          .cv_list = NULL,
                          .lhs = "PercentDamaged"
                          ) {

    ## @Function RunModelLOOCV
    ## TODO: write function summary

    if (is.null(.cv_list)  || is.null(.dmg_sets)){
        stop("Please add .cv_list & dmg_sets & .res_sets")
    }

    f <- paste0(.lhs, "~", rhs)
    f <- as.formula(f)

    ## Pull out the desired dataset:
    reg_df <- .dmg_sets[[trtmnt]]

    
    ## split for CV (is this cv even legit?)
    rtrain <- reg_df[-.cv_list[[trtmnt]][[fold]], ]
    rtest <- reg_df[.cv_list[[trtmnt]][[fold]], ]
    
    ## formula
    m <- glm2(f, data = rtrain, family = "poisson")
    
    ## return predictions:
    data.frame('predicted' = predict(m, newdata = rtest, type = "response"),
               'actual' = rtest[, .lhs],
               'totalNuts' = rtest[, 'Tot_Nuts']
                   )
}


PlotLOOCVmod <- function(loocvDF, model){

    m1set <- filter(loocvDF, rhs == model)
    m1dz <- FindDangerZone(m1set)
    
    ggplot(m1set, aes(x = predicted, y = actual)) +
        geom_point(size = 2) +
            geom_polygon(data = m1dz, aes(x,y), fill = "#d8161688")

}

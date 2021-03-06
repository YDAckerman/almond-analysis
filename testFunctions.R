## These are test functions. They help me mess with the parameters
## and conditions I use with my main 'wrapper' functions.

## wrapper functions
source("~/Documents/RosenheimLab/almond/R/wrapperFunctions.R")


testRunParametricCV <- function(K = 5,
                                bins = 5,
                                parallel = FALSE,
                                splitVar = "trt2",
                                .predict = FALSE,
                                formula = NULL
                                ) {

    ## @Function testRunParamentricCV
    ##           builds a grid of parameters, each line of which
    ##           uniquely defines a model and a cross validation
    ##           fold to assess predictive accuracy. A data frame
    ##           containing a variety of metrics for model assessment
    ##           and comparsion is returned.
    ## @Param K: int, sets number of folds for CV
    ## @Param bins: int, sets number of bins to split season into before
    ##                   averaging
    ## @Param parallel: logical, turns parallelization on or off
    ## @Param splitVar: character, determines how dmg with be split
    ##                             into subsets. Those subsets will
    ##                             be used as full data sets in the
    ##                             regressions to come.
    ## @Param .predict: logical, switch to predictive or descriptive model.
    ##                           I used the . prefix to clarify that this is
    ##                           a parameter - there's a lot of prediction
    ##                           floating around in this function.
    ## @Param formula: character, base model to use in regressions. e.g.
    ##                            "cbind(n, N - n) ~ x1 + x2:x1"


    ## gives us splitVar_sets, cv_list, & val_grid
    AssembleData(test = "testRunParametricCV",
                 K = K,
                 bins = bins,
                 splitVar = splitVar
                 )

    if (parallel) {registerDoMC(cores = 4)}

    par_opts <- list(
        .export = c("splitVar_sets", "cv_list"),
        .packages = c("lme4")
        )

    results <- mdply(val_grid,
                     RunParametricCV,
                     subset_sets = splitVar_sets,
                     cv_list = cv_list,
                     predictive = .predict,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE
                     )
}


testRunParameticCVagainstResiduals <- function(K = 5,
                                               bins = 5,
                                               parallel = FALSE,
                                               subset = NULL,
                                               rescale = FALSE,
                                               response = "D",
                                               seed = 10
                                           ) {

    ## @Function testRunParamentricCVagainstResiduals
    ## TODO: write function summary

    cv_list <- dlply(dmg, .(na.omit(trt2)), FoldData, k = K, .seed = seed)
    seas_bins <- ddply(c, .(Year, Ranch, Block), BinSeason, num.bins = bins)

    if (rescale) {
        rescaled <- llply(seas_bins[, 4:(3 + bins * 3)], rescaler)
        seas_bins <- cbind(seas_bins[, 1:3], as.data.frame(rescaled))
    }

    dmg_sets <-  dlply(dmg,
                       .(na.omit(trt2)),
                       merge,
                       y = seas_bins,
                       by = c("Year", "Ranch", "Block")
                       )

    insect_vars <- llply(c("M", "F", "E"),
                         function(x) c(paste0(x, rep(1:bins)), NA)
                         )

    insect_grid <- expand.grid(insect_vars, stringsAsFactors = FALSE)
    colnames(insect_grid) <- c("V1", "V2", "V3")
    insect_grid$Model <- apply(insect_grid, 1, paste, collapse = "+")
    insect_grid$ModelID <- 1:((bins + 1)^3)


    family <- switch(response,
                     D =, I =, ID = "binomial",
                     LD =, LI =, LID = "poisson"
                     )

    res_sets <- llply(dmg_sets, GetResiduals,
                      .response = response,
                      .family = family
                      )

    val_grid <- c(insect_vars,
                     list(as.character(na.omit(unique(dmg$trt2)))),
                     list(0:K)
                     )

    val_grid <- expand.grid(val_grid, stringsAsFactors = FALSE)
    colnames(val_grid) <- c("V1", "V2", "V3", "trtmnt", "fold")

    if (!is.null(subset)) {
        val_grid <- try(subset(val_grid, eval(parse(text = subset))))
        if (identical(class(val_grid), "try-error")) {
            stop("subset must be logical")
        }
    }

    if (parallel) {registerDoMC(cores = 4)}

    par_opts <- list(.export = c("dmg_sets", "cv_list", "res_sets"))

    results <- mdply(val_grid,
                     RunParametricCVagainstResiduals,
                     .dmg_sets = dmg_sets,
                     .cv_list = cv_list,
                     .res_sets = res_sets,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE
                     )

    merge(results, insect_grid, by = c("V1", "V2", "V3"))
}


testRunSimplePredModel <-  function(K = 0,
                                    bins = 4,
                                    rescale = FALSE,
                                    seed = 10,
                                    parallel = FALSE,
                                    lhs = "PercentDamaged"
                                    ){
    ## @Function testRunSimplePredModel
    ## TODO: write function summary

    ## this gives us dmgNP, cv_list, and dmgNP_sets
    AssembleData(test = "testRunSimplePredModel",
                 K = K,
                 rescale = rescale,
                 bins = bins,
                 seed = seed
                 )

    ## this give us val_grid
    AssembleParameterCombs(test = "testRunSimplePredModel",
                        bins = bins,
                        K = K
                        )

    ## parallel:
    if (parallel) {registerDoMC(cores = 4)}

    ## TODO: make par_opts work
    par_opts <- list(.export = c("dmgNP_sets", "cv_list"))

    ## run models:
    results <- mdply(val_grid,
                     RunSimplePredModel,
                     .dmg_sets = dmgNP_sets,
                     .cv_list = cv_list,
                     .lhs = lhs,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE
                     )

}

testModelLOOCV <- function(trtmnt = "ALL",
                           models = NULL,
                           bins = 3,
                           rescale = FALSE,
                           parallel = FALSE,
                           lhs = "PercentDamaged"
                           ) {

    if (is.null(models)) {
        stop("must have models")
    }

    AssembleData(test = "testModelLOOCV",
                 rescale = rescale,
                 bins = bins,
                 trtmnt = trtmnt
                 )
    
    AssembleParameterCombs(test = "testModelLOOCV",
                           models = models,
                           rows = which(!is.na(dmgNP_sets[[trtmnt]][, lhs])),
                           trtmnt = trtmnt
                           )

    ## parallel:
    if (parallel) {registerDoMC(cores = 4)}
    par_opts <- list(.export = c("dmgNP_sets", "cv_list"))

    results <- mdply(val_grid,
                     RunModelLOOCV,
                     .dmg_sets = dmgNP_sets,
                     .cv_list = cv_list,
                     .lhs = lhs,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE
                     )
}

testRunBinomialModel <- function(K = 0,
                                 bins = 3,
                                 rescale = FALSE,
                                 seed = 10,
                                 parallel = TRUE,
                                 model = "logistic"
                                 ){

    ## this gives us dmgNP, cv_list, and dmgNP_sets
    AssembleData(test = "testRunSimplePredModel",
                 K = K,
                 rescale = rescale,
                 bins = bins,
                 seed = seed
                 )

    ## this give us val_grid
    AssembleParameterCombs(test = "testRunSimplePredModel",
                        bins = bins,
                        K = K
                        )

    ## parallel:
    if (parallel) {registerDoMC(cores = 4)}

    ## TODO: make par_opts work
    par_opts <- list(.export = c("dmgNP_sets"))

    funcVar <- switch(model,
                      logistic = RunLogisticModel,
                      binomial = RunBinomialModel,
                      svm = RunSVMModel,
                      asin = RunAsinModel,
                      betabinomial = RunBetaBinomialModel
                      )

    val_grid <- filter(val_grid[, c('rhs', 'trtmnt')], trtmnt == "ALL")
    results <- mdply(val_grid[1,],
                     funcVar,
                     .dmg_sets = dmgNP_sets,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE
                     )

    attr(results, "model") <- model

    results
}

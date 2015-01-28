testRunParametricCV <- function(K = 5, bins = 5, parallel = FALSE) {

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
                     RunParametricCV,
                     dmg_sets = dmg_sets,
                     cv_list = cv_list,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE)
}



testRunParameticCVwithResiduals<- function(K = 5, bins = 5, parallel = FALSE) {

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
                     RunParametricCVwithResiduals,
                     dmg_sets = dmg_sets,
                     cv_list = cv_list,
                     .progress = "text",
                     .parallel = parallel,
                     .paropts = par_opts,
                     .inform = TRUE)
}


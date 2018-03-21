library(doParallel)
library(tidyverse)
library(parallel)
library(sticer) # devtools::install_github("lnalborczyk/sticer", dependencies = TRUE)

#cohensd = 0.8; nmin = 20; nmax = 500; boundary = exp(1); nsims = 100; ic = aic; cores = 2; batch = 1; b = 1;

simER2 <- function(
    cohensd = 0, nmin = 20, nmax = 200, boundary = 10,
    nsims = 20, ic = aic, cores = 2) {

    if (nmin == 0) {

        stop("nmin should be a positive integer")

    }

    if (nmin > nmax) {

        stop("n should be superior to nmin")

    }

    if (nsims %% cores != 0) {

        stop("nsims should be dividable by cores")

    }

    start <- Sys.time()
    print(paste0("Simulation started at ", start) )

    registerDoParallel(cores)

    sim <-
        foreach(
            batch = 1:getDoParWorkers(), .combine = rbind) %dopar% {

                max_b <- round(nsims / getDoParWorkers() )
                res.counter <- 1

                res <-
                    matrix(NA,
                        nrow = length(nmin:nmax) * max_b, ncol = 6,
                        dimnames =
                            list(
                                NULL,
                                c("id", "true.ES", "boundary", "n", "ER", "success") ) )

                for (b in 1:max_b) {

                    x <- cbind(rnorm(nmax / 2, 0, 1), rep(-0.5, nmax / 2) )
                    y <- cbind(rnorm(nmax / 2, cohensd, 1), rep(0.5, nmax / 2) )

                    df_pop <-
                        rbind(y, x) %>%
                        as.data.frame %>%
                        set_names(c("value", "group") ) %>%
                        sample_n(nrow(.) )

                    res0 <-
                        matrix(
                            NA, nrow = length(nmin:nmax), ncol = ncol(res),
                            dimnames = dimnames(res) )

                    for (i in nmin:nmax) {

                        samp <- df_pop[1:i, ]

                        mod1 <- lm(value ~ 1, data = samp)
                        mod2 <- lm(value ~ group, data = samp)

                        mods <- list(mod1 = mod1, mod2 = mod2)
                        model_comp <- ictab(mods, ic)

                        ER <-
                            model_comp$ic_wt[model_comp$modnames == "mod2"] /
                            model_comp$ic_wt[model_comp$modnames == "mod1"]

                        if (ER > boundary | ER < 1 / boundary) {

                            out_x <- cbind(rnorm(nmax / 2, 0, 1), rep(-0.5, nmax / 2) )
                            out_y <- cbind(rnorm(nmax / 2, cohensd, 1), rep(0.5, nmax / 2) )

                            out_df_pop <-
                                rbind(out_y, out_x) %>%
                                as.data.frame %>%
                                set_names(c("value", "group") ) %>%
                                sample_n(nrow(.) )

                            out_samp <- out_df_pop[1:i, ]

                            # computing out-of-sample deviance
                            n <- nobs(mod1)
                            sigma1 <- sigma(mod1)
                            sigma2 <- sigma(mod2)
                            sigmaML1 <- sigma1 * sqrt( (n - dim(model.matrix(mod1) )[2]) / n)
                            sigmaML2 <- sigma2 * sqrt( (n - dim(model.matrix(mod2) )[2]) / n)

                            #####################################################################################
                            # https://stats.stackexchange.com/questions/73196/recalculate-log-likelihood-from-a-simple-r-lm-model
                            # ll1 <- sum(log(dnorm(x = samp$value, mean = predict(mod1), sd = sigma.ML) ) )
                            # should be equivalent to logLik(mod1)
                            ########################################################

                            # out-of-sample log-likelihood
                            ll1 <- sum(log(dnorm(x = out_samp$value, mean = coef(mod1)[1], sd = sigmaML1) ) )
                            ll2 <- sum(log(dnorm(x = out_samp$value, mean = coef(mod2)[1] + coef(mod2)[2] * out_samp$group, sd = sigmaML2) ) )
                            # out-of-sample deviance
                            outdev1 <- -2 * ll1
                            outdev2 <- -2 * ll2

                            # in-sample deviance (just for information)
                            indev1 <- -2 * logLik(mod1)
                            indev2 <- -2 * logLik(mod2)

                            if (ER < 1 & outdev1 < outdev2 | ER > 1 & outdev1 > outdev2) {

                                # if out-of-sample deviance -based model comparison is coherent
                                # with the ER, recording it as a success (coded as 1)
                                success <- 1

                            } else {

                                # otherwise, recording it as a failure (coded as 0)
                                success <- 0

                            }

                            #break;

                        } else {

                            success <- 0.5

                        }

                        res0[which(nmin:nmax == i), ] <-
                            c(
                                # id is a unique id for each trajectory
                                id = batch * 10 ^ (floor(log(max_b, base = 10) ) + 2) + b,
                                true.ES	= cohensd,
                                boundary = boundary,
                                n = i,
                                ER = ER,
                                success = success
                            )

                    } # end of i

                    res[res.counter:(res.counter + nrow(res0) - 1), ] <- res0
                    res.counter <- res.counter + nrow(res0)

                } # end of b's

                batch <- NULL
                return(res)

            } # end of %dopar%

    res <- data.frame(sim, stringsAsFactors = FALSE)

    class(res) <- c("simER", "data.frame")

    end <- Sys.time()
    print(paste0("Simulation finished at ", end) )
    cat("Duration: ")
    print(end - start)

    return(res)

}

############################################################################################
# testing it, for different thresholds and different effect sizes
# nb: we should also consider the nmin (lot of errors at low n)...
#########################################################################

# for different effect sizes
ds <- seq(0, 0.9, by = 0.1)

# for different thresholds
ts <- c(1, exp(1), 2 * exp(1), 10)

# WARNING: this simulation can take a while to run...

for (i in ds) {

    for (j in ts) {

        sim0 <-
            simER2(
                cohensd = i, nmin = 20, nmax = 1e3, boundary = j,
                nsims = 1e2, ic = aic, cores = 4
            )

        if (!exists("sim1") ) sim1 <- sim0 else sim1 <- rbind(sim1, sim0)

    }

    if (!exists("sim") ) sim <- sim1 else sim <- rbind(sim, sim1)
    print(i)

}

res <-
    sim %>%
    # keeping only boundary hits
    filter(success != 0.5) %>%
    group_by(id, true.ES, boundary) %>%
    # keeping only the first boundary hit
    filter(n == min(n) ) %>%
    ungroup() %>%
    data.frame

# probability of being succesful in selecting the model having the lowest out-of-sample deviance,
# depending on the boundary (lower versus upper), and the population effect size

res %>%
    mutate(b = ifelse(ER < 1, "lower", "upper") ) %>%
    group_by(true.ES, boundary, b) %>%
    summarise(m = mean(success) ) %>%
    data.frame

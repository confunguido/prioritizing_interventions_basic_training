fit_R0_gam <- function(data_R0_in, target_size){
    fit_gam = gam(R0_mag ~ s(outbreak_size), data = data_R0_in)
    beta = coef(fit_gam)
    Vb = vcov(fit_gam)
    num_beta_vecs = 10000
    Cv = chol(Vb)
    nus = rnorm(num_beta_vecs * length(beta))
    beta_sims = beta + (t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs))
    d_beta = cbind(summary(fit_gam)$se, apply(beta_sims, 1, sd))

    n_obs = 10000
    sim_idx = sample.int(nrow(data_R0_in), size = n_obs, replace = TRUE)
    sim_dat = data.frame(outbreak_size = data_R0_in[sim_idx, c("outbreak_size")])

    covar_sim = predict(fit_gam, newdata = data.frame(outbreak_size = target_size), type = "lpmatrix")
    linpred_sim = covar_sim %*% beta_sims

    invlink = function(x) x
    exp_val_sim = invlink(linpred_sim)

    y_sim = matrix(rnorm(n = prod(dim(exp_val_sim)), 
                         mean = exp_val_sim, 
                         sd = summary(fit_gam)$scale), 
                   nrow = nrow(exp_val_sim), 
                   ncol = ncol(exp_val_sim))
    pred_int_sim = apply(y_sim, 1, quantile, prob = c(.025,0.5,  0.975))
    return(pred_int_sim)
}

##==================================================#
## Get R0 intervals----------
##==================================================#
testpos.time = read.csv('./calibration_sims.csv', stringsAsFactors = F)

tmp_df_FB = testpos.time[testpos.time$time == 22,]
tmp_df_FB$outbreak_size = tmp_df_FB$FB
tmp_df_FLW = testpos.time[testpos.time$time == 19,]
tmp_df_FLW$outbreak_size = tmp_df_FLW$FLW

set.seed(123456)
(fit_R0_gam(tmp_df_FB, 142))
(fit_R0_gam(tmp_df_FLW, 70))

# loading example data ----
## format: list of data.frames; each data.frame = predicate and argument pairs from a single document
## each coulmn for predicates and arguments
load("example_content.rda")

# loading helper functions ----
source("functions_helper.R")

library(Rcpp)
library(RcppArmadillo)
sourceCpp("functions_biTM_gibbs.cpp")


# preprocessing ---- 
## splitting data.frames to vocab and ids
exCont_dep <- lapply(example_content, function(df){df[, "dependent"]})
exCont_gov <- lapply(example_content, function(df){df[, "governor"]})

## ACTUAL data structure that is going to be used in training biTM models
### unique words
exCont_dep_voc <- unique(unlist(exCont_dep))
exCont_gov_voc <- unique(unlist(exCont_gov))

### vector of word ids per document
exCont_dep_id <- words_to_id(exCont_dep, exCont_dep_voc)
exCont_gov_id <- words_to_id(exCont_gov, exCont_gov_voc)


# training TM ----
## initializing the topic model object with 0s
## set hyperparameters: (alpha, beta_dep, beta_gov, K, iter)
mod_init_zero <- biTM_init_zeros(0.0, 0.01, 0.01, 20, 500, exCont_dep_voc, exCont_gov_voc, exCont_dep_id, exCont_gov_id)

## fill random numbers instead of 0s for Gibbs sampling
mod_init_rand <- biTM_init_rands(mod_init_zero$doc_id, mod_init_zero$ta_dep_gov, exCont_dep_id, exCont_gov_id, mod_init_zero$dep_t, mod_init_zero$gov_t, mod_init_zero$K)

## do collapsed Gibbs samling
mod_gibbs  <- biTM_colGibbs(mod_init_zero$alpha, mod_init_zero$beta_dep, mod_init_zero$beta_gov, mod_init_zero$K, mod_init_zero$no_iter, mod_init_zero$doc_id, 
                            exCont_dep_id, exCont_gov_id, exCont_dep_voc, exCont_gov_voc,
                            mod_init_rand$doc_topic, mod_init_rand$ta_dep_gov, mod_init_rand$dep_t, mod_init_rand$gov_t)


# posterior distribution ----
mod_post <- biTM_posterior(mod_gibbs$doc_topic, mod_gibbs$dep_t, mod_gibbs$gov_t, exCont_dep_voc, exCont_gov_voc,
                           mod_init_zero$alpha, mod_init_zero$beta_dep, mod_init_zero$beta_gov)


# descriptive results ----
## top 5 terms per topic
### dependent words (e.g., object nouns)
apply(mod_post$phi_dep, 1, function(row){colnames(mod_post$phi_dep)[order(row, decreasing = TRUE)[1:5]]})

### governing words (e.g., action verbs)
apply(mod_post$phi_gov, 1, function(row){colnames(mod_post$phi_gov)[order(row, decreasing = TRUE)[1:5]]})

## top 5 verbs for a noun
govs_for_dep1('function', exCont_dep_voc, exCont_gov_voc, mod_post$phi_dep, mod_post$phi_gov, 5, TRUE)
govs_for_dep1('variable', exCont_dep_voc, exCont_gov_voc, mod_post$phi_dep, mod_post$phi_gov, 5, TRUE)
govs_for_dep1('class',    exCont_dep_voc, exCont_gov_voc, mod_post$phi_dep, mod_post$phi_gov, 5, TRUE)

## top 5 nouns for a verb
deps_for_gov1('get',      exCont_dep_voc, exCont_gov_voc, mod_post$phi_dep, mod_post$phi_gov, 5, TRUE)
deps_for_gov1('write',    exCont_dep_voc, exCont_gov_voc, mod_post$phi_dep, mod_post$phi_gov, 5, TRUE)
deps_for_gov1('submit',   exCont_dep_voc, exCont_gov_voc, mod_post$phi_dep, mod_post$phi_gov, 5, TRUE)



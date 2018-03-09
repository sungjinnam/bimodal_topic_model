words_to_id <- function(li_words, vocab){
  li_id <- lapply(li_words, function(li){
    match(li, vocab)
  })
  return(li_id)
}

biTM_posterior <- function(dt, dep_t, gov_t, dep_q_train.vocab, gov_q_train.vocab,
                           alpha, beta_dep, beta_gov, 
                           add_folder=FALSE, fld_t=NULL, beta_fld=NULL, folder_label=NULL, 
                           add_blm=FALSE, blm_t=NULL, beta_bm=NULL, blm_label=NULL){
  theta <- (dt+alpha) / rowSums(dt+alpha)                      # topic probabilities per document
  phi_dep <- (dep_t+beta_dep) / rowSums(dep_t+beta_dep)        # topic probabilities per dep word
  phi_gov <- (gov_t+beta_gov) / rowSums(gov_t+beta_gov)        # topic probabilities per gov word
  colnames(phi_dep) <- dep_q_train.vocab
  colnames(phi_gov) <- gov_q_train.vocab
  
  phi_fld <- NULL
  phi_blm <- NULL
  if(add_folder){
    phi_fld <- (fld_t+beta_fld) / rowSums(fld_t+beta_fld)
    colnames(phi_fld) <- folder_label
  }
  if(add_blm){
    phi_blm <- (blm_t+beta_blm) / rowSums(blm_t+beta_blm)
    colnames(phi_blm) <- blm_label
  }
  return(list(theta=theta, phi_dep=phi_dep, phi_gov=phi_gov, phi_fld=phi_fld, phi_blm=phi_blm))
}


govs_for_dep1 <- function(dep_word, dep_vocab, gov_vocab, mult_p_dep, mult_p_gov, topn, most_similar=TRUE){
  tw_id <- which(dep_vocab==dep_word)
  probs <- mult_p_dep[,tw_id] %*% mult_p_gov
  govs <- as.vector(gov_vocab[order(probs, decreasing=most_similar)[1:topn]])
  # print(paste("verbs for the dep_noun:", dep_word))
  # return(verbs)
  return(list(probs=probs, top_govs=govs))
}

deps_for_gov1 <- function(gov_word, dep_vocab, gov_vocab, mult_p_dep, mult_p_gov, topn, most_similar=TRUE){
  tq_id <- which(gov_vocab==gov_word)
  probs <- mult_p_gov[,tq_id] %*% mult_p_dep
  deps  <- as.vector(dep_vocab[order(probs, decreasing=most_similar)[1:topn]])
  # print(paste("dep_nouns for the verb:", verb_word))
  return(list(probs=probs, top_deps=deps))
}

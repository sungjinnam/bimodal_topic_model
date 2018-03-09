#include <typeinfo>
#include <iostream>
// #include <Rcpp.h>
#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// 
// 
// 
// IntegerVector rowsum_cpp(IntegerMatrix x) {
//   int n_col = x.ncol();
//   int n_row = x.nrow();
//   
//   IntegerVector out(n_row);
//   for (int i=0; i < n_row; i++){
//     out[i] = sum(x(i, _));
//   }
//   return out;
// }


// [[Rcpp::export]]
List biTM_init_zeros (const float alpha, const float beta_dep, const float beta_gov, const int K, const int no_iter,
                     const List dep_vocab, const List gov_vocab, const List dep_id, const List gov_id){
  List ret;
  
  // returning: hyperparameters
  ret["K"] = K;
  alpha == 0.0 ? ret["alpha"] = 50/K : ret["alpha"] = alpha;
  ret["beta_dep"] = beta_dep;
  ret["beta_gov"] = beta_gov;
  ret["no_iter"] = no_iter;
  
  // term-topic assignments
  IntegerMatrix dep_t(K, dep_vocab.size());
  IntegerMatrix gov_t(K, gov_vocab.size());
  List ta_dep_gov(dep_id.size());
  for(int i=0; i < dep_id.size(); i++){
    IntegerVector temp_dep_id = dep_id[i];
    IntegerVector temp_zero_id (temp_dep_id.size(), 0);
    ta_dep_gov[i] = temp_zero_id;
  }

  // documentID
  IntegerVector doc_id = seq_len(dep_id.size());

  // returning rest of the output
  ret["dep_t"] = dep_t;
  ret["gov_t"] = gov_t;
  ret["ta_dep_gov"] = ta_dep_gov;
  ret["doc_id"] = doc_id;
  
  return ret;
}

// [[Rcpp::export]]
List biTM_init_rands (IntegerVector& doc_id, List& ta_dep_gov, List& dep_id, List& gov_id, IntegerMatrix& dep_t, IntegerMatrix& gov_t, int& K){
  List ret;
  
  IntegerVector K_seq = seq_len(K);
  IntegerMatrix doc_topic(ta_dep_gov.size(), K); 

  for(int d=0; d < doc_id.size(); d++){
    IntegerVector temp_term_topics = ta_dep_gov[d];
    IntegerVector temp_dep_id = dep_id[d];
    IntegerVector temp_gov_id = gov_id[d];

    // term-topic assignments
    for(int wq=0; wq < temp_term_topics.size(); wq++){
      temp_term_topics[wq] = (RcppArmadillo::sample(K_seq, 1, false)[0]);
      int temp_topic_idx = temp_term_topics[wq]-1;
      int dep_idx = temp_dep_id[wq]-1;
      int gov_idx = temp_gov_id[wq]-1;
    
      dep_t(temp_topic_idx, dep_idx) = dep_t(temp_topic_idx, dep_idx) + 1;
      gov_t(temp_topic_idx, gov_idx) = gov_t(temp_topic_idx, gov_idx) + 1;
    }

    // document-topic assignments
    for(int k=0; k < K_seq.size(); k++){
      // std::cout << "d: " << d << ", k: "<< k << ", k_count: " <<std::count(temp_term_topics.begin(), temp_term_topics.end(), k) << std::endl;
      // doc_topic(d, k) = 0;
      doc_topic(d, k) = std::count(temp_term_topics.begin(), temp_term_topics.end(), k + 1);
    }
    ta_dep_gov[d] = temp_term_topics;
  }
  
  // returning the output
  ret["ta_dep_gov"] = ta_dep_gov;
  ret["dep_t"] = dep_t;
  ret["gov_t"] = gov_t;
  ret["doc_topic"] = doc_topic;
  
  return ret;
}

// [[Rcpp::export]]
List biTM_colGibbs (float alpha, float beta_dep, float beta_gov, int K, int no_iter,
                   IntegerVector doc_id, List dep_id, List gov_id, List dep_vocab, List gov_vocab,
                   IntegerMatrix doc_topic, List ta_dep_gov, IntegerMatrix dep_t, IntegerMatrix gov_t){
  List ret;

  IntegerVector K_seq = seq_len(K);

  for (int i=0; i < no_iter; i++){
    for (int d=0; d<doc_id.size(); d++){
      IntegerVector temp_term_topics = ta_dep_gov[d];
      IntegerVector temp_dep_id = dep_id[d];
      IntegerVector temp_gov_id = gov_id[d];

      for (int wq=0; wq < temp_term_topics.size(); wq++){
        // subtract the topic assignment count for randomly assigned topic
        int t0_idx  = temp_term_topics[wq] - 1;
        int dep_idx = temp_dep_id[wq] - 1;
        int gov_idx = temp_gov_id[wq] - 1;

        doc_topic(d, t0_idx) = doc_topic(d, t0_idx) - 1;
        dep_t(t0_idx, dep_idx) = dep_t(t0_idx, dep_idx) - 1;
        gov_t(t0_idx, gov_idx) = gov_t(t0_idx, gov_idx) - 1;

        // updating the topic assignment for each word by sampling
        float denom_a = sum(doc_topic(d,_)) + K * alpha;
        // NumericVector denom_b(K);
        // NumericVector denom_c(K);
        NumericVector p_z(K);

        for(int kk=0; kk < K; kk++){
          float denom_b = sum(dep_t(kk,_)) + dep_vocab.size() * beta_dep;
          float denom_c = sum(gov_t(kk,_)) + gov_vocab.size() * beta_gov;
          p_z[kk] = ((dep_t(kk,dep_idx) + beta_dep) / denom_b) * ((gov_t(kk,gov_idx) + beta_gov) / denom_c) * ((doc_topic(d,kk) + alpha) / denom_a);
        }
        // std::cout << p_z << std::endl;

        int t1_idx = (RcppArmadillo::sample(K_seq, 1, false, p_z)[0] - 1);

        temp_term_topics[wq] = t1_idx + 1;
        doc_topic(d, t1_idx) = doc_topic(d, t1_idx) + 1;
        dep_t(t1_idx, dep_idx) = dep_t(t1_idx, dep_idx) + 1;
        gov_t(t1_idx, gov_idx) = gov_t(t1_idx, gov_idx) + 1;
      }
    }

    bool verbose=true;
    if(verbose){
    if(i % 100 == 1){
      std::time_t result = std::time(0);
      std::cout << "@iter:" << i << "/" << no_iter << "___" <<  std::asctime(std::localtime(&result)) << std::endl;
      }
    }
  }
  
  ret["doc_topic"] = doc_topic;
  ret["dep_t"] = dep_t;
  ret["gov_t"] = gov_t;
  return ret;
}



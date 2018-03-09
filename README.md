# Bimodal Topic Model for dependency pairs
Implementation of bimodal topic model in `R`

## Relevant literature
- Collapsed Gibbs sampling
    - Steyvers, M., & Griffiths, T. (2007). Probabilistic topic models. Handbook of latent semantic analysis, 427(7), 424-440. [pdf](http://173.236.226.255/tom/papers/SteyversGriffiths.pdf)
- Bimodal topic model
    - Wang, S., Roller, S., & Erk, K. (2017). Distributional model on a diet: One-shot word learning from text only. arXiv preprint arXiv:1704.04550. [link](https://arxiv.org/abs/1704.04550)
    - Roller, S., & Im Walde, S. S. (2013). A multimodal LDA model integrating textual, cognitive and visual modalities. In Proceedings of the 2013 Conference on Empirical Methods in Natural Language Processing (pp. 1146-1157). [pdf](http://www.aclweb.org/anthology/D13-1115)

## Goal
- To produce Bayesian clustering results of predicates and argument pairs from text
- Interpretable and richer representation of text clustering results

## Codes
- `example_biTM.R`: example analysis script with small toy data
- `functions_biTM_gibbs.cpp`: Rcpp implementation of the bimodal topic model with collapsed Gibbs sampling
- `functions_helper.R`: helper functions for preprocessing and posterior inference

## TODO
- Optimize the performance with alternative object structures, e.g., `quanteda`: objects for sparse matrix
- Collapse biTM training process into a single function

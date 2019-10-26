require(R6)
require(hash)
require(mvtnorm)

NaiveBayesClassifier <- R6Class('NaiveBayesClassifier',
  list(
    means = hash(),
    sigmas = hash(),
    priors = hash(),
    K = NULL,
    P = NULL,

    fit = function(X, y, naive = 0){
      self$K <- unique(y)

    
      for (k in self$K) {
        X_k <-matrix(X[y==k], ncol = ncol(X))
        self$means[[toString(k)]] <- colMeans(X_k)
        self$sigmas[[toString(k)]] <- (1 - naive) * cov(X_k) + naive * diag(diag(cov(X_k)))
        self$priors[[toString(k)]] <- length(X_k)/length(X)
    }
    
    },
    ## Predict definition 
    predict = function(X, user_pdf = dmvnorm){
      self$P <- matrix(0, ncol = length(self$K), nrow = nrow(X))
    
       for (k in self$K) {
          self$P[,k+1] <- user_pdf(X, self$means[[toString(k)]], self$sigmas[[toString(k)]]) * self$priors[[toString(k)]]
       }
      return(apply(self$P, 1, which.max) - 1)
        
    }
  )
)

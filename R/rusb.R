#' rusboost
#'
#' Implement RUSBoost algorithm to deal with unbalanced classes in classification problems
#' @param formula Formula for \code{rpart} models.
#' @param data Input dataset.
#' @param boot Use bootstrap sampling? (After undersampling phase)
#' @param iters How many classifiers to train.
#' @param coeflearn From \code{adabag}: "if 'Breiman'(by default), alpha=1/2ln((1-err)/err) is used. If 'Freund' alpha=ln((1-err)/err) is used. In both cases the AdaBoost.M1 algorithm is used and alpha is the weight updating coefficient. On the other hand, if coeflearn is 'Zhu' the SAMME algorithm is implemented with alpha=ln((1-err)/err)+ ln(nclasses-1)."
#' @param control Parameters used in \code{rpart} calls.
#' @param sampleFraction Percentage of each iteration's sample which should be in the \emph{negative} class. Required. 
#' @param idx Logical index vector indicating which observations in \code{data} are negative.
#' @keywords rusb
#' @export rusb
#' @examples
#' idx <- data$outcome=="N"
#' test.rusboost <- rusb(outcome ~ ., data = testdata, boot = F, iters = 20, sampleFraction = .01, idx = idx)


rusb <- function (formula, data, boot = FALSE, iters = 100, coeflearn = "Breiman", 
          control, sampleFraction, idx) 
{
  if (!(as.character(coeflearn) %in% c("Freund", "Breiman", 
                                       "Zhu"))) {
    stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
  }
  formula <- as.formula(formula)
  vardep <- data[, as.character(formula[[2]])]
  vardep <- vardep[[1]]
  n <- nrow(data)
  indices <- 1:n
  n.negative <- sum(idx)
  negatives <- data[idx,]
  nclasses <- nlevels(vardep)
  trees <- list()
  mweights <- rep(0, iters)

  # Initialize weights. Each 'w'[1:n] is an entry into the overall weights--matrix.weights[w, m].
  w <- rep(1/n, n)
  # Initialize weights matrix
  matrix.weights <- array(0, c(n, iters))
  
  ################ This is where the model hypotheses are created
  for (m in 1:iters) {
    # create a subset index from the 1:n vector, using ALL "Y"s and a SAMPLE of "N"s
    subset.index <- c(
                      sample(indices[idx], n.negative*sampleFraction, replace = FALSE),  
                      indices[!idx])
    # this is where the sample is subset in each iteration
    tmp.sample <- data[subset.index,]
    tmp.weights <- w[subset.index]
    t.s.l <- length(tmp.sample[,1])
    
    ### Fit the model using rpart
    if (boot == TRUE) {
      inner.tmp.weights <<- tmp.weights  ### not sure what the scoping rule is here, but this is necessary
      # Bootstrap samples from the temporary sample -- this probably isn't necessary most of the time.
      bootstrap <- sample(1:t.s.l, replace = TRUE, prob = tmp.weights) #not sure if weights should be used, but they are in the adabag function.
      # train model on undersampled data
      fit <- rpart(formula, data = tmp.sample[bootstrap, ], 
                   control = control, weights = inner.tmp.weights)
      # save predicteded class from model fit. ***make predictions using the FULL sample
      flearn <- predict(fit, newdata = data, type = "class")
      # errors are a simple count of wrong predictions. 
      # This is called "pseudo loss" because its calculated on all observations, including ones which weren't in the model training.
      ind <- as.numeric(vardep != flearn)
      # create overall error metric, which is simply vector multiplication of (observation weight)*(1 or 0)
      err <- as.numeric(w %*% ind)           
    }
    if (boot == FALSE) {
      inner.tmp.weights <<- tmp.weights
      # see notes on these steps above, in the bootstrap case
      fit <- rpart(formula = formula, data = tmp.sample, 
                   weights = inner.tmp.weights, control = control)
      flearn <- predict(fit, newdata = data, type = "class")
      ind <- as.numeric(vardep != flearn)
      err <- as.numeric(w %*% ind)           
    }
    
    # Compute this iteration's training error metric: c = f(total loss).
    c <- log((1 - err)/err)
    if (coeflearn == "Breiman") {
      c <- (1/2) * c
    }
    if (coeflearn == "Zhu") {
      c <- c + log(nclasses - 1)
    }
    
    ### In the first time through (m=1), w is just 1/n for all obs. Later, a subset of w will have changed.
    matrix.weights[, m] <- w   
    
    ### Key step: recalculate weights, using the same error penalty for each misclassified example
    # So what this does is take current weights and uses a function of them
    # to update the overall weights vector, for use in the next iteration.
    
    # This only applies to misclassified examples; properly identified ones stay the same.
    update.vector <- w * exp(c * ind)
    
    # Change the parts of the weight vector which need to be updated.
    w[ind==1] <- update.vector[ind==1]   
      
    
    #### Normalize weights once that's done (so the weights become proportions).
    w <- w/sum(w)
    maxerror <- 0.5
    eac <- 0.001
    if (coeflearn == "Zhu") {
      maxerror <- 1 - 1/nclasses
    }
    
    # Handle 
    # If the total loss is greater than .5, then an inverted hypothesis would work better.
    if (err >= maxerror) {
      weights <- rep(1/n, n)
      maxerror <- maxerror - eac
      c <- log((1 - maxerror)/maxerror)
      if (coeflearn == "Breiman") {
        c <- (1/2) * c
      }
      if (coeflearn == "Zhu") {
        c <- c + log(nclasses - 1)
      }
    }
    # If predictions are perfect, then c = is a constant -3.45...
    if (err == 0) {
      c <- log((1 - eac)/eac)
      if (coeflearn == "Breiman") {
        c <- (1/2) * c
      }
      if (coeflearn == "Zhu") {
        c <- c + log(nclasses - 1)
      }
    }
    
    # Model hypotheses are stored in 'trees' vector
    trees[[m]] <- fit
    # Models' weights are stored in 'mweights' vector
    mweights[m] <- c
  }
  ################ end model hypotheses creation
  
  # Normalize model weights (so the weights become proportions)
  mweights <- mweights/sum(mweights)
    
  ### simultaneously create 'ensemble' of predictions & variable importance array
  # initialize empty vectors
  pred <- data.frame(rep(0, n))
  #   nvar <- dim(varImp(trees[[1]], surrogates = FALSE, competes = FALSE))[1]
  nvar <- length(data[1,])-1 ##### replace this if I'm able to use the above line.
  imp <- array(0, c(iters, nvar))

  for (m in 1:iters) {
    # predictions
    if (m == 1) {
      pred <- predict(trees[[m]], data, type = "class")
    }
    else {
      pred <- data.frame(pred, predict(trees[[m]], data, type = "class"))
    }
    # importance statistics
#     k <- varImp(trees[[m]], surrogates = FALSE, competes = FALSE)
#     imp[m, ] <- k[sort(row.names(k)), ]
  }

  # derive final prediction from the above ensemble
  classfinal <- array(0, c(n, nlevels(vardep)))
  # for each possible class...
  for (i in 1:nlevels(vardep)) {
    # ...output a prediction score, which is a weighted combination of each models' prediction.
    classfinal[, i] <- matrix(as.numeric(pred == levels(vardep)[i]), 
                              nrow = n) %*% as.vector(mweights)
  }
  
  # 'votes' is the same info in 'classfinal', expressed as a proportion
  votes <- classfinal/apply(classfinal, 1, sum)
  

  # output one predicted class (the class with the largest share of weighted model votes)
  predclass <- rep("O", n)
  for (i in 1:n) {
    predclass[i] <- as.character(levels(vardep)[(order(classfinal[i,], decreasing = TRUE)[1])])
  }


#   imphyp <- as.vector(as.vector(mweights) %*% imp)
#   imphyp <- imphyp/sum(imphyp) * 100
#   names(imphyp) <- sort(row.names(k))


  ans <- list(formula = formula, trees = trees, weights = mweights, 
              votes = classfinal, prob = votes, class = predclass) #, 
#               importance = imphyp)
  class(ans) <- "rusb"
  ans
}

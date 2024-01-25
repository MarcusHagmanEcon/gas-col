# Calculates the cumulative response by adding up coefficient estimates
#and estimates standard errors of the cumulative responses using the delta method

cumulative_response_sym <- function(model){
  
  lambda_index <- "lag_e5" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  lambda <- model$coefficients[lambda_index]
  
  phi_x_lambda_index <- "lag_oil" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  phi <- - model$coefficients[phi_x_lambda_index] / lambda
  
  
  gamma_indices <- "diff_e5" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  # Gamma indices are offset by one relative to what's in the paper
  gamma_vec <- c( 10^9, model$coefficients[gamma_indices] )
  
  beta_indices <- "diff_oil" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  # Beta indices are offset by one relative to what's in the paper
  beta_vec <- model$coefficients[beta_indices]
  
  lag_e5_index <- "lag_e5" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  lag_oil_index <- "lag_oil" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  
  result <- data.frame(rep(NA, length(beta_indices) ), rep(NA, length(beta_indices)))
  names(result) <- c("coef", "se")
  
  result$coef[1] <- beta_vec[1] #(2)
  
  result$coef[2] <- 
    result$coef[1] + #(1)
    beta_vec[2] + #(2)
    gamma_vec[2] * result$coef[1] #(3)
    lambda * (result$coef[1] - phi) #(4)
  
  for (t in 3:length(beta_vec)){
    result$coef[t] <- 
      result$coef[t - 1] + #(1)
      beta_vec[t+1] + #(2)
      sum( gamma_vec[(t-1):2] *
             (result$coef[2:(t - 1)] - result$coef[1:(t-2)] ) ) + #(3)
      gamma_vec[t + 1] * result$coef[1] + #(3)
      lambda * (result$coef[t - 1] - phi)
  }
  
  # The matrix of derivatives depends on the partial derivatives with respect to
  #parameters and previous CRFs denote them as crf_tau_partial and crf_crf_partial
  
  # crf_tau_partial
  crf_tau_partial <- matrix(0, ncol = length(gamma_vec), nrow = length(model$coefficients)) %>%
    as.data.frame()
  for (t in 1:length(gamma_vec)){
    # betas
    crf_tau_partial[beta_indices[t],t] <- 1
    # gammas
    if (t > 1){
      crf_tau_partial[gamma_indices[t-1],t] <- result$coef[1]
      if(t > 2){
        crf_tau_partial[gamma_indices[1:(t-2)],t] <- (result$coef[(t-1):2] - result$coef[(t-2):1])
      }
    }
    # lambda
    if (t > 1){
      crf_tau_partial[lambda_index,t] <- result$coef[t-1] - phi
    }
    # phi x lambda
    if (t > 1){
      crf_tau_partial[phi_x_lambda_index,t] <- -1
    }
  }
  
  # crf_crf_partial
  crf_crf_partial <-  matrix(0, ncol = length(gamma_vec), nrow = length(gamma_vec)) %>%
    as.data.frame()
  for (t in 2:length(gamma_vec)){
    crf_crf_partial[t-1, t] <- 1 + lambda - gamma_vec[2]
    if (t > 2){
      crf_crf_partial[(t-2):1,t] <- gamma_vec[2:(t-1)] - gamma_vec[3:t]
    }
  }
  
  # crf_tau
  crf_tau_full <- matrix(0, ncol = length(gamma_vec), nrow = length(model$coefficients)) %>%
    as.data.frame()
  for (c in 1:length(gamma_vec)){
      crf_tau_full[,c] <-  crf_tau_partial[,c] + 
        as.matrix(crf_tau_partial) %*% as.matrix(crf_crf_partial[,c])
  }
  
  result$se <- sqrt(diag(t(as.matrix(crf_tau_full)) %*% 
                           vcov(model) %*%
                           as.matrix(crf_tau_full)))
  
  result$lag <- 1:nrow(result) - 1
  return(result)
}

cumulative_response_asym <- function(model, direction){
  
  lambda_index <- "lag_e5" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  lambda <- model$coefficients[lambda_index]
  
  phi_x_lambda_index <- "lag_oil" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  phi <- - model$coefficients[phi_x_lambda_index] / lambda
  
  
  gamma_pos_indices <- "diff_e5_pos" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  # Gamma indices are offset by one relative to what's in the paper
  gamma_pos_vec <- c( 10^9, model$coefficients[gamma_pos_indices] )
  
  gamma_neg_indices <- "diff_e5_neg" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  # Gamma indices are offset by one relative to what's in the paper
  gamma_neg_vec <- c( 10^9, model$coefficients[gamma_neg_indices] )
  
  beta_indices <- paste0("diff_oil_", direction) %>% grepl( model$coefficients %>% rownames() ) %>% which()
  # Beta indices are offset by one relative to what's in the paper
  beta_vec <- model$coefficients[beta_indices]
  
  lag_e5_index <- "lag_e5" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  lag_oil_index <- "lag_oil" %>% grepl( model$coefficients %>% rownames() ) %>% which()
  
  result <- data.frame(rep(NA, length(beta_indices) ), rep(NA, length(beta_indices)))
  names(result) <- c("coef", "se")
  
  result$coef[1] <- beta_vec[1] #(2)
  
  result$coef[2] <- 
    result$coef[1] + #(1)
    beta_vec[2] + #(2)
    ifelse(result$coef[1]>0, gamma_pos_vec[2], gamma_neg_vec[2]) * result$coef[1] #(3)
  lambda * (result$coef[1] - phi) #(4)
  
  for (t in 3:length(beta_vec)){
    
    result$coef[t] <- 
      result$coef[t - 1] + #(1)
      beta_vec[t+1] + #(2)
      sum( ifelse(result$coef[2:(t - 1)] - result$coef[1:(t-2)]>0,
                  gamma_pos_vec[(t-1):2], gamma_neg_vec[(t-1):2])  *
             (result$coef[2:(t - 1)] - result$coef[1:(t-2)] ) ) + #(3)
      ifelse(result$coef[1]>0, gamma_pos_vec[t+1], gamma_neg_vec[t+1]) * result$coef[1] + #(3)
      lambda * (result$coef[t - 1] - phi)
  }
  
  # The matrix of derivatives depends on the partial derivatives with respect to
  #parameters and previous CRFs denote them as crf_tau_partial and crf_crf_partial
  
  # crf_tau_partial
  crf_tau_partial <- matrix(0, ncol = length(beta_vec), nrow = length(model$coefficients)) %>%
    as.data.frame()
  for (t in 1:length(beta_vec)){
    # betas
    crf_tau_partial[beta_indices[t],t] <- 1
    # gammas
    if (t > 1){
      crf_tau_partial[gamma_pos_indices[t-1],t] <- max(result$coef[1],0)
      crf_tau_partial[gamma_neg_indices[t-1],t] <- min(result$coef[1],0)
      if(t > 2){
        crf_tau_partial[gamma_pos_indices[1:(t-2)],t] <- 
          max(result$coef[(t-1):2] - result$coef[(t-2):1])
        crf_tau_partial[gamma_neg_indices[1:(t-2)],t] <- 
          min(result$coef[(t-1):2] - result$coef[(t-2):1])
      }
    }
    # lambda
    if (t > 1){
      crf_tau_partial[lambda_index,t] <- result$coef[t-1] - phi
    }
    # phi x lambda
    if (t > 1){
      crf_tau_partial[phi_x_lambda_index,t] <- -1
    }
  }
  
  # crf_crf_partial
  crf_crf_partial <-  matrix(0, ncol = length(beta_vec), nrow = length(beta_vec)) %>%
    as.data.frame()
  for (t in 2:length(beta_vec)){
    if (t == 1){
      crf_crf_partial[t-1, t] <- 1 + lambda - 
        ifelse(result$coef[1], gamma_pos_vec[2], gamma_neg_vec[2])
    }
    
    if (t > 2){
      crf_crf_partial[t-1, t] <- 1 + lambda - 
        ifelse(result$coef[t-1] > result$coef[t-2], gamma_pos_vec[2], gamma_neg_vec[2])
      crf_crf_partial[(t-2):1,t] <- 
        ifelse(result$coef[(t-1):2] > result$coef[(t-2):1], 
               gamma_pos_vec[2:(t-1)], gamma_neg_vec[2:(t-1)]) -
        ifelse(result$coef[(t-1):2] > result$coef[(t-2):1], 
               gamma_pos_vec[3:t], gamma_neg_vec[3:t])
    }
  }
  
  # crf_tau
  crf_tau_full <- matrix(0, ncol = length(beta_vec), nrow = length(model$coefficients)) %>%
    as.data.frame()
  for (c in 1:length(beta_vec)){
    crf_tau_full[,c] <-  crf_tau_partial[,c] + 
      as.matrix(crf_tau_partial) %*% as.matrix(crf_crf_partial[,c])
  }
  
  result$se <- sqrt(diag(t(as.matrix(crf_tau_full)) %*% 
                           vcov(model) %*%
                           as.matrix(crf_tau_full)))
  
  result$lag <- 1:nrow(result) - 1
  return(result)
}

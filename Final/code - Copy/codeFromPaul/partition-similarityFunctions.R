
entropy <- function (x) {
  # return entropy of vector, essentiallyt number of bits required to specify
  if (length (x) == 0) {
    return (1.0)
  }
  
  freq_tbl <- table (x) / length (x)
  freq_vec <- as.vector (freq_tbl)
  return (-sum (freq_vec * log (freq_vec)))
}


mutual_information <- function (x, y) {
  # consistent with sklearn
  vec_len <- length (x)
  df <- data.frame (x=x, y=y)
  cont_tbl <- table (df) / vec_len
  freqs_x <- rowSums (cont_tbl)
  freqs_y <- colSums (cont_tbl)
  
  sum_xy <- 0
  for (i in 1:nrow (cont_tbl)) {
    for (j in 1:ncol (cont_tbl)) {
      p_xy <- cont_tbl[i, j]
      if (0.0 < p_xy) {
        p_x <- freqs_x[i]
        p_y <- freqs_y[j]
        add_xy <- p_xy * log (p_xy / (p_x * p_y))
        sum_xy <- sum_xy + add_xy
      }
    }
  }
  
  return (sum_xy)
}



homogeneity_completeness_vmeasure <- function (labels_true, labels_pred) {
  
  ## Preconditions & prep:
  stopifnot (length (labels_true) == length (labels_pred))
  cats_true <- factor (labels_true)
  cats_pred <- factor (labels_pred)
  
  ## Main:
  if (length (cats_true) == 0) {
    return (list (homogeneity=1.0, completeness=1.0, vmeasure=1.0)) 
  }
  
  entropy_c <- entropy (cats_true)
  entropy_k <- entropy (cats_pred)
  
  mutual_info <- mutual_information (cats_true, cats_pred)
  
  homogeneity <- mutual_info / ifelse (0.0 < entropy_c, entropy_c, 1.0)
  completeness <- mutual_info / ifelse (0.0 < entropy_k, entropy_k, 1.0)
  
  
  if ((homogeneity + completeness) == 0.0) {
    vmeasure <- 0.0
  } else {
    vmeasure <- 2.0 * homogeneity * completeness / (homogeneity + completeness)
  }
  
  return (list (homogeneity=homogeneity, completeness=completeness, vmeasure=vmeasure)) 
  
}


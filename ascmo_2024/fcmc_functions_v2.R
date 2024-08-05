# prepare list of spatio-temporal arrays for models and reanalysis files
prepare_x = function(m_files, r_files, desired_length, processed_data_dir) {
  for (i in seq_along(m_files)) {
    x[[i]] = readRDS(file.path(processed_data_dir, m_files[i]))
    dim3 = dim(x[[i]])[3]
    if (dim3 != desired_length) {
      desired_start = dim3 - desired_length + 1
      x[[i]] = x[[i]][,,seq(desired_start, dim3, by = 1)]
    }
  }
  
  # read reanalysis data
  # adjust from Kelvin to Celsius
  for (i in seq_along(r_files)) {
    x[[i + length(m_files)]] = readRDS(file.path(processed_data_dir,
                                                 r_files[i]))
    # adjust if temperature in Kelvin not Celsius
    if (max(x[[i + length(m_files)]], na.rm = TRUE) > 150) {
      x[[i + length(m_files)]] = x[[i + length(m_files)]] - 273.15
    }
    
    dim3 = dim(x[[i + length(m_files)]])[3]
    
    if (dim3 != desired_length) {
      desired_start = dim3 - desired_length + 1
      x[[i + length(m_files)]] = x[[i + length(m_files)]][,,seq(desired_start, dim3, by = 1)]
    }
  }
  return(x)
}

#' Prepare a stratified spatio-temporal array
#'
#' Each entry in x has three dimensions. The rows and
#' columns represent the spatial dimensions and the 
#  third dimension is the time dimension.

#' Prepares a list of spatio-temporal arrays for analysis
#' @param x A list of space-time arrays.
#' @param grp A factor vector with two levels indicating the 
#' group each element of x is associated with.
#' @param strata A factor vector indicating how to permute
#' The time dimension of x for stratified sampling.
#' @param idx A matrix indicating the row, column positions
#' of to be analyzed. If NULL, then all rows and columns are used.
#'
#' @return A three-dimensional array.
#' @export
#'
#' @examples
prepare_str_starray = function(x, grp, strata, idx = NULL) {
  arg_check_x(x)
  arg_check_grp(grp)
  x_dim = dim(x[[1]])
  arg_check_strata(strata, x_dim)
  # all indices
  if (missing(idx)) {
    idx = as.matrix(expand.grid(seq_len(x_dim[1]), seq_len(x_dim[2]), KEEP.OUT.ATTRS = FALSE))
    attr(idx, "dimnames") <- NULL
  }
  if (!is.matrix(idx)) {
    stop("idx must be a matrix")
  }
  
  x_array <- array(dim = c(nrow(idx),
                           dim(x[[1]])[3],
                           length(x)))
  
  for (i in seq_along(x)) {
    x_array[,,i] = t(apply(idx, MARGIN = 1, FUN = function(temp_idx) {
      idx1 = temp_idx[1]
      idx2 = temp_idx[2]
      x[[i]][idx1, idx2, , drop = TRUE]
    }))
  }
  
  structure(list(starray = x_array,
                 grp = grp,
                 strata = strata,
                 idx = idx),
            class = "sta")
}

#' Standard permutation test for distributional equality
#'
#' @param sta An sta object
#' @param perm_idxs The matrix of permutation indices
#' @param cl The cl argument of the pbapply function
#'
#' @return A list summarizing the results
#' @export
#'
#' @examples
perm_test_de_sta = function(sta, perm_idxs, cl = NULL) {

  tstats = perm_stats_de_sta(sta = sta,
                             perm_idxs = perm_idxs,
                             cl = cl)
  grp1 = which(sta$grp == levels(sta$grp)[1])
  grp2 = which(sta$grp != levels(sta$grp)[1])
  denominator = length(grp1) * length(grp2) * dim(sta$starray)[2]
  tstats = tstats/denominator
  pvalues = perm_pvalues(x = tstats, alternative = "greater")
  tstats_global = rowSums(tstats)/nrow(sta$idx)
  pvalue_global = perm_pvalues(x = matrix(tstats_global), alternative = "greater")
  return(list(tstats = tstats, pvalues = pvalues,
              tstats_global = tstats_global,
              pvalue_global = pvalue_global,
              idx = sta$idx))
}

# permutation statistics for distribution equality statistic
# computed for sta
perm_stats_de_sta = function(sta, perm_idxs = NULL, cl = NULL) {
  # statistics for permuted data
  # t to get in proper format
  t(pbapply::pbapply(perm_idxs, 2, function(perm_idxs_i) {
    stat_de_sta(sta$starray[,,perm_idxs_i], sta$grp)
  }, cl = cl))
}

# distribution equality statistic computed for starray
stat_de_sta = function(starray, grp) {
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  
  tstats = numeric(nrow(starray))
  for (i in grp1) {
    for (j in grp2) {
      # temporary sum of absolute differences
      tstats = tstats + rowSums(abs(starray[,,i] - starray[,,j]))
    }
  }
  return(tstats)
}

prepare_perm_idxs = function(nmr, nr) {
  # generate exact permutations for this test
  # number of total files choose number of r files combinations
  nm = (nmr - nr)
  r_combn = combn(seq_len(nmr), nr)
  r_pos = nm + seq_len(nr)
  perm_idxs = matrix(nrow = nmr, ncol = ncol(r_combn))
  # place them in last (length(r_files) rows)
  perm_idxs[r_pos, ] = r_combn
  for (i in seq_len(ncol(perm_idxs))) {
    tmp_r_idx = perm_idxs[r_pos, i]
    perm_idxs[seq_len(nm), i] = setdiff(seq_len(nmr), tmp_r_idx)
  }
  perm_idxs
}

perm_test_sta = function(sta, stat_func, perm_idxs,
                         alternative = "two.sided",
                         cl = cl,
                         ...) {
  message("Computing permutation test statistics")
  tstats = perm_stats_sta(sta, stat_func = stat_func, 
                          perm_idxs = perm_idxs, cl = cl,
                          ...)
  pvalues = perm_pvalues(x = tstats, alternative = alternative)
  return(list(tstats = tstats, pvalues = pvalues, idx = sta$idx))
}

# compute permutation test statistics for sta object based
# on perm_idxs
perm_stats_sta = function(sta, stat_func, perm_idxs, cl = NULL, ...) {
  # observed statistics
  sta_stats = compute_stats_sta(sta, stat_func, ...)
  
  # statistics for permuted data
  pbapply::pbapply(sta_stats, 1, function(sta_stats_i) {
    apply(perm_idxs, 2, function(perm_i) {
      stat_2sample(sta_stats_i[perm_i], grp = sta$grp)
    })
  }, cl = cl)
}

# compute statistics for sta object across each row and 3rd dimension
compute_stats_sta = function(sta, stat_func, ...) {
  apply(sta$starray, c(1, 3), stat_func, ...)
}

# permutation statistics for bsplines coefficients for
# sta object
perm_stats_bs_coefs_sta = function(sta, perm_idxs, cl = NULL) {
  # basis x
  xv = seq_len(dim(sta$starray)[2])
  # knots
  k = seq(0.5, dim(x[[1]])[3] + 0.5, by = 2.4)
  # set up a saturated B-spline basis
  basisobj <- fda::create.bspline.basis(range(k), breaks = k)
  
  t(pbapply::pbapply(perm_idxs, 2, function(perm_idx) {
    x_perm_array = sta$starray[,,perm_idx]
    apply(x_perm_array,
          MARGIN = 1,
          function(ymat) {
            coeffs_mat = fda::smooth.basis(xv, ymat, basisobj)$fd$coefs
            stat_bs_coeffs_mat(coeffs_mat, sta$grp)
          })
  }, cl = cl))
}

perm_test_bs_coefs_sta = function(sta, perm_idxs, cl = NULL) {
  message("Computing permutation test statistics")
  tstats = perm_stats_bs_coefs_sta(sta, perm_idxs = perm_idxs, cl = cl)
  grp1 = which(sta$grp == levels(sta$grp)[1])
  grp2 = which(sta$grp != levels(sta$grp)[1])
  denominator = length(grp1) * length(grp2) * dim(sta$starray)[2]
  tstats = tstats/denominator
  pvalues = perm_pvalues(x = tstats, alternative = "greater")
  return(list(tstats = tstats, pvalues = pvalues, idx = idx))
}

# sum abs difference in coefficients matrix computed for bs
stat_bs_coeffs_mat = function(coeffs_mat, grp) {
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  
  tstats = 0
  for (i in grp1) {
    for (j in grp2) {
      # temporary sum of absolute differences
      tstats = tstats + sum(abs(coeffs_mat[,i] - coeffs_mat[,j]))
    }
  }
  return(tstats)
}

# stratified permutation test statistics for bspline
# coefficients for sta object
strat_perm_stats_bs_coefs_sta = function(sta, nsim = 2, cl = NULL) {
  strata = sta$strata
  # n_in_strata = tapply(strata, strata, length)
  # names_strata = names(n_in_strata)
  #prepare stratified permutation indices
  strata_idxs = prepare_strata_idxs(nsim, sta$strata, sta$grp)
  # basis x
  xv = seq_len(dim(sta$starray)[2])
  # knots
  k = seq(0.5, dim(x[[1]])[3] + 0.5, by = 2.4)
  # set up a saturated B-spline basis
  basisobj <- fda::create.bspline.basis(range(k), breaks = k)
  
  tstats = pbapply::pblapply(seq_len(nsim + 1), function(b) {
    apply(strat_perm_starray(sta, strata_idxs[[b]]),
          MARGIN = 1,
          function(ymat) {
            coeffs_mat = fda::smooth.basis(xv, ymat, basisobj)$fd$coefs
            stat_bs_coeffs_mat(coeffs_mat, sta$grp)
          })
  }, cl = cl)
  do.call(rbind, tstats)
}

# stratified permutation test using bspline coefficients for sta object
strat_perm_test_bs_coefs_sta =  function(sta, nsim = 2, cl = NULL) {
    message("Computing permutation test statistics")
    tstats = strat_perm_stats_bs_coefs_sta(sta = sta, nsim = nsim, cl = cl)
    pvalues = perm_pvalues(x = tstats, alternative = "greater")
    return(list(tstats = tstats, pvalues = pvalues, idx = sta$idx))
}

# prepare indices for stratified permutation test
prepare_strata_idxs = function(nsim, strata, grp) {
  lapply(seq_len(nsim + 1), function(i) {
    if (i != (nsim + 1)) {
      temp_idxs = do.call(rbind, 
                          lapply(seq_len(nlevels(strata)), function(s) {
                            matrix(sample(seq_along(grp)), nrow = 1)
                          }))
    } else {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        matrix(seq_along(grp), nrow = 1)
      }))
    }
    return(temp_idxs)
  })
}

# create stratified permutation of an starray using strata_idx_b
strat_perm_starray = function(sta, strata_idx_b) {
  x_perm_array = array(dim = dim(sta$starray))
  for (lvli in seq_along(levels(sta$strata))) {
    strata_rows = which(sta$strata == levels(sta$strata)[lvli])
    perm_strata_idx = strata_idx_b[lvli,]
    x_perm_array[, strata_rows, ] = sta$starray[, strata_rows, perm_strata_idx]
  }
  x_perm_array  
}

# compute statistics for starray object across 2nd dimension
compute_stats_starray = function(starray, stat_func, ...) {
  apply(starray, 2, stat_func, ...)
}

# stratified permutation statistics for sta objects
strat_perm_stats_sta = function(sta, stat_func, nsim, cl = NULL, ...) {
  # n_in_strata = tapply(strata, strata, length)
  # names_strata = names(n_in_strata)
  
  #prepare stratified permutation indices
  strata_idxs = prepare_strata_idxs(nsim, sta$strata, sta$grp)

  stats_list <- 
  pbapply::pblapply(seq_len(nsim + 1), function(b) {
    grp_stats = apply(strat_perm_starray(sta, strata_idxs[[b]]),
          MARGIN = c(1, 3),
          FUN = stat_func, ...)
    apply(grp_stats, 1, stat_2sample, grp = sta$grp)
  }, cl = cl)
  do.call(rbind, stats_list)
}

# stratified permutation test for generate function based on 2sample
strat_perm_test_sta = function(sta, stat_func,
                               alternative = "two.sided",
                               nsim = 49, cl = NULL, ...) {
  message("Computing permutation test statistics")
  tstats = strat_perm_stats_sta(sta = sta,
                                stat_func = stat_func,
                                nsim = nsim,
                                cl = cl, ...)
  pvalues = perm_pvalues(x = tstats, alternative = alternative)
  return(list(tstats = tstats, pvalues = pvalues, idx = sta$idx))
}

# stratified permutation statistics for testing distributional equality
strat_perm_stats_de_sta = function(sta, nsim, cl = NULL) {

  strata_idxs = prepare_strata_idxs(nsim, sta$strata, sta$grp)

  grp1 = which(sta$grp == levels(sta$grp)[1])
  grp2 = which(sta$grp != levels(sta$grp)[1])
  
  tstats = pbapply::pblapply(seq_len(nsim + 1), function(b) {
    apply(strat_perm_starray(sta, strata_idxs[[b]]),
          MARGIN = 1,
          stat_de_mat, grp1 = grp1, grp2 = grp2)
  }, cl = cl)
  do.call(rbind, tstats)
}

# stratified permutation statistics for testing distributional equality
strat_perm_test_de_sta = function(sta, nsim = 49, cl = NULL) {
  message("Computing permutation test statistics")
  tstats = strat_perm_stats_de_sta(sta = sta, nsim = nsim, cl = cl)
  grp1 = which(sta$grp == levels(sta$grp)[1])
  grp2 = which(sta$grp != levels(sta$grp)[1])
  denominator = length(grp1) * length(grp2) * dim(sta$starray)[2]
  tstats = tstats/denominator
  pvalues = perm_pvalues(x = tstats, alternative = "greater")
  tstats_global = rowSums(tstats)/nrow(sta$idx)
  pvalue_global = perm_pvalues(x = matrix(tstats_global), alternative = "greater")
  return(list(tstats = tstats,
              pvalues = pvalues,
              tstats_global = tstats_global,
              pvalue_global = pvalue_global,
              idx = sta$idx))
}

perm_pvalues = function(x, alternative = "two.sided") {
  nsim = nrow(x) - 1
  if (alternative == "two.sided") {
    x = abs(x)
  } else if (alternative == "less") {
    x = -x
  }
  apply(x, 2, function(tstats) {
    ind = tstats[seq_len(nsim)] >= tstats[nsim + 1]
    (sum(ind) + 1)/(nsim + 1)
  })
}

stat_2sample = function(xstats, grp) {
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp == levels(grp)[2])
  tstat = 0
  for (i in grp1) {
    for (j in grp2) {
      tstat = tstat + abs(xstats[i] - xstats[j])
    }
  }
  tstat/(length(grp1) * length(grp2))
}

plot_test_object = function(x, y, test_object, alpha = 0.05,
                            type = "stats",
                            sig_only = TRUE,
                            ...) {
  img = matrix(nrow = length(x), ncol = length(y))
  nsim = nrow(test_object$tstats) - 1
  keep = seq_along(test_object$pvalues)
  if (sig_only) {
    keep = keep[test_object$pvalues <= alpha]
  }
  if (type == "stats") {
    img[test_object$idx[keep,]] = test_object$tstats[nsim + 1, keep]
  } else {
    img[test_object$idx[keep,]] = test_object$pvalues[keep]
  }
  
  if (length(keep) > 0) {
    # autoimage::autoimage(x, y, img, ...)
    autoimage::autoimage(x, y, img, map = "world")
  } else {
    warning("no locations plottable")
    xy = expand.grid(x, y)
    plot(xy[[1]], xy[,2], type = "n", ...)
  }
}

plot_test_objects = function(x, y, test_objects, alpha = 0.05,
                             type = "stats",
                             sig_only = TRUE,
                             ...) {
  img = array(NA, dim = c(length(x), length(y), length(test_objects)))
  nsim = sapply(test_objects, function(obj) nrow(obj$tstats) - 1)
  keep = lapply(test_objects, function(obj) seq_along(obj$pvalues))
  if (sig_only) {
    for (i in seq_along(test_objects)) {
      keep[[i]] = keep[[i]][test_objects[[i]]$pvalues <= alpha]
    }
  }
  for (i in seq_along(test_objects)) {
    if (type == "stats") {
      if (length(keep[[i]]) > 0) {
        img[cbind(test_objects[[i]]$idx[keep[[i]],], i)] = test_objects[[i]]$tstats[nsim[i] + 1, keep[[i]]]
      }
    } else {
      if (length(keep[[i]] > 0)) {
        img[cbind(test_objects[[i]]$idx[keep[[i]],], i)] = test_objects[[i]]$pvalues[keep[[i]]]
      }
    }
  }
  
  if (length(keep) > 0) {
    autoimage::autoimage(x, y, img, ...)
    # autoimage::autoimage(x, y, img, map = "world")
  } else {
    warning("no locations plottable")
    xy = expand.grid(x, y)
    plot(xy[[1]], xy[,2], type = "n", ...)
  }
}

iqr = function(x, prob = c(0.25, 0.75), ...) {
  diff(quantile(x, prob = c(0.25, 0.75), ...))
}

# distribution equality statistic when xmat is a matrix
# with N_t * N_n rows and length(grp1) + length(grp2) columns
# Not that xmat is really something like cbind(x[[1]][i, j, ], x[[2][i, j, ]]),
# i.e., the values for a specific index, which is very different from stat_de
stat_de_mat = function(xmat, grp1, grp2) {
  tstat = 0
  for (i in grp1) {
    for (j in grp2) {
      # temporary sum of absolute differences
      tstat = tstat + sum(abs(xmat[, i] - xmat[, j]))
    }
  }
  return(tstat)
}

arg_check_x = function(x) {
  is_array = lapply(x, is.array)
  if (any(is_array == FALSE)) {
    stop("each element of x must be an array")
  }
  dim_array = lapply(x, dim)
  if (sum(duplicated(dim_array)) != length(dim_array) - 1L) {
    stop("each element of x must have identical dimensions")
  }
  if (length(dim_array[[1]]) != 3) {
    stop("each element of x must have 3 dimensions")
  }
}

arg_check_grp = function(grp) {
  if (!is.factor(grp)) {
    stop("grp must be a factor")
  }
  if (length(levels(grp)) != 2) {
    stop("grp can only have two levels")
  }
}

arg_check_strata = function(strata, xdim) {
  if (!is.factor(strata)) {
    stop("strata must be a factor")
  }
  if (length(strata) != xdim[3]) {
    stop("length(strata) != dim(x[[1]])[3]")
  }
}

arg_check_nsim = function(nsim) {
  if (length(nsim) != 1) {
    stop("nsim must have length 1")
  }
  if (!is.numeric(nsim)) {
    stop("nsim must be numeric")
  }
  if (nsim < 1) {
    stop("nsim must be at least 1")
  }
}


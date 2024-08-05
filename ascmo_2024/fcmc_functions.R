perm_stats = function(x, grp, stat_func, nsim, idx,
                      cl = NULL, perm_idxs = NULL, ...) {

  # last column is original permutation
  if (is.null(perm_idxs)) {
    perm_idxs = matrix(seq_along(grp), nrow = length(grp),
                       ncol = nsim + 1)
    for (i in seq_len(nsim)) {
      perm_idxs[, i] = sample(seq_along(grp))
    }
  }
  # observed statistics
  xstats = compute_stats_x(x, stat_func, idx, ...)

  # statistics for permuted data
  pbapply::pbapply(xstats, 1, function(xstats_i) {
    apply(perm_idxs, 2, function(perm_i) {
      stat_2sample(xstats_i[perm_i], grp = grp)
    })
  }, cl = cl)
}

compute_stats_x = function(x, stat_func, idx, ...) {
  sapply(x, function(xi) {
    apply(idx, 1, function(i) {
      stat_func(xi[i[1], i[2], ], ...)
    })
  })
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

perm_test = function(x, grp, stat_func,
                     alternative = "two.sided", nsim = 49,
                     idx, cl = NULL, perm_idxs = NULL, ...) {
  arg_check_x(x)
  arg_check_grp(grp)
  arg_check_nsim(nsim)
  x_dim = dim(x[[1]])

  # all indices
  if (missing(idx)) {
    idx = as.matrix(expand.grid(seq_len(x_dim[1]), seq_len(x_dim[2]), KEEP.OUT.ATTRS = FALSE))
    attr(idx, "dimnames") <- NULL
  }
  if (!is.matrix(idx)) {
    stop("idx must be a matrix")
  }

  message("Computing permutation test statistics")
  tstats = perm_stats(x = x, grp = grp, stat_func = stat_func,
                      nsim = nsim, idx = idx, cl = cl,
                      perm_idxs = perm_idxs, ...)
  pvalues = perm_pvalues(x = tstats, alternative = alternative)
  return(list(tstats = tstats, pvalues = pvalues, idx = idx))
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

rangel = function(x, ...) {
  diff(range(x, ...))
}

strat_perm_stats = function(x, grp, strata,
                      stat_func, nsim, idx,
                      cl = NULL, ...) {
  n_in_strata = tapply(strata, strata, length)
  names_strata = names(n_in_strata)

  strata_idxs = lapply(seq_len(nsim + 1), function(i) {
    # sample grps within strata, then replicate to match
    # the length of each x[[i]][j, k]
    if (i != (nsim + 1)) {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        which_strata = which(names_strata == levels(strata)[s])
        matrix(sample(seq_along(grp)), nrow = 1)
      }))
    } else {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        matrix(seq_along(grp), nrow = 1)
      }))
    }
    return(temp_idxs)
  })

  pbapply::pbapply(idx, 1, function(i) {
    idx1 = i[1]
    idx2 = i[2]
    temp_x = sapply(x, function(xi) {
      xi[i[1], i[2],]
    })

    stats = numeric(nsim + 1)
    for (b in seq_len(nsim + 1)) {
      strata_idx_b = strata_idxs[[b]]
      perm_temp_x = temp_x
      for (lvli in seq_along(levels(strata))) {
        strata_rows = which(strata == levels(strata)[lvli])
        perm_strata_idx = strata_idx_b[lvli,]
        temp_x_lvli = temp_x[strata_rows, ]
        temp_x_lvli = temp_x_lvli[, perm_strata_idx]
        perm_temp_x[strata_rows, ] = temp_x_lvli
      }
      grp_stats = apply(perm_temp_x, 2, stat_func, ...)
      stats[b] = stat_2sample(grp_stats, grp)
    }
    stats
  }, cl = cl)
}

strat_perm_test = function(x, grp, strata, stat_func,
                           alternative = "two.sided",
                           nsim = 49, idx, cl = NULL, ...) {
  arg_check_x(x)
  arg_check_grp(grp)
  arg_check_nsim(nsim)
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

  message("Computing permutation test statistics")
  tstats = strat_perm_stats(x = x, grp = grp,
                            strata = strata,
                            stat_func = stat_func,
                            nsim = nsim, idx = idx,
                            cl = cl, ...)
  pvalues = perm_pvalues(x = tstats, alternative = alternative)
  return(list(tstats = tstats, pvalues = pvalues, idx = idx))
}

# distribution equality statistic for standard permutation test
# need x (list of 3 dimensional arrays), indices of grp1 and grp2
# and idx of x[[k]] that are relevant (i.e., non-NA)
stat_de = function(x, grp, idx) {
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])

  tstats = numeric(nrow(idx))
  for (i in grp1) {
    for (j in grp2) {
      # temporary sum of absolute differences
      tstats = tstats + (apply(abs(x[[i]] - x[[j]]), 1:2, sum))[idx]
    }
  }
  
  return(tstats)
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

perm_stats_de = function(x, grp, nsim, idx, cl = NULL, perm_idxs = NULL, ...) {
  # last column is original permutation
  if (is.null(perm_idxs)) {
    perm_idxs = matrix(seq_along(grp), nrow = length(grp),
                       ncol = nsim + 1)
    for (i in seq_len(nsim)) {
      perm_idxs[, i] = sample(seq_along(grp))
    }
  }

  # statistics for permuted data
  # t to get in proper format
  t(pbapply::pbapply(perm_idxs, 2, function(perm_idxs_i) {
    stat_de(x[perm_idxs_i], grp = grp, idx = idx)
  }, cl = cl))
}

perm_stats_fun = function(x, grp, nsim, idx, stat_fun, cl = NULL,
                          perm_idxs = NULL, ...) {
  # last column is original permutation
  if (is.null(perm_idxs)) {
    perm_idxs = matrix(seq_along(grp), nrow = length(grp),
                       ncol = nsim + 1)
    for (i in seq_len(nsim)) {
      perm_idxs[, i] = sample(seq_along(grp))
    }
  }
  
  # statistics for permuted data
  # t to get in proper format
  t(pbapply::pbapply(perm_idxs, 2, function(perm_idxs_i) {
    stat_fun(x[perm_idxs_i], grp = grp, idx = idx)
  }, cl = cl))
}

perm_test_de = function(x, grp, nsim = 49, idx, cl = NULL,
                        perm_idxs = NULL, ...) {
  arg_check_x(x)
  arg_check_grp(grp)
  arg_check_nsim(nsim)
  x_dim = dim(x[[1]])

  # all indices
  if (missing(idx)) {
    idx = as.matrix(expand.grid(seq_len(x_dim[1]), seq_len(x_dim[2]), KEEP.OUT.ATTRS = FALSE))
    attr(idx, "dimnames") <- NULL
  }
  if (!is.matrix(idx)) {
    stop("idx must be a matrix")
  }

  message("Computing permutation test statistics")
  tstats = perm_stats_de(x = x, grp = grp, nsim = nsim, idx = idx,
                         cl = cl, perm_idxs = perm_idxs, ...)
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  denominator = length(grp1) * length(grp2) * length(x[[1]][idx[1,1], idx[1,2], ])
  tstats = tstats/denominator
  pvalues = perm_pvalues(x = tstats, alternative = "greater")
  tstats_global = rowSums(tstats)/nrow(idx)
  pvalue_global = perm_pvalues(x = matrix(tstats_global), alternative = "greater")
  return(list(tstats = tstats, pvalues = pvalues,
              tstats_global = tstats_global, pvalue_global = pvalue_global,
              idx = idx))
}

perm_test_fun = function(x, grp, nsim = 49, idx, stat_fun,
                         cl = NULL, perm_idxs = NULL, ...) {
  arg_check_x(x)
  arg_check_grp(grp)
  arg_check_nsim(nsim)
  x_dim = dim(x[[1]])
  
  # all indices
  if (missing(idx)) {
    idx = as.matrix(expand.grid(seq_len(x_dim[1]), seq_len(x_dim[2]), KEEP.OUT.ATTRS = FALSE))
    attr(idx, "dimnames") <- NULL
  }
  if (!is.matrix(idx)) {
    stop("idx must be a matrix")
  }
  
  message("Computing permutation test statistics")
  tstats = perm_stats_fun(x = x, grp = grp, nsim = nsim, idx = idx,
                          stat_fun = stat_fun, 
                          cl = cl, perm_idxs = perm_idxs, ...)
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  denominator = length(grp1) * length(grp2) * length(x[[1]][idx[1,1], idx[1,2], ])
  tstats = tstats/denominator
  pvalues = perm_pvalues(x = tstats, alternative = "greater")
  tstats_global = rowSums(tstats)/nrow(idx)
  pvalue_global = perm_pvalues(x = matrix(tstats_global), alternative = "greater")
  return(list(tstats = tstats, pvalues = pvalues,
              tstats_global = tstats_global, pvalue_global = pvalue_global,
              idx = idx))
}

strat_perm_stats_de = function(x, grp, strata,
                            nsim, idx, cl = NULL, ...) {
  n_in_strata = tapply(strata, strata, length)
  names_strata = names(n_in_strata)

  strata_idxs = lapply(seq_len(nsim + 1), function(i) {
    if (i != (nsim + 1)) {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        which_strata = which(names_strata == levels(strata)[s])
        matrix(sample(seq_along(grp)), nrow = 1)
      }))
    } else {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        matrix(seq_along(grp), nrow = 1)
      }))
    }
    return(temp_idxs)
  })

  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])

  pbapply::pbapply(idx, 1, function(i) {
    idx1 = i[1]
    idx2 = i[2]
    temp_x = sapply(x, function(xi) {
      xi[i[1], i[2],]
    })

    stats = numeric(nsim + 1)
    for (b in seq_len(nsim + 1)) {
      strata_idx_b = strata_idxs[[b]]
      perm_temp_x = temp_x
      for (lvli in seq_along(levels(strata))) {
        strata_rows = which(strata == levels(strata)[lvli])
        perm_strata_idx = strata_idx_b[lvli,]
        temp_x_lvli = temp_x[strata_rows, ]
        temp_x_lvli = temp_x_lvli[, perm_strata_idx]
        perm_temp_x[strata_rows, ] = temp_x_lvli
      }
      stats[b] = stat_de_mat(x = perm_temp_x, grp1 = grp1, grp2 = grp2)
    }
    stats
  }, cl = cl)
}

strat_perm_stats_fun = function(x, grp, strata,
                                nsim, idx,
                                stat_fun = stat_fun, cl = NULL, ...) {
  n_in_strata = tapply(strata, strata, length)
  names_strata = names(n_in_strata)
  
  strata_idxs = lapply(seq_len(nsim + 1), function(i) {
    if (i != (nsim + 1)) {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        which_strata = which(names_strata == levels(strata)[s])
        matrix(sample(seq_along(grp)), nrow = 1)
      }))
    } else {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        matrix(seq_along(grp), nrow = 1)
      }))
    }
    return(temp_idxs)
  })
  
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  
  pbapply::pbapply(idx, 1, function(i) {
    idx1 = i[1]
    idx2 = i[2]
    temp_x = sapply(x, function(xi) {
      xi[i[1], i[2],]
    })
    
    stats = numeric(nsim + 1)
    for (b in seq_len(nsim + 1)) {
      strata_idx_b = strata_idxs[[b]]
      perm_temp_x = temp_x
      for (lvli in seq_along(levels(strata))) {
        strata_rows = which(strata == levels(strata)[lvli])
        perm_strata_idx = strata_idx_b[lvli,]
        temp_x_lvli = temp_x[strata_rows, ]
        temp_x_lvli = temp_x_lvli[, perm_strata_idx]
        perm_temp_x[strata_rows, ] = temp_x_lvli
      }
      stats[b] = stat_de_mat(x = perm_temp_x, grp1 = grp1, grp2 = grp2)
    }
    stats
  }, cl = cl)
}

strat_perm_test_de = function(x, grp, strata, nsim = 49, idx, cl = NULL, ...) {
  arg_check_x(x)
  arg_check_grp(grp)
  arg_check_nsim(nsim)
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

  message("Computing permutation test statistics")
  tstats = strat_perm_stats_de(x = x, grp = grp,
                              strata = strata,
                              nsim = nsim, idx = idx,
                              cl = cl, ...)
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  denominator = length(grp1) * length(grp2) * length(x[[1]][idx[1,1], idx[1,2], ])
  tstats = tstats/denominator
  pvalues = perm_pvalues(x = tstats, alternative = "greater")
  tstats_global = rowSums(tstats)/nrow(idx)
  pvalue_global = perm_pvalues(x = matrix(tstats_global), alternative = "greater")
  return(list(tstats = tstats, pvalues = pvalues,
              tstats_global = tstats_global, pvalue_global = pvalue_global,
              idx = idx))
}

perm_stats_bs_coefs = function(x, grp, nsim, idx, cl = NULL,
                               perm_idxs = NULL, ...) {
  # last column is original permutation
  if (is.null(perm_idxs)) {
    perm_idxs = matrix(seq_along(grp), nrow = length(grp),
                       ncol = nsim + 1)
    for (i in seq_len(nsim)) {
      perm_idxs[, i] = sample(seq_along(grp))
    }
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
  
  # basis x
  xv = seq_len(dim(x_array)[2])
  # knots
  k = seq(0.5, dim(x[[1]])[3] + 0.5, by = 2.4)
  # set up a saturated B-spline basis
  basisobj <- fda::create.bspline.basis(range(k), breaks = k)
  
  tstats = pbapply::pblapply(seq_len(ncol(perm_idxs)), function(b) {
    x_perm_array = x_array[,,perm_idxs[,b]]
    stats_idx = sapply(seq_len(nrow(idx)), function(j) {
      stats_mat = fda::smooth.basis(xv, x_perm_array[j, , ], basisobj)$fd$coefs
      sum(abs(stats_mat - stats_mat[,ncol(stats_mat)]))
    })
    stats_idx
  }, cl = cl)
  do.call(rbind, tstats)
}

perm_test_bs_coefs = function(x, grp, nsim = 49, idx, 
                              cl = NULL, perm_idxs = NULL, ...) {
  arg_check_x(x)
  arg_check_grp(grp)
  arg_check_nsim(nsim)
  x_dim = dim(x[[1]])
  
  # all indices
  if (missing(idx)) {
    idx = as.matrix(expand.grid(seq_len(x_dim[1]), seq_len(x_dim[2]), KEEP.OUT.ATTRS = FALSE))
    attr(idx, "dimnames") <- NULL
  }
  if (!is.matrix(idx)) {
    stop("idx must be a matrix")
  }
  
  message("Computing permutation test statistics")
  tstats = perm_stats_bs_coefs(x, grp, nsim, idx, cl = cl,
                               perm_idxs = perm_idxs, ...)
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  denominator = length(grp1) * length(grp2) * length(x[[1]][idx[1,1], idx[1,2], ])
  tstats = tstats/denominator
  pvalues = perm_pvalues(x = tstats, alternative = "greater")
  return(list(tstats = tstats, pvalues = pvalues, idx = idx))
}

strat_perm_stats_bs_coefs = function(x, grp, strata,
                                     nsim, idx,
                                     cl = NULL, ...) {
  n_in_strata = tapply(strata, strata, length)
  names_strata = names(n_in_strata)
  
  strata_idxs = lapply(seq_len(nsim + 1), function(i) {
    if (i != (nsim + 1)) {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        which_strata = which(names_strata == levels(strata)[s])
        matrix(sample(seq_along(grp)), nrow = 1)
      }))
    } else {
      temp_idxs = do.call(rbind, lapply(seq_len(nlevels(strata)), function(s) {
        matrix(seq_along(grp), nrow = 1)
      }))
    }
    return(temp_idxs)
  })
  
  grp1 = which(grp == levels(grp)[1])
  grp2 = which(grp != levels(grp)[1])
  
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
  
  x_perm_array <- array(dim = dim(x_array))
  
  # basis x
  xv = seq_len(dim(x_array)[2])
  # knots
  k = seq(0.5, dim(x[[1]])[3] + 0.5, by = 2.4)
  # set up a saturated B-spline basis
  basisobj <- fda::create.bspline.basis(range(k), breaks = k)
  
  tstats = pbapply::pblapply(seq_len(nsim + 1), function(b) {
    strata_idx_b = strata_idxs[[b]]
    for (lvli in seq_along(levels(strata))) {
      strata_rows = which(strata == levels(strata)[lvli])
      perm_strata_idx = strata_idx_b[lvli,]
      x_perm_array[, strata_rows, ] = x_array[, strata_rows, perm_strata_idx]
    }
    stats_idx = sapply(seq_len(nrow(idx)), function(j) {
      stats_mat = fda::smooth.basis(xv, x_perm_array[j, , ], basisobj)$fd$coefs
      sum(abs(stats_mat - stats_mat[,ncol(stats_mat)]))
    })
    stats_idx
  }, cl = cl)
  do.call(rbind, tstats)
}

strat_perm_test_bs_coefs =
  function(x, grp, strata, nsim = 49, idx, cl = NULL, ...) {
    arg_check_x(x)
    arg_check_grp(grp)
    arg_check_nsim(nsim)
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
    
    message("Computing permutation test statistics")
    tstats = strat_perm_stats_bs_coefs(x, grp, strata,
                                       nsim, idx,
                                       cl = cl, ...)
    pvalues = perm_pvalues(x = tstats, alternative = "greater")
    return(list(tstats = tstats, pvalues = pvalues, idx = idx))
  }

save_sim_results = function(nrep, seed, B, perm_idxs, cl,
                            simdata_dir, sim_name,
                            results_dir) {
  for (i in seq_len(nrep)) {
    x = readRDS(file.path(simdata_dir,
                          paste0(sim_name, "_", i, ".rds")))
    
    set.seed(seed + i)
    
    fout = file.path(results_dir,
                     paste0("std_perm_tests_de_", sim_name, "_", i, ".rds"))
    
    if (!file.exists(fout)) {
      std_perm_de_sim = perm_test_de(x,
                                     grp = grp,
                                     nsim = ncol(perm_idxs),
                                     idx = idx,
                                     cl = mc_cores,
                                     perm_idxs = perm_idxs)
      saveRDS(std_perm_de_sim,
              file = fout, compress = "xz")
    }
    
    set.seed(seed + 2 * i)
    
    fout = file.path(results_dir,
                     paste0("sp_test_de_", sim_name, "_", i, ".rds"))
    
    if(!file.exists(fout)) {
      sp_test_de_sim = strat_perm_test_de(x, grp = grp,
                                          strata = strata,
                                          nsim = B,
                                          idx = idx,
                                          cl = mc_cores)
      saveRDS(sp_test_de_sim,
              file = fout,
              compress = "xz")
    }
    
    set.seed(seed + 3 * i)
    
    fout = file.path(results_dir,
                     paste0("std_perm_tests_mean_", sim_name, "_", i, ".rds"))
    
    if(!file.exists(fout)) {
      std_perm_test_mean_sim = perm_test(x, grp, stat_func = mean,
                                         alternative = "two.sided",
                                         nsim = ncol(perm_idxs), idx = idx,
                                         perm_idxs = perm_idxs,
                                         cl = mc_cores)
      saveRDS(std_perm_test_mean_sim,
              file = fout,
              compress = "xz")
    }
    
    set.seed(seed + 4 * i)
    
    fout = file.path(results_dir,
                     paste0("sp_test_mean_", sim_name, "_", i, ".rds"))
    
    if(!file.exists(fout)) {
      sp_test_mean_sim = strat_perm_test(x, grp, strata,
                                         stat_func = mean,
                                         alternative = "two.sided",
                                         nsim = B, idx = idx,
                                         cl = mc_cores)
      saveRDS(sp_test_mean_sim,
              file = fout,
              compress = "xz")
    }
  }
}

read_sim_results = function(results_dir, 
                            test_name,
                            sim_name,
                            nrep = 100) {
  x = vector("list", nrep)
  for (i in seq_len(nrep)) {
    fpath1 = file.path(results_dir,
                       paste0(test_name,
                              sim_name, "_",
                              i, ".rds"))
    x[[i]] = readRDS(fpath1)
  }
  smerc::sgetElement(x, "pvalues")
}

# compute empirical error rate
er = function(prob, pvals) {
  mean(pvals <= prob)
}

combine_results = function(results_dir, sim_name,
                           p1 = c(0.1, 0.2, 0.3, 0.4, 0.5),
                           p2 = seq(0.05, 0.5, by = 0.05)) {
  std_de_pvals = read_sim_results(results_dir, 
                                  "std_perm_tests_de_",
                                  sim_name)
  sp_de_pvals = read_sim_results(results_dir, 
                                 "sp_test_de_",
                                 sim_name)
  std_mean_pvals = read_sim_results(results_dir, 
                                    "std_perm_tests_mean_",
                                    sim_name)
  sp_mean_pvals = read_sim_results(results_dir, 
                                   "sp_test_mean_",
                                   sim_name)
  sig_std_de = sapply(p1, er, pvals = std_de_pvals)
  sig_sp_de = sapply(p2, er, pvals = sp_de_pvals)
  sig_std_mean = sapply(p1, er, pvals = std_mean_pvals)
  sig_sp_mean = sapply(p2, er, pvals = sp_mean_pvals)
  
  std_de_pvals_by = apply(std_de_pvals, 2, 
                          p.adjust, method = "BY")
  sp_de_pvals_by = apply(sp_de_pvals, 2, 
                         p.adjust, method = "BY")
  std_mean_pvals_by = apply(std_de_pvals, 2, 
                            p.adjust, method = "BY")
  sp_mean_pvals_by = apply(sp_de_pvals, 2, 
                           p.adjust, method = "BY")
  sig_std_de_by = sapply(p1, er, pvals = std_de_pvals_by)
  sig_sp_de_by = sapply(p2, er, pvals = sp_de_pvals_by)
  sig_std_mean_by = sapply(p1, er, pvals = std_mean_pvals_by)
  sig_sp_mean_by = sapply(p2, er, pvals = sp_mean_pvals_by)
  df_std_de = data.frame(prob = p1,
                         significant = sig_std_de,
                         permutation = "standard",
                         statistic = "dist. equality",
                         adjustment = "none")
  df_sp_de = data.frame(prob = p2,
                        significant = sig_sp_de,
                        permutation = "stratified",
                        statistic = "dist. equality",
                        adjustment = "none")
  df_std_mean = data.frame(prob = p1,
                           significant = sig_std_mean,
                           permutation = "standard",
                           statistic = "mean",
                           adjustment = "none")
  df_sp_mean = data.frame(prob = p2,
                          significant = sig_sp_mean,
                          permutation = "stratified",
                          statistic = "mean",
                          adjustment = "none")
  df_std_de_by = data.frame(prob = p1,
                            significant = sig_std_de_by,
                            permutation = "standard",
                            statistic = "de",
                            adjustment = "BY")
  df_sp_de_by = data.frame(prob = p2,
                           significant = sig_sp_de_by,
                           permutation = "stratified",
                           statistic = "de",
                           adjustment = "BY")
  df_std_mean_by = data.frame(prob = p1, 
                              significant = sig_std_mean_by,
                              permutation = "standard",
                              statistic = "mean",
                              adjustment = "BY")
  df_sp_mean_by = data.frame(prob = p2,
                             significant = sig_sp_mean_by,
                             permutation = "stratified",
                             statistic = "mean",
                             adjustment = "BY")
  out = 
    rbind(df_std_de,
        df_sp_de,
        df_std_mean,
        df_sp_mean,
        df_std_de_by,
        df_sp_de_by,
        df_std_mean_by,
        df_sp_mean_by)
  cbind(out, sim = sim_name)
}

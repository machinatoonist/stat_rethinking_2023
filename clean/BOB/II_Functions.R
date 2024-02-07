# Information Imbalance R Implementation

# 1. Libraries -----------------------------------------------------------------
library(parallel)  # For parallel computation.
library(ggplot2)   # For plotting.
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



# 2. Detect Cores --------------------------------------------------------------
cores <- detectCores() # Equivalent of multiprocessing.cpu_count()
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



# 3. Metric Comparions class ---------------------------------------------------
MetricComparisons <- function(coordinates=NULL, distances=NULL, maxk=NULL, period=NULL, verbose=FALSE, metric=NULL, n_jobs=cores) {
  if (is.null(maxk) && !is.null(coordinates)) {
    maxk <- nrow(coordinates)-1
  }
  
  list(
    coordinates = coordinates,
    distances = distances,
    maxk = maxk,
    period = period,
    verbose = verbose,
    n_jobs = n_jobs,
    metric = metric
  )
}
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=




return_inf_imb_full_selected_coords <- function(metric_comparisons, coord_list, k = 1) {
  
  # Check if distances have been computed, if not, compute them
  if (is.null(metric_comparisons$distances)) {
    metric_comparisons <- compute_distances(metric_comparisons)  # Replace with actual distance computation function
  }
  
  if (metric_comparisons$verbose) {
    cat("Total number of computations is:", length(coord_list), "\n")
  }
  
  # Assuming 'return_inf_imb_target_selected_coords' is a function that has been defined
  imbalances <- vector("list", length(coord_list))
  for (i in seq_along(coord_list)) {
    imbalances[[i]] <- return_inf_imb_target_selected_coords(metric_comparisons, coord_list[[i]], k)
  }
  
  # Convert the list of imbalances to a matrix or other suitable data structure
  # Depending on the expected structure of the output, adjust the following line
  return(do.call(rbind, imbalances))
}

compute_distances <- function(metric_comparisons, maxk = NULL, metric = "euclidean", period = NULL, n_jobs = NULL) {
  if (metric_comparisons$verbose) {
    cat("Computation of distances started\n")
    start_time <- Sys.time()
  }
  
  metric_comparisons$metric <- metric
  
  if (!is.null(period)) {
    if (is.numeric(period) && length(period) == metric_comparisons$dims) {
      metric_comparisons$period <- period
    } else if (is.numeric(period) && length(period) == 1) {
      metric_comparisons$period <- rep(period, metric_comparisons$dims)
    } else {
      stop("'period' must be either a numeric scalar or a numeric vector of length equal to the number of dimensions")
    }
  }
  
  if (!is.null(maxk)) {
    metric_comparison$maxk <- maxk
  } else {
    stopifnot(!is.null(metric_comparisons$maxk))
  }
  
  if (metric_comparisons$verbose && !is.null(period)) {
    cat("Computing periodic distances. The coordinates are assumed to be in the range [0, period]\n")
  }
  
  if (!is.null(n_jobs)) {
    metric_comparisons$n_jobs <- n_jobs
  }
  
  if (metric_comparisons$verbose) {
    cat(sprintf("Computation of the distances up to %d NNs started\n", metric_comparisons$maxk))
  }
  
  # Placeholder for compute_nn_distances function - needs to be defined or adapted for R
  out <- compute_nn_distances(metric_comparisons)
  # out[[1]] is equivalent to "distances".
  # out[[2]] is equivalent to "dist_indices".
  metric_comparisons$distances <- out[[1]]  # Replace with actual computation
  metric_comparisons$dist_indices <- out[[2]]  # Replace with actual computation
  
  end_time <- Sys.time()
  if (metric_comparisons$verbose) {
    cat(sprintf("%.2f seconds for computing distances\n", as.numeric(difftime(end_time, start_time, units = "secs"))))
  }
  
  return(metric_comparisons)
}



compute_nn_distances <- function(metric_comparisons) {
  
  out <- compute_cross_nn_distances(metric_comparisons)
  # out[[1]] is equivalent to "distances".
  # out[[2]] is equivalent to "dist_indices".
  
  # Check for zero distances (indicating identical points)
  zero_dists <- sum(out[[1]][,-1] <= 1.01 * .Machine$double.eps)
  if (zero_dists > 0) {
    warning(
      "There are points with neighbors at 0 distance, meaning the dataset probably has identical points.\n",
      "This can cause problems in various routines.\n",
      "We suggest to either perform smearing of distances or remove identical points."
    )
  }
  
  return(list(distances = out[[1]], dist_indices = out[[2]]))
}


library(FNN)

compute_cross_nn_distances <- function(metric_comparisons) {
  X <- metric_comparisons$coordinates
  X_new <- metric_comparisons$coordinates
  if (is.null(metric_comparisons$period)) {
    # Non-periodic boundary condition case
    if (metric_comparisons$metric == "euclidean" || metric_comparisons$metric == "manhattan") {
      # Euclidean or Manhattan distance
      nn <- get.knnx(data = X, query = X_new, k = metric_comparisons$maxk+1, algorithm = "cover_tree")
    } else {
      stop("Unsupported metric for non-periodic boundary conditions")
    }
    
    distances <- nn$nn.dist
    dist_indices <- nn$nn.index
    
    # Adjust distances for Hamming distance, if applicable
    if (metric_comparisons$metric == "hamming") {
      distances <- distances * ncol(X)
    }
  } else {
    # Periodic boundary condition case - requires custom implementation or specialized package
    stop("Periodic boundary conditions are not supported in this implementation")
  }
  
  return(list(distances = distances, dist_indices = dist_indices))
}















return_inf_imb_target_selected_coords <- function(metric_comparisons, coord_list, k = 1) {
  stopifnot(nrow(metric_comparisons$dist_indices) == nrow(metric_comparisons$coordinates))  # Ensure target_ranks and X have the same number of rows
  
  if (metric_comparisons$verbose){
    cat("Total number of computations is:", length(coord_list), "\n")
  }
  
  # Assuming 'return_imb_with_coords' (renamed) is a function defined in R
  n1s_n2s <- return_imb_with_coords(metric_comparisons, coord_list, k)
  
  # Convert the list of results to a matrix or other suitable data structure
  return(do.call(cbind, n1s_n2s))
}







return_imb_with_coords <- function(metric_comparisons, coords, k) {
  # Selecting a subset of coordinates from X
  X_subset <- metric_comparisons
  X_subset$coordinates <- X_subset$coordinates[, coords]
  # Handling the period parameter
  if (!is.null(metric_comparisons$period)) {
    if (is.numeric(metric_comparisons$period) && length(metric_comparisons$period) == metric_comparisons$dims) {
      # If period is an array with the same length as the number of dimensions
      period_subset <- metric_comparisons$period[coords]
    } else if (is.numeric(metric_comparisons$period) && length(metric_comparisons$period) == 1) {
      # If period is a single numeric value
      period_subset <- rep(metric_comparisons$period, length(coords))
    } else {
      stop("'period' must be either a numeric scalar or a numeric vector of length equal to the number of dimensions")
    }
  } else {
    period_subset <- NULL
  }
  
  # Compute nearest neighbors for the subset
  nn_distances_subset <- compute_nn_distances(X_subset)
  
  # Compute information imbalance
  # Assuming '_return_imbalance' is a function defined in R
  imb_coords_full <- return_imbalance(nn_distances_subset$dist_indices, metric_comparisons$dist_indices, k)
  imb_full_coords <- return_imbalance(metric_comparisons$dist_indices, nn_distances_subset$dist_indices, k)
  
  return(list(imb_coords_full, imb_full_coords))
}


return_imbalance <- function(dist_indices_1, dist_indices_2, k = 1) {
  stopifnot(nrow(dist_indices_1) == nrow(dist_indices_2))
  
  N <- nrow(dist_indices_1)
  
  # Assuming '_return_ranks' is a function defined in R
  ranks <- return_ranks(dist_indices_1, dist_indices_2, k)
  
  imb <- mean(ranks) / (N / 2.0)
  
  return(imb)
}

return_ranks <- function(dist_indices_1, dist_indices_2, k = 1) {
  stopifnot(nrow(dist_indices_1) == nrow(dist_indices_2))
  
  N <- nrow(dist_indices_1)
  maxk_2 <- ncol(dist_indices_2)
  
  conditional_ranks <- matrix(0, nrow = N, ncol = k)
  
  for (i in 1:N) {
    idx_k_d1 <- dist_indices_1[i, 2:(k + 1)]  # Adjusted for R's 1-based indexing
    
    wr <- lapply(1:k, function(k_neighbor) {
      which(idx_k_d1[k_neighbor] == dist_indices_2[i,])
    })
    
    for (k_neighbor in 1:k) {
      if (length(wr[[k_neighbor]]) == 0) {
        conditional_ranks[i, k_neighbor] <- sample(maxk_2:N, 1)
      } else {
        conditional_ranks[i, k_neighbor] <- wr[[k_neighbor]][1]
      }
    }
  }
  
  return(conditional_ranks-1)
}



plot_inf_imb_plane <- function(imbalances, coord_list = NULL, labels = NULL) {
  # Create a data frame for plotting
  imb_df <- as.data.frame(imbalances)
  colnames(imb_df) <- c("imb1", "imb0")
  
  if (!is.null(coord_list)) {
    imb_df$label <- if (!is.null(labels)) {
      sapply(coord_list, function(coords) paste(labels[coords], collapse = ", "))
    } else {
      sapply(coord_list, function(coords) paste(coords, collapse = ", "))
    }
  } else {
    imb_df$label <- ""
  }
  
  # Creating the plot
  p <- ggplot(imb_df, aes(x = imb0, y = imb1, color = label)) +
    geom_point(size = 5, alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    xlab(expression(Delta(X["full"]  %->%  X["coords"]))) +
    ylab(expression(Delta(X["coords"]  %->%  X["full"]))) +
    theme_bw() +
    labs(colour="X Coordinates") #+
    #xlim(0,1) +
    #ylim(0,1)
  
  print(p)
}



return_inf_imb_two_selected_coords <- function(metric_comparisons, coords1, coords2, k = 1) {
  # Selecting subsets of coordinates from X
  X_subset1 <- metric_comparisons
  X_subset1$coordinates <- X_subset1$coordinates[, coords1]
  
  X_subset2 <- metric_comparisons
  X_subset2$coordinates <- X_subset2$coordinates[, coords2]
  
  # Compute nearest neighbor distances for the first subset of coordinates
  dist_indices_i <- compute_nn_distances(X_subset1)[[2]]
  
  # Compute nearest neighbor distances for the second subset of coordinates
  dist_indices_j <- compute_nn_distances(X_subset2)[[2]]
  
  # Calculate information imbalances
  imb_ij <- return_imbalance(dist_indices_i, dist_indices_j, k = k)
  imb_ji <- return_imbalance(dist_indices_j, dist_indices_i, k = k)
  
  return(list(imb_ij, imb_ji))
}







return_inf_imb_full_all_coords <- function(metric_comparisons, k = 1) {
  ncoords <- ncol(metric_comparisons$coordinates)
  
  # Create a list of single-feature coordinate subsets
  coord_list <- lapply(1:ncoords, function(i) c(i))
  
  # Use the previously defined function to compute imbalances
  imbalances <- return_inf_imb_full_selected_coords(metric_comparisons, coord_list, k)
  
  return(imbalances)
}





greedy_feature_selection_full <- function(metric_comparisons, n_coords, k = 1, n_best = 10) {
  if (is.null(metric_comparisons$distances)) {
    metric_comparisons <- compute_distances(metric_comparisons)  # Replace with actual distance computation function
  }
  
  # Call the greedy_feature_selection_target function
  results <- greedy_feature_selection_target(metric_comparisons, n_coords, k, n_best)
  
  return(results)
}






return_inf_imb_target_all_coords <- function(metric_comparisons, k = 1) {
  
  ncoords <- ncol(metric_comparisons$coordinates)
  coord_list <- lapply(1:ncoords, function(i) c(i))
  
  # Assuming 'return_inf_imb_target_selected_coords' is a function that has been defined
  imbalances <- vector("list", length(coord_list))
  for (i in seq_along(coord_list)) {
    imbalances[[i]] <- return_inf_imb_target_selected_coords(metric_comparisons, coord_list[[i]], k)
  }
  
  # Convert the list of imbalances to a matrix or other suitable data structure
  # Depending on the expected structure of the output, adjust the following line
  return(do.call(rbind, imbalances))
}







greedy_feature_selection_target <- function(metric_comparisons, n_coords, k, n_best) {
  
  dims <- ncol(metric_comparisons$coordinates)  # Number of features/variables
  cat("Total number of computations is:", length(coord_list), "\n")
  
  # Compute initial imbalances
  imbalances <- return_inf_imb_target_all_coords(metric_comparisons, k)
  
  selected_coords <- order(imbalances[,2])[1:n_best]
  
  selected_coords <- lapply(selected_coords, function(x) c(x))
  best_one <- selected_coords[[1]]
  best_tuples <- list(best_one)
  best_imbalances <- list(c(imbalances[best_one[1], 1], imbalances[best_one[1], 2]))
  all_imbalances <- list(list(imbalances[, 1], imbalances[, 2]))
  
  if (metric_comparisons$verbose) {
    cat("Best single variable selected:", best_one, "\n")
  }
  
  all_single_coords <- 1:dims
  
  while (length(best_tuples) < n_coords) {
    c_list <- list()
    for (i in selected_coords) {
      for (j in all_single_coords) {
        if (!(j %in% i)) {
          ii <- c(i, j)
          c_list <- c(c_list, list(ii))
        }
      }
    }
    coord_list <- unique(c_list)
    cat("Total number of computations is:", length(coord_list), "\n")
    
    # Assuming 'return_inf_imb_target_selected_coords' is a function that has been defined
    imbalances_ <- vector("list", length(coord_list))
    for (i in seq_along(coord_list)) {
      imbalances_[[i]] <- return_inf_imb_target_selected_coords(metric_comparisons, coord_list[[i]], k)
    }
    imbalances_ <- do.call(rbind, imbalances_)
    
    to_select <- order(imbalances_[,2])[1:n_best]
    
    best_ind <- to_select[1]
    best_tuples <- c(best_tuples, list(coord_list[[best_ind]]))
    best_imbalances <- c(best_imbalances, list(c(imbalances_[best_ind, 1], imbalances_[best_ind, 2])))
    all_imbalances <- c(all_imbalances, list(list(imbalances_[, 1], imbalances_[, 2])))
    selected_coords <- lapply(coord_list[to_select], function(x) as.integer(x))
  }
  
  return(list(best_tuples = best_tuples, best_imbalances = best_imbalances, all_imbalances = all_imbalances))
}

























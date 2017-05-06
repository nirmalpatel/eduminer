#' @import dplyr
#' @importFrom magrittr %>%
#' 
#' @export
keep_first_responses <- function(x) {
  
  xcols <- c("testid", "userid", "itemid", "response_order", "response")
  
  stopifnot(x %hascols% xcols)
  
  x <- x %keepcols% xcols
    
  x %>%
    arrange_(~ testid, ~ userid, ~ itemid, ~ response_order) %>%
    group_by_(~ testid, ~ userid, ~ itemid) %>%
    filter_(~ row_number() == 1) %>%
    ungroup()
}

#' @import dplyr
#' @importFrom magrittr %>%
#' 
#' @export
filter_tests <- function(x, items_bw, users_bw, verbose = TRUE) {
  
  xcols <- c("testid", "userid", "itemid", "response")
  
  stopifnot(x %hascols% xcols)
  
  x <- x %keepcols% xcols
  
  keep_tests_df <- x %>%
    group_by_(~ testid) %>%
    summarise_(n_items = ~ n_distinct(itemid),
               n_users = ~ n_distinct(userid)) %>%
    ungroup() %>%
    filter_(~ between(n_items, items_bw[1], items_bw[2]),
            ~ between(n_users, users_bw[1], users_bw[2]))
  
  if (verbose == TRUE) {
    
    all_tests <- unique(x[["testid"]])
    keep_tests <- unique(keep_tests_df[["testid"]])
    removed_tests <- setdiff(all_tests, keep_tests)
    
    cat("Removed", length(removed_tests), "tests:", paste(removed_tests, collapse = ", "), "\n")
    cat("\n")
  }
  
  x %>%
    semi_join(keep_tests_df, by = "testid")
}

#' @import dplyr
#' @importFrom purrr walk
#' @importFrom magrittr %>%
#' 
#' @export
filter_test_items <- function(x, pct_users_bw, verbose = TRUE) {
  
  xcols <- c("testid", "userid", "itemid", "response")
  
  stopifnot(x %hascols% xcols)
  
  x <- x %keepcols% xcols
  
  test_users_df <- x %>%
    group_by_(~ testid) %>%
    summarise_(test_users = ~ n_distinct(userid)) %>%
    ungroup()
  
  keep_items_df <- x %>%
    group_by_(~ testid, ~ itemid) %>%
    summarise_(item_users = ~ n_distinct(userid)) %>%
    ungroup() %>%
    inner_join(test_users_df, by = "testid") %>%
    filter_(~ between(item_users / test_users * 100, pct_users_bw[1], pct_users_bw[2]))
  
  if (verbose == TRUE) {
    remove_items_df <- x %>%
      anti_join(keep_items_df, by = c("testid", "itemid"))
    
    walk(split(remove_items_df, remove_items_df[["testid"]]), function(test_data) {
      test_name <- test_data[["testid"]][1]
      item_names <- sort(unique(test_data[["itemid"]]))
      
      cat("Test", test_name, "\n")
      cat("Removed items:", paste(item_names, collapse = ", "), "\n")
      cat("\n")
    })
  }
  
  x %>%
    semi_join(keep_items_df, by = c("testid", "itemid"))
  
}

#' @import dplyr
#' @importFrom purrr walk
#' @importFrom magrittr %>%
#' 
#' @export
filter_test_users <- function(x, pct_items_bw, verbose = TRUE) {
  
  xcols <- c("testid", "userid", "itemid", "response")
  
  stopifnot(x %hascols% xcols)
  
  x <- x %keepcols% xcols
  
  test_items_df <- x %>%
    group_by_(~ testid) %>%
    summarise_(test_items = ~ n_distinct(itemid)) %>%
    ungroup()
  
  keep_users_df <- x %>%
    group_by_(~ testid, ~ userid) %>%
    summarise_(user_items = ~ n_distinct(itemid)) %>%
    ungroup() %>%
    inner_join(test_items_df, by = "testid") %>%
    filter_(~ between(user_items / test_items * 100, pct_items_bw[1], pct_items_bw[2]))
  
  if (verbose == TRUE) {
    remove_users_df <- x %>%
      anti_join(keep_users_df, by = c("testid", "userid"))
    
    walk(split(remove_users_df, remove_users_df[["testid"]]), function(test_data) {
      test_name <- test_data[["testid"]][1]
      user_names <- sort(unique(test_data[["userid"]]))
      
      cat("Test", test_name, "\n")
      cat("Removed users:", paste(user_names, collapse = ", "), "\n")
      cat("\n")
    })
  }
  
  x %>%
    semi_join(keep_users_df, by = c("testid", "userid"))
  
}

#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @export
response_matrix <- function(x, keep_userid = FALSE) {
  
  stopifnot(x %hascols% c("userid", "itemid", "response"))
  
  out <- x %>%
    select_(~ userid, ~ itemid, ~ response) %>%
    spread_("itemid", "response")
  
  if (keep_userid == TRUE) {
    out
  } else {
    out %>%
      select_(~ -userid)
  }
}
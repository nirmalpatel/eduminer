#' @import dplyr
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @export
point_biserial <- function(x) {
  
  stopifnot(x %hascols% c("testid", "userid", "itemid", "response"))
  
  out <- map(split(x, x[["testid"]]), function(test_data) {
    
    test_name <- unique(test_data[["testid"]])[[1]]
    
    result <- point_biserial_internal(test_data %>% select_(~ -testid)) %>%
      mutate_(testid = ~ test_name) %>%
      select_(~ testid, ~ everything())
    
    result
  })
  
  bind_rows(out)
}

#' @import dplyr
#' @importFrom ltm biserial.cor
#' @importFrom purrr map
#' @importFrom magrittr %>%
point_biserial_internal <- function(x) {

  stopifnot(x %hascols% c("userid", "itemid", "response"))
  
  item_responses <- response_matrix(x) %>%
    select_(~ -userid) %>%
    as.matrix()

  itemids <- colnames(item_responses)

  out <- map(1:ncol(item_responses), function(i) {
    cors <- tryCatch(expr = {

      score_included <- rowMeans(item_responses, na.rm = TRUE)
      score_excluded <- rowMeans(item_responses[, -i, drop = FALSE], na.rm = TRUE)
      response_col <- item_responses[, i]
      
      if (all(c(0, 1) %in% response_col)) {
        c(biserial.cor(score_included, response_col, use = "complete.obs", level = 2),
          biserial.cor(score_excluded, response_col, use = "complete.obs", level = 2))  
      } else {
        stop("Item ", itemids[[i]], " has no variance in response.")
      }

    }, error = function(e) {
      
      warning(e)
      c(NA, NA)
    })

    tibble(itemid = itemids[[i]], biserial_incl = cors[[1]], biserial_excl = cors[[2]])
  })

  bind_rows(out)
}

#' @import dplyr
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @export
cronbach_alpha <- function(x) {
  stopifnot(x %hascols% c("testid", "userid", "itemid", "response"))
  
  out <- map(split(x, x[["testid"]]), function(test_data) {
    
    test_name <- unique(test_data[["testid"]])[[1]]
    
    result <- cronbach_alpha_internal(test_data %>% select_(~ -testid)) %>%
      mutate_(testid = ~ test_name) %>%
      select_(~ testid, ~ everything())
    
    result
  })
  
  bind_rows(out)
}

#' @import dplyr
#' @importFrom ltm cronbach.alpha
#' @importFrom purrr map
#' @importFrom magrittr %>%
cronbach_alpha_internal <- function(x) {

  stopifnot(x %hascols% c("userid", "itemid", "response"))

  item_responses <- response_matrix(x) %>%
    select_(~ -userid) %>%
    as.matrix()

  itemids <- colnames(item_responses)

  alpha_allitems <- tryCatch(expr = {

    cronbach.alpha(item_responses, na.rm = TRUE)$alpha

    }, error = function(e) {

      warning("Error calculating Cronbach's alpha for all items")
      NA
    })

  out <- map(1:ncol(item_responses), function(i) {

    alpha_oneitem <- tryCatch(expr = {

      cronbach.alpha(item_responses[, -i], na.rm = TRUE)$alpha

    }, error = function(e) {

      warning("Error calculating Cronbach's alpha alpha for item ", itemids[[i]])
      NA
    })

    tibble(itemid = itemids[[i]], alpha_incl = alpha_allitems, alpha_excl = alpha_oneitem)
  })

  bind_rows(out)
}

#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
item_descript <- function(x) {

  stopifnot(x %hascols% c("testid", "userid", "itemid", "response"))

  x %>%
    group_by_(~ testid, ~ itemid) %>%
    summarise_(n_responses = ~ n_distinct(userid),
              pct_correct = ~ prop_value(response, 1) * 100) %>%
    ungroup()
}

#' @importFrom mirt mirt
#' @importFrom purrr map
#' 
#' @export
irt_models <- function(x) {
  
  xcols <- c("testid", "userid", "itemid", "response")
  
  stopifnot(x %hascols% xcols)
  
  x <- x %keepcols% xcols
  
  out <- map(split(x, x[["testid"]]), function(test_data) {
    test_name <- unique(test_data[["testid"]])[[1]]
    test_items <- sort(unique(test_data[["itemid"]]))
    
    test_mod <- tryCatch({
      test_mod <- mirt(response_matrix(test_data) %>% as.matrix(), 1, "2PL")  
    }, error = function(e) {
      warning(e)
      NULL
    })
    
    list(test_name = test_name,
         test_items = test_items,
         test_mod = test_mod)
  })
  
  out
}

#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @importFrom tibble tibble
#' 
#' @export
irt_coef <- function(x) {
  
  require(mirt)
  
  out <- map(x, function(test_data) {
  
    if (is.null(test_data[["test_mod"]])) {
      
      tibble(testid = test_data[["test_name"]],
             itemid = test_data[["test_items"]],
             difficulty = NA,
             discrimination = NA)
    } else {
      mod_pars <- coef(test_data[["test_mod"]], IRTpars = TRUE, simplify = TRUE)$items
      
      tibble(testid = test_data[["test_name"]],
             itemid = rownames(mod_pars),
             difficulty = mod_pars[, "b", drop = TRUE],
             discrimination = mod_pars[, "a", drop = TRUE])
    }
  })
  
  bind_rows(out)
}
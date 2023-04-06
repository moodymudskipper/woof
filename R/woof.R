
#' Compare two objects
#'
#' A simple wrapper to use `woof()` around `waldo::compare()`
#'
#' @inheritParams waldo::compare
#'
#' @return a "woof_compare" objects
#' @export
#' @examples
#' foo <- list(
#' a = "b",
#' list(c = list("d"), x = "e"),
#' f = "g"
#' )
#' attr(foo$a, "my_attr") <- "attr1"
#'
#' bar <- list(
#'   a = "B",
#'   list(c = list("D"), x = "E")
#' )
#' attr(bar$a, "my_attr") <- "attr2"
#'
#' w <- woof_compare(foo, bar)
#'
#' w # prints like waldo::compare's output
#' w$`2`
#'
#' w$`1`$..attr
#'
#' \dontrun{
#' library(ggplot2)
#' p1 <- ggplot(cars, aes(speed, dist)) + geom_point()
#' p2 <- ggplot(cars, aes(speed, dist)) + geom_line()
#' w <- woof_compare(p1, p2)
#'
#' # the original output is huge, but we can navigate it easily with autocomplete
#' w$layers$`1`$geom_params
#' }
woof_compare <- function(
    x,
    y,
    ...,
    x_arg = "old",
    y_arg = "new",
    tolerance = NULL,
    max_diffs = if (in_ci()) Inf else 10,
    ignore_srcref = TRUE,
    ignore_attr = "waldo_opts",
    ignore_encoding = TRUE,
    ignore_function_env = FALSE,
    ignore_formula_env = FALSE,
    list_as_map = FALSE
) {
  x <- waldo::compare(
    x,
    y,
    ...,
    x_arg = x_arg,
    y_arg = y_arg,
    tolerance = tolerance,
    max_diffs = max_diffs,
    ignore_srcref = ignore_srcref,
    ignore_attr = ignore_attr,
    ignore_encoding = ignore_encoding,
    ignore_function_env = ignore_function_env,
    ignore_formula_env = ignore_formula_env,
    list_as_map = list_as_map
  )
  woof(x)
}

#' Structure waldo's output
#'
#' {woof} is {waldo}'s companion. Given a "waldo_compare" object it returns
#' a nested list easier to explore and programatically work with. Any subset
#' of the output prints like `waldo:compare()`'s output does.
#'
#' The output replicates the structure of the original compared objects but :
#' * only elements that changed are shown
#' * due to the above numeric indexes are converted to character
#' * attributes and environments are provided as elements, for easier exploration
#'
#' @param x a "waldo_compare" object
#'
#' @return a "woof_compare" object
#' @export
#' @examples
#' foo <- list(
#' a = "b",
#' list(c = list("d"), x = "e"),
#' f = "g"
#' )
#'
#' bar <- list(
#'   a = "B",
#'   list(c = list("D"), x = "E")
#' )
#' x <- waldo::compare(foo, bar)
#' w <- woof(x)
#' w$`2`
woof <- function(x) {
  # account for different type of separations in waldo, we have
  # `lhs` ...
  # lhs | ...
  # lhs vs ...
  # (and spaces)
  lhs <- sub("^`? *(.*?)(`| vs | +[|] +).*", "\\1", as.character(x))
  lhs <- gsub("_class", "..class", lhs)
  lhs <- gsub("_inherit", "..inherit", lhs)
  indices_expr <- parse(text = sub("^`(.*?)`.*", "\\1", lhs))

  recursor <- function() structure(list(), class = "woof_compare")
  old <- recursor()

  ops <- list(
    names = function(x) {
      input <- substitute(x)
      if (is.null(x[["..names"]])) eval(substitute(X[["..names"]] <<- recursor(), list(X = input)), ops)
      x[["..names"]]
    },
    `names<-` = function(x, value) {
      x[["..names"]]
      x[["..names"]] <- value
      x
    },
    environment = function(x) {
      input <- substitute(x)
      if (is.null(x[["..env"]])) eval(substitute(X[["..env"]] <<- recursor(), list(X = input)), ops)
      x[["..env"]]
    },
    `environment<-` = function(x, value) {
      x[["..env"]]
      x[["..env"]] <- value
      x
    },
    attr = function(x, y) {
      input <- substitute(x)
      if (is.null(x[["..attr"]])) {
        eval(substitute(X[["..attr"]] <<- recursor(), list(X = input)), ops)
      }
      if (is.null(x[["..attr"]][[y]])) {
        eval(substitute(X[["..attr"]][[y]] <<- recursor(), list(X = input)), ops)
      }
      x[["..attr"]][[y]]
    },
    `attr<-` = function(x, y, value) {
      x[["..attr"]][[y]]
      x[["..attr"]][[y]] <- value
      x
    },
    class = function(x) {
      input <- substitute(x)
      if (is.null(x[["..class"]])) eval(substitute(X[["..class"]] <<- recursor(), list(X = input)), ops)
      x[["..class"]]
    },
    `class<-` = function(x, value) {
      x[["..class"]]
      x[["..class"]] <- value
      x
    },
    body = function(x) {
      input <- substitute(x)
      if (is.null(x[["..body"]])) eval(substitute(X[["..body"]] <<- recursor(), list(X = input)), ops)
      x[["..body"]]
    },
    `body<-` = function(x, value) {
      x[["..body"]]
      x[["..body"]] <- value
      x
    },
    formals = function(x) {
      input <- substitute(x)
      if (is.null(x[["..formals"]])) eval(substitute(X[["..formals"]] <<- recursor(), list(X = input)), ops)
      x[["..formals"]]
    },
    `formals<-` = function(x, value) {
      x[["..formals"]]
      x[["..formals"]] <- value
      x
    },
    `[[` = function(x, i) {
      input <- substitute(x)
      i_chr <- as.character(i)
      if (identical(input, quote(`*tmp*`))) return(.subset2(x, i_chr))
      if (is.null(x[[i_chr]])) eval(substitute(X[[i_chr]] <<- recursor(), list(X = input)), ops)
      eval(substitute(.subset2(X, i_chr), list(X = input)), ops)
    },
    `[[<-` = function(x, i, value) {
      #browser()
      i <- as.character(i)
      x[[i]]
      x <- base::`[[<-`(x, i, value)
      x
    },
    `$` = function(x, i) {
      input <- substitute(x)
      i <- as.character(substitute(i)) # symbol to char ind
      #browser()
      if (identical(input, quote(`*tmp*`))) return(.subset2(x, i))
      i_chr <- as.character(i)
      if (is.null(x[[i]])) eval(substitute(X[[i]] <<- recursor(), list(X = input)), ops)
      eval(substitute(.subset2(X, i), list(X = input)), ops)
    },
    `$<-` = function(x, i, value) {
      input <- substitute(x)
      i <- as.character(substitute(i)) # symbol to char ind
      #browser()
      x[[i]]
      x <- base::`[[<-`(x, i, value)
      x
    },
    `[` = function(x, ...) x,
    `[<-` = function(x, i, value) {
      value
    }
  )

  for (i in seq_along(indices_expr)) {
    eval(indices_expr[[i]], ops)
    #eval(substitute(CALL$..compare <<- DESCR, list(CALL = indices_expr[[i]], DESCR = x[i])), ops)
    eval(substitute(CALL$..compare <<- structure(DESCR, class = "woof_compare"), list(CALL = indices_expr[[i]], DESCR = x[i])), ops)
  }
  old
}

in_ci <- function () {
  isTRUE(as.logical(Sys.getenv("CI", "FALSE")))
}

#' @export
print.woof_compare <- function(x, ...) {
  cat(unlist(x), sep = "\n\n")
  invisible(x)
}

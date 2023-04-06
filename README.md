
<!-- README.md is generated from README.Rmd. Please edit that file -->

# woof

{woof} is {waldo}’s [companion](https://waldo.fandom.com/wiki/Woof).

- Given a “waldo_compare” object `woof::woof()` returns a nested list
  easier to explore and programatically work with. Any subset of the
  output prints like `waldo:compare()`’s output does.

- `woof::woof_compare()` is a drop-in replacement for
  `waldo::compare()`, it prints just the same but can be subetted.

## Installation

Install with:

``` r
devtools::install_github("moodymudskipper/woof")
```

## Example

``` r
library(woof)

# we build 2 different lists
foo <- list(
  a = "b",
  list(c = list("d"), x = "e"),
  f = "g"
)
attr(foo$a, "my_attr") <- "attr1"

bar <- list(
  a = "B",
  list(c = list("D"), x = "E")
)
attr(bar$a, "my_attr") <- "attr2"

w <- woof_compare(foo, bar)

w # prints like waldo::compare's output
#> `old` is length 3
#> `new` is length 2
#> 
#> `names(old)`: "a" "" "f"
#> `names(new)`: "a" ""    
#> 
#> `attr(old[[1]], 'my_attr')`: "attr1"
#> `attr(new[[1]], 'my_attr')`: "attr2"
#> 
#> `old[[1]]`: "b"
#> `new[[1]]`: "B"
#> 
#> `old[[2]]$c[[1]]`: "d"
#> `new[[2]]$c[[1]]`: "D"
#> 
#> `old[[2]]$x`: "e"
#> `new[[2]]$x`: "E"
#> 
#> `old[[3]]` is a character vector ('g')
#> `new[[3]]` is absent

w$`2`
#> `old[[2]]$c[[1]]`: "d"
#> `new[[2]]$c[[1]]`: "D"
#> 
#> `old[[2]]$x`: "e"
#> `new[[2]]$x`: "E"

w$`1`$..attr
#> `attr(old[[1]], 'my_attr')`: "attr1"
#> `attr(new[[1]], 'my_attr')`: "attr2"

library(ggplot2)
p1 <- ggplot(cars, aes(speed, dist)) + geom_point()
p2 <- ggplot(cars, aes(speed, dist)) + geom_line()
w <- woof_compare(p1, p2)

# the original output is huge, but we can navigate it easily with autocomplete
w$layers$`1`$geom_params
#> `old$layers[[1]]$geom_params` is length 1
#> `new$layers[[1]]$geom_params` is length 2
#> 
#> `names(old$layers[[1]]$geom_params)`: "na.rm"              
#> `names(new$layers[[1]]$geom_params)`: "na.rm" "orientation"
#> 
#> `old$layers[[1]]$geom_params$orientation` is absent
#> `new$layers[[1]]$geom_params$orientation` is a logical vector (NA)
```

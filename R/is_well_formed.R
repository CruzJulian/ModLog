### FUNCTIONS FOR EVALUATE IS_WFF

characters <- function(a, k = NULL){

  chars <- c(LETTERS, parentheses, unary_symbols, binary_symbols)

  temp <- gsub(" ", "", a)

  "for"(i, 1:length(symbols_$symbols), gsub(symbols_$symbols[i], symbols_$numeric[i], temp, fixed = TRUE) -> temp)

  temp <- substring(temp, 1:nchar(temp), 1:nchar(temp))

  temp[temp %in% symbols_$numeric] <- symbols_$symbols[as.numeric(temp[temp %in% symbols_$numeric])]

  if(any(!temp %in% chars)) stop(Error.1)
  c(" ", temp, " ") -> ret
  "if"(is.null(k), ret, ret[k]) -> ret
  f1 -> class(ret)
  ret
}

types <- function(a.f1){

  temp <- "if"(f1 %in% class(a.f1), a.f1, characters(a.f1))

  ret <- rep(7, length(temp)) #No me gusta

  unaries <- which(temp %in% unary_symbols)
  binaries <- which(temp %in% binary_symbols)
  open <- which(temp %in% open_parentheses)
  close <- which(temp %in% close_parentheses)
  term <- which(temp %in% LETTERS)

  ret[unaries] <- 2
  ret[binaries] <- 3
  ret[open] <- 4
  ret[close] <- 5
  ret[term] <- 6
  ret[1] <- 1

  class(ret) <- c(f2)
  ret
}

parentheses_function <- function(a.f2){

  temp <- "if"(f2 %in% class(a.f2), a.f2, types(a.f2))

  values <- c(0, 0, 0, 1, -1, 0, 0)
  temp <- values[temp]
  cumsum(temp)

}

is_binary_well_formed <- function(a.f2){

  temp <- "if"(f2 %in% class(a.f2), a.f2, types(a.f2))

  values <- c(0, 0, 1, 0, 0, -1, 0)
  temp <- values[temp]
  partial_sums <- cumsum(temp)
  length(unique(partial_sums)) < 3

}

is_a_good_walk <- function(a.f2){

  temp <- "if"(f2 %in% class(a.f2), a.f2, types(a.f2))

  temp <- apply(
    X = data.frame(
      var1 = object_types[temp[-length(temp)]],
      var2 = object_types[temp[-1]]
    ),
    MARGIN = 1,
    FUN = paste,
    sep = "",
    collapse =""
  )

  possible_types <- apply(
    X = expand.grid(object_types, object_types),
    MARGIN = 1,
    FUN = paste,
    sep = "",
    collapse =""
  )

  observed_walk <-
    matrix(
      data = as.numeric(possible_types %in% temp),
      ncol = 7
    )

  !any(
    ! observed_walk <= walk_matrix
  )

}

is_term <- function(a){

  "if"(!is_well_formed(a), stop(Error.3))
  temp <- characters(a)
  !any(!(types(temp) %in% c(1,6,7)))

}

is_well_formed<-function(a){
  a <- types(a)
  the_parentheses <- parentheses_function(a)
  !any(
    !is_a_good_walk(a),
    tail(the_parentheses, 1) != 0,
    any(the_parentheses < 0),
    !is_binary_well_formed(a)
  )
}



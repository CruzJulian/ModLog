data_frame_wff <- function(a, id = runif(1), TERM = 1, by = 0){

  "if"(!is_well_formed(a), stop(Error.3))

  data.frame(
    id = id,
    name = a,
    TERM = TERM,
    by = by
  ) -> base

  unary_symbols -> .
  length(.) -> .
  rep(0, .) -> .
  t(.) -> .
  data.frame(.) -> .
  setNames(object = ., nm = unary_symbols) -> unary

  binary_symbols -> .
  length(.) -> .
  rep(0, .) -> .
  t(.) -> .
  data.frame(.) -> .
  setNames(object = ., nm = binary_symbols) -> binary

  data.frame(
    branche.1 = 1
  ) -> branches

  data.frame(
    world.1 = 1
  ) -> worlds

  build_f4(base = base, unary = unary, binary = binary, branches = branches, worlds = worlds)

}

# Clasificador
principal_symbol <- function(a){

  balanced <- parentheses_function(a) == 0
  unaries <- types(a) == 2
  binaries <- types(a) == 3

  balanced_unaries <- balanced & unaries
  balanced_binaries <- balanced & binaries

  "if"(xor(sum(balanced_unaries) > 1, sum(balanced_binaries) > 1), stop(Error.3))

  "if"(any(balanced_unaries), which(balanced_unaries), "if"(any(balanced_binaries), which(balanced_binaries), Error.3))
}


cut_formula<-function(a.f1, j){

  temp <- "if"(f1 %in% class(a.f1), a.f1, characters(a.f1))

  n <- length(temp)

  ret <- NULL

  ret[1] <- paste(temp[3:(j-2)], collapse = "")
  ret[2] <- paste(temp[(j+2):(n-2)], collapse = "")

  if(!is_well_formed(ret[1]) | !is_well_formed(ret[2])) stop(Error.3)

  ret

}

build_f4 <- function(a.f4 = NULL, base = NULL, unary = NULL, binary = NULL, branches = NULL, worlds = NULL){

  if(!is.null(a.f4)){
    attr(a.f4, "data") -> data
    if(is.null(base)) data[["base"]] -> base
    if(is.null(unary)) data[["unary"]] -> unary
    if(is.null(binary)) data[["binary"]] -> binary
    if(is.null(branches)) data[["branches"]] -> branches
    if(is.null(worlds)) data[["worlds"]] -> worlds
  }

  cbind(base, unary, binary, branches, worlds) -> .
  c(class(.), f4) -> class(.)
  list("base" = base, "unary" = unary, "binary" = binary, "branches" = branches, "worlds" = worlds) -> attr(., "data")
  .
}

extract.data <- function(list.f4, data) "if"("list" %in% class(list.f4), lapply(list.f4, function(y) attr(y, "data")[[data]]), attr(list.f4, "data")[[data]])

rbind_f4 <- function(list.f4){

  base <- do.call(what = rbind, extract.data(list.f4, "base"))
  unary <- do.call(what = rbind, extract.data(list.f4, "unary"))
  binary <- do.call(what = rbind, extract.data(list.f4, "binary"))
  branches <- do.call(what = rbind, extract.data(list.f4, "branches"))
  worlds <- do.call(what = rbind, extract.data(list.f4, "worlds"))

  build_f4(base = base, unary = unary, binary = binary, branches = branches, worlds = worlds)

}


select <- function(a.f4, i, j = NULL){

  if(is.null(j)){
    base <- extract.data(a.f4, "base")
    unary <- extract.data(a.f4, "unary")
    binary <- extract.data(a.f4, "binary")
    branches <- extract.data(a.f4, "branches")
    worlds <- extract.data(a.f4, "worlds")

    base <- base[i,]
    unary <- unary[i, ]
    binary <- binary[i, ]
    branches <- branches[i, ]
    worlds <- worlds[i, ]

    build_f4(base = base, unary = unary, binary = binary, branches = branches, worlds = worlds)
  } else {
    "[.data.frame"(a.f4, i , j)
  }
}

alpha <- function(a.f4, i, k){

  sub_table <- select(a.f4, i)
  name <- select(a.f4, i, "name")
  id <- select(a.f4, i, "id")

  ps <- characters(name, k)
  attr(a.f4, "data")[["binary"]][i, ps] <- 1
  attr(a.f4, "data")[["base"]][i, "TERM"] <- 0

  extract.data(sub_table, "branches") -> branches
  extract.data(sub_table, "worlds") -> worlds

  forms <- lapply(cut_formula(name, k), data_frame_wff, by = id)
  forms <- lapply(forms, build_f4, branches = branches, worlds = worlds)
  rbind_f4(c(list(a.f4), forms))
}

beta <- function(a.f4, i, k){

  sub_table <- select(a.f4, i)
  name <- select(a.f4, i, "name")
  id <- select(a.f4, i, "id")

  ps <- characters(name, k)
  attr(a.f4, "data")[["binary"]][i, ps] <- 1
  attr(a.f4, "data")[["base"]][i, "TERM"] <- 0

  extract.data(sub_table, "branches") -> branches
  extract.data(sub_table, "worlds") -> worlds

  new_branches <- beta_branches(
    extract.data(a.f4, "branches"),
    branches
  )

  forms <- lapply(cut_formula(name, k), data_frame_wff, by = id)
  forms <- lapply(forms, build_f4, branches = branches, worlds = worlds)
  forms <- rbind_f4(c(list(a.f4), forms))
  build_f4(forms, branches = new_branches)
}


beta_branches <- function(branches_all, new_rows){
  cbind(
    rbind(branches_all, new_rows, new_rows*0),
    rbind(branches_all, new_rows*0, new_rows)
  )
}


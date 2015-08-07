
## SET SOME CONSTANTS FOR THE LOGICAL LANGUAGE


f1 <- "formula1"
f2 <- "formula2"
f3 <- "formula3"
f4 <- "ModLogDF"

Error.1 <- "There are non-logical characters un the formula string."
Error.2 <- "Unnapropiated object type"
Error.3 <- "This is not a well formed formula"
Error.4 <- "There is something wrong with the classes."
Error.5 <- "This is a term. It can not be negated forward."

name_branches = "branche"
name_worlds = "world"
names_format <- c(pre = "$", middle = "_", post = "$")

open_parentheses <- c("(", "[")
close_parentheses <- c(")", "]")
parentheses <- c(open_parentheses, close_parentheses)
unary_symbols <- c(NOT = "¬", POSS = "0")
binary_symbols <- c(OR = "+|", AND = "&", THEN = ">")

symbols_ <- data.frame(
  symbols = c(unary_symbols, binary_symbols),
  numeric = 1:length(c(unary_symbols, binary_symbols)),
  names = names(c(unary_symbols, binary_symbols)),
  stringsAsFactors = FALSE
)


object_types <- factor(c("0", "unary", "binary", "open", "close", "term", "X"),
                       levels = c("0", "unary", "binary", "open", "close", "term", "X"))


walk_matrix <-
  matrix(
    c(0, 1, 0, 1, 0, 1, 0,
      0, 0, 0, 1, 0, 0, 0,
      0, 0, 0, 1, 0, 0, 0,
      0, 1, 0, 1, 0, 1, 0,
      0, 0, 1, 0, 1, 0, 1,
      0, 0, 0, 0, 1, 0, 1,
      0, 0, 0, 0, 0, 0, 1
    ),
    byrow = TRUE,
    ncol = 7,
    dimnames = list(
      object_types,
      object_types
    )
  )



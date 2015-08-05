
## SET SOME CONSTANTS FOR THE LOGICAL LANGUAGE


f1 <- "formula1"
f2 <- "formula2"
f3 <- "formula3"
f4 <- "ModLogDF"

Error.1 <- "There are non-logical characters un the formula string."
Error.2 <- "Unnapropiated object type"
Error.3 <- "This is not a well formed formula"
Error.4 <- "There is something wron with the classes."

open_parentheses <- c("(", "[")
close_parentheses <- c(")", "]")
parentheses <- c(open_parentheses, close_parentheses)
unary_symbols <- c("¬", "0")
binary_symbols <- c("|", "&", ">")

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



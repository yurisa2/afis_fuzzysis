a <- "old"
test <- function () {
   assign("a", "new", envir = .GlobalEnv)
}
test()

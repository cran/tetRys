.onAttach <- function(libname, pkgname)
{
  Ver <- as.character(packageVersion("tetRys")[1])
  Msg <- paste("This is tetRys, version ", Ver, ". A game inspired by Tetris.\nType 'tetRys()' to play a game of tetRys.", sep = "")

  packageStartupMessage(Msg)
}

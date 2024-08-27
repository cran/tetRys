\name{tetRys}
\alias{tetRys}

\title{Starts a game of tetRys.}

\description{
Opens a plot window device and starts a game of tetRys in it. Steer the tetrominos with the arrow keys, press Pause to pause and Esc to end the game.
}

\usage{
tetRys(FadeEffect = TRUE, Startlevel = 1, Music = FALSE, Sound = FALSE)
}

\arguments{
	\item{FadeEffect}{
		Choose whether to show a short fade effect once the game is over.
  }
	\item{Startlevel}{
		Choose your skill level to start with.
	}
	\item{Music}{
		Choose whether to play music or not (you can also turn music on/off during play by using the 'm' key).
	}
	\item{Sound}{
		Choose whether to play sound effects or not (ou can also turn sound effects on/off during play by using the 'e' key).
	}
}

\details{
This serves as a programming example. R does not provide the R programmer with a system message loop. This is due to different operating systems (Windows, Linux, MacOS, each of them handles system messages very differently) and to the fact that for usual purposes as a statistical software, R programmers hardly require information from the system messaging loop. However, for tetRys, this is essential, as to asynchronously handle key presses. As R has no interface to the system message system, the trick is to open a window using package 'tcltk' and use its message loop interface. The tcltk subsystem pumps keypress events to the R workspace using a callback function, which itself is then used to write the desired values into a variable embedded into a distinct environment. The game can access that and use it to control the tetrominos.\cr
\cr
This system works well in plain R in Windows (RGui and terminal). It also works under Linux (tested with Ubuntu). tetRys is untested on MacOS. tetRys will run in integrated environments including RStudio, R Commander, and alike. Hint: You may want to call \code{\link{X11}} before running tetRys for an extra window that provides more speed and accuracy. On some machines it is not possible to hide the tcltk window (mostly Linux, depending on the window manager used). However, this is only a visual impairment. Just make sure that the window titled "tetRys control" is focused so that it receives key presses.
}

\value{
Invisibly returns 0 if the game is terminated properly.
}

\author{
Carsten Croonenbroeck
}

\examples{
if (interactive()) tetRys()
}

\keyword{tetRys}

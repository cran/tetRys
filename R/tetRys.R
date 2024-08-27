utils::globalVariables(c("tetRysSound"))

tetRys <- function(FadeEffect = TRUE, Startlevel = 1, Music = FALSE, Sound = FALSE)
{
  Exit <- function()
  {
    tcltk::tkdestroy(tt)
    StopMusic()
  }

  on.exit(Exit)

  FIELDWIDTH <- 10
  FIELDHEIGHT <- 18

  a <- tuneR::readMP3(paste(tetRysSound$Path, "/tetRys.MP3", sep = ""))
  Wave <- rbind(a@left, a@right)
  class(Wave) = "audioSample"
  attr(Wave, "rate") = a@samp.rate
  attr(Wave, "bits") = a@bit

  a <- tuneR::readMP3(paste(tetRysSound$Path, "/Clear.MP3", sep = ""))
  CLEAR <- rbind(a@left, a@right)
  class(CLEAR) = "audioSample"
  attr(CLEAR, "rate") = a@samp.rate
  attr(CLEAR, "bits") = a@bit

  a <- tuneR::readMP3(paste(tetRysSound$Path, "/Drop.MP3", sep = ""))
  DROP <- rbind(a@left, a@right)
  class(DROP) = "audioSample"
  attr(DROP, "rate") = a@samp.rate
  attr(DROP, "bits") = a@bit

  a <- tuneR::readMP3(paste(tetRysSound$Path, "/Tick.MP3", sep = ""))
  TICK <- rbind(a@left, a@right)
  class(TICK) = "audioSample"
  attr(TICK, "rate") = a@samp.rate
  attr(TICK, "bits") = a@bit

  a <- tuneR::readMP3(paste(tetRysSound$Path, "/Turn.MP3", sep = ""))
  TURN <- rbind(a@left, a@right)
  class(TURN) = "audioSample"
  attr(TURN, "rate") = a@samp.rate
  attr(TURN, "bits") = a@bit

  rm(a)

  CYAN <- rgb(red = 0, green = 1, blue = 1)
  RED <- rgb(red = 1, green = 0, blue = 0)
  ORANGE <- rgb(red = 0.9, green = 0.6, blue = 0)
  YELLOW <- rgb(red = 1, green = 1, blue = 0)
  GREEN <- rgb(red = 0, green = 1, blue = 0)
  PURPLE <- rgb(red = 1, green = 0, blue = 1)
  BLUE <- rgb(red = 0, green = 0, blue = 1)
  BLACK <- rgb(red = 0, green = 0, blue = 0)

  SCREEN <- matrix(ncol = 30, nrow = 30, data = rgb(0.5, 0.5, 0.5))

  TETROMINO <- list(Shapes = NULL, Color = NULL, X = NULL, Y = NULL, Orientation = NULL)

  Field <- matrix(ncol = FIELDWIDTH, nrow = FIELDHEIGHT)
  Tetrominos <- list(TETROMINO, TETROMINO, TETROMINO, TETROMINO, TETROMINO, TETROMINO, TETROMINO)
  NextTetro <- TETROMINO
  ActiveTetro <- TETROMINO

  MyEnv <- environment()

  Score <- 0
  ClearedLines <- 0
  TotalClearedLines <- 0
  Level <- Startlevel
  Interval <- 1000 * (0.9 ^ (Level - 1))
  GameOver <- FALSE
  Pause <- FALSE

  MusicPlay <- Music
  SoundPlay <- Sound
  MusicDuration <- 38590
  MusicStarttime <- NULL
  MusicControl <- NULL

  InitTetrominos <- function()
  {
    for (x in 1:FIELDWIDTH) for (y in 1:FIELDHEIGHT) Field[y, x] <<- BLACK

    #Shape "I"
    Tetrominos[[1]]$Color <<- CYAN
    Tetrominos[[1]]$X <<- 5
    Tetrominos[[1]]$Y <<- 0
    Tetrominos[[1]]$Orientation <<- 1
    Tetrominos[[1]]$Shapes[[1]] <<- matrix(ncol = 4, nrow = 4, data = FALSE)
    Tetrominos[[1]]$Shapes[[2]] <<- Tetrominos[[1]]$Shapes[[1]]
    Tetrominos[[1]]$Shapes[[3]] <<- Tetrominos[[1]]$Shapes[[1]]
    Tetrominos[[1]]$Shapes[[4]] <<- Tetrominos[[1]]$Shapes[[1]]
    Tetrominos[[1]]$Shapes[[1]][1, 1] <<- TRUE
    Tetrominos[[1]]$Shapes[[1]][2, 1] <<- TRUE
    Tetrominos[[1]]$Shapes[[1]][3, 1] <<- TRUE
    Tetrominos[[1]]$Shapes[[1]][4, 1] <<- TRUE
    Tetrominos[[1]]$Shapes[[2]][1, 1] <<- TRUE
    Tetrominos[[1]]$Shapes[[2]][1, 2] <<- TRUE
    Tetrominos[[1]]$Shapes[[2]][1, 3] <<- TRUE
    Tetrominos[[1]]$Shapes[[2]][1, 4] <<- TRUE
    Tetrominos[[1]]$Shapes[[3]] <<- Tetrominos[[1]]$Shapes[[1]]
    Tetrominos[[1]]$Shapes[[4]] <<- Tetrominos[[1]]$Shapes[[2]]

    #Shape "J"
    Tetrominos[[2]]$Color <<- BLUE
    Tetrominos[[2]]$X <<- 5
    Tetrominos[[2]]$Y <<- 0
    Tetrominos[[2]]$Orientation <<- 1
    Tetrominos[[2]]$Shapes[[1]] <<- matrix(ncol = 4, nrow = 4, data = FALSE)
    Tetrominos[[2]]$Shapes[[2]] <<- Tetrominos[[2]]$Shapes[[1]]
    Tetrominos[[2]]$Shapes[[3]] <<- Tetrominos[[2]]$Shapes[[1]]
    Tetrominos[[2]]$Shapes[[4]] <<- Tetrominos[[2]]$Shapes[[1]]
    Tetrominos[[2]]$Shapes[[1]][3, 1] <<- TRUE
    Tetrominos[[2]]$Shapes[[1]][1, 2] <<- TRUE
    Tetrominos[[2]]$Shapes[[1]][2, 2] <<- TRUE
    Tetrominos[[2]]$Shapes[[1]][3, 2] <<- TRUE
    Tetrominos[[2]]$Shapes[[2]][1, 1] <<- TRUE
    Tetrominos[[2]]$Shapes[[2]][2, 1] <<- TRUE
    Tetrominos[[2]]$Shapes[[2]][2, 2] <<- TRUE
    Tetrominos[[2]]$Shapes[[2]][2, 3] <<- TRUE
    Tetrominos[[2]]$Shapes[[3]][1, 1] <<- TRUE
    Tetrominos[[2]]$Shapes[[3]][1, 2] <<- TRUE
    Tetrominos[[2]]$Shapes[[3]][2, 1] <<- TRUE
    Tetrominos[[2]]$Shapes[[3]][3, 1] <<- TRUE
    Tetrominos[[2]]$Shapes[[4]][1, 1] <<- TRUE
    Tetrominos[[2]]$Shapes[[4]][1, 2] <<- TRUE
    Tetrominos[[2]]$Shapes[[4]][1, 3] <<- TRUE
    Tetrominos[[2]]$Shapes[[4]][2, 3] <<- TRUE

    #Shape "L"
    Tetrominos[[3]]$Color <<- ORANGE
    Tetrominos[[3]]$X <<- 5
    Tetrominos[[3]]$Y <<- 0
    Tetrominos[[3]]$Orientation <<- 1
    Tetrominos[[3]]$Shapes[[1]] <<- matrix(ncol = 4, nrow = 4, data = FALSE)
    Tetrominos[[3]]$Shapes[[2]] <<- Tetrominos[[3]]$Shapes[[1]]
    Tetrominos[[3]]$Shapes[[3]] <<- Tetrominos[[3]]$Shapes[[1]]
    Tetrominos[[3]]$Shapes[[4]] <<- Tetrominos[[3]]$Shapes[[1]]
    Tetrominos[[3]]$Shapes[[1]][1, 1] <<- TRUE
    Tetrominos[[3]]$Shapes[[1]][2, 1] <<- TRUE
    Tetrominos[[3]]$Shapes[[1]][3, 1] <<- TRUE
    Tetrominos[[3]]$Shapes[[1]][3, 2] <<- TRUE
    Tetrominos[[3]]$Shapes[[2]][1, 1] <<- TRUE
    Tetrominos[[3]]$Shapes[[2]][1, 2] <<- TRUE
    Tetrominos[[3]]$Shapes[[2]][1, 3] <<- TRUE
    Tetrominos[[3]]$Shapes[[2]][2, 1] <<- TRUE
    Tetrominos[[3]]$Shapes[[3]][1, 1] <<- TRUE
    Tetrominos[[3]]$Shapes[[3]][1, 2] <<- TRUE
    Tetrominos[[3]]$Shapes[[3]][2, 2] <<- TRUE
    Tetrominos[[3]]$Shapes[[3]][3, 2] <<- TRUE
    Tetrominos[[3]]$Shapes[[4]][1, 3] <<- TRUE
    Tetrominos[[3]]$Shapes[[4]][2, 1] <<- TRUE
    Tetrominos[[3]]$Shapes[[4]][2, 2] <<- TRUE
    Tetrominos[[3]]$Shapes[[4]][2, 3] <<- TRUE

    #Shape "O"
    Tetrominos[[4]]$Color <<- YELLOW
    Tetrominos[[4]]$X <<- 5
    Tetrominos[[4]]$Y <<- 0
    Tetrominos[[4]]$Orientation <<- 1
    Tetrominos[[4]]$Shapes[[1]] <<- matrix(ncol = 4, nrow = 4, data = FALSE)
    Tetrominos[[4]]$Shapes[[2]] <<- Tetrominos[[4]]$Shapes[[1]]
    Tetrominos[[4]]$Shapes[[3]] <<- Tetrominos[[4]]$Shapes[[1]]
    Tetrominos[[4]]$Shapes[[4]] <<- Tetrominos[[4]]$Shapes[[1]]
    Tetrominos[[4]]$Shapes[[1]][1, 1] <<- TRUE
    Tetrominos[[4]]$Shapes[[1]][1, 2] <<- TRUE
    Tetrominos[[4]]$Shapes[[1]][2, 1] <<- TRUE
    Tetrominos[[4]]$Shapes[[1]][2, 2] <<- TRUE
    Tetrominos[[4]]$Shapes[[2]] <<- Tetrominos[[4]]$Shapes[[1]]
    Tetrominos[[4]]$Shapes[[3]] <<- Tetrominos[[4]]$Shapes[[1]]
    Tetrominos[[4]]$Shapes[[4]] <<- Tetrominos[[4]]$Shapes[[1]]

    #Shape "S"
    Tetrominos[[5]]$Color <<- GREEN
    Tetrominos[[5]]$X <<- 5
    Tetrominos[[5]]$Y <<- 0
    Tetrominos[[5]]$Orientation <<- 1
    Tetrominos[[5]]$Shapes[[1]] <<- matrix(ncol = 4, nrow = 4, data = FALSE)
    Tetrominos[[5]]$Shapes[[2]] <<- Tetrominos[[5]]$Shapes[[1]]
    Tetrominos[[5]]$Shapes[[3]] <<- Tetrominos[[5]]$Shapes[[1]]
    Tetrominos[[5]]$Shapes[[4]] <<- Tetrominos[[5]]$Shapes[[1]]
    Tetrominos[[5]]$Shapes[[1]][1, 2] <<- TRUE
    Tetrominos[[5]]$Shapes[[1]][1, 3] <<- TRUE
    Tetrominos[[5]]$Shapes[[1]][2, 1] <<- TRUE
    Tetrominos[[5]]$Shapes[[1]][2, 2] <<- TRUE
    Tetrominos[[5]]$Shapes[[2]][1, 1] <<- TRUE
    Tetrominos[[5]]$Shapes[[2]][2, 1] <<- TRUE
    Tetrominos[[5]]$Shapes[[2]][2, 2] <<- TRUE
    Tetrominos[[5]]$Shapes[[2]][3, 2] <<- TRUE
    Tetrominos[[5]]$Shapes[[3]] <<- Tetrominos[[5]]$Shapes[[1]]
    Tetrominos[[5]]$Shapes[[4]] <<- Tetrominos[[5]]$Shapes[[2]]

    #Shape "T"
    Tetrominos[[6]]$Color <<- PURPLE
    Tetrominos[[6]]$X <<- 5
    Tetrominos[[6]]$Y <<- 0
    Tetrominos[[6]]$Orientation <<- 1
    Tetrominos[[6]]$Shapes[[1]] <<- matrix(ncol = 4, nrow = 4, data = FALSE)
    Tetrominos[[6]]$Shapes[[2]] <<- Tetrominos[[6]]$Shapes[[1]]
    Tetrominos[[6]]$Shapes[[3]] <<- Tetrominos[[6]]$Shapes[[1]]
    Tetrominos[[6]]$Shapes[[4]] <<- Tetrominos[[6]]$Shapes[[1]]
    Tetrominos[[6]]$Shapes[[1]][1, 1] <<- TRUE
    Tetrominos[[6]]$Shapes[[1]][1, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[1]][1, 3] <<- TRUE
    Tetrominos[[6]]$Shapes[[1]][2, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[2]][1, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[2]][2, 1] <<- TRUE
    Tetrominos[[6]]$Shapes[[2]][2, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[2]][3, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[3]][1, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[3]][2, 1] <<- TRUE
    Tetrominos[[6]]$Shapes[[3]][2, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[3]][2, 3] <<- TRUE
    Tetrominos[[6]]$Shapes[[4]][1, 1] <<- TRUE
    Tetrominos[[6]]$Shapes[[4]][2, 1] <<- TRUE
    Tetrominos[[6]]$Shapes[[4]][2, 2] <<- TRUE
    Tetrominos[[6]]$Shapes[[4]][3, 1] <<- TRUE

    #Shape "Z"
    Tetrominos[[7]]$Color <<- RED
    Tetrominos[[7]]$X <<- 5
    Tetrominos[[7]]$Y <<- 0
    Tetrominos[[7]]$Orientation <<- 1
    Tetrominos[[7]]$Shapes[[1]] <<- matrix(ncol = 4, nrow = 4, data = FALSE)
    Tetrominos[[7]]$Shapes[[2]] <<- Tetrominos[[7]]$Shapes[[1]]
    Tetrominos[[7]]$Shapes[[3]] <<- Tetrominos[[7]]$Shapes[[1]]
    Tetrominos[[7]]$Shapes[[4]] <<- Tetrominos[[7]]$Shapes[[1]]
    Tetrominos[[7]]$Shapes[[1]][1, 1] <<- TRUE
    Tetrominos[[7]]$Shapes[[1]][1, 2] <<- TRUE
    Tetrominos[[7]]$Shapes[[1]][2, 2] <<- TRUE
    Tetrominos[[7]]$Shapes[[1]][2, 3] <<- TRUE
    Tetrominos[[7]]$Shapes[[2]][1, 2] <<- TRUE
    Tetrominos[[7]]$Shapes[[2]][2, 1] <<- TRUE
    Tetrominos[[7]]$Shapes[[2]][2, 2] <<- TRUE
    Tetrominos[[7]]$Shapes[[2]][3, 1] <<- TRUE
    Tetrominos[[7]]$Shapes[[3]] <<- Tetrominos[[7]]$Shapes[[1]]
    Tetrominos[[7]]$Shapes[[4]] <<- Tetrominos[[7]]$Shapes[[2]]

    NextTetro <<- Tetrominos[[trunc(runif(n = 1, min = 0, max = 7)) + 1]]
    ActiveTetro <<- Tetrominos[[trunc(runif(n = 1, min = 0, max = 7)) + 1]]

    Score <<- 0
    ClearedLines <<- 0
    TotalClearedLines <<- 0
    if (Startlevel < 1) Startlevel <- 1
    Level <<- Startlevel
    Interval <<- 1000 * (0.9 ^ (Level - 1))
  }

  Message <- function(MyText)
  {
    message(MyText)
    flush.console()
  }

  StartMusic <- function()
  {
    WaveControl <- audio::play(Wave)
    Message("Music started.")

    assign("MusicPlay", TRUE, MyEnv)
    assign("MusicControl", WaveControl, MyEnv)
    assign("MusicStarttime", Sys.time(), MyEnv)
  }

  StopMusic <- function()
  {
    if (!MusicPlay) return()

    audio::pause(MusicControl)
    Message("Music stopped.")

    assign("MusicPlay", FALSE, MyEnv)
  }

  PlaySound <- function(SoundObj)
  {
    if (SoundPlay)
    {
      if ("Void" %in% names(MyEnv)) audio::pause(MyEnv$Void)
      Void <- audio::play(SoundObj)
      assign("Void", Void, MyEnv)
    }
  }

  FadeOut <- function()
  {
    Resolution <- 300

    BigScreen <- matrix(ncol = Resolution, nrow = Resolution)

    for (x in 0:(Resolution - 1))
    {
      for (y in 0:(Resolution - 1))
      {
        BigScreen[y + 1, x + 1] <- SCREEN[(y / 10) + 1, (x / 10) + 1]
      }
    }

    BRed <- strtoi(paste("0x", substr(BigScreen, 2, 3), sep = ""))
    BGreen <- strtoi(paste("0x", substr(BigScreen, 4, 5), sep = ""))
    BBlue <- strtoi(paste("0x", substr(BigScreen, 6, 7), sep = ""))

    StartTime <- Sys.time()
    Noise <- matrix(ncol = Resolution, nrow = Resolution, data = runif(n = Resolution * Resolution, min = 0, max = 255))
    TargetRed <- (0 * Noise) + ((1 - 0) * BRed) #Nonsense, but here to acquire a valid time measure.
    TargetGreen <- (0 * Noise) + ((1 - 0) * BGreen)
    TargetBlue <- (0 * Noise) + ((1 - 0) * BBlue)
    Target <- matrix(ncol = Resolution, nrow = Resolution, data = rgb(TargetRed, TargetGreen, TargetBlue, maxColorValue = 255))
    grid::grid.newpage()
    grid::grid.raster(Target, interpolate = FALSE)
    sElapsed <- as.vector(difftime(Sys.time(), StartTime, units = "secs"))

    NoiseFrame <- 44100 * sElapsed
    BrownNoise <- cumsum(rnorm(n = 100 * NoiseFrame))
    if (min(BrownNoise)[1] < 0) BrownNoise <- BrownNoise + abs(min(BrownNoise))
    BrownNoise <- BrownNoise / max(BrownNoise)
    BrownNoise <- BrownNoise * 2
    BrownNoise <- BrownNoise - 1
    BrownNoise <- BrownNoise * sin(seq(from = 0, to = pi, length.out = length(BrownNoise)))
    PlaySound(BrownNoise)

    Sin <- sin(seq(from = 0, to = 0.5 * pi, length.out = 50))
    for (MySin in Sin)
    {
      Noise <- matrix(ncol = Resolution, nrow = Resolution, data = runif(n = Resolution * Resolution, min = 0, max = 255))

      TargetRed <- (MySin * Noise) + ((1 - MySin) * BRed)
      TargetGreen <- (MySin * Noise) + ((1 - MySin) * BGreen)
      TargetBlue <- (MySin * Noise) + ((1 - MySin) * BBlue)

      Target <- matrix(ncol = Resolution, nrow = Resolution, data = rgb(TargetRed, TargetGreen, TargetBlue, maxColorValue = 255))

      grid::grid.newpage()
      grid::grid.raster(Target, interpolate = FALSE)
    }
    Sin <- sin(seq(from = 0.5 * pi, to = pi, length.out = 50))
    for (MySin in Sin)
    {
      Noise <- matrix(ncol = Resolution, nrow = Resolution, data = runif(n = Resolution * Resolution, min = 0, max = MySin * 255))
      Target <- matrix(ncol = Resolution, nrow = Resolution, data = rgb(Noise, Noise, Noise, maxColorValue = 255))

      grid::grid.newpage()
      grid::grid.raster(Target, interpolate = FALSE)
    }
  }

  CheckSettle <- function()
  {
    for (x in 1:4)
    {
      for (y in 1:4)
      {
        if (ActiveTetro$Shapes[[ActiveTetro$Orientation]][y, x] && ActiveTetro$Y + y - 1 == FIELDHEIGHT) return(TRUE) #Tetro hit the floor.

        if (ActiveTetro$X + x - 1 <= FIELDWIDTH && ActiveTetro$Y + y - 1 <= FIELDHEIGHT - 1)
        {
          if (ActiveTetro$Shapes[[ActiveTetro$Orientation]][y, x] && Field[ActiveTetro$Y + y, ActiveTetro$X + x - 1] != BLACK) return(TRUE) #Tetro hit something in the current field.
        }
      }
    }

    return(FALSE)
  }

  HandleSettle <- function()
  {
    if (CheckSettle())
    {
      PlaySound(DROP)

      AddTetroToField()
      HandleRowDrops()

      if (ClearedLines >= 10)
      {
        TempClearedLines <- ClearedLines - 10
        assign("ClearedLines", TempClearedLines, MyEnv)

        TempLevel <- Level + 1
        assign("Level", TempLevel, MyEnv)
        Message(paste("Level: ", Level, sep = ""))

        TempInterval <- 1000 * (0.9 ^ (Level - 1))
        assign("Interval", TempInterval, MyEnv)
      }

      assign("ActiveTetro", NextTetro, MyEnv)
      TempTetro <- Tetrominos[[trunc(runif(n = 1, min = 0, max = 7)) + 1]]
      assign("NextTetro", TempTetro, MyEnv)

      if (CheckSettle())
      {
        Message("Game over!")
		    Message(paste("Cleared lines: ", TotalClearedLines, ".", sep = ""))
        assign("GameOver", TRUE, MyEnv)
        if (FadeEffect) FadeOut()
        Exit()
        return()
      }
    }
  }

  AddTetroToField <- function()
  {
    TempField <- Field

    for (x in 1:4)
    {
      for (y in 1:4)
      {
        if (ActiveTetro$Shapes[[ActiveTetro$Orientation]][y, x]) if (ActiveTetro$Y + y - 1 <= FIELDHEIGHT && ActiveTetro$Y + y - 1 > 0)
        {
          TempField[ActiveTetro$Y + y - 1, ActiveTetro$X + x - 1] <- ActiveTetro$Color
        }
      }
    }

    assign("Field", TempField, MyEnv)
  }

  HandleRowDrops <- function()
  {
    TempField <- Field
    ClearedNow <- 0

    while (TRUE)
    {
      Restart <- FALSE

      for (y in FIELDHEIGHT:1)
      {
        OccupiedPixels = 0
        for (x in 1:FIELDWIDTH)
        {
          if (TempField[y, x] != BLACK) OccupiedPixels = OccupiedPixels + 1
        }

        if (OccupiedPixels == FIELDWIDTH)
        {
          for (yInner in y:2)
          {
            TempField[yInner, ] <- TempField[yInner - 1, ]
          }

          ClearedNow <- ClearedNow + 1
          Restart <- TRUE
          break
        }
      }

      if (!Restart) break
    }

    if (ClearedNow)
    {
      PlaySound(CLEAR)

      TempScore <- Score
      if (ClearedNow == 1) TempScore <- TempScore + 40
      if (ClearedNow == 2) TempScore <- TempScore + 100
      if (ClearedNow == 3) TempScore <- TempScore + 300
      if (ClearedNow == 4) TempScore <- TempScore + 1200
      assign("Score", TempScore, MyEnv)

      Message(paste("Score: ", Score, sep = ""))

      TempClearedLines <- TotalClearedLines + ClearedNow
      assign("ClearedLines", ClearedLines + ClearedNow, MyEnv)
      assign("TotalClearedLines", TempClearedLines, MyEnv)
    }

    assign("Field", TempField, MyEnv)
  }

  DrawPixel <- function(x, y, Col)
  {
    SCREEN[y, x] <<- Col
  }

  Draw <- function()
  {
    #Draw field
    xOff <- 3
    yOff <- 3
    for (x in 1:FIELDWIDTH) for (y in 1:FIELDHEIGHT) DrawPixel(x + xOff, y + yOff, Field[y, x])

    #Draw active tetromino
    for (x in 1:4) for (y in 1:4) if (ActiveTetro$Shapes[[ActiveTetro$Orientation]][y, x]) DrawPixel(ActiveTetro$X + xOff + x - 1, ActiveTetro$Y + yOff + y - 1, ActiveTetro$Color)

    #Draw next tetromino
    xOff <- 17
    yOff <- 6
    for (x in 1:5) for (y in 1:6) DrawPixel(x + xOff, y + yOff, BLACK)
    for (x in 1:4) for (y in 1:4) if (NextTetro$Shapes[[1]][y, x]) DrawPixel(NextTetro$Shapes[[1]][y, x] + xOff + x, NextTetro$Shapes[[1]][y, x] + yOff + y, NextTetro$Color)

    #Blit everything to screen
    grid::grid.newpage()
    grid::grid.raster(SCREEN, interpolate = FALSE)
  }

  Blocked <- function(TestTetro)
  {
    for (x in 1:4)
    {
      for (y in 1:4)
      {
        if (TestTetro$Shapes[[TestTetro$Orientation]][y, x])
        {
          if (TestTetro$X + x - 1 > FIELDWIDTH || TestTetro$X + x - 1 < 1) return(TRUE)
          if (TestTetro$Y + y - 1 > FIELDHEIGHT || TestTetro$Y + y - 1 < 1) return(TRUE)
          if (Field[TestTetro$Y + y - 1, TestTetro$X + x - 1] != BLACK) return(TRUE)
        }
      }
    }

    return(FALSE)
  }

  UpdateFrame <- function()
  {
    HandleSettle()
    if (GameOver) return()

    TempTetro <- ActiveTetro
    TempTetro$Y <- TempTetro$Y + 1
    assign("ActiveTetro", TempTetro, MyEnv)

    Draw()
  }

  SetFocus <- function(Window)
  {
    info_sys <- Sys.info() #https://stackoverflow.com/questions/9622287/how-do-i-bring-an-r-tk-window-to-the-front-after-launching-via-rscript-from-anot
    if (info_sys["sysname"] == "Windows")
    {
      Dir <- getwd()
      on.exit(setwd(Dir))
      setwd(tempdir())
      shell("powershell -command [void] [System.Reflection.Assembly]::LoadWithPartialName('Microsoft.VisualBasic') ; [Microsoft.VisualBasic.Interaction]::AppActivate('tetRys control')")
    } else
    {
      invisible(tcltk::tkwm.deiconify(Window)) #At least, try this.
    }
  }

  Message("Controls: Arrow keys. Press 'Pause' to pause, 'm' to toggle music,")
  Message("and 'e' for sound effects. Hit Esc to end game.")

  InitTetrominos()
  if (MusicPlay) StartMusic()
  if (SoundPlay) Message("Sound started.")

  if (.Device == "null device") dev.new()

  tetRysEnvir <- new.env()
  tetRysEnvir$KeyCode <- NA
  tt <- tcltk::tktoplevel()
  invisible(tcltk::tkwm.title(tt, "tetRys control"))
  invisible(tcltk::tkwm.geometry(tt, "300x100+9000+500")) #https://stackoverflow.com/questions/14910858/how-to-specify-where-a-tkinter-window-opens
  invisible(tcltk::tkbind(tt, "<Key>", function(K) {tetRysEnvir$KeyCode <- K})) #https://stackoverflow.com/questions/52370937/access-keyboard-buffer-in-r
  SetFocus(tt)

  StartTime <- Sys.time()
  while (TRUE)
  {
    msElapsed <- as.vector(difftime(Sys.time(), StartTime, units = "secs")) * 1000
    if (msElapsed >= Interval)
    {
      StartTime <- Sys.time()

      if (!Pause) UpdateFrame()
    }

    if (GameOver) break

    if (MusicPlay)
    {
      if (difftime(Sys.time(), MusicStarttime, units = "secs") * 1000 >= MusicDuration)
      {
        WaveControl <- audio::play(Wave)
        assign("MusicControl", WaveControl, MyEnv)
        assign("MusicStarttime", Sys.time(), MyEnv)
      }
    }

    if (!is.na(tetRysEnvir$KeyCode))
    {
      if (tetRysEnvir$KeyCode == "Pause")
      {
        if (Pause)
        {
          Message("Pause released.")
        } else
        {
          Message("Game paused.")
          tetRysEnvir$KeyCode <- NA
        }
        assign("Pause", !Pause, MyEnv)
      }

      if (Pause) next

      if (tetRysEnvir$KeyCode == "m")
      {
        if (MusicPlay)
        {
          StopMusic()
        } else
        {
          StartMusic()
        }
      }

      if (tetRysEnvir$KeyCode == "e")
      {
        SoundPlay <- !SoundPlay

        if (SoundPlay) Message("Sound started.") else Message("Sound stopped.")
      }

      if (tetRysEnvir$KeyCode == "Escape")
      {
        Message("Game terminated.")
        Exit()
        break
      }

      if (tetRysEnvir$KeyCode == "Left" || tetRysEnvir$KeyCode == "a")
      {
        if (ActiveTetro$X != 1)
        {
          TestTetro <- ActiveTetro
          TestTetro$X <- TestTetro$X - 1

          if (!Blocked(TestTetro))
          {
            PlaySound(TICK)

            assign("ActiveTetro", TestTetro, MyEnv)
            Draw()
          }
        }
      }

      if (tetRysEnvir$KeyCode == "Right" || tetRysEnvir$KeyCode == "d")
      {
        TestTetro <- ActiveTetro
        TestTetro$X <- TestTetro$X + 1

        if (!Blocked(TestTetro))
        {
          PlaySound(TICK)

          assign("ActiveTetro", TestTetro, MyEnv)
          Draw()
        }
      }

      if (tetRysEnvir$KeyCode == "Up" || tetRysEnvir$KeyCode == "w" || tetRysEnvir$KeyCode == "space")
      {
        TestTetro <- ActiveTetro
        TestTetro$Orientation <- TestTetro$Orientation + 1
        if (TestTetro$Orientation == 5) TestTetro$Orientation <- 1

        if (!Blocked(TestTetro))
        {
          PlaySound(TURN)

          assign("ActiveTetro", TestTetro, MyEnv)
          Draw()
        }
      }

      if (tetRysEnvir$KeyCode == "Down" || tetRysEnvir$KeyCode == "s")
      {
        HandleSettle()
        if (GameOver)
        {
          Exit()
          break;
        }

        TestTetro <- ActiveTetro
        TestTetro$Y <- TestTetro$Y + 1
        if (!Blocked(TestTetro)) UpdateFrame()
      }

      tetRysEnvir$KeyCode <- NA
    }
  }

  return(invisible(0))
}

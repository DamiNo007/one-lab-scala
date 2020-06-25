package one.lab.tasks.week.one

/**
  * 1. You have to define three [[one.lab.tasks.week.one.Interfaces.Console]] class implementation, let's say Xbox,
  * PlayStation, Sega
  * 2. You also need to define implementation GameDisk traits for each of console, and some classes of games, see
  * [[one.lab.tasks.week.one.Interfaces.XboxGameDisk]] and [[one.lab.tasks.week.one.Interfaces.ForzaHorizon]]
  * 3. When creating implementation of Console be sure to properly implement play method,
  * so that when I try to play Xbox with PS game disk, it will print me that disk format is invalid.
  * But when I supply appropriate disk it will print s"playing ${disk.game()}"
  */
object Interfaces {

  trait GameDisk {
    val consoleType: String
    val game: String
  }

  trait Console {
    def play(disk: GameDisk): Unit
  }

  class PlayStation extends Console {
    override def play(disk: GameDisk): Unit = {
      if (disk.consoleType.equals("PlayStation"))
        println(s"Playing ${disk.game}")
      else
        println("Disk format is not valid!")
    }
  }

  class Sega extends Console {
    override def play(disk: GameDisk): Unit = {
      if (disk.consoleType.equals("Sega"))
        println(s"Playing ${disk.game}")
      else
        println("Disk format is not valid")
    }
  }

  class Xbox extends Console {
    override def play(disk: GameDisk): Unit = {
      if (disk.consoleType.equals("Xbox"))
        println(s"Playing ${disk.game}")
      else
        println("Disk format is not valid")
    }
  }

  trait XboxGameDisk extends GameDisk {
    override val consoleType: String = "Xbox"
  }

  trait PlayStationGameDisk extends GameDisk {
    override val consoleType: String = "PlayStation"
  }

  trait SegaGameDisk extends GameDisk {
    override val consoleType: String = "Sega"
  }

  class ForzaHorizon extends XboxGameDisk {
    override val game: String = "ForzaHorizon race game"
  }

  class RDR2 extends PlayStationGameDisk {
    override val game: String = "RDR2"
  }

  class Sonic extends SegaGameDisk {
    override val game: String = "Sonic"
  }

  //My own Test :)
  def main(args: Array[String]): Unit = {
    val ps = new PlayStation()
    val xbox = new Xbox()
    val sega = new Sega()

    val rdr2 = new RDR2()
    val forzaHorizon = new ForzaHorizon()
    val sonic = new Sonic()
    //Checking PlayStationGameDisk object on PlayStation
    ps.play(rdr2)
    //Checking XboxGameDisk object on PlayStation
    ps.play(forzaHorizon)
    //Checking SegaGame Disk object on PlayStation
    ps.play(sonic)
    //Checking XboxGameDisk object on Xbox
    xbox.play(forzaHorizon)
    //Checking PlayStationGameDisk object on Xbox
    xbox.play(rdr2)
    //Checking SegaGameDisk object on Xbox
    xbox.play(sonic)
    //Checking SegaGameDisk object on Sega
    sega.play(sonic)
    //Checking PlayStationGameDisk object on Sega
    sega.play(rdr2)
    //Checking XboxGameDisk object on Sega
    sega.play(forzaHorizon)
  }
}

package fishbot

import fishbot.FishException.WrongCall

import scala.io.{Source, StdIn}

object ManualFish {
  def main(args: Array[String]): Unit = {

    val players = Seq("a", "b", "c", "d", "e", "f").map(Player)

    var game = FishGame.random(players)

    println(s"Team 1: ${game.currTeam.players.map(_.name).mkString(", ")}")
    println(s"Team 2: ${game.oppTeam.players.map(_.name).mkString(", ")}")

    while (game.winner.isEmpty) {
      println(s"Current team score ${game.currTeam.points.size} points to ${game.oppTeam.points.size}")
      println(s"Current player: ${game.currPlayer.name}")
      println(s"Hand: ${game.hands.filter(_._2 == game.currPlayer).keys.mkString(", ")}")
      println()
      StdIn.readLine("Enter r to request a card, c to call: ") match {
        case "r" =>
          (for {
            p <- { val name = StdIn.readLine("Enter player name: ").trim
              players.find(_.name == name) }
            c <- { val card = StdIn.readLine("Enter desired card: ").trim
              Card.byName(card) }
          } yield game.requestCard(p, c)) match {
            case Some(Right(g)) => game = g
            case Some(Left(e)) => println(e.getMessage)
            case None => println("Couldn't find that! Try again.")
          }
        case "c" =>
          (for {
            hs <- {
              val name = StdIn.readLine("Enter half-suit to call: ").trim
              HalfSuit.byName(name)
          }} yield (hs, for {
            c <- Card.halfSuitSet(hs)
            p <- {
              val name = StdIn.readLine(s"Enter player name who has $c: ").trim
              players.find(_.name == name)
            }
          } yield c -> p)) match {
            case Some((hs, guess)) => game.callHalfsuit(hs, guess.toMap) match {
              case Left(e) => println(e.getMessage)
              case Right(Right(g)) => game = g
              case Right(Left((WrongCall(inc), g))) => game = g; println(s"Incorrect call: ${inc.mkString(", ")}")
            }
            case None => println("Couldn't find that! Try again.")
          }
        case _ => println("Bad input >:(")
      }
      println("--------------\n")
    }

    println(s"Players ${game.winner.get.players.map(_.name).mkString(", ")} win!")
  }
}

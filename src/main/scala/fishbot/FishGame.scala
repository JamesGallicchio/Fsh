package fishbot

import scala.util.Random

case class Player(name: String)
case class Team(players: Seq[Player], points: Set[HalfSuit])
// players should be sorted with most recent player as the head

object FishGame {
  def createRandom(team1players: Seq[Player], team2players: Seq[Player]): FishGame = {
    val deck = Random.shuffle(Card.values)

    val jumbledPlayers = Random.shuffle(team1players ++ team2players)

    val hands = deck.zip(
      Iterator.continually(jumbledPlayers).flatten
    ).toMap

    val team1 = Team(Random.shuffle(team1players), Set.empty)
    val team2 = Team(Random.shuffle(team2players), Set.empty)

    FishGame(team1, team2, hands)
  }
}
case class FishGame(currTeam: Team, oppTeam: Team, hands: Map[Card, Player]) {
  import fishbot.FishException._

  private def playerHasHalfsuit(player: Player, halfSuit: HalfSuit): Boolean =
    hands.exists { case (c, p) => c.halfSuit == halfSuit && p == player }

  private def onCurrTeam(player: Player): Boolean = currTeam.players.contains(player)
  private def currPlayer: Player = currTeam.players.head

  def requestCard(target: Player, card: Card): Either[InvalidRequest, FishGame] =
    if (onCurrTeam(target))
      Left(TargetOnTeam)
    else if (!playerHasHalfsuit(currPlayer, card.halfSuit))
      Left(NoCardInHalfsuit)
    else hands.get(card) match {
      case None =>
        Left(CardOutOfPlay)
      case Some(p) if p == currPlayer =>
        Left(AlreadyHasCard)
      case Some(p) =>
        Right(
          if (p == target)
            FishGame(
              currTeam, oppTeam, hands.updated(card, currPlayer)
            )
          else
            FishGame(
              Team(target +: oppTeam.players.filter(_ != target), oppTeam.points),
              currTeam,
              hands
            )
        )
    }

  private def winHalfSuit(halfSuit: HalfSuit): FishGame = {
    val newHand = hands.filter(_._1.halfSuit != halfSuit)

    FishGame(
      Team(
        currTeam.players.filter(p => newHand.values.exists(_ == p)),
        currTeam.points + halfSuit),
      Team(
        oppTeam.players.filter(p => newHand.values.exists(_ == p)),
        oppTeam.points),
      newHand
    )
  }

  private def loseHalfSuit(halfSuit: HalfSuit): FishGame = {
    val newHand = hands.filter(_._1.halfSuit != halfSuit)

    FishGame(
      Team(
        currTeam.players.filter(p => newHand.values.exists(_ == p)),
        currTeam.points),
      Team(
        oppTeam.players.filter(p => newHand.values.exists(_ == p)),
        oppTeam.points + halfSuit),
      newHand
    )
  }

  def callHalfsuit(halfSuit: HalfSuit, locations: Map[Card, Player]): Either[(InvalidCall, FishGame), FishGame] =
    if (locations.keySet != Card.halfSuitSet(halfSuit))
      Left((MissingCard, loseHalfSuit(halfSuit)))
    else {
      val diff = locations.filter(_._1.halfSuit == halfSuit).toSet -- locations

      if (diff.nonEmpty)
        Left((WrongCall(diff.toMap), loseHalfSuit(halfSuit)))
      else
        Right(winHalfSuit(halfSuit))
    }
}

sealed trait FishException extends Exception
object FishException {
  case object GameFinished    extends FishException
  case object NoPlayers       extends FishException

  sealed trait InvalidRequest extends FishException
  sealed trait InvalidCall    extends FishException

  case object NoCardInHalfsuit extends InvalidRequest with InvalidCall

  case object AlreadyHasCard extends InvalidRequest
  case object CardOutOfPlay  extends InvalidRequest
  case object TargetOnTeam   extends InvalidRequest

  case object MissingCard extends InvalidCall
  case class WrongCall(incorrect: Map[Card, Player]) extends InvalidCall
}

sealed trait HalfSuit
object HalfSuit {
  val values: Set[HalfSuit] = Set(
    LowSpades, HighSpades, LowHearts, HighHearts,
    LowClubs, HighClubs, LowDiamonds, HighDiamonds, EightsJokers
  )

  case object LowSpades    extends HalfSuit
  case object HighSpades   extends HalfSuit
  case object LowHearts    extends HalfSuit
  case object HighHearts   extends HalfSuit
  case object LowClubs     extends HalfSuit
  case object HighClubs    extends HalfSuit
  case object LowDiamonds  extends HalfSuit
  case object HighDiamonds extends HalfSuit
  case object EightsJokers extends HalfSuit
}

sealed class Card (val halfSuit: HalfSuit)
object Card {
  import HalfSuit._

  def values: Set[Card] = Set(
    S2, S3, S4, S5, S6, S7, S8, S9, SX, SJ, SQ, SK, SA,
    H2, H3, H4, H5, H6, H7, H8, H9, HX, HJ, HQ, HK, HA,
    C2, C3, C4, C5, C6, C7, C8, C9, CX, CJ, CQ, CK, CA,
    D2, D3, D4, D5, D6, D7, D8, D9, DX, DJ, DQ, DK, DA,
    RedJoker, BlackJoker
  )

  def halfSuitSet(halfSuit: HalfSuit): Set[Card] = values.filter(_.halfSuit == halfSuit)

  case object S2 extends Card(LowSpades)
  case object S3 extends Card(LowSpades)
  case object S4 extends Card(LowSpades)
  case object S5 extends Card(LowSpades)
  case object S6 extends Card(LowSpades)
  case object S7 extends Card(LowSpades)
  case object S8 extends Card(EightsJokers)
  case object S9 extends Card(HighSpades)
  case object SX extends Card(HighSpades)
  case object SJ extends Card(HighSpades)
  case object SQ extends Card(HighSpades)
  case object SK extends Card(HighSpades)
  case object SA extends Card(HighSpades)

  case object H2 extends Card(LowHearts)
  case object H3 extends Card(LowHearts)
  case object H4 extends Card(LowHearts)
  case object H5 extends Card(LowHearts)
  case object H6 extends Card(LowHearts)
  case object H7 extends Card(LowHearts)
  case object H8 extends Card(EightsJokers)
  case object H9 extends Card(HighHearts)
  case object HX extends Card(HighHearts)
  case object HJ extends Card(HighHearts)
  case object HQ extends Card(HighHearts)
  case object HK extends Card(HighHearts)
  case object HA extends Card(HighHearts)

  case object C2 extends Card(LowClubs)
  case object C3 extends Card(LowClubs)
  case object C4 extends Card(LowClubs)
  case object C5 extends Card(LowClubs)
  case object C6 extends Card(LowClubs)
  case object C7 extends Card(LowClubs)
  case object C8 extends Card(EightsJokers)
  case object C9 extends Card(HighClubs)
  case object CX extends Card(HighClubs)
  case object CJ extends Card(HighClubs)
  case object CQ extends Card(HighClubs)
  case object CK extends Card(HighClubs)
  case object CA extends Card(HighClubs)

  case object D2 extends Card(LowDiamonds)
  case object D3 extends Card(LowDiamonds)
  case object D4 extends Card(LowDiamonds)
  case object D5 extends Card(LowDiamonds)
  case object D6 extends Card(LowDiamonds)
  case object D7 extends Card(LowDiamonds)
  case object D8 extends Card(EightsJokers)
  case object D9 extends Card(HighDiamonds)
  case object DX extends Card(HighDiamonds)
  case object DJ extends Card(HighDiamonds)
  case object DQ extends Card(HighDiamonds)
  case object DK extends Card(HighDiamonds)
  case object DA extends Card(HighDiamonds)

  case object RedJoker extends Card(EightsJokers)
  case object BlackJoker extends Card(EightsJokers)
}
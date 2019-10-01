package fishbot

import scala.util.Random

case class Player(name: String)
case class Team(players: Seq[Player], points: Set[HalfSuit])
// players should be sorted with most recent player as the head

object FishGame {
  def random(players: Seq[Player]): FishGame = {
    val randomized = Random.shuffle(players)
    val (team1, team2) = randomized.splitAt(randomized.size / 2)

    randomHands(team1, team2)
  }

  def randomHands(team1players: Seq[Player], team2players: Seq[Player]): FishGame = {
    val deck = Random.shuffle(Card.values)

    val jumbledPlayers = Random.shuffle(team1players ++ team2players)

    val hands = deck.zip(
      Iterator.continually(jumbledPlayers).flatten
    ).toMap

    FishGame(Team(team1players, Set.empty), Team(team2players, Set.empty), hands)
  }
}
case class FishGame(currTeam: Team, oppTeam: Team, hands: Map[Card, Player]) {
  import fishbot.FishException._

  def winner: Option[Team] =
    if (currTeam.points.size > 4)
      Some(currTeam)
    else if (oppTeam.points.size > 4)
      Some(oppTeam)
    else None

  private def playerHasCards(player: Player): Boolean = hands.values.exists(_ == player)
  private def playerHasHalfsuit(player: Player, halfSuit: HalfSuit): Boolean =
    hands.exists { case (c, p) => c.halfSuit == halfSuit && p == player }

  def currPlayer: Player = currTeam.players.head
  private def onCurrTeam(player: Player): Boolean = currTeam.players.contains(player)

  // Ensures currPlayer has cards unless nobody has cards
  private def fixCurrPlayer: FishGame = {
    def bubbleUp(team: Team): Team =
      team.copy(players = team.players.
        foldRight(Seq.empty[Player]) { case (p, seq) =>
          if (playerHasCards(p)) p +: seq
          else seq :+ p
        })

    if (currTeam.players.exists(playerHasCards))
      FishGame(bubbleUp(currTeam), oppTeam, hands)
    else
      FishGame(bubbleUp(oppTeam), currTeam, hands)
  }

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
            ).fixCurrPlayer
          else
            FishGame(
              oppTeam.copy(players = target +: oppTeam.players.filter(_ != target)),
              currTeam,
              hands
            ).fixCurrPlayer
        )
    }

  def callHalfsuit(halfSuit: HalfSuit, locations: Map[Card, Player]): Either[InvalidCall, Either[(WrongCall, FishGame), FishGame]] =
    if (locations.keySet != Card.halfSuitSet(halfSuit))
      Left(NotHalfSuit)
    else if (!playerHasHalfsuit(currPlayer, halfSuit))
      Left(NoCardInHalfsuit)
    else Right({
      val diff = hands.filter(_._1.halfSuit == halfSuit).toSet -- locations
      val newHands = hands.filter(_._1.halfSuit != halfSuit)

      if (diff.nonEmpty) Left((WrongCall(diff.toMap),
        FishGame(
          oppTeam.copy(points = oppTeam.points + halfSuit),
          currTeam, newHands
        ).fixCurrPlayer
      )) else Right(
        FishGame(
          currTeam.copy(points = currTeam.points + halfSuit),
          oppTeam, newHands
        ).fixCurrPlayer
      )
    })
}

sealed abstract class FishException(msg: String) extends Exception(msg)
object FishException {
  case object GameFinished     extends FishException("Game has finished!")
  case object NoPlayers        extends FishException("There are no players!")

  sealed trait InvalidRequest  extends FishException

  case object AlreadyHasCard   extends FishException("Player already has this card!")
                                  with InvalidRequest
  case object CardOutOfPlay    extends FishException("Card is no longer in play.")
                                  with InvalidRequest
  case object TargetOnTeam     extends FishException("Cannot request cards from a player on the same team!")
                                  with InvalidRequest
  case object TargetOutOfPlay  extends FishException("Target does not have any cards!")
                                  with InvalidRequest

  sealed trait InvalidCall     extends FishException

  case object NotHalfSuit      extends FishException("Call did not specify exactly every card in the half-suit!")
                                  with InvalidCall

  case class WrongCall(incorrect: Map[Card, Player])
                               extends FishException("Call was incorrect about position of one or more cards in the half suit!")

  case object NoCardInHalfsuit extends FishException("Player does not have any cards in that half suit!")
                                  with InvalidRequest with InvalidCall
}

sealed trait HalfSuit extends Product
object HalfSuit {
  val values: Set[HalfSuit] = Set(
    LowSpades, HighSpades, LowHearts, HighHearts,
    LowClubs, HighClubs, LowDiamonds, HighDiamonds, EightsJokers
  )

  def byName(name: String): Option[HalfSuit] = values.find(_.productPrefix == name)

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

sealed abstract class Card (val halfSuit: HalfSuit) extends Product
object Card {
  import HalfSuit._

  val values: Set[Card] = Set(
    S2, S3, S4, S5, S6, S7, S8, S9, SX, SJ, SQ, SK, SA,
    H2, H3, H4, H5, H6, H7, H8, H9, HX, HJ, HQ, HK, HA,
    C2, C3, C4, C5, C6, C7, C8, C9, CX, CJ, CQ, CK, CA,
    D2, D3, D4, D5, D6, D7, D8, D9, DX, DJ, DQ, DK, DA,
    RedJoker, BlackJoker
  )

  def byName(name: String): Option[Card] = values.find(_.productPrefix == name)

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
package fishbot

import fishbot.Card
import fishbot.FishGame.{Player, Team}

import scala.util.{Failure, Success, Try}

object FishGame {
  type Player = String

  type Team = Set[Player]
}
case class FishGame(team1: Team, team2: Team, playerHolding: Map[Card, Player], currPlayer: Player) {
  def request(card: Card, from: Player): Try[FishGame] = {
    // Check if caller already has the card
    if (playerHolding(card) == currPlayer)
      Failure(new IllegalArgumentException("Player cannot request card in their hand."))
    // Check if caller has another card in half-suit
    else if (!playerHolding.exists { case (k, v) => k.halfSuit == card.halfSuit && v == currPlayer })
      Failure(new IllegalArgumentException("Player cannot request card unless they have another card in the half-suit."))
    else
      Success({
        // If called player has the card, move to caller and give another turn
        if (playerHolding(card) == from)
          FishGame(playerHolding updated(card, currPlayer), currPlayer)
        // Else no cards are exchanged,
        else
          FishGame(playerHolding, from)
      })
  }
}

sealed trait HalfSuit
object HalfSuit {
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

sealed class Card(val halfSuit: HalfSuit)
object Card {
  import HalfSuit._

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

object FishBot {

  case class CardInfo(known: Seq[Card], possible: Seq[Card])
  case class GameData(exposedData: CardInfo, teammates: Map[Player, CardInfo], opponents: Map[Player, CardInfo])

}
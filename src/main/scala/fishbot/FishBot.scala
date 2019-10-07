package fishbot

import scala.annotation.tailrec
import scala.language.postfixOps

object FishBot {
  case class VisibleInfo (players: Set[Player],
                     definite:   Map[Card, Either[Set[Player], Player]],
                     oneOf: Map[Player, Set[Set[Card]]]) { self =>

    def isValid: Boolean = definite.values.forall {
      case Right(_) => true
      case Left(s) => s != players
    }

    def deduce: VisibleInfo = {
      if (!isValid) throw new IllegalArgumentException

      // Infer Right(p) card locs from Left(players - p) negation of all other card locs
      val cleanD = definite.view.mapValues {
        case Right(p) => Right(p)
        case Left(s) => players diff s match {
          case diff if diff.isEmpty => throw new IllegalArgumentException
          case diff if diff.size == 1 =>
            Right(diff.head)
          case _ =>
            Left(s)
        }
      }.toMap

      @tailrec
      def rec(d: Map[Card, Either[Set[Player], Player]], o: Map[Player, Set[Set[Card]]]): VisibleInfo = {

        val setDeds =
        // Remove outdated oneOfs if we already know the player has a card in the set
          o.map { case (player, cardSets) =>
            val knownYes = d.collect[Card] {
              case (c, Right(p)) if player == p => c
            }
            (player, cardSets.filter(_ & knownYes.toSet isEmpty))
          }
        // Reduce oneOfs by removing cards we know a player doesn't have
          .map[Player, Set[Set[Card]]] { case (player, cardSets) =>
            val playerNos = d.collect[Card] {
              case (c, Right(p)) if player != p => c
              case (c, Left(s)) if s contains player => c
            }
            (player, cardSets.map(_ removedAll playerNos))
          }.view
        // Separate cardSets of single elements
          .mapValues { cardSets =>
            cardSets.partition(_.size == 1)
          }.toMap

        val newO  : Map[Player, Set[Set[Card]]] = setDeds.view.mapValues(_._2).toMap
        val newYes: Map[Card, Player]           = setDeds.flatMap { case (p, (cardSets, _)) =>
                                                                      cardSets.map(s => (s.head, p)) }

        // Infer Right(p) card locs from Set(p) in oneOf single element sets
        val newD = d ++ newYes.view.mapValues(Right(_)).toMap

        if (d == newD && o == newO)
          VisibleInfo(players, d, o)
        else
          rec(newD, newO)
      }

      rec(cleanD, oneOf)
    }
  }

  def main(args: Array[String]): Unit = {
    val players = Seq(Player("a"), Player("b"), Player("c"))

    val info = VisibleInfo(
      players.toSet,
      Map(
        Card.C2 -> Right(players(0)),
        Card.C3 -> Left(Set(players(1)))
      ),
      Map(
        players(1) -> Set(Set(Card.C2, Card.C3, Card.C4, Card.C5)),
        players(2) -> Set(Set(Card.C2, Card.C5))
      )
    )

    println(info)

    println(info.deduce)
  }
}

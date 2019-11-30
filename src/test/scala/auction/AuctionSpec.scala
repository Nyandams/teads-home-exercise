package auction

import org.scalatest.{BeforeAndAfterEach, FlatSpec}

class AuctionSpec extends FlatSpec with BeforeAndAfterEach {
  var buyerA: Buyer = _
  var buyerB: Buyer = _
  var buyerC: Buyer = _
  var buyerD: Buyer = _
  var buyerE: Buyer = _

  val item = AuctionItem(100)

  override def beforeEach(): Unit = {
    buyerA = Buyer("A", List(Bid(item, 110), Bid(item, 130)))
    buyerB = Buyer("B")
    buyerC = Buyer("C", List(Bid(item, 125)))
    buyerD = Buyer("D", List(Bid(item, 105), Bid(item, 115), Bid(item, 90)))
    buyerE = Buyer("E", List(Bid(item, 132), Bid(item, 135), Bid(item, 140)))
  }

  "An auction" should "return the good winner when it is not an equality case" in {
    val auction = Auction(item, List(buyerA, buyerB, buyerC, buyerD, buyerE))
    auction.findWinner() match {
      case Some(tupleWinner) => assert(tupleWinner._1 == "E" && tupleWinner._2 == 130)
      case None => assert (false)
    }
  }

  it should "return one of the winners when it is an equality case" in {
    buyerA = buyerA.bid(item, 140)
    val auction = Auction(item, List(buyerA, buyerB, buyerC, buyerD, buyerE))
    auction.findWinner() match {
      case Some(tupleWinner) => assert( (tupleWinner._1 == "E" || tupleWinner._1 == "A" ) && tupleWinner._2 == 140)
      case None => assert (false)
    }
  }

  it should "not return a  winner when no one bidded on the object" in {
    val auction = Auction(item, List())
    auction.findWinner() match {
      case Some(tupleWinner) => assert(false)
      case None => assert(true)
    }
  }

  it should "not return a winner when all the bids are under the reserve price" in {
    buyerA = Buyer("A", List(Bid(item, 99)))
    buyerB = Buyer("B", List(Bid(item, 80)))
    val auction = Auction(item, List(buyerA, buyerB))
    auction.findWinner() match {
      case Some(tupleWinner) => assert(false)
      case None => assert (true)
    }
  }
}
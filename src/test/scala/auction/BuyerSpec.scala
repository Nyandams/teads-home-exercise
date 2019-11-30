package auction

import org.scalatest.FlatSpec

class BuyerSpec extends FlatSpec  {

  "A auction.Buyer" should "be able to add a auction.Bid on an auction item" in {
    val buyer = Buyer("buyer")
    val item = AuctionItem(100)
    val buyerWithBid = buyer.bid(item, 110)
    assert(buyerWithBid.bids.nonEmpty)
  }

  it should "be able to return the maximum bid he did on an item" in {
    val item = AuctionItem(100)
    val listBids = List(Bid(item, 132), Bid(item, 135), Bid(item, 140))
    val buyer = Buyer("buyer", listBids)
    assert(buyer.getBiggestBidOnItem(item).get == 140)
  }

  it should "know if he has a bid on a specific item" in {
    val item = AuctionItem(100)
    val listBids = List(Bid(item, 140))
    val buyer = Buyer("buyer", listBids)
    assert(buyer.hasBidOnItem(item))
  }

  it should "know if he doesn't have a bid on a specific item" in {
    val item = AuctionItem(100)
    val listBids = List()
    val buyer = Buyer("buyer", listBids)
    assert(!buyer.hasBidOnItem(item))
  }
}
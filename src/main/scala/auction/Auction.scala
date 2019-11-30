package auction

import scala.util.Random

case class Auction(item: AuctionItem, buyers: List[Buyer]){
  import Auction._

  /**
   * Return the winner name with the second-price bid value if there is one
   * @return
   */
  def findWinner(): Option[(String, BigDecimal)] = {
    val mapBidder = mapBuyerNameBidValue(this.item, this.buyers)
    val mapBidderOverReserve = mapBidder.filter(x => x._2 >= this.item.reservePrice)
    if(mapBidderOverReserve.isEmpty){
      None
    } else {
      val maxBid = mapBidderOverReserve.valuesIterator.max
      val bidderSorted = List(mapBidderOverReserve.toSeq.sortWith(_._2 > _._2):_*)
      if(bidderSorted.size > 1){
        val winners = bidderSorted.filter(x => x._2 == maxBid)
        val randomNumber = Random.nextInt(winners.size)
        Some(bidderSorted(randomNumber)._1, bidderSorted(1)._2)
      } else {
        Some(bidderSorted(0)._1, this.item.reservePrice)
      }
    }
  }

}
object Auction {
  private def mapBuyerNameBidValue(item: AuctionItem, buyers: List[Buyer]) : Map [String, BigDecimal] = {
    buyers.filter(_.hasBidOnItem(item)).map(buyer => buyer.name -> buyer.getBiggestBidOnItem(item).get).toMap
  }
}

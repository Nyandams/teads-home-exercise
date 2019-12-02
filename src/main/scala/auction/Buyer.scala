package auction

case class Buyer(name: String, bids: List[Bid] = List()) {
  /**
   * Add a Bid for an item
   * @param item : item the buyer bid to
   * @param bidPrice: price of the bid
   */
  def bid(item: AuctionItem, bidPrice: BigDecimal): Buyer = copy(bids = Bid(item, bidPrice) :: this.bids)

  /**
   * Return the value of the biggest bid set by the buyer on the item passed in parameter
   * @param item we want the biggest bid value from the buyer
   * @return the biggest bid value or None
   */
  def getBiggestBidOnItem(item: AuctionItem): Option[BigDecimal] = {
    val bidsOnItem = this.bids.filter(bid => bid.item == item).map(_.price)
    if (bidsOnItem.isEmpty){
      None
    } else {
      Some(bidsOnItem.max)
    }
  }

  /**
   * Return true if the current Buyer has a bid on the item
   * @param item
   */
  def hasBidOnItem(item: AuctionItem): Boolean = {
    this.bids.exists(bid => bid.item == item)
  }
}

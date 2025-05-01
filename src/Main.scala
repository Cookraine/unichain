import scala.collection.mutable.Queue
import scala.util.Random

class Token(var amount:Double, var symb:String){
  private var _amount:Double  = amount
  private var _symb:String = symb

  def Amount:Double = _amount
  def Symb:String = _symb


  def Token_inf():String = "Amount: "+_amount + " " + _symb

  def +(value: Token): Token = {
    if (this.symb == value.symb)
      new Token(this.amount + value.amount, this.symb)
    else
      throw new IllegalArgumentException("Not match token symb!")
  }

  def -(value: Token): Token = {
    if (this.symb == value.symb && this.amount >= value.amount)
      new Token(this.amount - value.amount, this.symb)
    else
      throw new IllegalArgumentException("Not match token symb")
  }

  def >(value: Token): Boolean = {
    if (this.symb == value.symb)
      this.amount > value.amount
    else
      throw new IllegalArgumentException("Tokens have different symbols!")
  }

  def <(value: Token): Boolean = {
    if (this.symb == value.symb)
      this.amount < value.amount
    else
      throw new IllegalArgumentException("Tokens have different symbols!")
  }

}

class User(id: Int, balance: Token) {
  private var _id: Int = id
  private var _balance: Token = balance

  def getID: Int = _id
  def getBalance: Token = _balance

  def setBalance(balance: Token) = {
    _balance = balance
  }

  def createTransaction(receiverID: Int): Transaction = {
    var sendAmount: Double = Random.between(1, 100)
    var fee: Double = Random.between(10, 50)
    Transaction(id, sendAmount, fee, receiverID)
  }
}

case class Transaction(senderID: Int, amount: Double, txPriorityFee: Double, receiverID: Int)

class BlockBuilder() {
  private var _dataListUsers: List[User] = List()
  private var transactionQueue: Queue[Transaction] = Queue()

  def getDataListUsers: List[User] = _dataListUsers
  def getTransactionQueue: Queue[Transaction] = transactionQueue

  def PaymentTransaction(person: User, value: Token) = {
    val exchange = person.getBalance.amount - value
    person.setBalance(exchange)
  }

  def validateTransaction(tx: Transaction): Boolean = {
    _dataListUsers.find(_.getID == tx.senderID) match {
      case Some(user) =>
        val totalCost = tx.amount + tx.txPriorityFee
        if (user.getBalance.amount >= totalCost)  {
          PaymentTransaction(user, new Token (totalCost, user.getBalance.symb))
          true
        } else {
          false
        }
      case None =>
        false
      }
    }

  def addTransaction(tx: Transaction) = {
    if (validateTransaction(tx)) {
      transactionQueue.enqueue(tx)
      println(s"Transaction from ${tx.senderID} to ${tx.receiverID} added to queue.")
    } else {
      println(s"Transaction from ${tx.senderID} to ${tx.receiverID} is invalid and removed.")
    }
  }

  def addUser(user: User): Unit = {
    _dataListUsers = user :: _dataListUsers
  }

}

class Sequencer {
  def sortTransactions(transactions: Seq[Transaction]): Seq[Transaction] = {
    transactions.sortBy(-_.txPriorityFee)
  }
}

class Validator(id: Int, balance: Token) {
  private var _id: Int = id
  private var _balance: Token = balance
  private var _stakedAmount: Double = 0.0
  private var _stakingPool: List[Double] = List()
  private var _stakingWeight: Double = 0.0
  private var _shareIncome: Double = 0.0

  def getID: Int = _id
  def getBalance: Token = _balance
  def getStakedAmount: Double = _stakedAmount
  def getStakingPool: List[Double] = _stakingPool
  def getStakingWeight: Double = _stakingWeight
  def getShareIncome: Double = _shareIncome

  def setBalance(balance: Token) = {
    _balance = balance
  }

  def staking(): Double = {
    var value = Random.between(1.0, 100.0)
    if (_balance.Amount >= value) {
      _stakedAmount = value
      _balance = new Token(_balance.Amount - value, _balance.Symb)
      _stakingWeight = value
      value
    } else {
      0.0
    }
  }

  def setShareIncome(totalStaked: Double) = {
    if (totalStaked > 0) {
      _shareIncome = (_stakedAmount / totalStaked) * 100
    } else {
      _shareIncome = 0.0
    }
  }

  def receiveReward(rewardPool: Double) = {
    val reward = (rewardPool * _shareIncome / 100.0)
    _balance += new Token(reward, _balance.Symb)
  }
}

object Main {
  def main(args: Array[String]) = {

    var user1 = new User(1, new Token(100, "UC"))
    var user2 = new User(2, new Token(100, "UC"))
    var user3 = new User(3, new Token(100, "UC"))
    var user4 = new User(4, new Token(100, "UC"))
    var user5 = new User(5, new Token(100, "UC"))

    var blockBuilder = new BlockBuilder()
    blockBuilder.addUser(user1)
    blockBuilder.addUser(user2)
    blockBuilder.addUser(user3)
    blockBuilder.addUser(user4)
    blockBuilder.addUser(user5)

    var validator1 = new Validator(6, new Token(100, "UC"))
    var validator2 = new Validator(7, new Token(100, "UC"))
    var validator3 = new Validator(8, new Token(100, "UC"))
    var validator4 = new Validator(9, new Token(100, "UC"))
    var validator5 = new Validator(10, new Token(100, "UC"))


    val sequencer = new Sequencer()


      val validTransactions = blockBuilder.getTransactionQueue.toSeq
      val sortedTransactions = sequencer.sortTransactions(validTransactions)
      println("\nSorted Transactions:")
      sortedTransactions.foreach { tx =>
        println(s"From ${tx.senderID} to ${tx.receiverID}, Amount=${tx.amount}, PriorityFee=${tx.txPriorityFee}")
      }

  }
}


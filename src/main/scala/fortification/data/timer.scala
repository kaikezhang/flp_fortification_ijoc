package fortification.data

trait timer {
    var beginTime: Double = _
    var timeLimit: Double = _
  
    def timeUsed(): Double = {
      1.0 * (System.currentTimeMillis() - beginTime) / 1000
    }
    def timeLeft(): Double = {
      timeLimit - timeUsed
    }
  
    def timeLimitReached(): Boolean = {
      timeLeft < 0
    }
    
    
    var lastRecordTime = collection.mutable.Map.empty[String, Double]
      
    def recordNow(id: String = "default") = {
      lastRecordTime(id) = System.currentTimeMillis()
    }  
    def timeCheckout(id: String = "default"):Double = {
      val ret = 1.0 * (System.currentTimeMillis() - lastRecordTime(id)) / 1000
      recordNow(id)
      ret
    }
    
    def timeFromRecorded(id: String = "default") = {
      1.0 * (System.currentTimeMillis() - lastRecordTime(id)) / 1000
    }  
}
package fortification.data

//import org.json4s.JsonAST.JArray
//import org.json4s.JsonAST.JInt
//import org.json4s.JsonAST.JDouble


case class Coordinate(val lat: Double, val lng: Double);

object GeoComputer {
  def distance(coord1: Coordinate, coord2: Coordinate): Double = {    

    val lat1 = Math.toRadians(coord1.lat);
    val lng1 = Math.toRadians(coord1.lng);
    val lat2 = Math.toRadians(coord2.lat);
    val lng2 = Math.toRadians(coord2.lng);
    val theta = lng1 - lng2;

    var dist = 0.0;
    
    val deta_lng = lng1 - lng2;
    val deta_lat = lat1 - lat2;
  
    if (Math.abs(deta_lng) + Math.abs(deta_lat) < 1E-6) {
      return 0.0;
    }
  
    dist = Math.sin(lat1) * Math.sin(lat2) +
           Math.cos(lat1) * Math.cos(lat2) * Math.cos(theta);
    dist = Math.acos(dist);
    dist = dist.toDegrees;
    dist * 60 * 1.1515;    
        
  }
}

class DemandPoint(val index: Int,  val demand: Double, val emergencyCost:Double, override val lat: Double, override val lng: Double) extends Coordinate(lat, lng){
//  def toJArray() = JArray(List(JInt(index), JDouble(lat), JDouble(lng) , JDouble(demand)))
}

class CandidateLocation(val index: Int, val fixedCosts: Double,  override val lat: Double, override val lng: Double) extends Coordinate(lat, lng){
//  def toJArray() = JArray(List(JInt(index), JDouble(lat), JDouble(lng)))
}
package fortification.data.output

import java.text.SimpleDateFormat
import java.io.PrintWriter
import java.util.Date
import java.io.File
import scala.util.control.NonFatal
import java.io.OutputStream
import java.io.FileOutputStream

object ResultOutput {
  
  def createOutputPrinter(experiment:String): (PrintWriter, OutputStream) = {
    var fileSeparator = ""
    if (System.getProperty("os.name").startsWith("Windows")) {
      fileSeparator = "\\"
    } else {
      fileSeparator = "/"
    }

    val dateFormat = new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss")
    val date = new Date()

    var directory = new File("output")
    //check if the location exists
    if (!directory.exists()) {
      //let's try to create it
      try {
        directory.mkdir();
      } catch {
        //handle the exception
        case NonFatal(e)     => println("exception caught: " + e);
      }
    }

    val filename = "output" + fileSeparator + experiment + dateFormat.format(date) + ".txt"
    val logfile =  "output" + fileSeparator + experiment + dateFormat.format(date) + "-log" + ".txt"
    (new PrintWriter(new File(filename)), new FileOutputStream(new File(logfile)))
  }
  
  
}
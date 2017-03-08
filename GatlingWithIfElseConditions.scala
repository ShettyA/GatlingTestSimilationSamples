package ingestion
 
import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.jdbc.Predef._
import io.gatling.core.Predef.bootstrap._
import io.gatling.http.Headers.Names._
import scala.concurrent.duration._
import bootstrap._
import assertions._
import com.typesafe.config.{Config, ConfigFactory}
import java.io.{FileReader, BufferedReader}
import java.util.Random
 
class BasicRequestSimulation extends Simulation {
  val filename = "sample-test.conf"
  private val in:BufferedReader = new BufferedReader(new FileReader(filename));
  private val config = ConfigFactory.parseReader(in)
 
  // Load performance test config variables. Use environment variables to overwrite config set values.
  val perfTestUserCount = config.getInt("test.api.gatling.users")
  var perfTestRampCount = config.getInt("test.api.gatling.ramp")
  var perfTestRepeatCount = config.getInt("test.api.gatling.repeatCount")
  val perfTestScenario = config.getString("test.api.scenario")
  val perfTestDuration = config.getInt("test.api.gatling.duration")
 
  // Load Host details
  val testAPIHost = config.getString("test.api.host")
  val testAPIPort = config.getInt("test.api.port")
 
  val httpProtocol = http
    .baseURL("http://" + testAPIHost + ":" + testAPIPort)
    .acceptCharsetHeader("ISO-8859-1,utf-8;q=0.7,*;q=0.7")
    .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
 
  def generateHeaderData(thriftBlob: String): Map[String, String] = {
     Map(
      "Accept"->"*/*",
      "proxy_host_name"->"192.168.1.3",
      "user-agent"->"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; InfoPath.2; .NET CLR 1.1.4322)")
  }
 
  var experiment1 = Map(
    "SuccessA" -> "Success data set",
    "FailureA" -> "Failure data set",
    "SuccessB" -> "Success data set",
    "FailureB" -> "Failure data set"
  )
 
  var r = new Random();
 
  val chain1 = 
    exec(
      // val response = 
      http("Getting Config")
        .get("http://localhost:9000/getConfig/2")
        .check(bodyString)
        .check(regex("""\"confId\":[0-9]""").saveAs("conf"))
    )
    .exec(session => {
      // print the Session for debugging, don't do that on real Simulations
      println(session + session.attributes.get("conf").getOrElse("0").toString)
      session
    })
    .exec( 
      doIfOrElse(session => (session.attributes.get("conf").getOrElse("0").toString != "\"confId\":1") == true) {
        exec(
          doIfOrElse(r.nextDouble() < 0.3){
            exec(
              http("API ConfigA-Success Request")
                .post("http://" + testAPIHost + ":" + testAPIPort + "/verify")
                .headers((generateHeaderData(experiment1("SuccessA"))))
                .check(status.is(202))
            )
          }{
            exec(
              http("API ConfigA-Failure Request")
                .post("http://" + testAPIHost + ":" + testAPIPort + "/verify")
                .headers((generateHeaderData(experiment1("FailureA"))))
                .check(status.is(202))
            )
          }
        )
      } {
       exec(
          doIfOrElse(r.nextDouble() < 0.2){
          exec(
            http("API ConfigB-Success Request")
              .post("http://" + testAPIHost + ":" + testAPIPort + "/verify")
              .headers((generateHeaderData(experiment1("SuccessB"))))
              .check(status.is(202))
          )
        }{
          exec(
            http("API ConfigB-Failure Request")
              .post("http://" + testAPIHost + ":" + testAPIPort + "/verify")
              .headers((generateHeaderData(experiment1("FailureB"))))
              .check(status.is(202))
          )
        }
      )
      }
    )
 
  val getConfigScenario = scenario("Get Config")
    .during(Duration(perfTestDuration, "seconds")){
      // .repeat(perfTestRepeatCount) {
        // exec(chain1)
        chain1
      // }
    }
 
  // Execute the load test using performance parameters set using config file
  if (perfTestScenario == "all") {
    setUp(
      getConfigScenario.inject(ramp(perfTestUserCount users) over (perfTestRampCount seconds)))
      .protocols(httpProtocol)
      .assertions(
        // global.responseTime.max.between(0, 2000),
        global.successfulRequests.percent.greaterThan(95)
      )
    }
 
}

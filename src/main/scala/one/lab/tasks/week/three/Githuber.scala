package one.lab.tasks.week.three

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import RestClientImpl._
import scala.util.Failure
import scala.util.Success

object Githuber extends App {
  implicit val system: ActorSystem = ActorSystem("lalka")
  implicit val materializer: ActorMaterializer = ActorMaterializer.create(system)
  implicit val executionContext: ExecutionContextExecutor = ExecutionContext.global
  implicit val defaultFormats: DefaultFormats.type = DefaultFormats

  // TODO: поля можете добавить какие хотите
  case class GithubUser(login: String, name: String, avatarUrl: Option[String], public_repos: Option[String])

  case class GithubRepository(name: String,
                              size: Int,
                              fork: Boolean,
                              pushed_at: String,
                              stargazers_count: Int)

  //  https://api.github.com/users/{$USER}
  def getGithubUser(username: String): Future[GithubUser] = {
    get(s"https://api.github.com/users/$username")
      .map {
        body => parse(body).extract[GithubUser]
      }
  }

  def getUserRepositories(repoUrl: String): Future[List[GithubRepository]] = {
    get(repoUrl)
      .map {
        body => parse(body).extract[List[GithubRepository]]
      }
  }

  def getUserInfo(username: String): Unit = {
    getGithubUser(username)
      .flatMap(user => {
        println(s"${user.name} called ${user.login} under avatar ${user.avatarUrl.getOrElse("None")} has ${user.public_repos.getOrElse("None")} repositories in his github account: \n")
        getUserRepositories(s"https://api.github.com/users/${user.login}/repos")
      })
      .onComplete {
        case Success(responses) =>
          for (response <- responses)
            println(s"${response.name}: size = ${response.size}, stargazers = ${response.stargazers_count}, push date = ${response.pushed_at}, fork = ${if (response.fork) "Forked" else "Not Forked"}")
        case Failure(e) => println(e.getMessage)
      }
  }

  getUserInfo("DamiNo007")
}

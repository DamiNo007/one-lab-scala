package one.lab.tasks.week.one

import one.lab.tasks.week.one.Interfaces.{ForzaHorizon, PlayStation, Rdr2, Sega, Sonic, Xbox}

object Operators {

  /**
    * Simple person class, nothing special.
    * override [[toString]] method, so it will work same as [[getPersonInfo]].
    *
    * @param name    just name.
    * @param surname just surname.
    * @param age     just age.
    */
  class Person(val name: String, val surname: String, val age: Int) {
    override def toString(): String = s"$name $surname $age"
  }

  /**
    * Should return formatted string.
    *
    * @param name    just name.
    * @param surname just surname.
    * @param age     just age.
    * @return should return string in the following format: "name surname age".
    */
  def getPersonInfo(name: String, surname: String, age: Int): String = s"$name $surname $age"

  /**
    * should return Person info as in [[getPersonInfo]] method.
    *
    * @param person [[Person]].
    * @return return should be the same as in [[getPersonInfo]].
    * @hint: try to override [[Person.toString]] method.
    */
  def getPersonInfoObject(person: Person): String = person.toString()

  /**
    * Just compare to Persons by their age and return elder one.
    *
    * @param first  person.
    * @param second person.
    * @return eldest person.
    */
  def getElderPerson(first: Person, second: Person): Person = if (first.age > second.age) first else second

  //My own Test :)
  def main(args: Array[String]): Unit = {
    val person1 = new Person("Damir", "Moldabayev", 20)
    val person2 = new Person("Ruslan", "Safarov", 19)
    println(person1.toString())
    println(person2.toString())
    println(getElderPerson(person1, person2))
  }
}

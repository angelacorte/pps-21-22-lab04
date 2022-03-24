package u04lab.code

import List.*
import scala.Option.*

trait Student:
  def name: String
  def year: Int
  def enrolling(course: Course*): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?

trait Course:
  def name: String
  def teacher: String

object Student:
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

object Course:
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)

case class CourseImpl(_name: String, _teacher: String) extends Course:
  override def name: String = _name
  override def teacher: String = _teacher

case class StudentImpl(_name: String, _year: Int) extends Student:

  private var coursesList: List[Course] = Nil()
  override def name: String = _name
  override def year: Int = _year
  override def enrolling(course: Course*): Unit =
    course.foreach(c => if coursesList != Nil() then coursesList = Cons(c, coursesList)
     else coursesList = Cons(c, Nil()) )

  override def courses: List[String] = map(coursesList)(_.name)
  override def hasTeacher(teacher: String): Boolean = contains(map(coursesList)(_.teacher), teacher)

object SameTeacher:
  def unapply(courses: List[Course]): scala.Option[String] = courses match
    case Cons(c, t) if length(filter(map(courses)(_.teacher))(s => s == c.teacher)) == length(courses) => scala.Option(c.teacher)
    case _ => empty

@main def checkStudents(): Unit =
  val cPPS = Course("PPS", "Viroli")
  val cPCD = Course("PCD", "Ricci")
  val cSDR = Course("SDR", "D'Angelo")
  val cOOP = Course("OOP", "Viroli")
  val s1 = Student("mario", 2015)
  val s2 = Student("gino", 2016)
  val s3 = Student("rino") // defaults to 2017
  s1.enrolling(cPPS, cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS, cPCD, cSDR)
  println(
    (s1.courses, s2.courses, s3.courses)
  ) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true

  def sameTeacher(courses: List[Course]): String = courses match
    case SameTeacher(t) => s"$courses have same teacher $t"
    case _ => s"$courses have different teachers"

  println(sameTeacher(Cons(cPPS, Cons(cOOP, Nil()))))
  println(sameTeacher(Cons(cPPS, Cons(cPCD, Nil()))))


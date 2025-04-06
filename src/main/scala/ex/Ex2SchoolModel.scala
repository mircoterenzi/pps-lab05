package ex

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import util.Sequences.Sequence
import util.Sequences.Sequence.*

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course

    def teacher(name: String): Teacher
    def course(name: String): Course
    def emptySchool: School
    extension (school: School)
      def courses(): Sequence[Course]
      def teachers(): Sequence[Teacher]
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
      def hasTeacher(name: String): Boolean
      def hasCourse(name: String): Boolean

  object BasicSchoolModule extends SchoolModule:
    case class TeacherToCourse(t: Teacher, c: Course)

    override type School = Sequence[TeacherToCourse]
    override type Teacher = String
    override type Course = String

    def teacher(name: String): Teacher = name
    def course(name: String): Course = name
    def emptySchool: School = Nil()

    extension (school: School)
      def courses(): Sequence[String] = school.map {
        case TeacherToCourse(t, c) => c
      }.distinct()
      def teachers(): Sequence[String] = school.map {
        case TeacherToCourse(t, c) => t
      }.distinct()
      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        school.concat(Cons(TeacherToCourse(teacher, course), Nil()))
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        school.flatMap {
          case TeacherToCourse(t, c) if t == teacher => Cons(c, Nil())
          case _ => Nil()
        }
      def hasTeacher(name: String): Boolean = teachers().contains(name)
      def hasCourse(name: String): Boolean = courses().contains(name)


class SchoolModelTest:
  import SchoolModel.BasicSchoolModule.*
  val school: School = emptySchool
  val john: Teacher = teacher("John")
  val pablo: Teacher = teacher("Pablo")
  val math: Course = course("Math")
  val italian: Course = course("Italian")
  val school2: School = school.setTeacherToCourse(john, math)
  val school3: School = school2.setTeacherToCourse(john, italian).setTeacherToCourse(pablo, italian)

  @Test def testTeacher(): Unit =
    assertEquals(Nil(), school.teachers())
    assertEquals(Cons("John", Nil()), school2.teachers())
    assertEquals(Cons("John", Cons("Pablo", Nil())), school3.teachers())

  @Test def testCourses(): Unit =
    assertEquals(Nil(), school.courses())
    assertEquals(Cons("Math", Nil()), school2.courses())
    assertEquals(Cons("Math", Cons("Italian", Nil())), school3.courses())

  @Test def testHasTeacher(): Unit =
    assertFalse(school.hasTeacher("John"))
    assertTrue(school2.hasTeacher("John"))
    assertTrue(school3.hasTeacher("John"))

  @Test def testHasCourse(): Unit =
    assertFalse(school.hasCourse("Math"))
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school2.hasCourse("Italian"))
    assertTrue(school3.hasCourse("Math"))
    assertTrue(school3.hasCourse("Italian"))

  @Test def testCoursesOfATeacher(): Unit =
    assertEquals(Nil(), school.coursesOfATeacher(john))
    assertEquals(Cons("Math", Nil()), school2.coursesOfATeacher(john))
    assertEquals(Cons("Math", Cons("Italian", Nil())), school3.coursesOfATeacher(john))

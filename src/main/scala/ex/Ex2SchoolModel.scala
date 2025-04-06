package ex

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import util.Sequences.Sequence
import Sequence.*
import ex.SchoolModel.*
import BasicSchool.*

object SchoolModel:
  type School = SchoolModule
  type Teacher = String
  type Course = String

  trait SchoolModule:
    def courses(): Sequence[Course]
    def teachers(): Sequence[Teacher]
    def setTeacherToCourse(teacher: Teacher, course: Course): School
    def coursesOfATeacher(teacher: Teacher): Sequence[Course]
    def hasTeacher(name: String): Boolean
    def hasCourse(name: String): Boolean

  private case class TeacherToCourse(t: Teacher, c: Course)

  private class BasicSchoolModule(private var school: Sequence[TeacherToCourse] = Nil()) extends SchoolModule:
    def courses(): Sequence[String] = school.map {
      case TeacherToCourse(t, c) => c
    }.distinct()
    def teachers(): Sequence[String] = school.map {
      case TeacherToCourse(t, c) => t
    }.distinct()
    def setTeacherToCourse(teacher: Teacher, course: Course): School =
      BasicSchoolModule(Cons(TeacherToCourse(teacher, course), school))
    def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
      school.flatMap {
        case TeacherToCourse(t, c) if t == teacher => Cons(c, Nil())
        case _ => Nil()
      }
    def hasTeacher(name: String): Boolean = teachers().contains(name)
    def hasCourse(name: String): Boolean = courses().contains(name)

  object BasicSchool:
    def emptySchool: School = new BasicSchoolModule()
    def teacher(name: String): Teacher = name
    def course(name: String): Course = name

class SchoolModelTest:
  private val school = emptySchool
  private val john: Teacher = teacher("John")
  private val pablo: Teacher = teacher("Pablo")
  private val math: Course = course("Math")
  private val italian: Course = course("Italian")
  private val school2: School = school.setTeacherToCourse(john, math)
  private val school3: School = school2.setTeacherToCourse(john, italian).setTeacherToCourse(pablo, italian)

  @Test def testTeacher(): Unit =
    assertEquals(Nil(), school.teachers())
    assertEquals(Cons("John", Nil()), school2.teachers())
    assertEquals(Cons("Pablo", Cons("John", Nil())), school3.teachers())

  @Test def testCourses(): Unit =
    assertEquals(Nil(), school.courses())
    assertEquals(Cons("Math", Nil()), school2.courses())
    assertEquals(Cons("Italian", Cons("Math", Nil())), school3.courses())

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
    assertEquals(Cons("Italian", Cons("Math", Nil())), school3.coursesOfATeacher(john))

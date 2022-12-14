package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersection contains in both sets") {
    new TestSets :
      val s4 = union(s1, s2)
      val s5 = union(s3, s2)
      val s6 = intersect(s4, s5)
      assert(contains(s6, 2), "Union 2")
      assert(!contains(s6, 3), "Union 3")
  }

  test("diff") {
    new TestSets :
      val s4 = union(s1, s2)
      val s5 = union(s3, s2)
      val s6 = diff(s4, s5)
      assert(!contains(s6, 2), "Union 2")
      assert(contains(s6, 1), "Union 3")
  }

  test("filter") {
    new TestSets :
      def p(x: Int): Boolean = x < 3
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = union(s1, union(s2, union(s3, union(s4, s5))))
      assert(contains(filter(s6, p), 2), "Union 2")
      assert(!contains(filter(s6, p), 3), "Union 3")
  }

  test("forall") {
    new TestSets :
      def p1(x: Int): Boolean = x < 3
      def p2(x: Int): Boolean = x < 6
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = union(s1, union(s2, union(s3, union(s4, s5))))
      assert(!forall(s6, p1), "all elements are less then 3")
      assert(forall(s6, p2), "all elements are less then 6")
  }

  test("exists") {
    new TestSets :
      def p1(x: Int): Boolean = x < 3

      def p2(x: Int): Boolean = x > 6

      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = union(s1, union(s2, union(s3, union(s4, s5))))
      assert(exists(s6, p1), "all elements are less then 3")
      assert(!exists(s6, p2), "all elements are less then 6")
  }

  test("map") {
    new TestSets :
      def f(x: Int): Int = x * x

      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = union(s1, union(s2, union(s3, union(s4, s5))))
      assert(contains(map(s6, f), 25), "all elements are less then 3")
      assert(!contains(map(s6, f), 5), "all elements are less then 6")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

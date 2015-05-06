package siren.test

import org.scalatest._
import siren._

class UnionFindGridTest extends FunSuite with BeforeAndAfter {
  val indexRange = (250L, 499L) // 0 to 249
  val scanRange = (750L, 999L)  // 250 to 499
  val uf = new UnionFindGrid(indexRange, scanRange)

  test("toIndex for pos in index range") {
    assert(uf.toIndex(300) == 50)
  }
  
  test("toIndex for pos in scan range") {
    assert(uf.toIndex(800) == (250 + 50))
  }
}
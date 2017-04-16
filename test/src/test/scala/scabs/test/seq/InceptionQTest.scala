package scabs.test.seq

import org.scalatest._
import scabs.seq.{InceptionQ, Sequence}
import InceptionQ._
import cats.data.Prod
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Gen, Prop}
import scabs.test.seq.StackOps.taglessFinal.StackTF

/**
  *
  *
  * @author Matthew Pocock
  */
class InceptionQTest extends WordSpec with PropertyChecks with Matchers with AppendedClues {

  "An InceptionQ" when {
    "empty" should {
      "be empty" in {
        val ei = InceptionQ.empty[Int]
        ei.isEmpty should be(true)
      }

      "reuse the same instance" in {
        val ei = InceptionQ.empty[Int]
        val es = InceptionQ.empty[String]
        ei should be theSameInstanceAs es
      }

      "append to itself" should {
        val sum = InceptionQ.empty[Int] ++ InceptionQ.empty[Int]

        "be empty" in {
          sum.isEmpty should be(true)
        }

        "be the same instance" in {
          sum should be theSameInstanceAs InceptionQ.empty[Int]
        }
      }
    }

    "any queue" should {
      implicit val genOps: Gen[StackOps[Int]] = StackOps.genOps[Int]

      "cons uncons" in {
        forAll(genOps) { ops =>
          val Q = implicitly[Sequence[InceptionQ]]

          val queue = StackOps.taglessFinal.replay[InceptionQ, Int](ops)

          val cQueue = 42 +: queue
          val Some((h, dQ)) = cQueue.uncons

          cQueue.size should be (queue.size + 1)
          dQ.size should be (queue.size)
          h should be (42)
        }
      }

      "snoc unsnoc" in {
        forAll(genOps) { ops =>
          val Q = implicitly[Sequence[InceptionQ]]

          val queue = StackOps.taglessFinal.replay[InceptionQ, Int](ops)

          val sQueue = queue :+ 42
          val Some((dQ, h)) = sQueue.unsnoc

          sQueue.size should be (queue.size + 1)
          dQ.size should be (queue.size)
          h should be (42)
        }
      }

      "uncons" should {
        "to a queue with one fewer elements" in {
          forAll(genOps) { ops: StackOps[Int] =>
            implicit val L = scabs.seq.StdlibInstances.listSequenceInstance
            val Q = implicitly[Sequence[InceptionQ]]

            val asList = StackOps.taglessFinal.replay[List, Int](ops)
            val queue = StackOps.taglessFinal.replay[InceptionQ, Int](ops)

            { asList.size should be(Q.lengthSeq(queue)) } withClue s"from list:\n\t$asList\nas queue\n\t$queue"

            var fc: InceptionQ[Int] = null
            var pc = queue
            var uc = Q.uncons(queue)
            for {
              (el, i) <- asList.zipWithIndex
            } {
              uc match {
                case Some((h, tl)) =>
                  {
                    h should be (el)
                    tl.size should be (asList.size - i - 1)
                  } withClue s"with prior queue:\n\t$fc\nqueue:\n\t$pc\nproducing tail:\n\t$tl\nfrom list:\n\t$asList\nstarting with:\n\t$queue\nat:\n\t$i"
                  fc = pc
                  pc = tl
                  uc = Q.uncons(tl)
                case None =>
                  asList.isEmpty
              }
            }
          }
        }
      }

      "unsnoc" should {
        "to a queue with one fewer elements" in {
          forAll(genOps) { ops: StackOps[Int] =>
            implicit val L = scabs.seq.StdlibInstances.listSequenceInstance
            val Q = implicitly[Sequence[InceptionQ]]

            val asList = StackOps.taglessFinal.replay[List, Int](ops).reverse
            val queue = StackOps.taglessFinal.replay[InceptionQ, Int](ops)

            { asList.size should be(Q.lengthSeq(queue)) } withClue s"from list:\n\t$asList\nas queue\n\t$queue"

            var fs: InceptionQ[Int] = null
            var ps = queue
            var us = Q.unsnoc(queue)
            for {
              (el, i) <- asList.zipWithIndex
            } {
              us match {
                case Some((tl, h)) =>
                  {
                    h should be (el)
                    tl.size should be (asList.size - i - 1)
                  } withClue s"with prior queue:\n\t$fs\nqueue:\n\t$ps\nproducing tail:\n\t$tl\nfrom list:\n\t$asList\nstarting with:\n\t$queue\nat:\n\t$i"
                  fs = ps
                  ps = tl
                  us = Q.unsnoc(tl)
                case None =>
                  asList.isEmpty
              }
            }
          }
        }
      }

      "construction" should {
        "build things of the correct sizes" in {
          forAll(genOps, maxSize(1000)) { ops =>
            implicit val L = scabs.seq.StdlibInstances.listSequenceInstance
            val Q = implicitly[Sequence[InceptionQ]]

            StackOps.taglessFinal.replay[Prod[List, InceptionQ, ?], Int](ops)(new StackTF[Prod[List, InceptionQ, ?]] {
              override def empty[T]: Prod[List, InceptionQ, T] = Prod(L.empty, Q.empty)

              override def cons[T](t: T,
                                   ops: Prod[List, InceptionQ, T]): Prod[List, InceptionQ, T] = {
                val cns = Prod(L.cons(t, ops.first), Q.cons(t, ops.second))

                { cns.second.size should be(ops.second.size + 1) }
                { cns.second.size should be(cns.first.size) }

                cns
              }

              override def snoc[T](t: T,
                                   ops: Prod[List, InceptionQ, T]): Prod[List, InceptionQ, T] = {
                val prd = Prod(L.snoc(ops.first, t), Q.snoc(ops.second, t))

                { prd.second.size should be(ops.second.size + 1) }
                { prd.second.size should be(prd.first.size) }

                prd
              }

              override def append[T](lhs: Prod[List, InceptionQ, T],
                                     rhs: Prod[List, InceptionQ, T]): Prod[List, InceptionQ, T] = {
                val lApp = L.concat(lhs.first, rhs.first)
                val qApp = Q.concat(lhs.second, rhs.second)

                {
                  lApp.size should be(qApp.size)
                  qApp.size should be(Q.lengthSeq(qApp))
                } withClue s"with lhs:\n\t${lhs.second}\nand rhs\n\t${rhs.second}\nand sum\n\t$qApp\ncompared to:\n\t$lApp\nwith lhs:\n\t${lhs.first}\nand rhs\n\t${rhs.first}"

                Prod(lApp, qApp)
              }
            })
          }
        }
      }

    }
  }

}

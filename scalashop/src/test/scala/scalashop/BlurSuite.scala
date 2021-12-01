package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.{assertArrayEquals, assertEquals}


class BlurSuite {

  def boxBlurKernelBad(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val clampX = clamp(_, 0, src.width - 1)
    val clampY = clamp(_, 0, src.height - 1)
    var res, counter = 0

    var xs = clampX(x - radius)
    while (xs <= clampX(x + radius)) {
      var ys = clampY(y - radius)
      while (ys <= clampY(y + radius)) {
        res += src(xs, ys)
        ys += 1
        counter += 1
      }
      xs += 1
    }

    res / counter
  }

  @Test def `boxBlurKernel should return the correct value on an interior pixel of a 3x4 image with radius 1`: Unit = {
    val src = new Img(3, 4,
      Array(
        4, 5, 7,
        1, 2, 4,
        5, 12, 1,
        10, 3, 4
      ))

    assert(boxBlurKernel(src, 0, 0, 1) == 3)
    assert(boxBlurKernel(src, 1, 0, 1) == 3)
    assert(boxBlurKernel(src, 2, 0, 1) == 4)

    assert(boxBlurKernel(src, 0, 2, 1) == 5)
    assert(boxBlurKernel(src, 1, 2, 1) == 4)
    assert(boxBlurKernel(src, 2, 2, 1) == 4)
  }

  @Test def `boxBlurKernel should correctly handle radius 0`: Unit = {
    val src = new Img(3, 3, Array(1, 2, 3, 4, 5, 6, 7, 8, 9))

    for (x <- 0 until 3; y <- 0 until 3) {
      assert(src(x, y) == boxBlurKernel(src, x, y, 0))
    }
  }

  @Test def `boxBlurKernel should compute the averages of red, blue, green and alpha channels separately`: Unit = {
    val src = new Img(3, 3, Array(256, 258, 123, 255, 256, 19, 12, 12, 7))

    assert(boxBlurKernel(src, 0, 0, 1) == 64 && boxBlurKernelBad(src, 0, 0, 1) == 256)
    assert(boxBlurKernel(src, 1, 0, 1) == 66 && boxBlurKernelBad(src, 1, 0, 1) == 194)
    assert(boxBlurKernel(src, 2, 0, 1) == 36 && boxBlurKernelBad(src, 2, 0, 1) == 164)
  }

  @Test def `HorizontalBoxBlur/VerticalBoxBlur parBlur with 32 tasks should modify each pixel exactly once`: Unit = {
    val src = new Img(32, 32)
    val dstH = new Img(32, 32)
    val dstHPar = new Img(32, 32)
    val dstV = new Img(32, 32)
    val dstVPar = new Img(32, 32)
    val numTasks = 32
    for (x <- 0 until 32; y <- 0 until 32) src(x, y) = rgba(2 * x, 3 * y, x + y, x)

    HorizontalBoxBlur.blur(src, dstH, 0, src.height, 3)
    HorizontalBoxBlur.parBlur(src, dstHPar, numTasks, 3)

    VerticalBoxBlur.blur(src, dstV, 0, src.width, 3)
    VerticalBoxBlur.parBlur(src, dstVPar, numTasks, 3)

    for (x <- 0 until 32; y <- 0 until 32) {
      assert(dstH(x, y) == dstHPar(x, y))
      assert(dstV(x, y) == dstVPar(x, y))
    }
  }

  @Test def `HorizontalBoxBlur/VerticalBoxBlur with radius 1 and 4 tasks should correctly blur the image` = {
    val src = new Img(3, 3, Array(35, 122, 145, 92, 231, 12, 46, 81, 182))
    val dstV = new Img(3, 3)
    val dstH = new Img(3, 3)

    HorizontalBoxBlur.blur(src, dstH, 0, src.height, 1)
    VerticalBoxBlur.blur(src, dstV, 0, src.width, 1)

    for (x <- 0 until 3; y <- 0 until 3) {
      assert(dstH(x, y) == boxBlurKernel(src, x, y, 1))
      assert(dstV(x, y) == boxBlurKernel(src, x, y, 1))
    }
  }

  @Test def `HorizontalBoxBlur/VerticalBoxBlur parBlur should not forget the last strip` = {
    val src = new Img(32, 32)
    for (x <- 0 until 32; y <- 0 until 32) src(x, y) = rgba(2 * x, 3 * y, x + y, x)
    val dstHPar = new Img(32, 32)
    val dstVPar = new Img(32, 32)
    val numTasks = 16
    val range = 0 to src.width by src.width / Math.min(src.width, numTasks)
    val (from, to) = (range zip range.tail).last

    HorizontalBoxBlur.parBlur(src, dstHPar, numTasks, 3)
    VerticalBoxBlur.parBlur(src, dstVPar, numTasks, 3)

    for (cr <- from until to; rc <- 0 until 32) {
      assert(dstHPar(rc, cr) == boxBlurKernel(src, rc, cr, 3))
      assert(dstVPar(cr, rc) == boxBlurKernel(src, cr, rc, 3))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

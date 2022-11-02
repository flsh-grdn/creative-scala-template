import doodle.core._
import doodle.image._
import doodle.image.syntax.all._
import doodle.image.syntax.core._
import doodle.java2d._
import doodle.algebra.Size

import doodle.core.PathElement._

object Chapter4 {
    val image = Image.circle(100).fillColor(Color.paleGoldenrod).strokeColor(Color.indianRed)

    // archery target
    val target = Image.circle(100).fillColor(Color.black) on
        Image.circle(150).fillColor(Color.white) on
        Image.circle(200).fillColor(Color.yellow)    

    val mount = Image.rectangle(200,50).fillColor(Color.brown) above
        Image.rectangle(250, 50).fillColor(Color.brown)

    val ground = Image.rectangle(300,50).fillColor(Color.green)

    val mountedTarget = target above mount above ground

    // street scene
    val tree = Image.circle(50).fillColor(Color.green) above
        Image.rectangle(10, 20).fillColor(Color.brown)

    val house = Image.triangle(50, 40).fillColor(Color.red) above
        Image.rectangle(40,10).fillColor(Color.yellow) above
        (
            Image.rectangle(15,10).fillColor(Color.yellow) beside
            Image.rectangle(10,10).fillColor(Color.black) beside
            Image.rectangle(15,10).fillColor(Color.yellow)
        )
    val road = Image.rectangle(20,5).fillColor(Color.white) beside
        Image.rectangle(20,5).fillColor(Color.black) beside
        Image.rectangle(20,5).fillColor(Color.white) beside
        Image.rectangle(20,5).fillColor(Color.black)

    val houseUnit = (house beside tree) above road
}

object Chapter6 {
    def boxes(color: Color): Image = {
        val box =
            Image.rectangle(40,40)
                .strokeWidth(5.0)
                .strokeColor(color.spin(30.degrees))
                .fillColor(color)

        box beside box beside box beside box beside box
    }
}

object Chapter7 {
    val aBox = Image.rectangle(50,50).fillColor(Color.blue).strokeColor(Color.blue.spin(30.degrees))
    val aCircle = Image.circle(50).fillColor(Color.red).strokeColor(Color.red.spin(30.degrees))

    def boxes(count: Int) : Image = {
        count match {
            case 0 => Image.empty
            case n => aBox.beside(boxes(n-1))
        }
        
    }

    def alternatingRow(count: Int) : Image = {
        count match {
            case 0 => Image.empty
            case n => {
                (n % 2 == 0) match {
                    case true => aBox.beside(alternatingRow(n-1))
                    case false => aCircle.beside(alternatingRow(n-1))
                }
            }
        }
    }

    def cross(count: Int) : Image = {
        count match {
            case 0 => aBox
            case n => {
                aCircle beside (aCircle above cross(n-1) above aCircle) beside aCircle
            }
        }
    }
}

object Chapter8 {

    def chessboard(count: Int): Image = {
        val block1 = Image.rectangle(10,10).strokeWidth(0).fillColor(Color.red)
        val block2 = Image.rectangle(10,10).strokeWidth(0).fillColor(Color.black)
        val section = (block1 above block2) beside (block2 above block1)

        /* nested declaration to avoid multiple creation of variables defined above */
        def loop(count:Int): Image = {
            count match {
                case 0 => section
                case n =>
                    val unit = loop(n-1)
                    (unit beside unit) above (unit beside unit)
            }
        }

        loop(count)
    }

    def sierpinski(count: Int): Image = {
        val triangle = Image.triangle(10, 8.6).strokeColor(Color.red.spin(30.degrees))
        val unit = triangle above (triangle beside triangle)

        count match {
            case 0 => unit
            case n => 
                val recursive = sierpinski(n-1)
                recursive above (recursive beside recursive)
        }
    }

    def growingBoxes1(count: Int): Image = {
        count match {
            case 0 => Image.empty
            case n => growingBoxes1(n-1).beside(Image.square(n*10))
        }
    }

    def gradientBoxes(count: Int): Image = {
        count match {
            case 0 => Image.empty
            case n => 
                gradientBoxes(n-1)
                    .beside(
                        Image.square(50)
                        .fillColor(Color.royalBlue.spin((count*15).degrees))
                    )
        }
    }

    def concentricCircles(count: Int, size: Int): Image = {
        count match {
            case 0 => Image.empty
            case n => Image.circle(size)
                        .strokeWidth(2)
                        .strokeColor(Color.blue.spin((count * 10).degrees))
                        .on(
                            concentricCircles(n-1, size + 10)
                        )
        }
    }

    def chessboard2(count: Int, color: Color): Image = {
        count match {
            case 0 =>
                val contrast = color.spin(180.degrees)
                val box = Image.square(20)
                box
                    .fillColor(color)
                    .beside(box.fillColor(contrast))
                    .above(box.fillColor(contrast).beside(box.fillColor(color)))
            case n =>
                chessboard2(n-1, color.spin(17.degrees))
                    .beside(chessboard2(n-1, color.spin(-7.degrees)))
                    .above(chessboard2(n-1, color.spin(-19.degrees))
                        .beside(chessboard2(n-1, color.spin(3.degrees))))
        }
    }
}

object Chapter9 {
    val squareInput = (input: Int) => input * input
    val spinColor = (color: Color) => color.spin(15.degrees)
    val copy4Images = (image: Image) =>
        def loop (image:Image, count: Int): Image = {
            count match {
                case 0 => Image.empty
                case n => image.beside(loop(image.rotate(10.degrees), n-1))
            }
        }

        loop(image, 4)

    def sampleCircle(samples: Int): Image = {
        val step = Angle.one / samples
        val dot = Image
                    .triangle(10,10)
                    .fillColor(Color.limeGreen)
                    .strokeColor(Color.lawngreen)
        def loop(count: Int): Image = {
            val angle = step * count
            count match {
                case 0 => Image.empty
                case n =>
                    dot.at(Point.polar(200,angle)).on(loop(n-1))
            }
        }

        loop(samples)
    }

    // call using Chapter9.sampleCurve(50, Image.circle(5), Chapter9.logarithmicSpiralFunction).draw()
    val logarithmicSpiralFunction = (angle: Angle) =>
        Point((Math.exp(angle.toTurns) - 1) * 200, angle)

    def sampleCurve (samples: Int, dot: Image, curveFunction: Angle => Point): Image = {
        val step = Angle.one / samples

        def loop (count: Int): Image = {
            val angle = step * count
            count match {
                case 0 => Image.empty
                case n => dot.at(curveFunction(angle)).on(loop(n-1))
            }
        }

        loop(samples)
    }

    // methods to return curve functions
    def rose(k: Int = 7): Angle => Point =
        (angle: Angle) => Point((angle * k).cos, angle)

    def lissajous(a: Int = 3, b: Int = 2, offset: Angle = 90.degrees): Angle => Point =
        (angle: Angle) => Point(((angle * a) + offset).sin, (angle * b).sin)

    def epicycloid(a: Int = 1, b: Int = 6, c: Int = 14): Angle => Point =
        (angle: Angle) => (Point(75, angle * a).toVec + Point(32, angle * b).toVec + Point(15, angle * c).toVec).toPoint

    // using function composition
    
    val parametricCircle: Angle => Point =
        (angle: Angle) => Point(1.0, angle)

    def scale(factor: Double): Point => Point =
        (pt: Point) => Point(pt.r * factor, pt.angle)

    val circle100 = parametricCircle.andThen(scale(100))

    val parametricSpiral: Angle => Point =
        (angle: Angle) => Point(Math.exp(angle.toTurns - 1), angle)

    val spiral100 = parametricSpiral.andThen(scale(100))

    val growingDot: Point => Image =
        (pt: Point) => Image.circle(pt.angle.toTurns * 20).at(pt)

    val growingCircle = parametricCircle.andThen(scale(100)).andThen(growingDot)

    def sample(samples: Int, curveFunction: Angle => Image): Image =
        val step = Angle.one / samples

        def loop (count: Int): Image = {
            val angle = step * count
            count match {
                case 0 => Image.empty
                case n => curveFunction(angle).on(loop(n-1))
            }
        }

        loop(samples)

    // composition

    // call using conentricShapes(10, outlinedCircle _ )    // convert method to function
    def concentricShapes(count: Int, singleShape: Int => Image): Image =
        count match {
            case 0 => Image.empty
            case n => singleShape(n).on(concentricShapes(n-1, singleShape))
        }

    def outlinedCircle(n: Int): Image =
        Image.circle(n * 10)

    def circleOrSquare(n: Int) =
        if (n % 2 == 0)
            Image.square(n * 20)
        else
            Image.circle(n * 10)

    def shapeAndColor(shape: Int => Image, color: Int => Color): Int => Image =
        (n: Int) => shape(n).strokeWidth(10).strokeColor(color(n))

    def fadingBlue(n: Int): Color =
        Color.blue.fadeOut((1 - n / 20.0).normalized)

    def spinningBlue(n: Int): Color =
        Color.blue.desaturate(0.5.normalized).spin((n * 30).degrees)

    def size(n: Int): Double =
        100 + 24 * n

    def circle(n: Int): Image =
        Image.circle(size(n))

    def square(n: Int): Image =
        Image.square(size(n))

    def triangle(n: Int): Image =
        Image.triangle(size(n), size(n))

    val shapesComposition =
        concentricShapes(10, shapeAndColor(circle, spinningBlue))
            .beside(concentricShapes(10, shapeAndColor(triangle, fadingBlue))
                .beside(concentricShapes(10, shapeAndColor(square, spinningBlue))))
}

import scala.collection.mutable.ListBuffer

object Chapter10 {
    val triangle =
        List(
            lineTo(Point(50, 100)),
            lineTo(Point(100, 0)),
            lineTo(Point(0, 0))
        )

    val curve =
        List(
            curveTo(Point(50, 100),
                Point(100, 100),
                Point(150, 0)
            )
        )
    
    def blueStyle(image: Image): Image =
        image.strokeWidth(6.0)
            .strokeColor(Color.royalBlue)
            .fillColor(Color.skyBlue)

    val openPaths =
        blueStyle(Image.openPath(triangle).beside(Image.openPath(curve)))

    val closedPaths =
            blueStyle(Image.closedPath(triangle).beside(Image.closedPath(curve)))

    val pathsExample = openPaths.above(closedPaths)

    def polygonPoints(points: Int): List[PathElement] =
        var pointsList = new ListBuffer[PathElement]()

        val angle = Angle.turns(1) / points

        pointsList += moveTo(Point(50, 0.0))

        for (vertex <- 1 to (points - 1) ) {
            pointsList += lineTo(Point(50, angle * vertex))
        }

        pointsList.toList

    def polygonCurvePoints(numPoints: Int): List[PathElement] =
            var pointsList = new ListBuffer[PathElement]()

            val angle = Angle.turns(1) / numPoints

            pointsList += moveTo(Point(50, 0.0))

            for (vertex <- 1 to (numPoints - 1) ) {
                pointsList += curveTo(Point(70, angle * vertex), Point(30, angle * vertex), Point(50, angle * vertex))
            }

            pointsList.toList

    var polygonsExercise =
        blueStyle(Image.closedPath(polygonPoints(3))
            .beside(Image.closedPath(polygonPoints(4)))
            .beside(Image.closedPath(polygonPoints(5)))
        )

    var polygonsCurveExercise =
            blueStyle(Image.closedPath(polygonCurvePoints(3))
                .beside(Image.closedPath(polygonCurvePoints(4)))
                .beside(Image.closedPath(polygonCurvePoints(5)))
            )

    // type variable: method works with lists of any type
    def length[A](list: List[A]): Int =
        list match {
            case Nil => 0
            case head :: tail => 1 + length(tail)
        }

    def ones(n: Int): List[Int] = {
        n match {
            case 0 => Nil
            case n => 1 :: ones(n - 1)
        }
    }

    def descending(n: Int): List[Int] =
        n match {
            case 0 => List.empty[Int]
            case n => n :: descending(n - 1)
        }

    def ascending(n: Int): List[Int] = {
        def loop(n: Int, accum: Int): List[Int] =
            n match {
                case 0 => Nil
                case n => accum :: loop(n - 1, accum + 1)
            }
        loop(n, 1)
    }

    def fill[A](n: Int, element: A): List[A] = {
        n match {
            case 0 => Nil
            case n => element :: fill(n - 1, element)
        }
    }
}
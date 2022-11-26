import doodle.core._
import doodle.core.PathElement._
import Point._
import doodle.image._
import doodle.image.syntax._
import doodle.image.syntax.all._
import doodle.image.syntax.core._
import doodle.java2d._
import doodle.algebra.Size
import doodle.interact.syntax._
import doodle.reactor._

import doodle.turtle._
import doodle.turtle.Instruction._

import doodle.random._

val blackFrame = Frame.fitToPicture(border = 20).background(Color.black)

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

    def double(input: List[Int]): List[Int] = {
        input match {
            case Nil => Nil
            case head :: tail => 2 * head :: double(tail)
        }
    }

    def product(input: List[Int]): Int = {
        input match {
            case Nil => 1
            case head :: tail => head * product(tail)
        }
    }

    // whether list contains a particular element
    def contains[A](input: List[A], check: A): Boolean = {
        input match {
            case Nil => false
            case head :: tail => (head == check) || contains(tail, check)
        }
    }

    // first item in list, otherwise if empty then return the element
    def firstOrThis[A](list: List[A], element: A): A = {
        list match {
            case Nil => element
            case head :: tail => head
        }
    }

    def reverse[A](list: List[A]): List[A] = {
        def iterate[A](remaining: List[A], reversed: List[A]): List[A] = {
            remaining match {
                case Nil => reversed
                case element :: tail => iterate(tail, element :: reversed)
            }
        }
        iterate(list, Nil)
    }

    // colored polygons exercise

    def polygon(sides: Int, size: Int, initialRotation: Angle): Image = {
        def iter(n: Int, rotation: Angle): List[PathElement] =
            n match {
                case 0 => Nil
                case n =>
                    LineTo(polar(size, rotation * n + initialRotation)) :: iter(n-1, rotation)
            }
        Image.closedPath(moveTo(polar(size, initialRotation)) :: iter(sides, Angle.turns(1.0) / sides))
    }

    def styleVioletRed(img: Image): Image = {
        img
            .strokeWidth(3.0)
            .strokeColor(Color.mediumVioletRed)
            .fillColor(Color.paleVioletRed.fadeOut(0.5.normalized))
    }

    def makeShape(n: Int, sizeIncrement: Int): Image =
        polygon(sides = n+2, size = n*sizeIncrement, initialRotation = 0.degrees)

    def makeColor(n: Int, spinIncrement: Angle, start: Color): Color =
        start.spin(angle = spinIncrement * n)

    val baseColor = Color.hsl(0.degrees, 0.7, 0.7)

    // run using e.g. makeIncrementalImage(15)
    def makeIncrementalImage(n: Int): Image = {
        n match {
            case 0 => Image.empty
            case n =>
                val shape = makeShape(n, sizeIncrement = 10)
                val color = makeColor(n, spinIncrement = 30.degrees, baseColor)
                makeIncrementalImage(n-1).on(shape.fillColor(color))
        }
    }

    // transforming sequences
    // "to" is inclusive, "until" is non-inclusive

    def incrementUsingMap(list: List[Int]): List[Int] =
        list.map(x => x + 1)

    def onesUsingMap(n: Int): List[Int] = {
        (0 until n by 1).map(x => 1).toList
    }

    def descendingUsingMap(n: Int): List[Int] = {
        n match {
            case 0 => Nil
            case m => (0 until m).toList.map(a => m - a)

        }
        // also works using (n until 0 by -1).toList
    }

    def ascendingUsingMap(n: Int): List[Int] = {
        n match {
            case 0 => Nil
            case m => (1 to n).toList
        }
    }

    def doubleUsingMap(list: List[Int]): List[Int] = {
        list.map(x => x * 2)
    }

    def polygonUsingMap(sides: Int, size: Int, initialRotation: Angle): Image = {
        val rotation = Angle.turns(1) / sides

        Image.closedPath(
            moveTo(polar(size, initialRotation)) 
            :: (1 to sides).toList.map(
                vertex => lineTo(
                    polar(size, rotation * vertex + initialRotation))
            )
        )
    }

    def ascendingOpenInterval(n: Int): List[Int] = {
        // empty range 1 to 0 becomes an empty list
        (1 to n).toList
    }

    def star(p: Int, n: Int, radius: Double): Image = {
        val rotation = Angle.turns(1) / p

        Image.closedPath(
            moveTo(polar(radius, 0.degrees))
            :: (1 to p).toList.map(
                vertex => lineTo(
                    polar(radius, rotation * vertex * n)
                )
            )
        )
    }

    def starSolution(sides: Int, skip: Int, radius: Double): Image = {
        val rotation = Angle.turns(1) * skip / sides

        val start = moveTo(polar(radius, 0.degrees))
        val elements = (1 until sides).toList.map(
                vertex => lineTo(
                    polar(radius, rotation * vertex)
                )
            )

        Image.closedPath(start :: elements) strokeWidth(2)
    }

    // allBeside((1 to 5).toList.map{skip => star(11, skip, 100)})
    def allBeside(images: List[Image]): Image = {
        def iter(toDo: List[Image]): Image = {
            toDo match {
                case Nil => Image.empty
                case head :: tail => head beside iter(tail)
            }
        }
        iter(images)
    }

    def allAbove(images: List[Image]): Image = {
        images match {
            case Nil => Image.empty
            case head :: tail => head above allAbove(tail)
        }
    }

    def pyramidOfStars(rows: Int, startingColor: Color = Color.green, spinAngle: Angle = 20.degrees): Image = {
        allAbove((1 to rows).toList.map{
            row => allBeside((1 to row).toList.map{
                skip => 
                    star(row*2+1, skip, 20)
                        .noStroke
                        .fillColor(startingColor.spin(spinAngle*(row+skip)))
            })
        })
    }
}

object Chapter11 {

    val standardBlackFrame = Frame.fitToPicture().background(Color.black)
    
    def translatePoint(x_translation: Double, y_translation: Double): Point => Point = {
        (point: Point) => Point(point.x + x_translation, point.y + y_translation)
    }

    def rotatePoint(angle: Angle): Point => Point = {
        (point: Point) => point.rotate(angle)
    }

    def roseRotate(k: Int, angle: Angle): Point => Point = {
        (point: Point) => Point((point.angle * k).cos*50 + point.r, point.angle + angle)
    }

    // Chapter11.travellingCircle.run(Frame.size(600,600))
    val travellingCircle =
        Reactor.init(Point(-300, 0))
            .withOnTick(translatePoint(3, 0))
            .withRender(pt => Image.circle(10).at(pt))
            .withStop(pt => pt.x >= 300)
    
    // Chapter11.orbitingCircle.run(Frame.size(600,600))
    val orbitingCircle =
        Reactor.init(Point(0, 300))
            .withOnTick(roseRotate(k = 7, 3.degrees))
            .withRender(pt => Image.circle(10).at(pt))

    val bubble =
        Reactor.linearRamp(0, 200)
            .withRender(r => Image.circle(r))
    
}

object Chapter12 {

    // Turtle.draw(Chapter12.squareUsingTurtle).draw()
    val squareUsingTurtle: List[Instruction] =
        List(
            forward(10), turn(90.degrees),
            forward(10), turn(90.degrees),
            forward(10), turn(90.degrees),
            forward(10)
        )

    def polygonUsingTurtle(sides: Int, sideLength: Double): Image =
        val turnAngle = Angle.turns(1) / sides  // or could use Angle.one / sides

        def iter(n: Int): List[Instruction] =
            n match {
                case 0 => Nil
                case n => List(forward(sideLength), turn(turnAngle)) ::: iter(n-1)
                // or could use ele :: ele :: iter
            }
        Turtle.draw(iter(sides))

    def squareSpiral(iterations: Int, lengthIncrease: Double = 1.02, angleIncrease: Angle = 1.degrees): Image = {
        val initialLength = 5.0
        
        def iter(n: Int): List[Instruction] =
            n match {
                case 0 => Nil
                case n => forward(initialLength + n * lengthIncrease) :: turn(90.degrees + angleIncrease) :: iter(n-1)
            }

        Turtle.draw(iter(iterations))
    }

    // Chapter12.squareSpiralSolution(50,5.0,88.degrees,3.0).draw()
    def squareSpiralSolution(steps: Int, 
        distance: Double = 5.0, 
        angle: Angle = 88.degrees, 
        increment: Double = 1.0): Image = {

        def iter(n: Int, distance: Double): List[Instruction] = {
            n match {
                case 0 => Nil
                case n => forward(distance) :: turn(angle) :: iter(n-1, distance + increment)
            }
        }

        Turtle.draw(iter(steps, distance))
    }

    val branchExample = Turtle.draw(List(
        forward(100),
        branch(turn(45.degrees), forward(100)),
        branch(turn(-45.degrees), forward(100))
    ))

    def doubleUsingFlatMap[A](list: List[A]): List[A] = {
        list.flatMap(ele => List(ele, ele))
    }

    def nothing[A](input: List[A]): List[A] = {
        // List.empty[A]
        input.flatMap(input => List.empty)
    }

    def rewrite(instructions: List[Instruction], 
            rule: Instruction => List[Instruction]
        ): List[Instruction] = {
        instructions.flatMap(
            instruction => instruction match {
                case Branch(instruction) => List(branch(rewrite(instruction, rule):_*))
                case other => rule(other)
            })
    }

    def iterate(steps: Int, 
        seed: List[Instruction], 
        rule: Instruction => List[Instruction]
        ): List[Instruction] = {
    
        steps match {
            case 0 => seed
            case n => iterate(n-1, rewrite(seed, rule), rule)
        }

    }

    // (Turtle.draw(Chapter12.iterate(1, List(forward(10), noop), Chapter12.simpleLSystemBranch)) beside Turtle.draw(Chapter12.iterate(2, List(forward(10), noop), Chapter12.simpleLSystemBranch)) beside Turtle.draw(Chapt r12.iterate(3, List(forward(10), noop), Chapter12.simpleLSystemBranch))).draw()
    def simpleLSystemBranch(instruction: Instruction): List[Instruction] =
        val stepSize = 10
        
        instruction match { 
            case Forward(_) => List(forward(stepSize), forward(stepSize))
            case NoOp =>
                List(branch(turn(45.degrees), forward(stepSize), noop), 
                branch(turn(-45.degrees), forward(stepSize), noop))
            case other => List(other)
        }

    // Turtle.draw(Chapter12.iterate(5, List(forward(300)), Chapter12.kochCurve)).draw()
    def kochCurve(instruction: Instruction): List[Instruction] =

        // use subdivision of any straight lines into the more complex shape

        instruction match {
            case Forward(a) => List(
                forward(a/3), turn(45.degrees), 
                forward(a/3), turn(-90.degrees), 
                forward(a/3), turn(45.degrees), 
                forward(a/3))
            case other => List(other)
        }

    def polygonUsingTurtleAndFlatMap(sides: Int, sideLength: Double): Image =
        val turnAngle = Angle.one / sides

        Turtle.draw(
            (0 until sides).toList.flatMap(i => List(turn(turnAngle), forward(sideLength)))
        )

    def squareSpiralUsingTurtle(steps: Int, distance: Double, 
        angle: Angle, increment: Double): Image =

        Turtle.draw(
        (0 until steps).toList.flatMap(step => List(
            forward(distance+(step * increment)), 
            turn(angle)))
        )

    def randomCircle(radius: Double, color: Random[Color]): Random[Image] = {
        color.map(fill => Image.circle(radius).fillColor(fill))
    }

}

object Chapter13 {
    // suspended random - does not generate the random number until the run method is called
    val randomDouble = Random.double

    // Chapter13.randomAngle.run
    val randomAngle: Random[Angle] =
        Random.double.map(x => x.turns)

    def randomColor(saturation: Double, lightness: Double): Random[Color] =
        randomAngle.map(hue => Color.hsl(hue, saturation, lightness))

    def concentricCircles(count: Int, size: Int, color: Color): Image =
        count match {
            case 0 => Image.empty
            case n =>
                Image.circle(size).fillColor(color).on(concentricCircles(n-1, size+5, color.spin(15.degrees)))
        }

    def randomConcentricCircles(count: Int, size: Int): Random[Image] =
        count match {
            case 0 => Random.always(Image.empty)
            case n =>
                val randomPastel = randomColor(0.7, 0.7)
                Chapter12.randomCircle(size, randomPastel).flatMap( circle =>
                    randomConcentricCircles(n-1, size + 5).map( circles =>
                        circle.on(circles)))
        }

    def randomRectangle(width: Double, height: Double, color: Random[Color]): Random[Image] =
        color.map(fillColor => Image.rectangle(width, height).fillColor(fillColor))

    // Chapter13.rowOfRandomBoxes(20, 20, 5).run.draw()
    def rowOfRandomBoxes(width: Double, height: Double, count: Int): Random[Image] =
        count match {
            case 0 => Random.always(Image.empty)
            case n =>
                val randomPastel = randomColor(0.7, 0.7)
                randomRectangle(width, height, randomPastel).flatMap(rectangle =>
                    rowOfRandomBoxes(width, height, n-1).map(rectangles => rectangle beside rectangles))
        }

    // solution
    def coloredRectangle(color: Color): Image =
        Image.rectangle(20, 20).fillColor(color)

    // Chapter13.randomColorBoxes(5).run.draw()
    def randomColorBoxes(count: Int): Random[Image] =
        count match {
            case 0 => randomColor(0.7, 0.7).map( c => coloredRectangle(c))
            case n =>
                val thisBox = randomColor(0.7, 0.7).map{ c => coloredRectangle(c) }
                val otherBoxes = randomColorBoxes(n-1)
                thisBox.flatMap{ thisBoxI => otherBoxes.map{ otherBoxesI => thisBoxI beside otherBoxesI }}
        }

    // structured randomness
    def coloredRectangle(color: Color, size: Int): Image =
        Image.rectangle(size, size)
            .strokeWidth(5.0).strokeColor(color.spin(30.degrees))
            .fillColor(color)

    // generate random color using normal distribution, with mean and std
    def nextColor(color: Color): Random[Color] =
        val spin = Random.normal(15.0, 10.0)
        spin.map{ s => color.spin(s.degrees) }

    // Chapter13.randomGradientBoxes(10, Color.blue).run.draw()
    def randomGradientBoxes(count: Int, color: Color): Random[Image] =
        count match {
            case 0 => Random.always(Image.empty)
            case n =>
                val box = coloredRectangle(color, 20)
                val boxes = nextColor(color).flatMap{ c => randomGradientBoxes( n-1, c) }
                boxes.map{ b => box beside b }
        }

    // particle systems
    val start = Random.always(Point.zero)

    def step(current: Point): Random[Point] =
        val drift = Point(current.x + 10, current.y)
        val noise =
            Random.normal(0.0, 5.0) flatMap { 
                x => Random.normal(0.0, 5.0) map { 
                    y => Vec(x, y)
                }
            }

        noise.map(vec => drift + vec)

    def render(point: Point): Image =
        Image.circle(5.0).at(point)

    def walk(start: Random[Point], steps: Int, render: Point => Image): Random[Image] =
        def loop(count: Int, current: Point, image: Image): Random[Image] = {
            count match {
                case 0 => Random.always(image on render(current))
                case n =>
                    val next = step(current)
                    next.flatMap { pt => loop(count - 1, pt, image on render(current)) }
            }
        }

        start.flatMap { pt => loop(steps, pt, Image.empty) }

    def particleSystem(
        particles: Int, 
        render: Point => Image, 
        start: Random[Point], 
        walk: (Random[Point], Int, Point => Image) => Random[Image], 
        steps: Int
    ): Random[Image] = {
        def loop(count: Int): Random[Image] = {
            count match {
                case 0 => Random.always(Image.empty)
                case n =>
                    walk(start, steps, render).flatMap(img => loop(count - 1).map(others => img on others))
            }
        }

        loop(particles)
    }

    // For comprehension

    
}
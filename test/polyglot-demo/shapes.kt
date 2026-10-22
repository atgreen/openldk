data class Circle(val r: Double) {
    fun area() = Math.PI * r * r
}

fun circleArea(r: Double): Double = Circle(r).area()

// Compiled against the Scala component's classes.
fun squaresPlusOne(n: Int): Int = Stats.sumOfSquares(n) + 1

// Compiled against a Maven dependency.
fun wrapped(text: String): String = org.apache.commons.text.WordUtils.wrap(text, 10)

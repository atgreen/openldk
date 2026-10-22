// Wonderland Books' catalog and pricing engine.
//
// ASDF compiles this file with kotlinc (a :kotlin-file component, at
// compile-op) against commons-text, a :maven-jar component, and puts the
// classes on the classpath.  showcase.lisp calls the top-level functions
// as static methods of wonderland.CatalogKt.

package wonderland

import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.runBlocking
import org.apache.commons.text.WordUtils
import java.util.function.ToDoubleBiFunction

data class Book(
    val isbn: String,
    val title: String,
    val author: String,
    val category: String,
    val price: Double,
    val stock: Int,
)

data class OrderLine(val isbn: String, val quantity: Int)

private val books = listOf(
    Book("9780262510875", "structure and interpretation of computer programs", "Harold Abelson", "Computing", 45.00, 12),
    Book("9780262560993", "the little schemer", "Daniel P. Friedman", "Computing", 38.00, 4),
    Book("9780262561006", "the seasoned schemer", "Daniel P. Friedman", "Computing", 36.00, 3),
    Book("9780465026562", "goedel, escher, bach", "Douglas Hofstadter", "Science", 22.00, 6),
    Book("9780141439761", "alice's adventures in wonderland", "Lewis Carroll", "Fiction", 7.50, 30),
    Book("9780141195346", "through the looking-glass", "Lewis Carroll", "Fiction", 8.25, 9),
    Book("9780140434903", "the hunting of the snark", "Lewis Carroll", "Fiction", 5.00, 0),
).map { it.copy(title = WordUtils.capitalizeFully(it.title)) }

fun catalog(): List<Book> = books

fun findBook(isbn: String): Book? = books.find { it.isbn == isbn }

/** "ISBN QUANTITY"; String.toInt throws NumberFormatException on a bad quantity. */
fun parseOrderLine(line: String): OrderLine {
    val fields = line.trim().split(Regex("\\s+"))
    require(fields.size == 2) { "expected an ISBN and a quantity: '$line'" }
    return OrderLine(fields[0], fields[1].toInt())
}

/**
 * The price of each line, under the store's pricing policy -- which the
 * caller supplies, here a Lisp object implementing ToDoubleBiFunction --
 * computed concurrently, one coroutine per line.
 */
fun priceLines(lines: List<OrderLine>, policy: ToDoubleBiFunction<Book, Int>): List<Double> =
    runBlocking {
        lines.map { line -> async { policy.applyAsDouble(findBook(line.isbn)!!, line.quantity) } }.awaitAll()
    }

// The bookshop's catalog, in Kotlin: compiled by ASDF (compile-op) against
// commons-text, a Maven dependency, and called from showcase.lisp.

import org.apache.commons.text.WordUtils
import java.util.function.Function

data class Book(val title: String, val author: String, val year: Int, val price: Double)

fun catalog(): List<Book> = listOf(
    Book("alice's adventures in wonderland", "Lewis Carroll", 1865, 7.50),
    Book("through the looking-glass", "Lewis Carroll", 1871, 8.25),
    Book("the hunting of the snark", "Lewis Carroll", 1876, 5.00),
    Book("structure and interpretation of computer programs", "Abelson & Sussman", 1985, 45.00),
    Book("goedel, escher, bach", "Douglas Hofstadter", 1979, 22.00)
).map { it.copy(title = WordUtils.capitalizeFully(it.title)) }

// Takes a java.util.function.Function: the showcase passes a Lisp object.
fun discounted(book: Book, pricing: Function<Any, Any>): Any = pricing.apply(book.price)

// Coroutines: every book priced concurrently.
fun totalWithTax(books: List<Book>, rate: Double): Double = kotlinx.coroutines.runBlocking {
    books.map { book -> kotlinx.coroutines.async { book.price * (1 + rate) } }.sumOf { it.await() }
}

// Runs a Lisp function; a Lisp error in it arrives as a Java exception.
fun guarded(action: () -> Any?): String =
    try {
        "it returned ${action()}"
    } catch (e: ArithmeticException) {
        "Kotlin caught the Lisp error as an ArithmeticException"
    }

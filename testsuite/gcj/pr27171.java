public class pr27171 {

	public static void main(String[] args) throws Throwable {
	  // Isolated low surrogate.
	  char x = 56478;  // 0xdc9e
	  String xs = new String(new char[] { x });
	  // JDK UTF-8 encoding replaces an isolated surrogate with one byte.
	  System.out.println(xs.getBytes("UTF-8").length);

	  // isolated high surrogate -- at end of input stream
	  char y = 0xdaee;
	  String ys = new String(new char[] { y });
	  // The isolated high surrogate uses the same replacement byte.
	  System.out.println(ys.getBytes("UTF-8").length);
	}
}


/* { dg-output "OK" } */

package aaa;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

// ProcessBuilder: read a child's output, get its exit code, write to its
// stdin, merge stderr into stdout, and fail cleanly for a missing program.
public class process
{
    static String readAll(InputStream in) throws IOException
    {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        for (int b; (b = in.read()) != -1; ) out.write(b);
        return out.toString().trim();
    }

    public static void main(String[] args) throws Exception
    {
        Process echo = new ProcessBuilder("echo", "hello", "world").start();
        String echoed = readAll(echo.getInputStream());
        int echoExit = echo.waitFor();

        Process fail = new ProcessBuilder("sh", "-c", "exit 3").start();
        int failExit = fail.waitFor();

        Process cat = new ProcessBuilder("cat").start();
        OutputStream stdin = cat.getOutputStream();
        stdin.write("piped in".getBytes());
        stdin.close();
        String catted = readAll(cat.getInputStream());
        cat.waitFor();

        Process merged = new ProcessBuilder("sh", "-c", "echo to-stderr 1>&2")
            .redirectErrorStream(true).start();
        String mergedOut = readAll(merged.getInputStream());
        merged.waitFor();

        boolean missing = false;
        try {
            new ProcessBuilder("no-such-program-openldk").start();
        } catch (IOException e) {
            missing = true;
        }

        boolean ok = echoed.equals("hello world") && echoExit == 0 && failExit == 3
            && catted.equals("piped in") && mergedOut.equals("to-stderr") && missing;
        System.out.println(ok ? "OK" : "FAIL: [" + echoed + "] " + echoExit + " " + failExit
                           + " [" + catted + "] [" + mergedOut + "] " + missing);
    }
}

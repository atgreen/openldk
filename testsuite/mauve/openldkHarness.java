package mauve;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.ResourceNotFoundException;

public class openldkHarness extends gnu.testlet.TestHarness
{
    private int count;
    private String className;
    private boolean verbose = false;

    private String last_check;

    public openldkHarness(Testlet t, boolean verbose)
    {
        this.verbose = verbose;
        className = t.getClass().getName();
    }

    public void check(boolean result)
    {
        String message = (result ? "PASS" : "FAIL") + ": " + className
            + ((last_check == null) ? "" : (": " + last_check))
            + " (number " + count++ + ")";
        System.out.println(message);
    }

    public Reader getResourceReader(String name) throws ResourceNotFoundException
    {
        return new BufferedReader(new InputStreamReader(getResourceStream(name)));
    }

    public InputStream getResourceStream(String name)
        throws ResourceNotFoundException
    {
        // The following code assumes File.separator is a single character.
        if (File.separator.length() > 1)
            throw new Error("File.separator length is greater than 1");
        String realName = name.replace('#', File.separator.charAt(0));
        try
            {
                return new FileInputStream(getSourceDirectory() + File.separator
                                           + realName);
            }
        catch (FileNotFoundException ex)
            {
                throw new ResourceNotFoundException(ex.getLocalizedMessage() + ": "
                                                    + getSourceDirectory()
                                                    + File.separator + realName);
            }
    }

    public File getResourceFile(String name) throws ResourceNotFoundException
    {
        // The following code assumes File.separator is a single character.
        if (File.separator.length() > 1)
            throw new Error("File.separator length is greater than 1");
        String realName = name.replace('#', File.separator.charAt(0));
        File f = new File(getSourceDirectory() + File.separator + realName);
        if (!f.exists())
            {
                throw new ResourceNotFoundException("cannot find mauve resource file"
                                                    + ": " + getSourceDirectory()
                                                    + File.separator + realName);
            }
        return f;
    }

    public void verbose(String message) {
        System.out.println(message);
    }

    public void checkPoint (String name)
    {
        last_check = name;
        count = 0;
    }

    public void debug (String message)
    {
        debug(message, true);
    }

    public void debug (String message, boolean newline)
    {
        if (newline)
            System.out.println(message);
        else
            System.out.print(message);
    }

    public void debug (Throwable ex)
    {
        ex.printStackTrace(System.out);
    }

    public void debug (Object[] o, String desc)
    {
        debug("Dumping Object Array: " + desc);
        if (o == null)
            {
                debug("null");
                return;
            }

        for (int i = 0; i < o.length; i++) {
            if (o[i] instanceof Object[])
                debug((Object[]) o[i], desc + " element " + i);
            else
                debug("  Element " + i + ": " + o[i]);
        }
    }

    public static void main(String[] args) throws Exception {
        Class<?> clazz = Class.forName(args[0]);
        Testlet t = (Testlet) clazz.newInstance();
        t.test(new openldkHarness(t, false));
    }
}

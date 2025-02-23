import java.io.*;

public class Greek
{
    public static void main(String argv[])
    {
            write("Χρυσή Αυγή ανεπίσημη σελίδα του Λαικού συνδέσμου");
    }

    public static void write(String s)
    {
        StringBuffer sb=new StringBuffer();
        for(int i=0;i<s.length();i++)
                sb.append(" "+(int)s.charAt(i));
        sb.append("\n");
        System.out.println(sb.toString());
    }

}

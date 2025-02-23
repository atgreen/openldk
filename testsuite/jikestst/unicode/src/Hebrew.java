import java.io.*;

public class Hebrew
{
    public static void main(String argv[])
    {
            write("תא ליבות הנוכנ הגהנה .לארשי תנידמ יחרזאבו לארשי תנידמב ןימאמ ינא");
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

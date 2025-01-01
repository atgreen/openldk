// Tags: not-a-test
package gnu.testlet.java.lang.Number;

class NewNumber extends Number
{
	private int intfld; 
	public NewNumber()
	{
		super();
	}
	public NewNumber(int i)
	{
		intfld = i;
	}
	public int intValue()
	{
		return intfld;
	}
	public float floatValue()
	{
		return intfld;
	}
	public double doubleValue()
	{
		return intfld;
	}
	public long longValue()
	{
		return intfld;
	}
	public byte byteValue()
	{
		return super.byteValue();
	}
	public short shortValue()
	{
		return super.shortValue();
	}
}

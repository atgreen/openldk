package gnu.testlet.javax.swing.JList.AccessibleJList;


import javax.accessibility.AccessibleContext;
import javax.swing.JList;

/**
 * Overrides JList in order to expose the AccssibleJList methods.
 */
public class TestList extends JList
{
  public TestList()
  {
    super();
  }
  public TestList(Object[] values)
  {
    super(values);
  }
  public class AccessibleTestList extends AccessibleJList
  {
  }
  public AccessibleContext getAccessibleContext()
  {
    return new AccessibleTestList();
  }

}

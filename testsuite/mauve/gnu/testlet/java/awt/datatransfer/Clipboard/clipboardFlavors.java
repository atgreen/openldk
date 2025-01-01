// Tags: JDK1.5

// Copyright (C) 2005 Mark J. Wielaard <mark@klomp.org>

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.datatransfer.Clipboard;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.datatransfer.*;
import java.io.IOException;

public class clipboardFlavors
  extends Clipboard
  implements Testlet, Transferable, ClipboardOwner, FlavorListener
{

  public clipboardFlavors()
  {
    super("mauve");
  }

  private clipboardFlavors(String name)
  {
    super(name);
  }

  private clipboardFlavors(String[] mimeTypes) throws ClassNotFoundException
  {
    this("mimeTypes.startingwith[" + mimeTypes[0] + "]");
    transferDataFlavors = new DataFlavor[mimeTypes.length];
    for (int i = 0; i < mimeTypes.length; i++)
      transferDataFlavors[i] = new DataFlavor(mimeTypes[i]);
  }

  public void test(TestHarness harness)      
  {
    try
      {
	// Not much in a new empty Clipboard.
	harness.checkPoint("empty");
	DataFlavor[] flavors = this.getAvailableDataFlavors();
	harness.check(flavors != null);
	harness.check(flavors.length, 0);
	harness.check(this.isDataFlavorAvailable(DataFlavor.imageFlavor),
		      false);
	boolean exception_thrown = false;
	try
	  {
	    Object o = this.getData(DataFlavor.imageFlavor);
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	FlavorListener[] listeners = this.getFlavorListeners();
	harness.check(listeners != null);
	harness.check(listeners.length, 0);
	// Add ourselves as listener.
	harness.checkPoint("Add self listener");
	this.addFlavorListener(this);
	listeners = this.getFlavorListeners();
	harness.check(listeners.length, 1);
	harness.check(listeners[0], this);
	// Remove a null listener.
	harness.checkPoint("Remove null listener");
	this.removeFlavorListener(null);
	listeners = this.getFlavorListeners();
	harness.check(listeners.length, 1);
	harness.check(listeners[0], this);
	// Remove something never added.
	harness.checkPoint("Remove non-existing listener");
	this.removeFlavorListener(new clipboardFlavors("dummy-to-remove"));
	listeners = this.getFlavorListeners();
	harness.check(listeners.length, 1);
	harness.check(listeners[0], this);
	// Remove ourselves.
	harness.checkPoint("Remove self");
	this.removeFlavorListener(this);
	listeners = this.getFlavorListeners();
	harness.check(listeners != null);
	harness.check(listeners.length, 0);
	// Remove ourselves again.
	harness.checkPoint("Remove self again");
	this.removeFlavorListener(this);
	listeners = this.getFlavorListeners();
	harness.check(listeners != null);
	harness.check(listeners.length, 0);
	
	// Put in some data and register us as owner
	// and register us as listener.
	harness.checkPoint("put in data");
	this.setContents(new clipboardFlavors(new String[] {"x/z", "q/w"}),
			 this);
	this.addFlavorListener(this);
	flavors = getAvailableDataFlavors();
	harness.check(flavors != null);
	harness.check(flavors.length, 2);
	harness.check(flavors[0].getMimeType().equals("x/z")
		      || flavors[1].getMimeType().equals("x/z"));
	harness.check(flavors[0].getMimeType().equals("q/w")
		      || flavors[1].getMimeType().equals("q/w"));
	harness.check(this.isDataFlavorAvailable(DataFlavor.imageFlavor),
		      false);
	harness.check(this.isDataFlavorAvailable(new DataFlavor("x/z")));
	harness.check(this.isDataFlavorAvailable(new DataFlavor("q/w")));
	exception_thrown = false;
	try
	  {
	    Object o = this.getData(DataFlavor.imageFlavor);
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	harness.check(this.getData(new DataFlavor("x/z")), "x/z");
	harness.check(this.getData(new DataFlavor("q/w")), "q/w");
	
	// Change the contents with some different DataFlavors
	harness.checkPoint("change contents");
	this.setContents(new clipboardFlavors(new String[] {"a/b"}), this);
	harness.check(this.flavorChangedCalled);
	harness.check(this.flavorChangedEvent != null);
	harness.check(this.flavorChangedEvent.getSource(), this);
	
	flavors = getAvailableDataFlavors();
	harness.check(flavors != null);
	harness.check(flavors.length, 1);
	harness.check(this.isDataFlavorAvailable(DataFlavor.imageFlavor),
		      false);
	harness.check(this.isDataFlavorAvailable(new DataFlavor("x/z")),
		      false);
	harness.check(this.isDataFlavorAvailable(new DataFlavor("q/w")),
		      false);
	harness.check(this.isDataFlavorAvailable(new DataFlavor("a/b")));
	exception_thrown = false;
	try
	  {
	    Object o = this.getData(DataFlavor.imageFlavor);
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	exception_thrown = false;
	try
	  {
	    Object o = this.getData(new DataFlavor("x/z"));
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	exception_thrown = false;
	try
	  {
	    Object o = this.getData(new DataFlavor("q/w"));
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	
	harness.check(this.getData(new DataFlavor("a/b")), "a/b");
	
	// Add some other listener and change the flavors again.
	harness.checkPoint("other listener");
	flavorChangedCalled = false;
	flavorChangedEvent = null;
	clipboardFlavors cf2 = new clipboardFlavors("cf2");
	this.addFlavorListener(cf2);
	this.setContents(new clipboardFlavors(new String[] {"a/b", "z/x"}),
			 this);
	harness.check(this.flavorChangedCalled);
	harness.check(this.flavorChangedEvent != null);
	harness.check(this.flavorChangedEvent.getSource(), this);
	harness.check(cf2.flavorChangedCalled);
	harness.check(cf2.flavorChangedEvent != null);
	harness.check(cf2.flavorChangedEvent.getSource(), this);
	
	flavors = getAvailableDataFlavors();
	harness.check(flavors != null);
	harness.check(flavors.length, 2);
	harness.check(flavors[0].getMimeType().equals("a/b")
		      || flavors[1].getMimeType().equals("a/b"));
	harness.check(flavors[0].getMimeType().equals("z/x")
		      || flavors[1].getMimeType().equals("z/x"));
	harness.check(this.isDataFlavorAvailable(DataFlavor.imageFlavor),
		      false);
	harness.check(this.isDataFlavorAvailable(new DataFlavor("x/z")),
		      false);
	harness.check(this.isDataFlavorAvailable(new DataFlavor("q/w")),
		      false);
	harness.check(this.isDataFlavorAvailable(new DataFlavor("a/b")));
	harness.check(this.isDataFlavorAvailable(new DataFlavor("z/x")));
	exception_thrown = false;
	try
	  {
	    Object o = this.getData(DataFlavor.imageFlavor);
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	exception_thrown = false;
	try
	  {
	    Object o = this.getData(new DataFlavor("x/z"));
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	exception_thrown = false;
	try
	  {
	    Object o = this.getData(new DataFlavor("q/w"));
	    harness.debug("got data: " + o);
	  }
	catch(UnsupportedFlavorException ufe)
	  {
	    exception_thrown = true;
	  }
	harness.check(exception_thrown);
	
	harness.check(this.getData(new DataFlavor("a/b")), "a/b");
	harness.check(this.getData(new DataFlavor("z/x")), "z/x");
      }
    catch(IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false, ioe.toString());
      }
    catch(ClassNotFoundException cnfe)
      {
	harness.debug(cnfe);
	harness.check(false, cnfe.toString());
      }
    catch(UnsupportedFlavorException ufe)
      {
	harness.debug(ufe);
	harness.check(false, ufe.toString());
      }
  }
  
  // Transferable
  private DataFlavor[] transferDataFlavors;
  public DataFlavor[] getTransferDataFlavors()
  {
    return transferDataFlavors;
  }

  public boolean isDataFlavorSupported(DataFlavor flavor)
  {
    for (int i = 0; i < transferDataFlavors.length; i++)
      if (flavor.equals(transferDataFlavors[i]))
	return true;

    return false;
  }

  public Object getTransferData(DataFlavor flavor)
    throws UnsupportedFlavorException
  {
    // Cheat
    if (isDataFlavorSupported(flavor))
      return flavor.getMimeType();
    else
      throw new UnsupportedFlavorException(flavor);
  }

  // ClipboardOwner
  private boolean lostOwnerCalled = false;
  private Clipboard lostOwnerClipboard = null;
  private Transferable lostOwnerTransferable = null;
  public void lostOwnership(Clipboard clipboard, Transferable contents)
  {
    lostOwnerCalled = true;
    lostOwnerClipboard = clipboard;
    lostOwnerTransferable = contents;
  }

  // FlavorListener
  private boolean flavorChangedCalled = false;
  private FlavorEvent flavorChangedEvent = null;
  public void flavorsChanged(FlavorEvent event)
  {
    flavorChangedCalled = true;
    flavorChangedEvent = event;
  }

  // For debug readability
  public String toString()
  {
    return "[name=" + getName() + "]";
  }
}

/* DnDTest.java --
   Copyright (C) 2006 Red Hat
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/


package gnu.testlet.java.awt.dnd;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BorderLayout;
import java.awt.Button;
import java.awt.Color;
import java.awt.Frame;
import java.awt.Label;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceContext;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;

public class DnDTest
    implements Testlet
{
  TestHarness harness;
  Robot r;

  boolean unsuccessful;
  boolean dragGestRec;
  boolean dragEnter;
  boolean dragEnterTar;
  boolean dragOver;
  boolean dragOverTar;
  boolean drop;
  boolean finished;
  boolean actionPerformed;
  boolean dragExit;
  boolean dropActionChanged;
  boolean dragExitTar;
  boolean dropActionChangedTar;
  
  public synchronized void test(TestHarness h)
  {
    harness = h;
    r = harness.createRobot ();
    
    new MainClass("");
  }

  class MainClass
      extends Frame
      implements ActionListener, DropTargetListener
  {
    MouseThread mt;
    int start;
    int end;
    DragLabel source = new DragLabel("Drag and drop me to the following Button",
                                     Label.CENTER);
    Button target = new Button();
    
    public MainClass(String title)
    {
      super(title);
      source.setForeground(Color.red);
      add(source, BorderLayout.NORTH);

      target.addActionListener(this);
      add(target, BorderLayout.SOUTH);

      new DropTarget(target, DnDConstants.ACTION_COPY_OR_MOVE, this);
      setSize(205, 100);
      setVisible(true);
      
      r.waitForIdle();
      r.delay (1000);
      
      doDnD();
      
      r.delay(3000);

      harness.check(!unsuccessful);
      harness.check(finished);
      harness.check(dragGestRec);
      harness.check(dragEnter);
      harness.check(dragEnterTar);
      harness.check(dragOver);
      harness.check(dragOverTar);
      harness.check(drop);
      harness.check(!actionPerformed);
      harness.check(!dragExit);
      harness.check(!dropActionChanged);
      harness.check(!dragExitTar);
      harness.check(!dropActionChangedTar);
    }
    
    void doDnD()
    {
      Point sLoc = source.getLocationOnScreen();
      Rectangle sSize = source.getBounds();

      Point tLoc = target.getLocationOnScreen();
      Rectangle tSize = target.getBounds();
      
      // To focus the window
      r.mouseMove(sLoc.x + sSize.width/2, sLoc.y + sSize.height/2);
      r.mousePress(InputEvent.BUTTON1_MASK);
      r.mouseRelease(InputEvent.BUTTON1_MASK);

      // drag and drop
      r.delay(1000);
      r.mousePress(InputEvent.BUTTON1_MASK);
      r.delay(1000);
      r.mouseMove(tLoc.x + tSize.width/2, tLoc.y + tSize.height/2);
      
      start = tLoc.y + tSize.height/2;
      end = start + 5;
        
      mt = new MouseThread();
      mt.start();
      
      r.delay(1000);
      mt.shouldStop=true;
      r.mouseRelease(InputEvent.BUTTON1_MASK);
    }
    
    class MouseThread
        extends Thread
    {
      public boolean shouldStop;

      public void run()
      {
        try
          {
            shouldStop = false;
            Robot robot = new Robot();

            for (;;)
              {
                for (int i = start; i < end; i++)
                  {
                    if (shouldStop)
                      break;
                    
                    robot.mouseMove(150, i);
                    yield();
                  }
                for (int i = end; i > start; i--)
                  {
                    if (shouldStop)
                      break;
                    
                    robot.mouseMove(150, i);
                    yield();
                  }
                if (shouldStop)
                  break;
              }
          }
        catch (Exception e)
          {
            unsuccessful = true;
          }
      }
    }
    
    public void actionPerformed(ActionEvent e)
    {
      Button b = (Button) e.getSource();
      b.setLabel("");
      source.setText("Drag and drop me to the following Button");
      actionPerformed = true;
    }

    public void dragEnter(DropTargetDragEvent e)
    {
      dragEnter = true;
    }

    public void dragExit(DropTargetEvent e)
    {
      dragExit = true;
    }

    public void dragOver(DropTargetDragEvent e)
    {
      dragOver = true;
    }

    public void drop(DropTargetDropEvent e)
    {
      drop = true;
      try
        {
          Transferable t = e.getTransferable();

          if (e.isDataFlavorSupported(DataFlavor.stringFlavor))
            {
              e.acceptDrop(e.getDropAction());

              String s;
              s = (String) t.getTransferData(DataFlavor.stringFlavor);

              target.setLabel(s);

              e.dropComplete(true);
            }
          else
            {
              unsuccessful = true;
              e.rejectDrop();
            }
        }
      catch (java.io.IOException e2)
        {
          unsuccessful = true;
        }
      catch (UnsupportedFlavorException e2)
        {
          unsuccessful = true;
        }
    }

    public void dropActionChanged(DropTargetDragEvent e)
    {
      dropActionChanged = true;
    }
  }

  class DragLabel
      extends Label
      implements DragGestureListener, DragSourceListener
  {
    private DragSource ds = DragSource.getDefaultDragSource();

    public DragLabel(String s, int alignment)
    {
      super(s, alignment);
      int action = DnDConstants.ACTION_COPY_OR_MOVE;
      ds.createDefaultDragGestureRecognizer(this, action, this);
    }

    public void dragGestureRecognized(DragGestureEvent e)
    {
      try
        {
          Transferable t = new StringSelection(getText());
          e.startDrag(DragSource.DefaultCopyNoDrop, t, this);
          dragGestRec = true;
        }
      catch (InvalidDnDOperationException e2)
        {
          unsuccessful = true;
        }
    }

    public void dragDropEnd(DragSourceDropEvent e)
    {
      if (e.getDropSuccess() == false)
        {
          unsuccessful = true;
          return;
        }

      int action = e.getDropAction();
      if ((action & DnDConstants.ACTION_MOVE) != 0)
        setText("");
      finished = true;
    }

    public void dragEnter(DragSourceDragEvent e)
    {
      dragEnterTar = true;
      DragSourceContext ctx = e.getDragSourceContext();

      int action = e.getDropAction();
      if ((action & DnDConstants.ACTION_COPY) != 0)
        ctx.setCursor(DragSource.DefaultCopyDrop);
      else
        ctx.setCursor(DragSource.DefaultCopyNoDrop);
    }

    public void dragExit(DragSourceEvent e)
    {
      dragExitTar = true;
    }

    public void dragOver(DragSourceDragEvent e)
    {
      dragOverTar = true;
    }

    public void dropActionChanged(DragSourceDragEvent e)
    {
      dropActionChangedTar = true;
    }
  }
}

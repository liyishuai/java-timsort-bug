/* This file has been generated by Stubmaker (de.uka.ilkd.stubmaker)
 * Date: Wed Nov 26 11:26:00 CET 2014
 */
package java.nio.channels.spi;

public abstract class AbstractInterruptibleChannel extends java.lang.Object implements java.nio.channels.Channel, java.nio.channels.InterruptibleChannel
{


   /*@ requires true; ensures true; assignable \everything; */
   protected AbstractInterruptibleChannel();

   /*@ requires true; ensures true; assignable \everything; signals (java.io.IOException e) true; */
   public final void close() throws java.io.IOException;

   /*@ requires true; ensures true; assignable \everything; signals (java.io.IOException e) true; */
   protected abstract void implCloseChannel() throws java.io.IOException;

   /*@ requires true; ensures true; assignable \everything; */
   public final boolean isOpen();

   /*@ requires true; ensures true; assignable \everything; */
   protected final void begin();

   /*@ requires true; ensures true; assignable \everything; signals (java.nio.channels.AsynchronousCloseException e) true; */
   protected final void end(boolean arg0) throws java.nio.channels.AsynchronousCloseException;
}
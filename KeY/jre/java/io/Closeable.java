/* This file has been generated by Stubmaker (de.uka.ilkd.stubmaker)
 * Date: Wed Nov 26 11:26:00 CET 2014
 */
package java.io;

public interface Closeable extends java.lang.AutoCloseable
{


   /*@ requires true; ensures true; assignable \everything; signals (java.io.IOException e) true; */
   public void close() throws java.io.IOException;
}
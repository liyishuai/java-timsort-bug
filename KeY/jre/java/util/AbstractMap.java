/* This file has been generated by Stubmaker (de.uka.ilkd.stubmaker)
 * Date: Wed Nov 26 11:26:00 CET 2014
 */
package java.util;

public abstract class AbstractMap extends java.lang.Object implements java.util.Map
{
   java.util.Set keySet;
   java.util.Collection values;


   /*@ requires true; ensures true; assignable \everything; */
   protected AbstractMap();

   /*@ requires true; ensures true; assignable \everything; */
   public int size();

   /*@ requires true; ensures true; assignable \everything; */
   public boolean isEmpty();

   /*@ requires true; ensures true; assignable \everything; */
   public boolean containsValue(java.lang.Object arg0);

   /*@ requires true; ensures true; assignable \everything; */
   public boolean containsKey(java.lang.Object arg0);

   /*@ requires true; ensures true; assignable \everything; */
   public java.lang.Object get(java.lang.Object arg0);

   /*@ requires true; ensures true; assignable \everything; */
   public java.lang.Object put(java.lang.Object arg0, java.lang.Object arg1);

   /*@ requires true; ensures true; assignable \everything; */
   public java.lang.Object remove(java.lang.Object arg0);

   /*@ requires true; ensures true; assignable \everything; */
   public void putAll(java.util.Map arg0);

   /*@ requires true; ensures true; assignable \everything; */
   public void clear();

   /*@ requires true; ensures true; assignable \everything; */
   public java.util.Set keySet();

   /*@ requires true; ensures true; assignable \everything; */
   public java.util.Collection values();

   /*@ requires true; ensures true; assignable \everything; */
   public abstract java.util.Set entrySet();

   /*@ requires true; ensures true; assignable \everything; */
   public boolean equals(java.lang.Object arg0);

   /*@ requires true; ensures true; assignable \everything; */
   public int hashCode();

   /*@ requires true; ensures true; assignable \everything; */
   public java.lang.String toString();

   /*@ requires true; ensures true; assignable \everything; signals (java.lang.CloneNotSupportedException e) true; */
   protected java.lang.Object clone() throws java.lang.CloneNotSupportedException;
}

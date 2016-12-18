/* Time.java -- Wrapper around java.util.Date
   Copyright (C) 1999, 2000, 2003, 2004, 2005  Free Software Foundation, Inc.

   This file is part of GNU Classpath.

   GNU Classpath is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU Classpath is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Classpath; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301 USA.

   Linking this library statically or dynamically with other modules is
   making a combined work based on this library.  Thus, the terms and
   conditions of the GNU General Public License cover the whole
   combination.

   As a special exception, the copyright holders of this library give you
   permission to link this library with independent modules to produce an
   executable, regardless of the license terms of these independent
   modules, and to copy and distribute the resulting executable under
   terms of your choice, provided that you also meet, for each linked
   independent module, the terms and conditions of the license of that
   module.  An independent module is a module which is not derived from
   or based on this library.  If you modify this library, you may extend
   this exception to your version of the library, but you are not
   obligated to do so.  If you do not wish to do so, delete this
   exception statement from your version. */

package clojure.lang;

/**
 * Used for parsing and formatting this date.
 * @deprecated used by deprecated functions
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
*/

/**
 * This class is a wrapper around java.util.Date to allow the JDBC
 * driver to identify the value as a SQL TimeStamp.  Note that this
 * class also adds an additional field for nano-seconds, and so
 * is not completely identical to <code>java.util.Date</code> as
 * the <code>java.sql.Date</code> and <code>java.sql.Time</code>
 * classes are.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class TimeStamp extends java.util.Date
{
    /**
     * @serial
     */
    static final long serialVersionUID = 1752345689675676457L;

    /*
     * nanoseconds
     */
    private int nanos;

    /**
     * Used for parsing and formatting this date.
     * @deprecated used by deprecated functions
    private static SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    private static DecimalFormat decimalFormat = new DecimalFormat("000000000");
    private static StringBuffer sbuf = new StringBuffer(29);
    */

    /**
     * This method initializes a new instance of this class with the
     * specified time value representing the number of milliseconds since
     * Jan 1, 1970 at 12:00 midnight GMT.
     *
     * @param date The time value to intialize this <code>Time</code> to.
     */
    public TimeStamp(long date)
    {
        // first create the TimeStamp
        super(date - (date % 1000));
        nanos = (int)(date % 1000) * 1000000;
    }

    /**
     * This method returns a new instance of this class by parsing a
     * date in JDBC format into a Java date.
     *
     * @param str The string to parse.
     * @return The resulting <code>java.sql.TimeStamp</code> value.
     * @deprecated this is managed by clojure.instant
    public static TimeStamp valueOf(String str)
    {
        int nanos = 0;
        int dot = str.indexOf('.');
        if (dot != -1)
        {
            if (str.lastIndexOf('.') != dot)
                throw new IllegalArgumentException(str);

            int len = str.length() - dot - 1;
            if (len < 1 || len > 9)
                throw new IllegalArgumentException(str);

            nanos = Integer.parseInt(str.substring(dot + 1));
            for (int i = len; i < 9; i++)
              nanos *= 10;

            str = str.substring(0, dot);
        }

        try
        {
            java.util.Date d;
            synchronized (dateFormat)
            {
                d = (java.util.Date) dateFormat.parseObject(str);
            }

            if (d == null)
                throw new IllegalArgumentException(str);

            TimeStamp ts = new TimeStamp(d.getTime() + nanos / 1000000);
            ts.nanos = nanos;
            return ts;
        }
        catch (ParseException e)
        {
            throw new IllegalArgumentException(str);
        }
    }
    */

    /**
     * This method returns this date in JDBC format.
     *
     * @return This date as a string.
     * @deprecated this is managed by clojure.instant
    public String toString()
    {
        synchronized (dateFormat)
        {
            sbuf.setLength(0);
            dateFormat.format(this, sbuf, null);
            sbuf.append('.');
            decimalFormat.format(nanos, sbuf, null);

            int end = sbuf.length() - 1;
            while (end > 20 && sbuf.charAt(end) == '0')
                end--;

            return sbuf.substring(0, end + 1);
        }
    }
    */

    /**
     * Return the value of this TimeStamp as the number of milliseconds
     * since Jan 1, 1970 at 12:00 midnight GMT.
     */
    public long getTime() {
        return super.getTime() + (nanos / 1000000);
    }

    // setTime is done by constructor

    /**
     * This method returns the nanosecond value for this object.
     * @return The nanosecond value for this object.
     */
    public int getNanos()
    {
        return nanos;
    }

    /**
     * This method sets the nanosecond value for this object.
     *
     * @param nanos The nanosecond value for this object. ! it will discard time < 1 sec
     * @return this, to able it in a functional flux.
     */
    public TimeStamp setNanos(int nanos)
    {
        if (nanos > 999999999 || nanos < 0)
            throw new IllegalArgumentException("nanos > 999999999 or < 0");

        // Warning values < 1 sec will be disacarded
        this.nanos = nanos;
        return this;
    }

    /**
     * This method these the specified <code>Object</code> for equality
     * against this object.  This will be true if an only if the specified
     * object is an instance of <code>TimeStamp</code> and has the same
     * time value fields.
     *
     * @param obj The object to test against for equality.
     *
     * @return <code>true</code> if the specified object is equal to this
     * object, <code>false</code> otherwise.
     */
    public boolean equals(Object obj)
    {
        return (obj instanceof TimeStamp) &&
               equals((TimeStamp) obj);
    }

    /**
     * This method tests the specified timestamp for equality against this
     * object.  This will be true if and only if the specified object is
     * not <code>null</code> and contains all the same time value fields
     * as this object.
     *
     * @param ts The <code>TimeStamp</code> to test against for equality.
     *
     * @return <code>true</code> if the specified object is equal to this
     * object, <code>false</code> otherwise.
     */
    public boolean equals(TimeStamp ts)
    {
        return ts != null &&
               ts.getTime() == getTime() &&
               ts.getNanos() == getNanos();
    }

    /**
     * Compares this <code>TimeStamp</code> to another one.
     *
     * @param ts The other TimeStamp.
     * @return <code>0</code>, if both <code>TimeStamp</code>'s represent exactly
     *         the same date, a negative value if this <code>TimeStamp</code> is
     *         before the specified <code>TimeStamp</code> and a positive value
     *         otherwise.
     * @since 1.2
     */
    public int compareTo(TimeStamp ts)
    {
        int s = super.compareTo((java.util.Date) ts);
        if (s != 0)
            return s;

        // If Date components were equal, then we check the nanoseconds.
        return nanos - ts.nanos;
    }

    /**
     * Compares this <code>TimeStamp</code> to another one. This behaves like
     * <code>compareTo(TimeStamp)</code>, but it may throw a
     * <code>ClassCastException</code>, if the specified object is not of type
     * <code>TimeStamp</code>.
     *
     * @param obj The object to compare with.
     * @return <code>0</code>, if both <code>TimeStamp</code>'s represent exactly
     *         the same date, a negative value if this <code>TimeStamp</code> is
     *         before the specified <code>TimeStamp</code> and a positive value
     *         otherwise.
     * @exception ClassCastException if obj is not of type TimeStamp.
     * @see #compareTo(TimeStamp)
     * @since 1.2
     */
    public int compareTo(java.util.Date obj)
    {
        return compareTo((TimeStamp) obj);
    }

}

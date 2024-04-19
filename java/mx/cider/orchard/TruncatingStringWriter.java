package mx.cider.orchard;

import java.io.*;

/**
 * A <code>java.io.StringWriter</code> with limits on both a single write and
 * the resulting string size. Limits are not guaranteed to be exact, because of
 * appending ellipsis the final size can be a few characters longer.
 *
 * This class is not thread-safe.
 */
public class TruncatingStringWriter extends StringWriter {

    private int totalLimit;
    private int singleWriteLimit;

    public TruncatingStringWriter(int singleWriteLimit, int totalLimit) {
        super();
        if (singleWriteLimit < 5) {
            throw new IllegalArgumentException("Bad singleWriteLimit: " + singleWriteLimit);
        }
        if (totalLimit < 10) {
            throw new IllegalArgumentException("Bad totalLimit: " + totalLimit);
        }
        this.singleWriteLimit = singleWriteLimit;
        this.totalLimit = totalLimit;
    }

    private void writeEllipsis() {
        super.write("...");
        totalLimit -= 3;
    }

    public boolean canWrite() {
        if (totalLimit > 0) return true;
        // We assume that if the caller checks this predicate, they intend to
        // write. At limit=0 we tell them not to, but append ellipsis anyway.
        if (totalLimit == 0)
            writeEllipsis();
        return false;
    }

    @Override
    public void write(int c) {
        if (totalLimit > 0) {
            super.write(c);
            totalLimit--;
        } else if (totalLimit == 0) {
            writeEllipsis();
        }
    }

    @Override
    public void write(char[] cbuf, int off, int len) {
        boolean singleTooBig = (len > singleWriteLimit);
        len = Math.min(len, singleWriteLimit);
        if (len <= totalLimit) {
            super.write(cbuf, off, len);
            totalLimit -= len;
            if (singleTooBig)
                writeEllipsis();
        } else if (totalLimit >= 0) {
            super.write(cbuf, off, totalLimit);
            writeEllipsis();
        }
    }

    @Override
    public void write(String str) {
        this.write(str, 0, str.length());
    }

    @Override
    public void write(String str, int off, int len) {
        boolean singleTooBig = (len > singleWriteLimit);
        len = Math.min(len, singleWriteLimit);
        if (len <= totalLimit) {
            super.write(str, off, len);
            totalLimit -= len;
            if (singleTooBig)
                writeEllipsis();
        } else if (totalLimit >= 0) {
            super.write(str, off, totalLimit);
            writeEllipsis();
        }
    }
}

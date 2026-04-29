package mx.cider.orchard;

import java.io.*;

/**
 * A <code>java.io.StringWriter</code> with limits on both a single write and
 * the resulting string size. Limits are not guaranteed to be exact, because of
 * appending ellipsis the final size can be a few characters longer. If the
 * total limit is exceeded, the writer throws a TotalLimitExceeded error.
 *
 * This class is not thread-safe.
 */
public class TruncatingStringWriter extends StringWriter {

    public static class TotalLimitExceeded extends Error {}

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

    @Override
    public void write(int c) {
        if (totalLimit > 0) {
            super.write(c);
            totalLimit--;
        } else if (totalLimit == 0) {
            writeEllipsis();
            throw new TotalLimitExceeded();
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
        } else {
            if (totalLimit >= 0) {
                super.write(cbuf, off, totalLimit);
                totalLimit = 0;
                writeEllipsis();
            }
            throw new TotalLimitExceeded();
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
        } else {
            if (totalLimit >= 0) {
                super.write(str, off, totalLimit);
                totalLimit = 0;
                writeEllipsis();
            }
            throw new TotalLimitExceeded();
        }
    }
}

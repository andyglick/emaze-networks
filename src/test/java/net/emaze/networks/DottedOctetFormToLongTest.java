package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class DottedOctetFormToLongTest {
    
    private final static long LOCALHOST_AS_LONG = 2130706433L;

    @Test(expected = IllegalArgumentException.class)
    public void parsingANullStringYieldsException() {
        new DottedOctetFormToLong().perform(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingAMalformedDottedOctetFormYieldsException() {
        new DottedOctetFormToLong().perform("...");
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingAnInvalidDottedOctetFormYieldsException() {
        new DottedOctetFormToLong().perform("192.168.0.256");
    }

    @Test
    public void parsingAValidDottedOctetFormStringYieldsExpected() {
        final long longForm = new DottedOctetFormToLong().perform("127.0.0.1");
        Assert.assertEquals(LOCALHOST_AS_LONG, longForm);
    }
}

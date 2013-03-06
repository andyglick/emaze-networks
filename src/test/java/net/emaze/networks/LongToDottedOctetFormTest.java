package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class LongToDottedOctetFormTest {

    private final static long LOCALHOST_AS_LONG = 2130706433L;

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentYieldsException() {
        new LongToDottedOctetForm().perform(null);
    }

    @Test
    public void conversionYieldsCorrectRepresentation() {
        final String dottedOctetForm = new LongToDottedOctetForm().perform(LOCALHOST_AS_LONG);
        Assert.assertEquals("127.0.0.1", dottedOctetForm);
    }
}

package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class IntToDottedOctetFormTest {

    private final static int LOCALHOST_AS_LONG = 2130706433;

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentYieldsException() {
        new IntToDottedOctetForm().perform(null);
    }

    @Test
    public void conversionYieldsCorrectRepresentation() {
        final String dottedOctetForm = new IntToDottedOctetForm().perform(LOCALHOST_AS_LONG);
        Assert.assertEquals("127.0.0.1", dottedOctetForm);
    }
}

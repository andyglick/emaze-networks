package net.emaze.networks.old;

import net.emaze.networks.old.DottedOctetFormToLong;
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
        new DottedOctetFormToLong().perform("333.-1.999.256");
    }

    @Test
    public void parsingAValidDottedOctetFormStringYieldsExpected() {
        final long longForm = new DottedOctetFormToLong().perform("127.0.0.1");
        Assert.assertEquals(LOCALHOST_AS_LONG, longForm);
    }

    @Test
    public void parsingAValidDottedOctetFormHexStringYieldsExpected() {
        final long longForm = new DottedOctetFormToLong().perform("0x7F.0x00.0x00.0x01");
        Assert.assertEquals(LOCALHOST_AS_LONG, longForm);
    }

    @Test
    public void parsingAValidDottedOctetFormOctStringYieldsExpected() {
        final long longForm = new DottedOctetFormToLong().perform("0177.0000.0000.0001");
        Assert.assertEquals(LOCALHOST_AS_LONG, longForm);
    }

    @Test
    public void parsingAValidDottedOctetFormMixedStringYieldsExpected() {
        final long longForm = new DottedOctetFormToLong().perform("0177.0x00.0.0X01");
        Assert.assertEquals(LOCALHOST_AS_LONG, longForm);
    }
}

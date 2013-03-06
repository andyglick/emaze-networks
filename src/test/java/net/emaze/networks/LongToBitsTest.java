package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class LongToBitsTest {

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentYieldsException() {
        new LongToBits().perform(null);
    }

    @Test
    public void validNetmaskYieldsExpected() {
        int bits = new LongToBits().perform(0xFFFF0000L);
        Assert.assertEquals(16, bits);
    }

    @Test(expected = IllegalArgumentException.class)
    public void invalidNetmaskYieldsExpected() {
        int bits = new LongToBits().perform(0xFFFF0001L);
    }
}

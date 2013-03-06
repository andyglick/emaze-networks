package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class BitsToLongTest {

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentYieldsException() {
        new BitsToLong().perform(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeOrZeroArgumentYieldsException() {
        new BitsToLong().perform(0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void over32ArgumentYieldsException() {
        new BitsToLong().perform(33);
    }

    @Test
    public void valueBetweenOneAnd32YieldsExpected() {
        final long expected = 0xFFFF0000L;
        final long got = new BitsToLong().perform(16);
        Assert.assertEquals(expected, got);
    }
}

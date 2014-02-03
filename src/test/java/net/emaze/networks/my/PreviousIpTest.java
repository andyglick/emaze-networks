package net.emaze.networks.my;

import org.junit.Assert;
import org.junit.Test;

public class PreviousIpTest {

    private static final Ip ADDRESS = Ip.parse("127.0.0.1");
    private static final Ip BEFORE_ADDRESS = Ip.parse("127.0.0.0");

    @Test
    public void yieldsPreviousIp() {
        Assert.assertEquals(BEFORE_ADDRESS, new PreviousIp().perform(ADDRESS));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new PreviousIp().perform(null);
    }

}

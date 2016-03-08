package net.emaze.networks.ipv4;

import org.junit.Assert;
import org.junit.Test;

public class Ipv4PreviousIpTest {

    private static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");
    private static final Ipv4 BEFORE_ADDRESS = Ipv4.parse("127.0.0.0");

    @Test
    public void yieldsPreviousIpv4() {
        Assert.assertEquals(BEFORE_ADDRESS, new Ipv4PreviousIp().apply(ADDRESS));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new Ipv4PreviousIp().apply(null);
    }
}

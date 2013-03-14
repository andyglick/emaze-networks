package net.emaze.networks;

import junit.framework.Assert;
import org.junit.Test;

public class PreviousIpv4Test {
    
    private static final Ip ADDRESS = Ip.parse("127.0.0.1");
    private static final Ip BEFORE_ADDRESS = Ip.parse("127.0.0.0");
    
    @Test
    public void yieldsPreviousIpv4() {
        Assert.assertEquals(BEFORE_ADDRESS, new PreviousIpv4().perform(ADDRESS));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new PreviousIpv4().perform(null);
    }
}

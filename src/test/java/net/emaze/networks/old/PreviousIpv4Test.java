package net.emaze.networks.old;

import net.emaze.networks.old.PreviousIpv4;
import net.emaze.networks.old.Ipv4;
import junit.framework.Assert;
import org.junit.Test;

public class PreviousIpv4Test {
    
    private static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");
    private static final Ipv4 BEFORE_ADDRESS = Ipv4.parse("127.0.0.0");
    
    @Test
    public void yieldsPreviousIpv4() {
        Assert.assertEquals(BEFORE_ADDRESS, new PreviousIpv4().perform(ADDRESS));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new PreviousIpv4().perform(null);
    }
}

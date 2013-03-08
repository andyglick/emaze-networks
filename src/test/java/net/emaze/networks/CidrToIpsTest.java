package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;

public class CidrToIpsTest {
    
    @Test
    public void canTransformCidrToPairOfIpv4() {
        final Pair<Ipv4, Ipv4> expected = Pair.of(Ipv4.FIRST_IP, Ipv4.LAST_IP);
        final Cidr cidr = Cidr.parse("0.0.0.0/0");
        Assert.assertEquals(expected, new CidrToIps().perform(cidr));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new CidrToIps().perform(null);
    }
}

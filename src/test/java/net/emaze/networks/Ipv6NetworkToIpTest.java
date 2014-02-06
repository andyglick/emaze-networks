package net.emaze.networks;

import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;
import org.junit.Assert;

public class Ipv6NetworkToIpTest {

    @Test
    public void canTransformCidrToPairOfIpv6() {
        final Pair<Ipv6, Ipv6> expected = Pair.of(Ipv6.getFirstIp(), Ipv6.getLastIp());
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("::/0");
        Assert.assertEquals(expected, new Ipv6NetworkToIp().perform(cidr));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new Ipv6NetworkToIp().perform(null);
    }

}

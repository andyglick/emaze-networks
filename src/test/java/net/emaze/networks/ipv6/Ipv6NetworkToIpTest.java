package net.emaze.networks.ipv6;

import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6NetworkToIpTest {

    @Test
    public void canTransformCidrToPairOfIpv6() {
        final Pair<Ipv6, Ipv6> expected = Pair.of(Ipv6.getFirstIp(), Ipv6.getLastIp());
        final Ipv6Network cidr = Ipv6Network.fromCidrNotation("::/0");
        Assert.assertEquals(expected, new Ipv6NetworkToIp().apply(cidr));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new Ipv6NetworkToIp().apply(null);
    }

}

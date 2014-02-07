package net.emaze.networks;

import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4NetworkToIpTest {

    @Test
    public void canTransformCidrToPairOfIpv4() {
        final Pair<Ipv4, Ipv4> expected = Pair.of(Ipv4.getFirstIp(), Ipv4.getLastIp());
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("0.0.0.0/0");
        Assert.assertEquals(expected, new Ipv4NetworkToIp().perform(cidr));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new Ipv4NetworkToIp().perform(null);
    }
}

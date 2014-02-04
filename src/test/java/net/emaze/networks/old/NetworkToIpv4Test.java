package net.emaze.networks.old;

import net.emaze.networks.old.NetworkToIpv4;
import net.emaze.networks.old.Ipv4Network;
import net.emaze.networks.old.Ipv4;
import junit.framework.Assert;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;

public class NetworkToIpv4Test {

    @Test
    public void canTransformCidrToPairOfIpv4() {
        final Pair<Ipv4, Ipv4> expected = Pair.of(Ipv4.FIRST_IP, Ipv4.LAST_IP);
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("0.0.0.0/0");
        Assert.assertEquals(expected, new NetworkToIpv4().perform(cidr));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new NetworkToIpv4().perform(null);
    }
}

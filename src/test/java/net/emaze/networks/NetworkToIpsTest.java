package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;

public class NetworkToIpsTest {

    @Test
    public void canTransformCidrToPairOfIpv4() {
        final Pair<Ip, Ip> expected = Pair.of(Ip.FIRST_IP, Ip.LAST_IP);
        final Network cidr = Network.fromCidrNotation("0.0.0.0/0");
        Assert.assertEquals(expected, new NetworkToIps().perform(cidr));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new NetworkToIps().perform(null);
    }
}

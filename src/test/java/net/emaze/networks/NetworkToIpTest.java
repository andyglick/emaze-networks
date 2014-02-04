package net.emaze.networks;

import net.emaze.networks.Network;
import net.emaze.networks.IpPolicy;
import net.emaze.networks.NetworkToIp;
import net.emaze.networks.Ip;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;
import org.junit.Assert;

public class NetworkToIpTest {
    @Test
    public void canTransformCidrToPairOfIp() {
        final Pair<Ip, Ip> expected = Pair.of(new IpPolicy.V4().getFirstIp(), new IpPolicy.V4().getLastIp());
        final Network cidr = Network.fromCidrNotation("0.0.0.0/0");
        Assert.assertEquals(expected, new NetworkToIp().perform(cidr));
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullArgumentThrows() {
        new NetworkToIp().perform(null);
    }
}
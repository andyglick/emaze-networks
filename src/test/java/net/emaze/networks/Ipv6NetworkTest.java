package net.emaze.networks;

import java.util.Arrays;
import java.util.List;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.ranges.Range;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;
import org.junit.Assert;

public class Ipv6NetworkTest {

    private final Ipv6 ip = Ipv6.parse("2001:0DB8:0000:CD31::");
    private final Ipv6Mask mask = Ipv6Mask.net(64);
    private Ipv6Network network = Ipv6Network.fromCidrNotation(ip, mask);

    @Test
    public void canFetchFirstIpOfANetwork() {
        final Ipv6 got = network.firstIp();
        Assert.assertEquals(ip, got);
    }

    @Test
    public void canFetchLastIpOfANetwork() {
        final Ipv6 got = network.lastIp();
        final Ipv6 expected = Ipv6.parse("2001:0DB8:0000:CD31:FFFF:FFFF:FFFF:FFFF");
        Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canSplitNetwork() {
        final Pair<Ipv6Network, Ipv6Network> got = network.split();
        final Pair<Ipv6Network, Ipv6Network> expected = Pair.of(
                Ipv6Network.fromCidrNotation("2001:0DB8:0000:CD31::", mask.size() + 1), 
                Ipv6Network.fromCidrNotation("2001:0DB8:0000:CD31:8000::", mask.size() + 1));
        Assert.assertEquals(expected, got);
    }
    
    @Test
    public void networksContainsFirstIp() {
        Assert.assertTrue(network.contains(network.firstIp()));
    }
    
    @Test
    public void networksContainsLastIp() {
        Assert.assertTrue(network.contains(network.lastIp()));
    }
}

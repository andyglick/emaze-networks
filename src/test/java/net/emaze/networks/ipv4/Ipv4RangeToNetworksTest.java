package net.emaze.networks.ipv4;

import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4RangeToNetworksTest {

    private static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");
    private static final Ipv4RangeToNetworks instance = new Ipv4RangeToNetworks();

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullFirstIpThrows() {
        instance.apply(null, ADDRESS);
    }

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullLastIpThrows() {
        instance.apply(ADDRESS, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void callingWithFirstIpGreaterThanLastIpThrows() {
        instance.apply(ADDRESS.next(), ADDRESS);
    }

    @Test
    public void sameFirstAndLastIpYieldsSingleCidr() {
        final List<Ipv4Network> got = instance.apply(ADDRESS, ADDRESS);
        final List<Ipv4Network> expected = Arrays.asList(Ipv4Network.byContainedIp(ADDRESS, Ipv4Mask.net(32)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.0");
        final Ipv4 lastIp = Ipv4.parse("192.168.0.255");
        final List<Ipv4Network> got = instance.apply(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(Ipv4Network.fromCidrNotation("192.168.0.0", 24));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.0");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.1");
        final List<Ipv4Network> got = instance.apply(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.0", 24),
                Ipv4Network.fromCidrNotation("192.168.1.0", 31));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.128");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.255");
        final List<Ipv4Network> got = instance.apply(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.128", 25),
                Ipv4Network.fromCidrNotation("192.168.1.0", 24));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.128");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.127");
        final List<Ipv4Network> got = instance.apply(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.128", 25),
                Ipv4Network.fromCidrNotation("192.168.1.0", 25));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToCidr() throws UnknownHostException {
        final Ipv4 firstIp = Ipv4.parse("37.116.130.0");
        final Ipv4 lastIp = Ipv4.parse("37.116.191.255");
        final List<Ipv4Network> got = instance.apply(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.130.0"), Ipv4Mask.net(23)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.132.0"), Ipv4Mask.net(22)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.136.0"), Ipv4Mask.net(21)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.144.0"), Ipv4Mask.net(20)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.160.0"), Ipv4Mask.net(19)));
        Assert.assertEquals(expected, got);
    }
}

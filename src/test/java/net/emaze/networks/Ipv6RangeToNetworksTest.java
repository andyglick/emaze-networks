package net.emaze.networks;

import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6RangeToNetworksTest {

    private static final Ipv6 ADDRESS = Ipv6.parse("1234::");
    private static final Ipv6RangeToNetworks instance = new Ipv6RangeToNetworks();

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullFirstIpThrows() {
        instance.perform(null, ADDRESS);
    }

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullLastIpThrows() {
        instance.perform(ADDRESS, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void callingWithFirstIpGreaterThanLastIpThrows() {
        instance.perform(ADDRESS.next(), ADDRESS);
    }

    @Test
    public void sameFirstAndLastIpYieldsSingleCidr() {
        final List<Ipv6Network> got = instance.perform(ADDRESS, ADDRESS);
        final List<Ipv6Network> expected = Arrays.asList(Ipv6Network.byContainedIp(ADDRESS, Ipv6Mask.net(128)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningCidr() {
        final Ipv6 firstIp = Ipv6.parse("1234::");
        final Ipv6 lastIp = Ipv6.parse("1234::FFFF");
        final List<Ipv6Network> got = instance.perform(firstIp, lastIp);
        final List<Ipv6Network> expected = Arrays.asList(Ipv6Network.fromCidrNotation("1234::", 112));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningCidr() {
        final Ipv6 firstIp = Ipv6.parse("1234::");
        final Ipv6 lastIp = Ipv6.parse("1234::1:1");
        final List<Ipv6Network> got = instance.perform(firstIp, lastIp);
        final List<Ipv6Network> expected = Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::", 112),
                Ipv6Network.fromCidrNotation("1234::1:0000", 127));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningCidr() {
        final Ipv6 firstIp = Ipv6.parse("1234::8000");
        final Ipv6 lastIp = Ipv6.parse("1234::1:FFFF");
        final List<Ipv6Network> got = instance.perform(firstIp, lastIp);
        final List<Ipv6Network> expected = Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::8000", 113),
                Ipv6Network.fromCidrNotation("1234::1:0000", 112));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningCidr() {
        final Ipv6 firstIp = Ipv6.parse("1234::0080");
        final Ipv6 lastIp = Ipv6.parse("1234::017F");
        final List<Ipv6Network> got = instance.perform(firstIp, lastIp);
        final List<Ipv6Network> expected = Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::0080", 121),
                Ipv6Network.fromCidrNotation("1234::0100", 121));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToCidr() throws UnknownHostException {
        final Ipv6 firstIp = Ipv6.parse("1234::0080");
        final Ipv6 lastIp = Ipv6.parse("1234::047F");

        final List<Ipv6Network> got = instance.perform(firstIp, lastIp);
        final List<Ipv6Network> expected = Arrays.asList(
                Ipv6Network.fromCidrNotation("1234::0080", 121),
                Ipv6Network.fromCidrNotation("1234::0100", 120),
                Ipv6Network.fromCidrNotation("1234::0200", 119),
                Ipv6Network.fromCidrNotation("1234::0400", 121));
        Assert.assertEquals(expected, got);
    }

}

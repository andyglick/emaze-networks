package net.emaze.networks.ipv4;

import org.junit.Assert;
import org.junit.Test;

public class Ipv4RangeToSpanningNetworkTest {

    public static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");

    @Test(expected = IllegalArgumentException.class)
    public void nullFirstIpYieldsException() {
        new Ipv4RangeToSpanningNetwork().apply(null, ADDRESS);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullLastIpYieldsException() {
        new Ipv4RangeToSpanningNetwork().apply(ADDRESS, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void firstAddressGreaterThanLastAddressThrows() {
        new Ipv4RangeToSpanningNetwork().apply(ADDRESS.next(), ADDRESS);
    }

    @Test
    public void spanningWhenFirstAndLastCoincidesYieldsCidr() {
        final Ipv4Network expected = Ipv4Network.byContainedIp(ADDRESS, Ipv4Mask.net(32));
        final Ipv4Network spanning = new Ipv4RangeToSpanningNetwork().apply(ADDRESS, ADDRESS);
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAValidRangeYieldsExpected() {
        final Ipv4Network expected = Ipv4Network.byContainedIp(Ipv4.parse("37.116.130.0"), Ipv4Mask.net(18));
        final Ipv4Network spanning = new Ipv4RangeToSpanningNetwork().apply(Ipv4.parse("37.116.130.0"), Ipv4.parse("37.116.191.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundLastIpCornerCase() {
        final Ipv4Network expected = Ipv4Network.fromCidrNotation(Ipv4.parse("255.255.255.254"), Ipv4Mask.net(31));
        final Ipv4Network spanning = new Ipv4RangeToSpanningNetwork().apply(Ipv4.parse("255.255.255.254"), Ipv4.parse("255.255.255.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundFirstIpCornerCase() {
        final Ipv4Network expected = Ipv4Network.fromCidrNotation(Ipv4.parse("0.0.0.0"), Ipv4Mask.net(31));
        final Ipv4Network spanning = new Ipv4RangeToSpanningNetwork().apply(Ipv4.parse("0.0.0.0"), Ipv4.parse("0.0.0.1"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningWholeAddressSpaceCornerCase() {
        final Ipv4Network expected = Ipv4Network.fromCidrNotation(Ipv4.parse("0.0.0.0"), Ipv4Mask.net(0));
        final Ipv4Network spanning = new Ipv4RangeToSpanningNetwork().apply(Ipv4.getFirstIp(), Ipv4.getLastIp());
        Assert.assertEquals(expected, spanning);
    }
}

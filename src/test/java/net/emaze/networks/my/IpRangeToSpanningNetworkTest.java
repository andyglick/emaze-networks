package net.emaze.networks.my;

import org.junit.Test;
import org.junit.Assert;

public class IpRangeToSpanningNetworkTest {

    public static final Ip ADDRESS = Ip.parse("127.0.0.1");

    @Test(expected = IllegalArgumentException.class)
    public void nullFirstIpYieldsException() {
        new IpRangeToSpanningNetwork().perform(null, ADDRESS);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullLastIpYieldsException() {
        new IpRangeToSpanningNetwork().perform(ADDRESS, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void firstAddressGreaterThanLastAddressThrows() {
        new IpRangeToSpanningNetwork().perform(ADDRESS.next(), ADDRESS);
    }

    @Test
    public void spanningWhenFirstAndLastCoincidesYieldsCidr() {
        final Network expected = Network.byContainedIp(ADDRESS, Mask.netV4(32));
        final Network spanning = new IpRangeToSpanningNetwork().perform(ADDRESS, ADDRESS);
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAValidRangeYieldsExpected() {
        final Network expected = Network.byContainedIp(Ip.parse("37.116.130.0"), Mask.netV4(18));
        final Network spanning = new IpRangeToSpanningNetwork().perform(Ip.parse("37.116.130.0"), Ip.parse("37.116.191.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundLastIpCornerCase() {
        final Network expected = Network.fromCidrNotation(Ip.parse("255.255.255.254"), Mask.netV4(31));
        final Network spanning = new IpRangeToSpanningNetwork().perform(Ip.parse("255.255.255.254"), Ip.parse("255.255.255.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundFirstIpCornerCase() {
        final Network expected = Network.fromCidrNotation(Ip.parse("0.0.0.0"), Mask.netV4(31));
        final Network spanning = new IpRangeToSpanningNetwork().perform(Ip.parse("0.0.0.0"), Ip.parse("0.0.0.1"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningWholeAddressSpaceCornerCase() {
        final Network expected = Network.fromCidrNotation(Ip.parse("0.0.0.0"), Mask.netV4(0));
        final Network spanning = new IpRangeToSpanningNetwork().perform(new IpPolicy.V4().getFirstIp(), new IpPolicy.V4().getLastIp());
        Assert.assertEquals(expected, spanning);
    }

}

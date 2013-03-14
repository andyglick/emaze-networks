package net.emaze.networks;

import org.junit.Assert;
import org.junit.Test;

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
        final Network expected = Network.byContainedIp(ADDRESS, Mask.net(32));
        final Network spanning = new IpRangeToSpanningNetwork().perform(ADDRESS, ADDRESS);
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAValidRangeYieldsExpected() {
        final Network expected = Network.byContainedIp(Ip.parse("37.116.130.0"), Mask.net(18));
        final Network spanning = new IpRangeToSpanningNetwork().perform(Ip.parse("37.116.130.0"), Ip.parse("37.116.191.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundLastIpCornerCase() {
        final Network expected = Network.fromCidrNotation(Ip.parse("255.255.255.254"), Mask.net(31));
        final Network spanning = new IpRangeToSpanningNetwork().perform(Ip.parse("255.255.255.254"), Ip.parse("255.255.255.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundFirstIpCornerCase() {
        final Network expected = Network.fromCidrNotation(Ip.parse("0.0.0.0"), Mask.net(31));
        final Network spanning = new IpRangeToSpanningNetwork().perform(Ip.parse("0.0.0.0"), Ip.parse("0.0.0.1"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningWholeAddressSpaceCornerCase() {
        final Network expected = Network.fromCidrNotation(Ip.parse("0.0.0.0"), Mask.net(0));
        final Network spanning = new IpRangeToSpanningNetwork().perform(Ip.FIRST_IP, Ip.LAST_IP);
        Assert.assertEquals(expected, spanning);
    }
}

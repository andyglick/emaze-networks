package net.emaze.networks;

import org.junit.Assert;
import org.junit.Test;

public class IpRangeToSpanningCidrTest {

    public static final Ip ADDRESS = Ip.parse("127.0.0.1");

    @Test(expected = IllegalArgumentException.class)
    public void nullFirstIpYieldsException() {
        new IpRangeToSpanningCidr().perform(null, ADDRESS);
    }

    @Test(expected = IllegalArgumentException.class)
    public void nullLastIpYieldsException() {
        new IpRangeToSpanningCidr().perform(ADDRESS, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void firstAddressGreaterThanLastAddressThrows() {
        new IpRangeToSpanningCidr().perform(ADDRESS.next(), ADDRESS);
    }

    @Test
    public void spanningWhenFirstAndLastCoincidesYieldsCidr() {
        final Cidr expected = Cidr.byContainedIp(ADDRESS, Mask.net(32));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(ADDRESS, ADDRESS);
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAValidRangeYieldsExpected() {
        final Cidr expected = Cidr.byContainedIp(Ip.parse("37.116.130.0"), Mask.net(18));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ip.parse("37.116.130.0"), Ip.parse("37.116.191.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundLastIpCornerCase() {
        final Cidr expected = new Cidr(Ip.parse("255.255.255.254"), Mask.net(31));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ip.parse("255.255.255.254"), Ip.parse("255.255.255.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundFirstIpCornerCase() {
        final Cidr expected = new Cidr(Ip.parse("0.0.0.0"), Mask.net(31));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ip.parse("0.0.0.0"), Ip.parse("0.0.0.1"));
        Assert.assertEquals(expected, spanning);
    }
    @Test
    public void spanningWholeAddressSpaceCornerCase() {
        final Cidr expected = new Cidr(Ip.parse("0.0.0.0"), Mask.net(0));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ip.FIRST_IP, Ip.LAST_IP);
        Assert.assertEquals(expected, spanning);
    }
}

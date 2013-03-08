package net.emaze.networks;

import org.junit.Assert;
import org.junit.Test;

public class IpRangeToSpanningCidrTest {

    public static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");

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
        new IpRangeToSpanningCidr().perform(ADDRESS.offset(1), ADDRESS);
    }

    @Test
    public void spanningWhenFirstAndLastCoincidesYieldsCidr() {
        final Cidr expected = new Cidr(ADDRESS, Netmask.fromBits(32));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(ADDRESS, ADDRESS);
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAValidRangeYieldsExpected() {
        final Cidr expected = new Cidr(Ipv4.parse("37.116.130.0"), Netmask.fromBits(18));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ipv4.parse("37.116.130.0"), Ipv4.parse("37.116.191.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundLastIpCornerCase() {
        final Cidr expected = new Cidr(Ipv4.parse("255.255.255.254"), Netmask.fromBits(31));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ipv4.parse("255.255.255.254"), Ipv4.parse("255.255.255.255"));
        Assert.assertEquals(expected, spanning);
    }

    @Test
    public void spanningAroundFirstIpCornerCase() {
        final Cidr expected = new Cidr(Ipv4.parse("0.0.0.0"), Netmask.fromBits(31));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ipv4.parse("0.0.0.0"), Ipv4.parse("0.0.0.1"));
        Assert.assertEquals(expected, spanning);
    }
    @Test
    public void spanningWholeAddressSpaceCornerCase() {
        final Cidr expected = new Cidr(Ipv4.parse("0.0.0.0"), Netmask.fromBits(0));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(Ipv4.FIRST_IP, Ipv4.LAST_IP);
        Assert.assertEquals(expected, spanning);
    }
}

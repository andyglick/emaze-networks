package net.emaze.networks;

import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import junit.framework.Assert;
import org.junit.Test;

public class IpRangeToCidrsTest {
    
    public static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullFirstIpThrows() {
        new IpRangeToCidrs().perform(null, ADDRESS);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullLastIpThrows() {
        new IpRangeToCidrs().perform(ADDRESS, null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void callingWithFirstIpGreaterThanLastIpThrows() {
        new IpRangeToCidrs().perform(ADDRESS.offset(1), ADDRESS);
    }
    
    @Test
    public void sameFirstAndLastIpYieldsSingleCidr() {
        final List<Cidr> got = new IpRangeToCidrs().perform(ADDRESS, ADDRESS);
        final List<Cidr> expected = Arrays.asList(new Cidr(ADDRESS, Netmask.fromBits(32)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.0");
        final Ipv4 lastIp = Ipv4.parse("192.168.0.255");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(Cidr.parse("192.168.0.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.0");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.1");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.0", 24),
                Cidr.parse("192.168.1.0", 31));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.128");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.255");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.128", 25),
                Cidr.parse("192.168.1.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.128");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.127");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.128", 25),
                Cidr.parse("192.168.1.0", 25));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToCidr() throws UnknownHostException {
        final Ipv4 firstIp = Ipv4.parse("37.116.130.0");
        final Ipv4 lastIp = Ipv4.parse("37.116.191.255");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                new Cidr(Ipv4.parse("37.116.130.0"), Netmask.fromBits(23)),
                new Cidr(Ipv4.parse("37.116.132.0"), Netmask.fromBits(22)),
                new Cidr(Ipv4.parse("37.116.136.0"), Netmask.fromBits(21)),
                new Cidr(Ipv4.parse("37.116.144.0"), Netmask.fromBits(20)),
                new Cidr(Ipv4.parse("37.116.160.0"), Netmask.fromBits(19)));
        org.junit.Assert.assertEquals(expected, got);
    }
}

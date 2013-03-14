package net.emaze.networks;

import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import junit.framework.Assert;
import org.junit.Test;

public class IpRangeToCidrsTest {
    
    public static final Ip ADDRESS = Ip.parse("127.0.0.1");

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
        new IpRangeToCidrs().perform(ADDRESS.next(), ADDRESS);
    }
    
    @Test
    public void sameFirstAndLastIpYieldsSingleCidr() {
        final List<Cidr> got = new IpRangeToCidrs().perform(ADDRESS, ADDRESS);
        final List<Cidr> expected = Arrays.asList(Cidr.byContainedIp(ADDRESS, Mask.net(32)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.0");
        final Ip lastIp = Ip.parse("192.168.0.255");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(Cidr.parse("192.168.0.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.0");
        final Ip lastIp = Ip.parse("192.168.1.1");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.0", 24),
                Cidr.parse("192.168.1.0", 31));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.128");
        final Ip lastIp = Ip.parse("192.168.1.255");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.128", 25),
                Cidr.parse("192.168.1.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.128");
        final Ip lastIp = Ip.parse("192.168.1.127");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.128", 25),
                Cidr.parse("192.168.1.0", 25));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToCidr() throws UnknownHostException {
        final Ip firstIp = Ip.parse("37.116.130.0");
        final Ip lastIp = Ip.parse("37.116.191.255");
        final List<Cidr> got = new IpRangeToCidrs().perform(firstIp, lastIp);
        final List<Cidr> expected = Arrays.asList(
                new Cidr(Ip.parse("37.116.130.0"), Mask.net(23)),
                new Cidr(Ip.parse("37.116.132.0"), Mask.net(22)),
                new Cidr(Ip.parse("37.116.136.0"), Mask.net(21)),
                new Cidr(Ip.parse("37.116.144.0"), Mask.net(20)),
                new Cidr(Ip.parse("37.116.160.0"), Mask.net(19)));
        org.junit.Assert.assertEquals(expected, got);
    }
}

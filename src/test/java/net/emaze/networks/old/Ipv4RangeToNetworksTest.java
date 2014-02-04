package net.emaze.networks.old;

import net.emaze.networks.old.Ipv4RangeToNetworks;
import net.emaze.networks.old.Ipv4Mask;
import net.emaze.networks.old.Ipv4Network;
import net.emaze.networks.old.Ipv4;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import junit.framework.Assert;
import org.junit.Test;

public class Ipv4RangeToNetworksTest {
    
    public static final Ipv4 ADDRESS = Ipv4.parse("127.0.0.1");

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullFirstIpThrows() {
        new Ipv4RangeToNetworks().perform(null, ADDRESS);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullLastIpThrows() {
        new Ipv4RangeToNetworks().perform(ADDRESS, null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void callingWithFirstIpGreaterThanLastIpThrows() {
        new Ipv4RangeToNetworks().perform(ADDRESS.next(), ADDRESS);
    }
    
    @Test
    public void sameFirstAndLastIpYieldsSingleCidr() {
        final List<Ipv4Network> got = new Ipv4RangeToNetworks().perform(ADDRESS, ADDRESS);
        final List<Ipv4Network> expected = Arrays.asList(Ipv4Network.byContainedIp(ADDRESS, Ipv4Mask.net(32)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.0");
        final Ipv4 lastIp = Ipv4.parse("192.168.0.255");
        final List<Ipv4Network> got = new Ipv4RangeToNetworks().perform(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(Ipv4Network.fromCidrNotation("192.168.0.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.0");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.1");
        final List<Ipv4Network> got = new Ipv4RangeToNetworks().perform(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.0", 24),
                Ipv4Network.fromCidrNotation("192.168.1.0", 31));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.128");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.255");
        final List<Ipv4Network> got = new Ipv4RangeToNetworks().perform(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.128", 25),
                Ipv4Network.fromCidrNotation("192.168.1.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningCidr() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.128");
        final Ipv4 lastIp = Ipv4.parse("192.168.1.127");
        final List<Ipv4Network> got = new Ipv4RangeToNetworks().perform(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation("192.168.0.128", 25),
                Ipv4Network.fromCidrNotation("192.168.1.0", 25));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToCidr() throws UnknownHostException {
        final Ipv4 firstIp = Ipv4.parse("37.116.130.0");
        final Ipv4 lastIp = Ipv4.parse("37.116.191.255");
        final List<Ipv4Network> got = new Ipv4RangeToNetworks().perform(firstIp, lastIp);
        final List<Ipv4Network> expected = Arrays.asList(
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.130.0"), Ipv4Mask.net(23)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.132.0"), Ipv4Mask.net(22)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.136.0"), Ipv4Mask.net(21)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.144.0"), Ipv4Mask.net(20)),
                Ipv4Network.fromCidrNotation(Ipv4.parse("37.116.160.0"), Ipv4Mask.net(19)));
        org.junit.Assert.assertEquals(expected, got);
    }
}

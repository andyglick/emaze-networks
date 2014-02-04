package net.emaze.networks;

import net.emaze.networks.Network;
import net.emaze.networks.Mask;
import net.emaze.networks.Ip;
import net.emaze.networks.IpRangeToNetworks;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

public class IpRangeToNetworksTest {

    public static final Ip IPV4 = Ip.parse("127.0.0.1");
    public static final Ip IPV6 = Ip.parse("2001:0DB8:0000:CD31::");

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullFirstIpThrows() {
        new IpRangeToNetworks().perform(null, IPV4);
    }

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullLastIpThrows() {
        new IpRangeToNetworks().perform(IPV4, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void callingWithFirstIpGreaterThanLastIpThrows() {
        new IpRangeToNetworks().perform(IPV4.next(), IPV4);
    }

    @Test
    public void sameFirstAndLastIpYieldsSingleCidr() {
        final List<Network> got = new IpRangeToNetworks().perform(IPV4, IPV4);
        final List<Network> expected = Arrays.asList(Network.byContainedIp(IPV4, Mask.hostV4(0)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.0");
        final Ip lastIp = Ip.parse("192.168.0.255");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(Network.fromCidrNotation("192.168.0.0", 24));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.0");
        final Ip lastIp = Ip.parse("192.168.1.1");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation("192.168.0.0", 24),
                Network.fromCidrNotation("192.168.1.0", 31));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.128");
        final Ip lastIp = Ip.parse("192.168.1.255");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation("192.168.0.128", 25),
                Network.fromCidrNotation("192.168.1.0", 24));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningCidr() {
        final Ip firstIp = Ip.parse("192.168.0.128");
        final Ip lastIp = Ip.parse("192.168.1.127");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation("192.168.0.128", 25),
                Network.fromCidrNotation("192.168.1.0", 25));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToCidr() throws UnknownHostException {
        final Ip firstIp = Ip.parse("37.116.130.0");
        final Ip lastIp = Ip.parse("37.116.191.255");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation(Ip.parse("37.116.130.0"), Mask.netV4(23)),
                Network.fromCidrNotation(Ip.parse("37.116.132.0"), Mask.netV4(22)),
                Network.fromCidrNotation(Ip.parse("37.116.136.0"), Mask.netV4(21)),
                Network.fromCidrNotation(Ip.parse("37.116.144.0"), Mask.netV4(20)),
                Network.fromCidrNotation(Ip.parse("37.116.160.0"), Mask.netV4(19)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void sameFirstAndLastIpYieldsSingleIPv6Cidr() {
        final List<Network> got = new IpRangeToNetworks().perform(IPV6, IPV6);
        final List<Network> expected = Arrays.asList(Network.byContainedIp(IPV6, Mask.hostV6(0)));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningIPv6Cidr() {
        final Ip firstIp = Ip.parse("2001:0DB8:0000:CD31::");
        final Ip lastIp = Ip.parse("2001:0DB8:0000:CD31::FFFF");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(Network.fromCidrNotation("2001:0DB8:0000:CD31::", 112));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningIPv6Cidr() {
        final Ip firstIp = Ip.parse("2001:0DB8:0000:CD31::");
        final Ip lastIp = Ip.parse("2001:0DB8:0000:CD31::1:1");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation("2001:0DB8:0000:CD31::", 112),
                Network.fromCidrNotation("2001:0DB8:0000:CD31::1:0", 127));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningIPv6Cidr() {
        final Ip firstIp = Ip.parse("2001:0DB8:0000:CD31::8000");
        final Ip lastIp = Ip.parse("2001:0DB8:0000:CD31::1:FFFF");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation("2001:0DB8:0000:CD31::8000", 113),
                Network.fromCidrNotation("2001:0DB8:0000:CD31::1:0", 112));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningIPv6Cidr() {
        final Ip firstIp = Ip.parse("2001:0DB8:0000:CD31::8000");
        final Ip lastIp = Ip.parse("2001:0DB8:0000:CD31::1:7FFF");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation("2001:0DB8:0000:CD31::8000", 113),
                Network.fromCidrNotation("2001:0DB8:0000:CD31::1:0", 113));
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToIPv6Cidr() throws UnknownHostException {
        final Ip firstIp = Ip.parse("2001:0DB8:0000:CD31::FFE5:0");
        final Ip lastIp = Ip.parse("2001:0DB8:0000:CD31::1:3:FFFF");
        final List<Network> got = new IpRangeToNetworks().perform(firstIp, lastIp);
        final List<Network> expected = Arrays.asList(
                Network.fromCidrNotation(Ip.parse("2001:0DB8:0000:CD31::FFE5:0"), Mask.netV6(112)),
                Network.fromCidrNotation(Ip.parse("2001:0DB8:0000:CD31::FFE6:0"), Mask.netV6(111)),
                Network.fromCidrNotation(Ip.parse("2001:0DB8:0000:CD31::FFE8:0"), Mask.netV6(109)),
                Network.fromCidrNotation(Ip.parse("2001:0DB8:0000:CD31::FFF0:0"), Mask.netV6(108)),
                Network.fromCidrNotation(Ip.parse("2001:0DB8:0000:CD31::1:0:0"), Mask.netV6(110)));
        Assert.assertEquals(expected, got);
    }
}

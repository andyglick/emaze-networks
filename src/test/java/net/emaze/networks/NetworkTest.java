package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;

public class NetworkTest {

    @Test
    public void parseYieldsExpectedCidr() {
        final Network expected = Network.fromCidrNotation(Ip.parse("10.0.0.0"), Mask.net(8));
        final Network got = Network.fromCidrNotation("10.0.0.0/8");
        Assert.assertEquals(expected, got);
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingNullYieldsException() {
        Network.fromCidrNotation(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingMalformedCidrYieldsException() {
        Network.fromCidrNotation("");
    }

    @Test
    public void containsYieldsTrueForNetworkAddress() {
        final Network cidr = Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ip.parse("10.0.0.0")));
    }

    @Test
    public void containsYieldsTrueForLastIpOfNetwork() {
        final Network cidr = Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ip.parse("10.255.255.255")));
    }

    @Test
    public void containsYieldsTrueForIpInsideCidr() {
        final Network cidr = Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ip.parse("10.255.255.254")));
    }

    @Test
    public void containsYieldsFalseForIpOutsideCidr() {
        final Network cidr = Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertFalse(cidr.contains(Ip.parse("172.16.0.1")));
    }

    @Test
    public void containsYieldsTrueForIncludedNetwork() {
        final Network container = Network.fromCidrNotation("10.0.0.0/8");
        final Network contained = Network.fromCidrNotation("10.128.0.0/10");
        Assert.assertTrue(container.contains(contained));
    }

    @Test
    public void containsYieldsFalseForSeparateNetwork() {
        final Network container = Network.fromCidrNotation("10.0.0.0/8");
        final Network contained = Network.fromCidrNotation("192.168.0.0/16");
        Assert.assertFalse(container.contains(contained));
    }

    @Test
    public void splitYieldsTwoHalvesOfCidr() {
        final Network source = Network.fromCidrNotation("192.168.0.0", 24);
        final Pair<Network, Network> expected = Pair.of(Network.fromCidrNotation("192.168.0.0", 25), Network.fromCidrNotation("192.168.0.128", 25));
        final Pair<Network, Network> split = source.split();
        Assert.assertEquals(expected, split);
    }

    @Test(expected = IllegalArgumentException.class)
    public void cannotSplitASingleIpCidr() {
        Network.fromCidrNotation("192.168.0.0", 32).split();
    }

    @Test
    public void cidrIsBuiltFromNetworkPartOfIpAddress() {
        final Network out = Network.byContainedIp(Ip.parse("255.255.255.255"), Mask.net(24));
        Assert.assertEquals(Ip.parse("255.255.255.0"), out.firstIp());
    }

    @Test
    public void cidrFromSameIpAndNetmaskAreEquals() {
        Assert.assertEquals(Network.fromCidrNotation("10.0.0.0", 8), Network.fromCidrNotation("10.0.0.0", 8));
    }

    @Test
    public void cidrWithDifferentNetworkAndSameNetmaskAreDifferent() {
        Assert.assertFalse(Network.fromCidrNotation("10.0.0.0", 8).equals(Network.fromCidrNotation("11.0.0.0", 8)));
    }

    @Test
    public void cidrWithSameNetworkAndDifferentNetmaskAreDifferent() {
        Assert.assertFalse(Network.fromCidrNotation("10.0.0.0", 8).equals(Network.fromCidrNotation("10.0.0.0", 9)));
    }

    @Test
    public void cidrIsDifferentFromNull() {
        Assert.assertFalse(Network.fromCidrNotation("10.0.0.0", 8).equals(null));
    }

    @Test
    public void cidrIsDifferentFromOtherObjects() {
        Assert.assertFalse(Network.fromCidrNotation("10.0.0.0", 8).equals(new Object()));
    }

    @Test
    public void firstIpYieldsLowerEnd() {
        Assert.assertEquals(Ip.parse("10.0.0.0"), Network.fromCidrNotation("10.0.0.0/8").firstIp());
    }

    @Test
    public void lastIpYieldsUpperEnd() {
        Assert.assertEquals(Ip.parse("10.255.255.255"), Network.fromCidrNotation("10.0.0.0/8").lastIp());
    }
}

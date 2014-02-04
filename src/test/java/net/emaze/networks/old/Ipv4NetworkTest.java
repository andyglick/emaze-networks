package net.emaze.networks.old;

import net.emaze.networks.old.Ipv4Mask;
import net.emaze.networks.old.Ipv4;
import net.emaze.networks.old.Ipv4Network;
import junit.framework.Assert;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;

public class Ipv4NetworkTest {

    @Test
    public void parseYieldsExpectedCidr() {
        final Ipv4Network expected = Ipv4Network.fromCidrNotation(Ipv4.parse("10.0.0.0"), Ipv4Mask.net(8));
        final Ipv4Network got = Ipv4Network.fromCidrNotation("10.0.0.0/8");
        Assert.assertEquals(expected, got);
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingNullYieldsException() {
        Ipv4Network.fromCidrNotation(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void parsingMalformedCidrYieldsException() {
        Ipv4Network.fromCidrNotation("");
    }

    @Test
    public void containsYieldsTrueForNetworkAddress() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.0.0.0")));
    }

    @Test
    public void containsYieldsTrueForLastIpOfNetwork() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.255.255.255")));
    }

    @Test
    public void containsYieldsTrueForIpInsideCidr() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.255.255.254")));
    }

    @Test
    public void containsYieldsFalseForIpOutsideCidr() {
        final Ipv4Network cidr = Ipv4Network.fromCidrNotation("10.0.0.0", 8);
        Assert.assertFalse(cidr.contains(Ipv4.parse("172.16.0.1")));
    }

    @Test
    public void containsYieldsTrueForIncludedNetwork() {
        final Ipv4Network container = Ipv4Network.fromCidrNotation("10.0.0.0/8");
        final Ipv4Network contained = Ipv4Network.fromCidrNotation("10.128.0.0/10");
        Assert.assertTrue(container.contains(contained));
    }

    @Test
    public void containsYieldsFalseForSeparateNetwork() {
        final Ipv4Network container = Ipv4Network.fromCidrNotation("10.0.0.0/8");
        final Ipv4Network contained = Ipv4Network.fromCidrNotation("192.168.0.0/16");
        Assert.assertFalse(container.contains(contained));
    }

    @Test
    public void splitYieldsTwoHalvesOfCidr() {
        final Ipv4Network source = Ipv4Network.fromCidrNotation("192.168.0.0", 24);
        final Pair<Ipv4Network, Ipv4Network> expected = Pair.of(Ipv4Network.fromCidrNotation("192.168.0.0", 25), Ipv4Network.fromCidrNotation("192.168.0.128", 25));
        final Pair<Ipv4Network, Ipv4Network> split = source.split();
        Assert.assertEquals(expected, split);
    }

    @Test(expected = IllegalArgumentException.class)
    public void cannotSplitASingleIpCidr() {
        Ipv4Network.fromCidrNotation("192.168.0.0", 32).split();
    }

    @Test
    public void cidrIsBuiltFromNetworkPartOfIpAddress() {
        final Ipv4Network out = Ipv4Network.byContainedIp(Ipv4.parse("255.255.255.255"), Ipv4Mask.net(24));
        Assert.assertEquals(Ipv4.parse("255.255.255.0"), out.firstIp());
    }

    @Test
    public void cidrFromSameIpAndNetmaskAreEquals() {
        Assert.assertEquals(Ipv4Network.fromCidrNotation("10.0.0.0", 8), Ipv4Network.fromCidrNotation("10.0.0.0", 8));
    }

    @Test
    public void cidrWithDifferentNetworkAndSameNetmaskAreDifferent() {
        Assert.assertFalse(Ipv4Network.fromCidrNotation("10.0.0.0", 8).equals(Ipv4Network.fromCidrNotation("11.0.0.0", 8)));
    }

    @Test
    public void cidrWithSameNetworkAndDifferentNetmaskAreDifferent() {
        Assert.assertFalse(Ipv4Network.fromCidrNotation("10.0.0.0", 8).equals(Ipv4Network.fromCidrNotation("10.0.0.0", 9)));
    }

    @Test
    public void cidrIsDifferentFromNull() {
        Assert.assertFalse(Ipv4Network.fromCidrNotation("10.0.0.0", 8).equals(null));
    }

    @Test
    public void cidrIsDifferentFromOtherObjects() {
        Assert.assertFalse(Ipv4Network.fromCidrNotation("10.0.0.0", 8).equals(new Object()));
    }

    @Test
    public void firstIpYieldsLowerEnd() {
        Assert.assertEquals(Ipv4.parse("10.0.0.0"), Ipv4Network.fromCidrNotation("10.0.0.0/8").firstIp());
    }

    @Test
    public void lastIpYieldsUpperEnd() {
        Assert.assertEquals(Ipv4.parse("10.255.255.255"), Ipv4Network.fromCidrNotation("10.0.0.0/8").lastIp());
    }
}

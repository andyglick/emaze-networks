package net.emaze.networks;

import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;
import junit.framework.Assert;
import org.junit.Test;

public class CidrTest {

    @Test
    public void containsYieldsTrueForNetwork() {
        final Cidr cidr = new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8));
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.0.0.0")));
    }

    @Test
    public void containsYieldsTrueForLastIpOfNetwork() {
        final Cidr cidr = new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8));
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.255.255.255")));
    }

    @Test
    public void containsYieldsTrueForIpInsideCidr() {
        final Cidr cidr = new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8));
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.255.255.254")));
    }

    @Test
    public void containsYieldsFalseForIpOutsideCidr() {
        final Cidr cidr = new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8));
        Assert.assertFalse(cidr.contains(Ipv4.parse("172.16.0.1")));
    }

    @Test
    public void exclude() throws UnknownHostException {
        final Cidr cidr = new Cidr(Ipv4.parse("255.0.0.0"), Netmask.fromBits(30));
        final List<Cidr> got = cidr.exclude(Ipv4.parse("255.0.0.1"));
        final List<Cidr> expected = Arrays.asList(
                new Cidr(Ipv4.parse("255.0.0.0"), Netmask.fromBits(32)),
                new Cidr(Ipv4.parse("255.0.0.2"), Netmask.fromBits(31)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void cidrExtractsNetworkPartOfIpAddress() {
        final Cidr out = new Cidr(Ipv4.parse("255.255.255.255"), Netmask.fromBits(24));
        Assert.assertEquals(out.network(), Ipv4.parse("255.255.255.0"));
    }

    @Test
    public void cidrFromSameIpAndNetmaskAreEquals() {
        Assert.assertEquals(new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8)), new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8)));
    }

    @Test
    public void cidrWithDifferentNetworkAndSameNetmaskAreDifferent() {
        Assert.assertFalse(new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8)).equals(new Cidr(Ipv4.parse("11.0.0.0"), Netmask.fromBits(8))));
    }
    
    @Test
    public void cidrWithSameNetworkAndDifferentNetmaskAreDifferent() {
        Assert.assertFalse(new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8)).equals(new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(9))));
    }

    @Test
    public void cidrIsDifferentFromNull() {
        Assert.assertFalse(new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8)).equals(null));
    }
    
    @Test
    public void cidrIsDifferentFromOtherObjects() {
        Assert.assertFalse(new Cidr(Ipv4.parse("10.0.0.0"), Netmask.fromBits(8)).equals(new Object()));
    }
}

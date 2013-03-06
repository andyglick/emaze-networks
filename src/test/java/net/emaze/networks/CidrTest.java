package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import junit.framework.Assert;
import org.junit.Test;

public class CidrTest {

    @Test
    public void containsYieldsTrueForNetwork() {
        final Cidr cidr = Cidr.parse("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.0.0.0")));
    }

    @Test
    public void containsYieldsTrueForLastIpOfNetwork() {
        final Cidr cidr = Cidr.parse("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.255.255.255")));
    }

    @Test
    public void containsYieldsTrueForIpInsideCidr() {
        final Cidr cidr = Cidr.parse("10.0.0.0", 8);
        Assert.assertTrue(cidr.contains(Ipv4.parse("10.255.255.254")));
    }

    @Test
    public void containsYieldsFalseForIpOutsideCidr() {
        final Cidr cidr = Cidr.parse("10.0.0.0", 8);
        Assert.assertFalse(cidr.contains(Ipv4.parse("172.16.0.1")));
    }
    
    @Test
    public void excludingIpOutsideCidrYieldsCidr() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = cidr.exclude(Ipv4.parse("10.0.0.0"));
        Assert.assertEquals(Arrays.asList(cidr), got);
    }
    
    @Test
    public void excludingAllCidrContentsYieldsEmptyList() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 32);
        final List<Cidr> got = cidr.exclude(Ipv4.parse("255.0.0.0"));
        Assert.assertEquals(Collections.emptyList(), got);
    }

    @Test
    public void excludingFirstIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = cidr.exclude(Ipv4.parse("255.0.0.0"));
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("255.0.0.1", 32),
                Cidr.parse("255.0.0.2", 31));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void excludingMiddleIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = cidr.exclude(Ipv4.parse("255.0.0.2"));
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("255.0.0.0", 31),
                Cidr.parse("255.0.0.3", 32));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = cidr.exclude(Ipv4.parse("255.0.0.3"));
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("255.0.0.0", 31),
                Cidr.parse("255.0.0.2", 32));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void cidrExtractsNetworkPartOfIpAddress() {
        final Cidr out = Cidr.parse("255.255.255.255", 24);
        Assert.assertEquals(out.network(), Ipv4.parse("255.255.255.0"));
    }

    @Test
    public void cidrFromSameIpAndNetmaskAreEquals() {
        Assert.assertEquals(Cidr.parse("10.0.0.0", 8), Cidr.parse("10.0.0.0", 8));
    }

    @Test
    public void cidrWithDifferentNetworkAndSameNetmaskAreDifferent() {
        Assert.assertFalse(Cidr.parse("10.0.0.0", 8).equals(Cidr.parse("11.0.0.0", 8)));
    }
    
    @Test
    public void cidrWithSameNetworkAndDifferentNetmaskAreDifferent() {
        Assert.assertFalse(Cidr.parse("10.0.0.0", 8).equals(Cidr.parse("10.0.0.0", 9)));
    }

    @Test
    public void cidrIsDifferentFromNull() {
        Assert.assertFalse(Cidr.parse("10.0.0.0", 8).equals(null));
    }
    
    @Test
    public void cidrIsDifferentFromOtherObjects() {
        Assert.assertFalse(Cidr.parse("10.0.0.0", 8).equals(new Object()));
    }
}

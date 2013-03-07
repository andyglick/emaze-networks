package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import junit.framework.Assert;
import net.emaze.dysfunctional.tuples.Pair;
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
    public void splitYieldsTwoHalvesOfCidr() {
        final Cidr source = Cidr.parse("192.168.0.0", 24);
        final Pair<Cidr, Cidr> expected = Pair.of(Cidr.parse("192.168.0.0", 25), Cidr.parse("192.168.0.128", 25));
        final Pair<Cidr, Cidr> split = source.split();
        Assert.assertEquals(expected, split);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void cannotSplitASingleIpCidr() {
        Cidr.parse("192.168.0.0", 32).split();
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

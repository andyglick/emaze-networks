package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import junit.framework.Assert;
import org.junit.Test;

public class SubtractFromCidrTest {
    public static final SubtractIpFromCidr subtractor = new SubtractIpFromCidr();
    
    @Test
    public void excludingIpOutsideCidrYieldsCidr() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final Set<Cidr> got = subtractor.perform(cidr, Ipv4.parse("10.0.0.0"));
        Assert.assertEquals(Collections.singleton(cidr), got);
    }
    
    @Test
    public void excludingAllCidrContentsYieldsEmptySet() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 32);
        final Set<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.0"));
        Assert.assertEquals(Collections.emptySet(), got);
    }

    @Test
    public void excludingFirstIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final Set<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.0"));
        final Set<Cidr> expected = new HashSet<>(Arrays.asList(
                Cidr.parse("255.0.0.1", 32),
                Cidr.parse("255.0.0.2", 31)));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void excludingMiddleIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final Set<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.2"));
        final Set<Cidr> expected = new HashSet<>(Arrays.asList(
                Cidr.parse("255.0.0.0", 31),
                Cidr.parse("255.0.0.3", 32)));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final Set<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.3"));
        final Set<Cidr> expected = new HashSet<>(Arrays.asList(
                Cidr.parse("255.0.0.0", 31),
                Cidr.parse("255.0.0.2", 32)));
        org.junit.Assert.assertEquals(expected, got);
    }

}

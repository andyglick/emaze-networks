package net.emaze.networks;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import junit.framework.Assert;
import org.junit.Test;

public class SubtractFromCidrTest {
    public static final SubtractIpFromCidr subtractor = new SubtractIpFromCidr();
    
    @Test
    public void excludingIpOutsideCidrYieldsCidr() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = subtractor.perform(cidr, Ipv4.parse("10.0.0.0"));
        Assert.assertEquals(Arrays.asList(cidr), got);
    }
    
    @Test
    public void excludingAllCidrContentsYieldsEmptyList() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 32);
        final List<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.0"));
        Assert.assertEquals(Collections.emptyList(), got);
    }

    @Test
    public void excludingFirstIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.0"));
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("255.0.0.1", 32),
                Cidr.parse("255.0.0.2", 31));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void excludingMiddleIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.2"));
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("255.0.0.0", 31),
                Cidr.parse("255.0.0.3", 32));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void excludingLastIpYieldsExpected() {
        final Cidr cidr = Cidr.parse("255.0.0.0", 30);
        final List<Cidr> got = subtractor.perform(cidr, Ipv4.parse("255.0.0.3"));
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("255.0.0.0", 31),
                Cidr.parse("255.0.0.2", 32));
        org.junit.Assert.assertEquals(expected, got);
    }

}
